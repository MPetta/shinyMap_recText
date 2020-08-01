#load libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(RColorBrewer)

#import data
data <- read.csv("dietFull.csv")
data$cuisine <- as.factor(data$cuisine)   

#categorize earthquake depth
data$ob_level <- ifelse(data$adult_obesity_rate <= 10, "Low", 
                        ifelse(data$adult_obesity_rate <= 20 | data$adult_obesity_rate > 10, "Medium", 
                               ifelse(data$adult_obesity_rate > 20, "High", "other")))

ui <- bootstrapPage(
    tags$head(tags$style(type = "text/css", "html, body {width:100%;height:100%}",
               HTML('#sidebar {opacity : .65}', '#sidebar1 {opacity : .65}'))),
    leafletOutput(outputId = "mymap", width = "100%", height = "100%"),
    absolutePanel(top = 7, right = 1150, style="z-index:500;",
                  checkboxInput("markers", "Levels", TRUE),
                  checkboxInput("heat", "Heatmap", FALSE)),
                  selectInput(inputId = "cuisineType",
                              label = "Select Cuisine",
                              choices = levels(data$cuisine)),
    
    absolutePanel(id = "sidebar1", class = "panel panel-default", top = 10, right = 475, style="z-index:600;",
                  p(""),
                  tags$i("Select from the dropdown below which cuisine you are interested in."),
                  selectInput(inputId = "cuisineType2",
                              label = "Select Cuisine",
                              choices = levels(data$cuisine)), align = "center",
                  p("")),
    
    absolutePanel(id = "sidebar", class = "panel panel-default", top = 10, right = 10, style="z-index:500;", fixed=TRUE, draggable = TRUE,
                  p(""),
                  h4("Cuisine Type:"),
                  textOutput("selected_var"), align = "center",
                  p(""),
                  h4("5 Most Used Ingredients:"),
                  textOutput("ing"),
                  p(""),
                  p(""),
                  plotOutput("obesePlot",  width = "100%"),
                  tags$br(),
                  p("Source:"),
                  tags$a(href="https://www.cia.gov/library/publications/resources/the-world-factbook/", "https://www.cia.gov/library/publications/resources/the-world-factbook/"),
                  p(""),
                  tags$a(href="https://www.kaggle.com/kaggle/recipe-ingredients-dataset/data", "https://www.kaggle.com/kaggle/recipe-ingredients-dataset/data"),
                  p("")
    )
)

# Define server logic required to draw map
server <- function(input, output, session) {
    
    #define the color pallate for the magnitidue of the earthquake
    pal <- colorNumeric(
        palette = c('darkorchid1', 'darkorchid4', 'dark orange', 'orange red', 'red', 'dark red'),
        domain = data$adult_obesity_rate
    )
    
    #define the color of for the depth of the earquakes
    pal2 <- colorFactor(
        palette = c('darkorchid1', 'dark orange', 'dark red'),
        domain = data$ob_level
    )
    
    output$selected_var <- renderText({ 
        paste(input$cuisineType2)})
    
    output$ing <- renderText({ 
        paste( data$ingredients[data$cuisine == input$cuisineType2])})
    
    output$obesePlot <- renderPlot({
        barplot(data$adult_obesity_rate, main="Adult Obesity Rates by Cuisine", horiz=FALSE, col=("#69b3a2"),
                names.arg=c("Brazil", "Britian", "USA Cajun", "China", "Filipino", "French", "Greek", "Indian", "Irish", "Italian", "Jamaica", "Japan", "Korea", "Mexico", "Morocco", "Russia", "USA South", "Spanish", "Thai", "Vietnam"),
                las = 2) })
        
    output$expensePlot <- renderPlot({
        barplot(data$health_expenditures, main="Health Expenditures by Country", horiz=FALSE, col=("#69b3a2"),
                names.arg=c("Brazil", "Britian", "USA Cajun", "China", "Filipino", "French", "Greek", "Indian", "Irish", "Italian", "Jamaica", "Japan", "Korea", "Mexico", "Morocco", "Russia", "USA South", "Spanish", "Thai", "Vietnam"),
                las = 2) })
    
    #create the map
    output$mymap <- renderLeaflet({
        leaflet(data) %>% 
            setView(lng = 95, lat = 21, zoom = 2.4)  %>% #setting the view over ~ center of North America
            addTiles() %>% 
            addCircles(data = data, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~sqrt(adult_obesity_rate)*70000, popup = ~as.character(adult_obesity_rate), label = ~as.character(paste0("Adult Obesity Rate: ", sep = " ", adult_obesity_rate)), color = ~pal(adult_obesity_rate), fillOpacity = 0.5) %>%
            addProviderTiles(providers$CartoDB.PositronNoLabels)
    })
    
    #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
    observe({
        proxy <- leafletProxy("mymap", data = data)
        proxy %>% clearMarkers()
        if (input$markers) {
            proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(ob_level), fillOpacity = 0.2,
                                       label = ~as.character(paste0("Adult Obesity Rate: ", sep = " ", adult_obesity_rate))) %>%
                addLegend("bottomright", pal = pal2, values = data$ob_level,
                          title = "Level of Obesity",
                          opacity = 1)}
        else {
            proxy %>% clearMarkers() %>% clearControls()
        }
    })
    
    observe({
        proxy <- leafletProxy("mymap", data = data)
        proxy %>% clearMarkers()
        if (input$heat) {
            proxy %>%  addHeatmap(lng=~longitude, lat=~latitude, intensity = ~adult_obesity_rate, blur =  10, max = 0.05, radius = 15) 
        }
        else{
            proxy %>% clearHeatmap()
        }
        
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
