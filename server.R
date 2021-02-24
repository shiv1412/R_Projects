#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$coolplot <- renderPlot({
        filtered <-
            bcl_file %>%
            filter(Price >= input$priceInput[1],
                   Price <= input$priceInput[2],
                   Type ==input$typeInput,
                   Country == input$countryInput)
        
        ggplot(filtered,aes(Alcohol_Content))+geom_histogram()
        

    })

})


shinyApp(ui=ui,server=server)
