#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

getwd()
setwd("C:/Users/sharm/OneDrive/Desktop")
bcl_file <- read.table(file="bcl-data.csv", sep="," , header=TRUE)
head(bcl_file)
attach(bcl_file)
bcl_file

# Define UI for application that draws a histogram
ui <- fluidPage (
    
    # Application title
    titlePanel("BC Liquor Store Prices"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
        sliderInput("priceInput", "Price",0,100,c(25,40),pre="$"),
        radioButtons("typeInput","Product type",choices= c("BEER","REFRESHMENT","SPIRITS","WINE"),selected = "WINE"),
        selectInput("countryInput","Country",choices=c("CANADA","FRANCE","ITALY"))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("coolplot"),
            br(),br(),
            tableOutput("results")
        )
    )
)
