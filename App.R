#libraries import
library(shiny) 
library(ggplot2)
library(dplyr)
library(plotly)
library(odbc)
library(DBI)
#selecting the working directory
getwd()
setwd("D:/Data Visualization")
#reading the data set
master_file <- read.csv("Master_data_set.csv", stringsAsFactors = FALSE)
master_file
print(str(master_file))
data(master_file)
#preparing a data frame for using in the ui and server logic
df1<-  sort(unique(master_file$Year))
print(df1)

#ui logic for reading the data set parameters
ui <- fluidPage(
  #designing the UI for dashboard
  titlePanel("Energy Consumption VS CO2 Emission"),
  sidebarLayout(sidebarPanel(
    uiOutput("YearOutput")
  ),
  mainPanel( 
    #creating different panels for each dashboard
    tabsetPanel(
      tabPanel("PrimaryForest Area",plotOutput("coolplot1")),
      tabPanel("NaturalGas Production Per person",plotOutput("coolplot2")),
      tabPanel("co2 emissions perperson",plotOutput("coolplot3"))
    )
    
  )
  )
)

# logic for server part 
server <- function(input, output) {
  output$YearOutput <- renderUI({ selectInput("YearInput", "Year",df1, selected = "2010")
  })
  #adding filtering to each input parameter
  filtered <- reactive({
    if (is.null(input$YearInput)) { return(NULL)
    }
    
    master_file %>%
      filter(Year >= input$YearInput
      )
  })
  
  # plotting for each dashboard
  output$coolplot1 <- renderPlot({ if (is.null(filtered())) {
    return()
  }
    
    ggplot(filtered(), aes(Region,Primary.Forst.Area)) + geom_bar(stat="identity",width=0.7,fill="blue")
  })
  
  output$coolplot2 <- renderPlot({ if (is.null(filtered())) {
    return()
  }
    
    ggplot(filtered(), aes(Region,Natural.Gas.Production.per.person)) + geom_point(stat="identity",width=0.7,fill="blue")
    
  })
  
  
  output$coolplot3 <- renderPlot({ if (is.null(filtered())) {
    return()
  }
    
    ggplot(filtered(), aes(Region,co2.emissions.tonnes.per.person)) + geom_histogram(stat="identity",width=0.7,fill="blue")
  })
  
}
# invoking the ui and server 
shinyApp(ui = ui, server = server)
