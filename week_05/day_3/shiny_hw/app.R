#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(shinythemes)

scot_export_data <- CodeClanData::scottish_exports
all_sectors <- scot_export_data$sector


ui <- fluidPage(
  
  
  titlePanel(tags$h2("Scottish Exports by Sector: 2002-2017")),
  
  theme = shinytheme("yeti"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
          "year_input",
          tags$i("Export Year"),
          choices = c("2002", "2003", "2004", "2005", "2006", "2007", "2008",
                    "2009", "2010", "2011", "2012", "2013", "2014", "2016",
                    "2017")
    ),
    
    HTML("<br><br>"),
      
    selectizeInput(
      "sector_input",
      tags$i("Sector Area"),
      choices = c("Agriculture_Forestry_Fishing", "Mining_Quarrying",
                  "Manufacturing", "Utilities", "Construction", "Services")
    )
  ),
  
      mainPanel(
        plotOutput("export_Plot"),
        tags$a("Scot Gov Export Statistics", href = 
                 "https://www.gov.scot/collections/export-statistics/")
      )
  )
)


server <- function(input, output) {

    output$export_Plot <- renderPlot({
      
      scot_export_data %>%
        #filter(year == input$year_input) %>%
        filter(sector == input$sector_input) %>%
        ggplot()+
        aes(x = year, y = exports, fill = sector) +
        geom_col(fill = "steel blue", colour = "black")


})
}

shinyApp(ui = ui, server = server)
