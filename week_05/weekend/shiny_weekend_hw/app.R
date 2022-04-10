library(shiny)
library(DT)
library(tidyverse)


ui <- fluidPage(
  
  
  titlePanel("🎮👾🔫 MK Games Dashboard 🔫👾🎮"),
  
  
  tabsetPanel(
    tabPanel(
      "Games Data Table",
      dataTableOutput("table_output")
    ),
    
    tabPanel(
      "User Score Filter",
      plotOutput("user_plot"),
      
      column(3, 
             selectInput("genre_input",
                         "Select Genre",
                         choices = unique(games_data$genre))
      ),
      
      column(3, 
             selectInput(
               "developer_input",
               "Select Developer",
               choices = unique(games_data$developer))
      ),
      
      column(3,
             selectInput("platform_input",
                         "Select Platform",
                         choices = unique(games_data$platform))
      ),
      
      
      column(3, 
             actionButton("update",
                          "Refresh Plot")
             
      )),
    
    tabPanel(
      "Game Sales by Year",
      plotOutput("games_plot"),
      selectInput(
        "year_input",
        "Highlight Year of Release",
        choices = unique(games_data$year_of_release)
      )
    ),
    
    tabPanel(
      "South Park",
      tags$a("Gamer gif", href = 
               "https://giphy.com/gifs/southpark-comedy-central-10x08-xTiTnwgQ8Wjs1sUB4k/tile")
    )
  )
)



server <- function(input, output) {
  
  filtered_data <- eventReactive(input$update, {
    games_data %>% 
      filter(developer == input$developer_input) %>% 
      filter(platform == input$platform_input) %>% 
      filter(genre == input$genre_input)
    
  })
  
  output$user_plot <- renderPlot({
    
    filtered_data() %>% 
      ggplot() +
      aes(x = name, y = user_score) +
      geom_col(colour = "grey10", fill = "pink", show.legend = F) +
      coord_flip()+
      labs(title = "User Score Filter",
           y = "User Score",
           x = "Game") +
      theme_bw()
    
  })
  
  output$table_output <- renderDataTable({
    
    games_data
    
  })
  
  output$games_plot <- renderPlot({
    
    games_data %>%
      mutate(year_select = if_else(
        year_of_release == input$year_input, "yes", "no")) %>% 
      ggplot() +
      aes(x = year_of_release, y = sales) +
      geom_col(aes(fill = year_select), show.legend = F) +
      labs(title = "Total Game Sales by Year",
           x = "Year of Release",
           y = "Total Sales") +
      theme_bw()
  })
}

shinyApp(ui = ui, server = server)
