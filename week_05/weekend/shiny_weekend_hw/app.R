library(shiny)
library(DT)
library(tidyverse)



ui <- fluidPage(

    
    titlePanel("🎮👾🔫 MK Games Dashboard 🔫👾🎮"),

    
    sidebarLayout(
        sidebarPanel(
          selectInput("developer_input",
                      "Choose Developer",
                      choices = unique(games_data$developer)
            
          ),
          
          selectInput("platform_input",
                      "Choose Platform",
                      choices = unique(games_data$platform)
            
          ),
          
          selectInput("genre_input",
                      "Choose Genre",
                      choices = unique(games_data$genre)
            
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("games_plot")
        )
    )
)

server <- function(input, output) {
  
  filtered_data <- eventReactive({
    games_data %>% 
      filter(developer == input$developer_input) %>% 
      filter(platform == input$platform_input) %>% 
      filter(genre == input$genre_input)
  })

    output$games_plot <- renderPlot({
      
      filtered_data %>% 
      ggplot() +
        aes(x = critic_score, y = user_score) +
        geom_point()
      
    })
}

shinyApp(ui = ui, server = server)
