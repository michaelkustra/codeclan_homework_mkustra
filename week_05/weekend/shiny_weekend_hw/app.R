library(shiny)
library(DT)
library(tidyverse)


ui <- fluidPage(

    
    titlePanel("🎮👾🔫 MK Games Dashboard 🔫👾🎮"),

    
    fluidPage(
      fluidRow(
        column(2,
               selectInput("developer_input",
                    "Choose Developer",
                    choices = unique(games_data$developer))
                    
        ),
        
        column(2,
               selectInput("platform_input",
                    "Choose Platform",
                    choices = unique(games_data$platform))
                    
        ),
        
        column(2,
               selectInput("genre_input",
                    "Choose Genre",
                    choices = unique(games_data$genre))
                    
        ),
        
        column(2,
               selectInput(
                 "year_input",
                 "Highlight Year of Release",
                 choices = unique(games_data$year_of_release)
               )),
        
        column(3, 
               actionButton("update",
                     "Refresh Selection"))
      ),
         

        # Show a plot of the generated distribution
        fluidRow(
           column(6, 
                  plotOutput("games_plot")),
      
      
          column(6,
                 plotOutput("cor_plot"))
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
    
    output$cor_plot <- renderPlot({
      
      filtered_data() %>% 
        ggplot() +
        aes(x = critic_score, y = user_score, fill = input$developer_input) +
        geom_point()
    })
}

shinyApp(ui = ui, server = server)
