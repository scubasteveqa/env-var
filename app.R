library(shiny)
library(bslib)
library(reactable)

ui <- page_sidebar(
  title = "Selected Environment Variables",
  sidebar = sidebar(
    title = "Options",
    selectInput("sort_order", "Sort by",
               choices = c("Name (A-Z)" = "name_asc", 
                           "Name (Z-A)" = "name_desc",
                           "Value (A-Z)" = "value_asc", 
                           "Value (Z-A)" = "value_desc"),
               selected = "name_asc")
  ),
  card(
    card_header("R_CONFIG_ACTIVE and QUARTO_PROFILE Environment Variables"),
    reactableOutput("env_vars_table")
  )
)

server <- function(input, output, session) {
  
  # Get filtered environment variables
  filtered_env_vars <- reactive({
    # Get all environment variables
    env_vars <- as.list(Sys.getenv())
    
    # Create a data frame with only the variables we want
    env_df <- data.frame(
      Variable = c("R_CONFIG_ACTIVE", "QUARTO_PROFILE"),
      Value = c(env_vars[["R_CONFIG_ACTIVE"]], env_vars[["QUARTO_PROFILE"]]),
      stringsAsFactors = FALSE
    )
    
    # Apply sorting
    if (input$sort_order == "name_asc") {
      env_df <- env_df[order(env_df$Variable), ]
    } else if (input$sort_order == "name_desc") {
      env_df <- env_df[order(env_df$Variable, decreasing = TRUE), ]
    } else if (input$sort_order == "value_asc") {
      env_df <- env_df[order(env_df$Value), ]
    } else if (input$sort_order == "value_desc") {
      env_df <- env_df[order(env_df$Value, decreasing = TRUE), ]
    }
    
    return(env_df)
  })
  
  # Render the table
  output$env_vars_table <- renderReactable({
    reactable(
      filtered_env_vars(),
      striped = TRUE,
      highlight = TRUE,
      columns = list(
        Variable = colDef(minWidth = 150),
        Value = colDef(minWidth = 300)
      )
    )
  })
}

shinyApp(ui = ui, server = server)
