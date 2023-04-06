## ----libraries ------------------------------------------------------------------------------------------------
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(rsconnect)
library(shinyWidgets)

## ----importing data -------------------------------------------------------------------------------------------
dashboard_indicators <- read.csv("db_indicators.csv")
dashboard_sectors <- read.csv("db_sectors.csv")

## ----Calculating mean indicator value--------------------------------------------------------------------------
dashboard_indicators <- dashboard_indicators %>% 
  group_by(indicator) %>% 
  mutate(mean_value = mean(value))

## ----User Interface -------------------------------------------------------------------------------------------
# ui <- fluidPage(
# 
#   titlePanel("Similarity Index Indicators"),
#   fluidRow(
#     box(
#       title = "Table",
#       width = 6,
#       status = "primary",
#       solidHeader = TRUE,
#       # selectInput("county", "Select a County:",
#       #             choices = unique(dashboard_indicators$County),
#       #             selected = "Philadelphia"),
#       # selectInput("indicator", "Select an Economic Indicator:",
#       #             choices = unique(dashboard_indicators$indicator)),
#       
#       selectizeInput("county", "Select a County:",
#                      choices = unique(dashboard_indicators$County),
#                      selected = "Philadelphia",
#                      options = list(maxOptions = 3000)),
#       
#       selectizeInput("indicator", "Select an Economic Indicator:",
#                      choices = unique(dashboard_indicators$indicator),
#                      options = list(maxOptions = 1000)),
#       
#       tableOutput("table")
#     ),
#     box(
#       title = "Pie Chart",
#       width = 6,
#       status = "primary",
#       solidHeader = TRUE,
#       
#       # selectInput("county_piechart", "Select County:",
#       #             choices = unique(dashboard_sectors$County),
#       #             selected = "Philadelphia"),
#       
#       selectizeInput("county_piechart", "Select County:",
#                      choices = unique(dashboard_sectors$County),
#                      selected = "Philadelphia",
#                      options = list(maxOptions = 3000)),
#       
#       plotlyOutput("piechart")
#     )
#   )
# )
# 
# ## ----Server------- -------------------------------------------------------------------------------------------
# server <- function(input, output) {
# 
#   output$table <- renderTable({
#     data_filtered <- dashboard_indicators %>%
#       filter(County == input$county & indicator == input$indicator)
# 
#     data_display <- data.frame(Value = data_filtered$value,
#                                Mean = data_filtered$mean_value)
# 
#     colnames(data_display) <- c("Value", "Mean")
# 
#     data_display
#   })
# 
# 
#   output$piechart <- renderPlotly({
#     filtered_data <- filter(dashboard_sectors, County == input$county_piechart)
# 
#     plot_ly(filtered_data, labels = ~sector, values = ~size, type = 'pie') %>%
#       layout(title = paste0("Economic Sectors in ", input$county_piechart),
#              showlegend = TRUE,
#              margin = list(t = 70), # increase top margin
#              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
#   })
# 
# }
# 
# ## ----App---------------------------------------------------------------------------------------------------
# shinyApp(ui = ui, server = server)


## ----User Interface -------------------------------------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("Similarity Index Indicators"),
  fluidRow(
    box(
      title = "Table",
      width = 6,
      status = "primary",
      solidHeader = TRUE,
      selectizeInput("county", "Select a County:",
                     choices = NULL,
                     selected = "Philadelphia",
                     options = list(
                       placeholder = 'Choose a County',
                       create = FALSE,
                       preload = TRUE
                     )),
      selectizeInput("indicator", "Select an Economic Indicator:",
                     choices = NULL,
                     options = list(
                       placeholder = 'Choose an Economic Indicator',
                       create = FALSE,
                       preload = TRUE
                     )),
      tableOutput("table")
    ),
    box(
      title = "Pie Chart",
      width = 6,
      status = "primary",
      solidHeader = TRUE,
      selectizeInput("county_piechart", "Select County:",
                     choices = NULL,
                     selected = "Philadelphia",
                     options = list(
                       placeholder = 'Choose a County',
                       create = FALSE,
                       preload = TRUE
                     )),
      plotlyOutput("piechart")
    )
  )
)

## ----Server------- -------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # create reactive data for the county, indicator and piechart dropdown menus
  county_choices <- reactive({
    choices <- unique(dashboard_indicators$County)
    choices
  })
  
  indicator_choices <- reactive({
    choices <- unique(dashboard_indicators$indicator)
    choices
  })
  
  county_piechart_choices <- reactive({
    choices <- unique(dashboard_sectors$County)
    choices
  })
  
  # update selectizeInput choices based on user input
  observeEvent(input$county, {
    updateSelectizeInput(session, "indicator", choices = indicator_choices())
  })
  
  observeEvent(input$county_piechart, {
    updateSelectizeInput(session, "county_piechart", choices = county_piechart_choices())
  })
  
  # update selectizeInput choices at app start
  observe({
    updateSelectizeInput(session, "county", choices = county_choices())
    updateSelectizeInput(session, "indicator", choices = indicator_choices())
    updateSelectizeInput(session, "county_piechart", choices = county_piechart_choices())
  })
  
  output$table <- renderTable({
    data_filtered <- dashboard_indicators %>%
      filter(County == input$county & indicator == input$indicator)
    
    data_display <- data.frame(Value = data_filtered$value,
                               Mean = data_filtered$mean_value)
    
    colnames(data_display) <- c("Value", "Mean")
    
    data_display
  })
  
  
  output$piechart <- renderPlotly({
    filtered_data <- filter(dashboard_sectors, County == input$county_piechart)
    
    plot_ly(filtered_data, labels = ~sector, values = ~size, type = 'pie') %>%
      layout(title = paste0("Economic Sectors in ", input$county_piechart),
             showlegend = TRUE,
             margin = list(t = 70), # increase top margin
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  })

}

shinyApp(ui = ui, server = server)
                          


