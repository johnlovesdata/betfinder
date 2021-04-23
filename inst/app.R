
# global ------------------------------------------------------------------

# libraries
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(reactable)

# load data
dash_data <- readRDS("dash_data.rds")

# ui ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel('Props Dash'),
  br(),
  fluidRow(
    # select a bunch of stuff
    column(width = 1,
           pickerInput('sport', 'Sport',
                       choices = sort(unique(dash_data$sport)),
                       selected = sort(unique(dash_data$sport)))),
    column(width = 3,
           pickerInput('player', 'Player',
                       options = list(`actions-box` = TRUE,
                                      `live-search` = TRUE),
                       choices = sort(unique(dash_data$tidyplayer)),
                       selected = sort(unique(dash_data$tidyplayer)),
                       multiple = TRUE)),
    column(width = 2,
           pickerInput('team', 'Team',
                       options = list(`actions-box` = TRUE,
                                      `live-search` = TRUE),
                       choices = sort(unique(dash_data$tidyteam)),
                       selected = sort(unique(dash_data$tidyteam)),
                       multiple = TRUE))),
  fluidRow(
    column(width = 5,
           pickerInput('prop', 'Prop',
                       options = list(`actions-box` = TRUE,
                                      `live-search` = TRUE),
                       choices = sort(unique(dash_data$prop)),
                       selected = sort(unique(dash_data$prop)),
                       multiple = TRUE))),
  fluidRow(
    column(width = 12,
           reactableOutput('prop_table'))
  ))

# server ------------------------------------------------------------------

server <- function(input, output) {

  #### set plot themes ####
  # theme_set(theme_minimal(base_size = 12))
  # theme_update(legend.position = 'none',
  #              legend.title = element_blank())

  #### get data for table ####
  table_data <- reactive({
    td <- dash_data %>%
      filter(sport == input$sport,
             tidyplayer %in% input$player,
             prop %in% input$prop,
             tidyteam %in% input$team)
    # remove columns that are all NA from table
    col_remover <- apply(td, 2, function(x) all(is.na(x)))
    td <- td[, !col_remover]
    td
    })

  #### make the reactable ####
  output$prop_table <- renderReactable({


    reactable(
      table_data(),
      pagination = FALSE,
      rownames = FALSE)

  })

}

# Create Shiny object
shinyApp(ui = ui, server = server)
