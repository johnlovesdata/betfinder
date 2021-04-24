
# global ------------------------------------------------------------------

# libraries
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(reactable)

# load data
dash_data <- readRDS("dash_data.rds")

# make a function
fmt_plus <- function(value, digits) {
    if (value >= 0) {
      paste0("+", round(value, digits))
    } else
      if (value < 0) {
        paste0(round(value, digits))
      } else {
        paste0('')
      }
}


# ui ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel('Props Dash'),
  br(),
  fluidRow(
    # select a bunch of stuff
    column(width = 3,
           pickerInput('sport', 'Sport',
                       choices = sort(unique(dash_data$sport)),
                       selected = sort(unique(dash_data$sport)),
                       multiple = TRUE))),
  fluidRow(
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
                       multiple = TRUE)),
    column(width = 3,
           pickerInput('prop', 'Prop',
                       options = list(`actions-box` = TRUE,
                                      `live-search` = TRUE),
                       choices = sort(unique(dash_data$prop)),
                       selected = c('first player to score', 'first team to score'),
                       multiple = TRUE)),
    column(width = 2,
           pickerInput('ou', 'Over/Under',
                       options = list(`actions-box` = TRUE,
                                      `live-search` = TRUE),
                       choices = c('over', 'under', 'N/A'),
                       selected = c('over', 'under', 'N/A'),
                       multiple = TRUE))),
  fluidRow(
    column(width = 10,
           reactableOutput('prop_table'))
  ))

# server ------------------------------------------------------------------

server <- function(input, output) {

  #### get data for table ####

  table_data <- reactive({
    td <- dash_data %>%
      filter(sport == input$sport,
             if_else(is.na(tidyplayer), TRUE, tidyplayer %in% input$player),
             prop %in% input$prop,
             tidyteam %in% input$team,
             tidyou %in% input$ou) %>%
      select(
        sport,
        tidyplayer,
        tidyteam,
        prop,
        tidyou,
        tidyline,
        count_odds,
        draftkings,
        fanduel,
        pointsbet,
        mean_odds
      )

    })

  #### make the reactable ####
  output$prop_table <- renderReactable({

    validate(need(nrow(table_data()) > 0, "waiting for input..."))

    reactable(
      table_data(),
      pagination = FALSE,
      rownames = FALSE,
      sortable = TRUE,
      columns = list(
        sport = colDef(show = FALSE),
        tidyplayer = colDef(name = "Player", sortNALast = TRUE, minWidth = 125),
        tidyteam = colDef(name = "Team", sortNALast = TRUE, minWidth = 50),
        prop = colDef(name = "Prop", sortNALast = TRUE, minWidth = 125),
        tidyou = colDef(name = "OU",
                        sortNALast = TRUE,
                        minWidth = 50),
        tidyline = colDef(name = "Line",
                          format = colFormat(digits = 1),
                          sortNALast = TRUE,
                          minWidth = 50),
        count_odds = colDef(show = FALSE),
        draftkings = colDef(name = "Draftkings",
                            sortNALast = TRUE,
                            format = colFormat(digits = 0)),
        fanduel = colDef(name = "Fanduel",
                         sortNALast = TRUE,
                         format = colFormat(digits = 0)),
        pointsbet = colDef(name = "PointsBet",
                           sortNALast = TRUE,
                           format = colFormat(digits = 0)),
        mean_odds = colDef(name = "AvgOdds",
                           sortNALast = TRUE,
                           format = colFormat(digits = 1))),
      columnGroups = list(
        colGroup(name = "Odds", columns = c("draftkings", "fanduel", "pointsbet", "mean_odds"))
      ))

  })

}

# Create Shiny object
shinyApp(ui = ui, server = server)
