# global ----
## libraries
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(reactable)
## load data
search_props_raw <- readRDS("props.rds")

# ui ----
ui <- fluidPage(
  titlePanel('Props Dash'),
  br(),
  fluidRow(
    # select a bunch of stuff
    column(width = 2,
           pickerInput('sport', 'Sport',
                       choices = sort(unique(search_props_raw$sport)),
                       selected = sort(unique(search_props_raw$sport)),
                       multiple = TRUE))),
  fluidRow(
    column(width = 2,
           pickerInput('player', 'Player',
                       options = list(`actions-box` = TRUE,
                                      `live-search` = TRUE),
                       choices = sort(unique(search_props_raw$tidyplayer)),
                       selected = sort(unique(search_props_raw$tidyplayer)),
                       multiple = TRUE)),
    column(width = 2,
           pickerInput('team', 'Team',
                       options = list(`actions-box` = TRUE,
                                      `live-search` = TRUE),
                       choices = sort(unique(search_props_raw$tidyteam)),
                       selected = sort(unique(search_props_raw$tidyteam)),
                       multiple = TRUE)),
    column(width = 2,
           pickerInput('prop', 'Prop',
                       options = list(`actions-box` = TRUE,
                                      `live-search` = TRUE),
                       choices = sort(unique(search_props_raw$prop)),
                       selected = sort(unique(search_props_raw$prop)),
                       # selected = c('first player to score', 'first team to score'),
                       multiple = TRUE)),
    column(width = 2,
           pickerInput('ou', 'Over/Under',
                       options = list(`actions-box` = TRUE,
                                      `live-search` = TRUE),
                       choices = c('over', 'under', 'N/A'),
                       selected = c('over', 'under', 'N/A'),
                       multiple = TRUE)),
    column(width = 2,
           pickerInput('count_books', 'Minimum Books',
                       choices = sort(unique(search_props_raw$count_books)),
                       selected = 1))),
  br(),
  tabsetPanel(
    tabPanel('Search Props',
             br(),
             fluidRow(
               column(width = 12,
                      reactableOutput('prop_table')))),
    tabPanel('Evaluate',
             br(),
             fluidRow(
               column(width = 12)
             ))))

# server ------------------------------------------------------------------

server <- function(input, output) {
  #### get data for table ####

  search_props_data <- reactive({
    td <- search_props_raw %>%
      filter(
        sport == input$sport,
        if_else(is.na(tidyplayer), TRUE, tidyplayer %in% input$player),
        prop %in% input$prop,
        tidyteam %in% input$team,
        tidyou %in% input$ou,
        count_books >= input$count_books
      ) %>%
      select(
        sport,
        tidyplayer,
        tidyteam,
        home_away,
        tidyopp,
        prop,
        tidyou,
        tidyline,
        count_books,
        best_odds,
        draftkings,
        fanduel,
        pointsbet,
        mean_odds,
        next_best_ratio
      )
  })

  #### make the reactable ####
  output$prop_table <- renderReactable({
    validate(need(nrow(search_props_data()) > 0, "waiting for input..."))

    reactable(
      search_props_data(),
      pagination = FALSE,
      rownames = FALSE,
      sortable = TRUE,
      columns = list(
        sport = colDef(show = FALSE),
        tidyplayer = colDef(
          name = "Player",
          sortNALast = TRUE,
          minWidth = 125
        ),
        tidyteam = colDef(
          name = "Team",
          sortNALast = TRUE,
          width = 60
        ),
        home_away = colDef(
          name = "",
          cell = function(value) {
            if (grepl('home', value)) 'vs'
            else '@'
          },
          style = list(color = 'gray'),
          width = 45
        ),
        tidyopp = colDef(
          name = "Opp",
          width = 60
        ),
        prop = colDef(
          name = "Prop",
          sortNALast = TRUE,
          minWidth = 125
        ),
        tidyou = colDef(
          name = "OU",
          sortNALast = TRUE,
          style = function(value, index) {
            if (value == 'N/A') list(color = "lightgray", fontStyle = "italic")
            else return()
          },
          minWidth = 50
        ),
        tidyline = colDef(
          name = "Line",
          sortNALast = TRUE,
          cell = function(value) {
            if (is.na(value)) 'N/A'
            else if (value == '') 'N/A'
            else if (value > 0) as.character(round(value, 1))
            else if (value <= 0) as.character(round(value, 1))
            else TRUE
          },
          style = function(value, index) {
            if (is.na(value)) list(color = "lightgray", fontStyle = "italic")
            else TRUE
          },
          minWidth = 50
        ),
        count_books = colDef(show = FALSE),
        draftkings = colDef(
          name = "Draftkings",
          sortNALast = TRUE,
          cell = function(value) {
            if (is.na(value)) ''
            else if (value > 0) paste0('+', round(value))
            else if (value <= 0) as.character(round(value))
            else TRUE
          },
          style = function(value, index) {
            if (round(value) == round(search_props_data()$best_odds[[index]]) && !is.na(value))
              list(background = "green", color = "white", fontWeight = "bold")
            else
              list(background = "white")
          }
        ),
        fanduel = colDef(
          name = "Fanduel",
          sortNALast = TRUE,
          cell = function(value) {
            if (is.na(value)) ''
            else if (value > 0) paste0('+', round(value))
            else if (value <= 0) as.character(round(value))
            else
              list(background = "white")
          },
          style = function(value, index) {
            if (round(value) == round(search_props_data()$best_odds[[index]]) && !is.na(value))
              list(background = "green", color = "white", fontWeight = "bold")
            else
              list(background = "white")
          }
        ),
        pointsbet = colDef(
          name = "PointsBet",
          sortNALast = TRUE,
          cell = function(value) {
            if (is.na(value)) ''
            else if (value > 0) paste0('+', round(value))
            else if (value <= 0) as.character(round(value))
            else TRUE
          },
          style = function(value, index) {
            if (round(value) == round(search_props_data()$best_odds[[index]]) && !is.na(value))
              list(background = "green", color = "white", fontWeight = "bold")
            else
              list(background = "white")
          }
        ),
        mean_odds = colDef(
          name = "Mean Odds",
          sortNALast = TRUE,
          cell = function(value) {
            if (is.na(value)) ''
            else if (value > 0) paste0('+', round(value))
            else if (value <= 0) as.character(round(value))
            else TRUE
          },
          format = colFormat(digits = 1)
        ),
        best_odds = colDef(show = FALSE),
        next_best_ratio = colDef(
          name = "Odds 1:2 Ratio",
          sortNALast = TRUE,
          format = colFormat(digits = 2))
      ),
      columnGroups = list(colGroup(
        name = "Odds",
        columns = c("draftkings", "fanduel", "pointsbet")
      ))
    )

  })

}

# RUN ----
shinyApp(ui = ui, server = server)
