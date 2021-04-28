# global ----
## libraries
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(reactable)
## load data
search_props_raw <- readRDS("props.rds") %>%
  filter(!is.na(tidyopp))

# ui ----
ui <- fluidPage(
  titlePanel('Props Dash'),
  h5(paste0('Data updated at: ', Sys.time(), ' CDT')),
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
                       choices = sort(unique(search_props_raw$tidyou)),
                       selected = sort(unique(search_props_raw$tidyou)),
                       multiple = TRUE)),
    # TODO: FIGURE OUT LINE FILTERING ---
    # column(width = 2, numericInput('line', 'Line Cutoff', min = NA, max = NA, value = NA)),
    column(width = 2,
           pickerInput('count_books', 'Minimum Books',
                       choices = sort(unique(search_props_raw$count_books)),
                       selected = 1))),
  br(),
  tabsetPanel(
    tabPanel('All Props',
             br(),
             fluidRow(
               column(width = 12,
                      reactableOutput('search_props')))),
    tabPanel('Projections',
             br(),
             fluidRow(
               column(width = 12,
                      reactableOutput('projections'))))))

# server ------------------------------------------------------------------

server <- function(input, output) {
  #### get data for table ####

  if (file.exists('restart.txt')) system('touch restart.txt') else file.create('restart.txt')

  # filter based on inputs
  filtered_data <- reactive({

    fd <- search_props_raw %>%
      filter(
        sport == input$sport,
        tidyplayer %in% input$player,
        prop %in% input$prop,
        tidyteam %in% input$team,
        tidyou %in% input$ou,
        count_books >= input$count_books
      )

    # TODO: FIGURE OUT LINE FILTERING
    # if (!is.null(input$line)) {
    #   fd <- fd %>%
    #     filter(tidyline <= input$line)
    # }
    # if (is.null(input$line[[2]])) {
    #   fd <- fd %>%
    #     filter(tidyline <= input$line[[2]])
    # }

    fd

  })

  # make data for all props searchable view
  all_props_data <- reactive({

    filtered_data() %>%
      select(
        sport,
        tidyplayer,
        tidyteam,
        home_away,
        tidyopp,
        prop,
        tidyou,
        tidyline,
        next_best_ratio,
        count_books,
        best_odds,
        draftkings,
        fanduel,
        pointsbet,
        mean_odds
      )
  })

  # make data for projections searchable view
  projections_data <- reactive({
    filtered_data() %>%
      filter(
        !is.na(projected_prob)
      ) %>%
      select(
        sport,
        tidyplayer,
        tidyteam,
        home_away,
        tidyopp,
        prop,
        projected_odds,
        best_odds,
        best_delta,
        best_books
      )
  })

  #### make the table to search props ####

  output$search_props <- renderReactable({
    validate(need(nrow(all_props_data()) > 0, "waiting for input..."))

    reactable(
      all_props_data(),
      pagination = TRUE,
      defaultPageSize = 20,
      rownames = FALSE,
      sortable = TRUE,
      defaultSorted = list(next_best_ratio = 'asc'),
      columns = list(
        sport = colDef(show = FALSE),
        tidyplayer = colDef(
          name = "Player",
          sortNALast = TRUE,
          minWidth = 90
        ),
        tidyteam = colDef(
          name = "Team",
          sortNALast = TRUE,
          width = 70
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
          width = 70
        ),
        prop = colDef(
          name = "Prop",
          sortNALast = TRUE,
          minWidth = 80
        ),
        tidyou = colDef(
          name = "OU",
          sortNALast = TRUE,
          cell = function(value) {
            if (is.na(value)) 'N/A'
            else if (value == 'NA') 'N/A'
            else value
          },
          style = function(value, index) {
            if (value == 'N/A') list(color = "lightgray", fontStyle = "italic")
            else return()
          },
          width = 60
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
          width = 60
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
            if (round(value) == round(all_props_data()$best_odds[[index]]) && !is.na(value))
              list(background = "green", color = "white", fontWeight = "bold")
            else
              list(background = "white")
          },
          minWidth = 60
        ),
        fanduel = colDef(
          name = "Fanduel",
          sortNALast = TRUE,
          cell = function(value) {
            if (is.na(value)) ''
            else if (value > 0) paste0('+', round(value))
            else if (value <= 0) as.character(round(value))
            else value
          },
          style = function(value, index) {
            if (round(value) == round(all_props_data()$best_odds[[index]]) && !is.na(value))
              list(background = "green", color = "white", fontWeight = "bold")
            else
              list(background = "white")
          },
          minWidth = 60
        ),
        pointsbet = colDef(
          name = "PointsBet",
          sortNALast = TRUE,
          cell = function(value) {
            if (is.na(value)) ''
            else if (value > 0) paste0('+', round(value))
            else if (value <= 0) as.character(round(value))
            else value
          },
          style = function(value, index) {
            if (round(value) == round(all_props_data()$best_odds[[index]]) && !is.na(value))
              list(background = "green", color = "white", fontWeight = "bold")
            else
              list(background = "white")
          },
          minWidth = 60
        ),
        mean_odds = colDef(
          name = "Mean Odds",
          sortNALast = TRUE,
          cell = function(value) {
            if (is.na(value)) ''
            else if (value > 0) paste0('+', round(value))
            else if (value <= 0) as.character(round(value))
            else value
          },
          format = colFormat(digits = 1),
          minWidth = 60
        ),
        best_odds = colDef(show = FALSE),
        next_best_ratio = colDef(
          name = "Best Odds Ratio",
          sortNALast = TRUE,
          format = colFormat(digits = 2),
          minWidth = 75
          )
      ),
      columnGroups = list(colGroup(
        name = "Odds",
        columns = c("draftkings", "fanduel", "pointsbet")
      )))
  })

  #### make the table to show projections vs props ####

  output$projections <- renderReactable({
    validate(need(nrow(projections_data()) > 0, "waiting for input..."))

    reactable(
      projections_data(),
      pagination = FALSE,
      rownames = FALSE,
      sortable = TRUE,
      defaultSorted = list(best_delta = 'desc'),
      columns = list(
        sport = colDef(show = FALSE),
        tidyplayer = colDef(
          name = "Player",
          sortNALast = TRUE,
          width = 180
        ),
        tidyteam = colDef(
          name = "Team",
          sortNALast = TRUE,
          width = 70
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
          width = 70
        ),
        prop = colDef(
          name = "Prop",
          sortNALast = TRUE,
          width = 180
        ),
        projected_odds = colDef(
          name = "Projected Odds",
          sortNALast = TRUE,
          format = colFormat(digits = 0),
          cell = function(value) {
            if (is.na(value)) ''
            else if (value > 0) paste0('+', round(value))
            else if (value <= 0) as.character(round(value))
            else value
          },
          width = 150
        ),
        best_odds = colDef(
          name = 'Best Odds',
          sortNALast = TRUE,
          format = colFormat(digits = 0),
          cell = function(value) {
            if (is.na(value)) ''
            else if (value > 0) paste0('+', round(value))
            else if (value <= 0) as.character(round(value))
            else value
          },
          width = 100
        ),
        best_delta = colDef(
          name = 'Best Edge',
          sortNALast = TRUE,
          cell = function(value) {
            v <- paste0(round(value, 3) * 100, '%')
            if (value > 0) paste0('+', v)
            else v
          },
          style = function(value, index) {
            if (value > 0 && !is.na(value))
              list(background = "white", color = "green", fontWeight = "bold")
            else return()
          },
          width = 100
          ),
        best_books = colDef(
          name = 'Best Books',
          sortNALast = TRUE,
          style = function(value, index) {
            if (projections_data()$best_delta[[index]] > 0 && !is.na(projections_data()$best_delta[[index]])) list(background = "green", color = "white", fontWeight = "bold")
            else return()
          },
          minWidth = 100
        )))
    })
}

# RUN ----
shinyApp(ui = ui, server = server)
