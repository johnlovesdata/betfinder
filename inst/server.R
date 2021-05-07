shinyServer(
  function(input, output) {
    # restart
    if (file.exists('restart.txt')) system('touch restart.txt') else file.create('restart.txt')

    # filter based on inputs
    filtered_data <- reactive({

      fd <- search_props_raw %>%
        filter(
          sport %in% input$sport,
          tidyplayer %in% input$player,
          tidyteam %in% input$team,
          injury_status %in% c(NA_character_, input$injury_status),
          prop %in% input$prop,
          tidyou %in% input$ou,
          tidyline %in% c(NA_real_, seq(input$line[[1]], input$line[[2]], .5)),
          count_books >= input$count_books
        )

      fd

    })

    # make data for all props searchable view
    all_props_data <- reactive({

      filtered_data() %>%
        select(
          sport,
          tidyplayer,
          injury_status,
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
          injury_status,
          tidyteam,
          home_away,
          tidyopp,
          prop,
          tidyou,
          tidyline,
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
            style = function(value, index) {
              if (grepl('Likely', all_props_data()$injury_status[[index]])) list(color = "green", fontWeight = "bold")
              else if (grepl('Unlikely', all_props_data()$injury_status[[index]])) list(color = "red", fontWeight = "bold")
              else if (grepl('Toss', all_props_data()$injury_status[[index]])) list(color = "orange", fontWeight = "bold")
              else list()
            },
            minWidth = 90
          ),
          injury_status = colDef(
            name = "Status",
            show = FALSE,
            sortNALast = FALSE,
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
              else if (grepl('away', value)) '@'
              else ''
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
            style = function(value, index) {
              if (grepl('Likely', projections_data()$injury_status[[index]])) list(color = "green", fontWeight = "bold")
              else if (grepl('Unlikely', projections_data()$injury_status[[index]])) list(color = "red", fontWeight = "bold")
              else if (grepl('Toss', projections_data()$injury_status[[index]])) list(color = "orange", fontWeight = "bold")
              else list()
            },
            minWidth = 90
          ),
          injury_status = colDef(
            name = "Status",
            show = FALSE,
            sortNALast = FALSE,
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
              else if (grepl('away', value)) '@'
              else ''
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
)
