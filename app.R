# app.R
# Fantasy League Shiny App
# Folder structure:
#   your-project/
#   ├── app.R
#   └── data/
#       ├── fantasy_matchup_data.csv
#       ├── fantasy_name_data.csv
#       └── fantasy_player_data.csv

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(janitor)
library(stringr)
library(tidyr)

# ---- Load data ----
matchups_raw <- read_csv("data/fantasy_matchup_data.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  select(-starts_with("unnamed"))

names_raw <- read_csv("data/fantasy_name_data.csv", show_col_types = FALSE) %>%
  clean_names()

players_raw <- read_csv("data/fantasy_player_data.csv", show_col_types = FALSE) %>%
  clean_names()

# ---- Clean data ----
matchups <- matchups_raw %>%
  mutate(
    week = as.integer(week),
    year = as.integer(year),
    points_for = as.numeric(points_for),
    points_against = as.numeric(points_against),
    win = as.integer(win),
    loss = as.integer(loss)
  )

team_names <- names_raw %>%
  mutate(year = as.integer(year))

players <- players_raw %>%
  mutate(
    week = as.integer(week),
    year = as.integer(year),
    proj = as.numeric(proj),
    fpts = as.numeric(fpts)
  )

years <- sort(unique(c(matchups$year, players$year)), decreasing = TRUE)
teams <- sort(unique(c(matchups$team, matchups$opposing_team, players$fantasy_team)))

# ---- UI ----
ui <- fluidPage(
  titlePanel("Fantasy League Dashboard"),

  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Season", choices = years, selected = max(years, na.rm = TRUE)),
      selectInput("week", "Week", choices = NULL),
      selectInput("team", "Team", choices = c("All Teams", teams), selected = "All Teams"),
      hr(),
      p("Update the CSV files in the data folder each week, then rerun or redeploy the app.")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          br(),
          h3("Standings"),
          DTOutput("standings_table"),
          br(),
          h3("Points For by Team"),
          plotOutput("points_plot", height = "420px")
        ),

        tabPanel(
          "Matchups",
          br(),
          h3("Weekly Matchups"),
          DTOutput("matchups_table")
        ),

        tabPanel(
          "Players",
          br(),
          h3("Player Performances"),
          DTOutput("players_table"),
          br(),
          h3("Top Players"),
          plotOutput("top_players_plot", height = "420px")
        ),

        tabPanel(
          "Managers",
          br(),
          h3("Team Names and Managers"),
          DTOutput("managers_table")
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  observeEvent(input$year, {
    available_weeks <- matchups %>%
      filter(year == input$year) %>%
      pull(week) %>%
      unique() %>%
      sort()

    updateSelectInput(
      session,
      "week",
      choices = c("All Weeks", available_weeks),
      selected = "All Weeks"
    )
  }, ignoreInit = FALSE)

  filtered_matchups <- reactive({
    df <- matchups %>%
      filter(year == input$year)

    if (!is.null(input$week) && input$week != "All Weeks") {
      df <- df %>% filter(week == as.integer(input$week))
    }

    if (!is.null(input$team) && input$team != "All Teams") {
      df <- df %>% filter(team == input$team | opposing_team == input$team)
    }

    df
  })

  filtered_players <- reactive({
    df <- players %>%
      filter(year == input$year)

    if (!is.null(input$week) && input$week != "All Weeks") {
      df <- df %>% filter(week == as.integer(input$week))
    }

    if (!is.null(input$team) && input$team != "All Teams") {
      df <- df %>% filter(fantasy_team == input$team)
    }

    df
  })

  season_standings <- reactive({
    matchups %>%
      filter(year == input$year) %>%
      group_by(team) %>%
      summarise(
        wins = sum(win, na.rm = TRUE),
        losses = sum(loss, na.rm = TRUE),
        points_for = round(sum(points_for, na.rm = TRUE), 2),
        points_against = round(sum(points_against, na.rm = TRUE), 2),
        avg_points = round(mean(points_for, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      left_join(
        team_names %>% filter(year == input$year),
        by = c("team" = "team_name")
      ) %>%
      select(team, manager, wins, losses, points_for, points_against, avg_points) %>%
      arrange(desc(wins), desc(points_for))
  })

  output$standings_table <- renderDT({
    datatable(
      season_standings(),
      rownames = FALSE,
      options = list(pageLength = 12, autoWidth = TRUE)
    )
  })

  output$points_plot <- renderPlot({
    season_standings() %>%
      ggplot(aes(x = reorder(team, points_for), y = points_for)) +
      geom_col() +
      coord_flip() +
      labs(
        x = NULL,
        y = "Total Points For",
        title = paste("Total Points For -", input$year)
      ) +
      theme_minimal()
  })

  output$matchups_table <- renderDT({
    filtered_matchups() %>%
      select(
        year, week, team, points_for,
        opposing_team, points_against, win, loss
      ) %>%
      arrange(week, desc(points_for)) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 20, autoWidth = TRUE)
      )
  })

  output$players_table <- renderDT({
    filtered_players() %>%
      select(
        year, week, fantasy_team, player_name,
        pos, team, slot, proj, fpts
      ) %>%
      arrange(desc(fpts)) %>%
      datatable(
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 20, autoWidth = TRUE)
      )
  })

  output$top_players_plot <- renderPlot({
    filtered_players() %>%
      filter(!is.na(fpts)) %>%
      group_by(player_name, fantasy_team, pos) %>%
      summarise(total_fpts = sum(fpts, na.rm = TRUE), .groups = "drop") %>%
      slice_max(total_fpts, n = 15) %>%
      ggplot(aes(x = reorder(player_name, total_fpts), y = total_fpts)) +
      geom_col() +
      coord_flip() +
      labs(
        x = NULL,
        y = "Fantasy Points",
        title = "Top Player Performances"
      ) +
      theme_minimal()
  })

  output$managers_table <- renderDT({
    team_names %>%
      filter(year == input$year) %>%
      arrange(manager) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 20, autoWidth = TRUE)
      )
  })
}

shinyApp(ui = ui, server = server)
