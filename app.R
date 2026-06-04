# app.R
# Fantasy League Hub
#
# Expected folder structure:
# fantasy-app/
# ├── app.R
# └── data/
#     ├── fantasy_matchup_data.csv
#     ├── fantasy_name_data.csv
#     └── fantasy_player_data.csv

required_packages <- c(
  "shiny", "dplyr", "ggplot2", "DT", "readr", "tidyr",
  "stringr", "janitor", "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install missing packages first: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(tidyr)
library(stringr)
library(janitor)
library(scales)

# ---- Helpers ----

safe_read <- function(path) {
  if (!file.exists(path)) {
    stop("Missing file: ", path, call. = FALSE)
  }

  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::select(-dplyr::starts_with("unnamed"))
}

score_fmt <- function(x) {
  ifelse(is.na(x), NA, sprintf("%.2f", x))
}

make_record <- function(wins, losses) {
  paste0(wins, "-", losses)
}

datatable_clean <- function(data, page_length = 15) {
  DT::datatable(
    data,
    rownames = FALSE,
    filter = "top",
    extensions = "Buttons",
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel")
    )
  )
}

# ---- Load data ----

matchups <- safe_read("data/fantasy_matchup_data.csv") |>
  mutate(
    week = as.integer(week),
    year = as.integer(year),
    points_for = as.numeric(points_for),
    points_against = as.numeric(points_against),
    win = as.integer(win),
    loss = as.integer(loss),
    margin = points_for - points_against,
    result = case_when(
      win == 1 ~ "Win",
      loss == 1 ~ "Loss",
      TRUE ~ "Tie/Unknown"
    )
  )

team_names <- safe_read("data/fantasy_name_data.csv") |>
  mutate(year = as.integer(year))

players <- safe_read("data/fantasy_player_data.csv") |>
  mutate(
    week = as.integer(week),
    year = as.integer(year),
    proj = as.numeric(proj),
    fpts = as.numeric(fpts),
    fantasy_team = as.character(fantasy_team),
    player_name = as.character(player_name)
  )

years <- sort(unique(c(matchups$year, players$year, team_names$year)), decreasing = TRUE)
all_teams <- sort(unique(c(matchups$team, matchups$opposing_team, players$fantasy_team)))
all_positions <- sort(unique(players$pos))
latest_year <- max(years, na.rm = TRUE)

# ---- UI pieces ----

card <- function(title, value, subtitle = NULL) {
  div(
    class = "metric-card",
    div(class = "metric-title", title),
    div(class = "metric-value", value),
    if (!is.null(subtitle)) div(class = "metric-subtitle", subtitle)
  )
}

ui <- navbarPage(
  title = div(class = "brand-title", "Fantasy League Hub"),
  id = "main_tabs",
  windowTitle = "Fantasy League Hub",

  header = tags$head(
    tags$style(HTML("
      body {
        background: #f6f7fb;
      }

      .navbar {
        border-radius: 0;
        margin-bottom: 0;
      }

      .brand-title {
        font-weight: 800;
        letter-spacing: 0.2px;
      }

      .page-wrap {
        padding: 24px;
      }

      .section-card {
        background: #ffffff;
        border-radius: 16px;
        padding: 20px;
        margin-bottom: 18px;
        box-shadow: 0 4px 16px rgba(0,0,0,0.06);
      }

      .metric-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(210px, 1fr));
        gap: 14px;
        margin-bottom: 18px;
      }

      .metric-card {
        background: #ffffff;
        border-radius: 16px;
        padding: 18px;
        box-shadow: 0 4px 16px rgba(0,0,0,0.06);
        min-height: 118px;
      }

      .metric-title {
        color: #667085;
        font-size: 13px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        margin-bottom: 8px;
      }

      .metric-value {
        color: #101828;
        font-size: 26px;
        font-weight: 800;
        line-height: 1.1;
      }

      .metric-subtitle {
        color: #667085;
        font-size: 13px;
        margin-top: 8px;
      }

      .control-row {
        display: flex;
        gap: 12px;
        flex-wrap: wrap;
        align-items: end;
        margin-bottom: 16px;
      }

      .control-row .form-group {
        min-width: 180px;
        margin-bottom: 0;
      }

      h2, h3 {
        font-weight: 800;
      }

      .muted {
        color: #667085;
      }
    "))
  ),

  tabPanel(
    "Dashboard",
    div(
      class = "page-wrap",
      div(
        class = "section-card",
        h2("League Dashboard"),
        p(class = "muted", "Quick snapshot of the selected season and week."),
        div(
          class = "control-row",
          selectInput("dash_year", "Season", choices = years, selected = latest_year),
          uiOutput("dash_week_ui")
        )
      ),
      uiOutput("dashboard_cards"),
      div(
        class = "section-card",
        h3("Current Standings"),
        DTOutput("dash_standings")
      ),
      div(
        class = "section-card",
        h3("Season Points For"),
        plotOutput("dash_points_plot", height = "430px")
      )
    )
  ),

  tabPanel(
    "Matchups",
    div(
      class = "page-wrap",
      div(
        class = "section-card",
        h2("Matchups"),
        p(class = "muted", "Explore every weekly matchup by season, week, and team."),
        div(
          class = "control-row",
          selectInput("match_year", "Season", choices = years, selected = latest_year),
          uiOutput("match_week_ui"),
          selectInput("match_team", "Team", choices = c("All Teams", all_teams), selected = "All Teams")
        )
      ),
      div(
        class = "section-card",
        h3("Weekly Matchup Table"),
        DTOutput("matchups_table")
      ),
      div(
        class = "section-card",
        h3("Team Scores This Week"),
        plotOutput("weekly_scores_plot", height = "430px")
      )
    )
  ),

  tabPanel(
    "Teams",
    div(
      class = "page-wrap",
      div(
        class = "section-card",
        h2("Teams"),
        p(class = "muted", "Pick a team to view record, scoring trends, and matchup history."),
        div(
          class = "control-row",
          selectInput("team_year", "Season", choices = years, selected = latest_year),
          selectInput("team_select", "Team", choices = all_teams, selected = all_teams[1])
        )
      ),
      uiOutput("team_cards"),
      div(
        class = "section-card",
        h3("Weekly Score Trend"),
        plotOutput("team_score_trend", height = "400px")
      ),
      div(
        class = "section-card",
        h3("Matchup History"),
        DTOutput("team_history_table")
      )
    )
  ),

  tabPanel(
    "Players",
    div(
      class = "page-wrap",
      div(
        class = "section-card",
        h2("Players"),
        p(class = "muted", "Search player performance by season, week, team, position, and roster slot."),
        div(
          class = "control-row",
          selectInput("player_year", "Season", choices = years, selected = latest_year),
          uiOutput("player_week_ui"),
          selectInput("player_team", "Fantasy Team", choices = c("All Teams", all_teams), selected = "All Teams"),
          selectInput("player_pos", "Position", choices = c("All Positions", all_positions), selected = "All Positions"),
          textInput("player_search", "Player Search", placeholder = "Example: Josh Allen")
        )
      ),
      div(
        class = "section-card",
        h3("Top Players"),
        plotOutput("top_players_plot", height = "430px")
      ),
      div(
        class = "section-card",
        h3("Player Performances"),
        DTOutput("players_table")
      )
    )
  ),

  tabPanel(
    "League Records",
    div(
      class = "page-wrap",
      div(
        class = "section-card",
        h2("League Records"),
        p(class = "muted", "All-time league history, best weeks, closest games, and biggest blowouts.")
      ),
      uiOutput("records_cards"),
      div(
        class = "section-card",
        h3("Highest Single-Week Scores"),
        DTOutput("highest_scores_table")
      ),
      div(
        class = "section-card",
        h3("Closest Games"),
        DTOutput("closest_games_table")
      ),
      div(
        class = "section-card",
        h3("Biggest Blowouts"),
        DTOutput("blowouts_table")
      ),
      div(
        class = "section-card",
        h3("All-Time Manager/Team Records"),
        DTOutput("all_time_records_table")
      )
    )
  )
)

# ---- Server ----

server <- function(input, output, session) {

  weeks_for_year <- function(data, year_value) {
    data |>
      filter(year == year_value) |>
      pull(week) |>
      unique() |>
      sort()
  }

  output$dash_week_ui <- renderUI({
    weeks <- weeks_for_year(matchups, input$dash_year)
    selectInput("dash_week", "Week", choices = weeks, selected = max(weeks, na.rm = TRUE))
  })

  output$match_week_ui <- renderUI({
    weeks <- weeks_for_year(matchups, input$match_year)
    selectInput("match_week", "Week", choices = c("All Weeks", weeks), selected = max(weeks, na.rm = TRUE))
  })

  output$player_week_ui <- renderUI({
    weeks <- weeks_for_year(players, input$player_year)
    selectInput("player_week", "Week", choices = c("All Weeks", weeks), selected = max(weeks, na.rm = TRUE))
  })

  season_matchups <- reactive({
    req(input$dash_year)
    matchups |> filter(year == input$dash_year)
  })

  dash_week_matchups <- reactive({
    req(input$dash_year, input$dash_week)
    matchups |>
      filter(year == input$dash_year, week == as.integer(input$dash_week))
  })

  standings <- reactive({
    season_matchups() |>
      group_by(team) |>
      summarise(
        wins = sum(win, na.rm = TRUE),
        losses = sum(loss, na.rm = TRUE),
        points_for = sum(points_for, na.rm = TRUE),
        points_against = sum(points_against, na.rm = TRUE),
        point_diff = points_for - points_against,
        avg_score = mean(points_for, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(record = make_record(wins, losses)) |>
      arrange(desc(wins), desc(points_for))
  })

  output$dashboard_cards <- renderUI({
    week_data <- dash_week_matchups()
    season_data <- season_matchups()

    high_score <- week_data |> slice_max(points_for, n = 1, with_ties = FALSE)
    low_score <- week_data |> slice_min(points_for, n = 1, with_ties = FALSE)

    pair_data <- week_data |>
      mutate(pair_id = paste(pmin(team, opposing_team), pmax(team, opposing_team), week, year, sep = "__")) |>
      group_by(pair_id) |>
      summarise(
        year = first(year),
        week = first(week),
        team_a = first(team),
        team_b = first(opposing_team),
        score_a = first(points_for),
        score_b = first(points_against),
        margin = abs(first(points_for) - first(points_against)),
        .groups = "drop"
      )

    closest <- pair_data |> slice_min(margin, n = 1, with_ties = FALSE)
    blowout <- pair_data |> slice_max(margin, n = 1, with_ties = FALSE)
    league_avg <- mean(week_data$points_for, na.rm = TRUE)

    div(
      class = "metric-grid",
      card(
        "Highest Scorer",
        paste0(high_score$team, " — ", score_fmt(high_score$points_for)),
        paste0("Week ", input$dash_week)
      ),
      card(
        "Lowest Scorer",
        paste0(low_score$team, " — ", score_fmt(low_score$points_for)),
        paste0("Week ", input$dash_week)
      ),
      card(
        "Closest Matchup",
        paste0(closest$team_a, " vs ", closest$team_b),
        paste0("Margin: ", score_fmt(closest$margin))
      ),
      card(
        "Biggest Blowout",
        paste0(blowout$team_a, " vs ", blowout$team_b),
        paste0("Margin: ", score_fmt(blowout$margin))
      ),
      card(
        "League Avg Score",
        score_fmt(league_avg),
        paste0("Week ", input$dash_week)
      ),
      card(
        "Games Logged",
        nrow(season_data) / 2,
        paste0("Season ", input$dash_year)
      )
    )
  })

  output$dash_standings <- renderDT({
    standings() |>
      transmute(
        Team = team,
        Record = record,
        Wins = wins,
        Losses = losses,
        `Points For` = round(points_for, 2),
        `Points Against` = round(points_against, 2),
        `Point Diff` = round(point_diff, 2),
        `Avg Score` = round(avg_score, 2)
      ) |>
      datatable_clean(page_length = 12)
  })

  output$dash_points_plot <- renderPlot({
    standings() |>
      ggplot(aes(x = reorder(team, points_for), y = points_for)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Total Points For", title = "Season Points For by Team") +
      theme_minimal(base_size = 13)
  })

  matchup_filtered <- reactive({
    req(input$match_year, input$match_week, input$match_team)

    data <- matchups |> filter(year == input$match_year)

    if (input$match_week != "All Weeks") {
      data <- data |> filter(week == as.integer(input$match_week))
    }

    if (input$match_team != "All Teams") {
      data <- data |> filter(team == input$match_team | opposing_team == input$match_team)
    }

    data
  })

  output$matchups_table <- renderDT({
    matchup_filtered() |>
      arrange(desc(year), week, team) |>
      transmute(
        Season = year,
        Week = week,
        Team = team,
        `Points For` = round(points_for, 2),
        Opponent = opposing_team,
        `Points Against` = round(points_against, 2),
        Margin = round(margin, 2),
        Result = result
      ) |>
      datatable_clean(page_length = 20)
  })

  output$weekly_scores_plot <- renderPlot({
    plot_data <- matchup_filtered()

    validate(
      need(nrow(plot_data) > 0, "No matchup data found for these filters.")
    )

    plot_data |>
      group_by(team) |>
      summarise(points_for = mean(points_for, na.rm = TRUE), .groups = "drop") |>
      ggplot(aes(x = reorder(team, points_for), y = points_for)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Points For", title = "Scores for Selected Matchups") +
      theme_minimal(base_size = 13)
  })

  selected_team_data <- reactive({
    req(input$team_year, input$team_select)

    matchups |>
      filter(year == input$team_year, team == input$team_select) |>
      arrange(week)
  })

  output$team_cards <- renderUI({
    data <- selected_team_data()

    validate(
      need(nrow(data) > 0, "No team data found.")
    )

    wins <- sum(data$win, na.rm = TRUE)
    losses <- sum(data$loss, na.rm = TRUE)
    pf <- sum(data$points_for, na.rm = TRUE)
    pa <- sum(data$points_against, na.rm = TRUE)
    best_week <- data |> slice_max(points_for, n = 1, with_ties = FALSE)
    worst_week <- data |> slice_min(points_for, n = 1, with_ties = FALSE)

    div(
      class = "metric-grid",
      card("Record", make_record(wins, losses), paste0("Season ", input$team_year)),
      card("Points For", score_fmt(pf), paste0("Average: ", score_fmt(mean(data$points_for, na.rm = TRUE)))),
      card("Points Against", score_fmt(pa), paste0("Average: ", score_fmt(mean(data$points_against, na.rm = TRUE)))),
      card("Best Week", score_fmt(best_week$points_for), paste0("Week ", best_week$week, " vs ", best_week$opposing_team)),
      card("Worst Week", score_fmt(worst_week$points_for), paste0("Week ", worst_week$week, " vs ", worst_week$opposing_team))
    )
  })

  output$team_score_trend <- renderPlot({
    selected_team_data() |>
      ggplot(aes(x = week, y = points_for)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = pretty_breaks()) +
      labs(x = "Week", y = "Points For", title = paste("Weekly Score Trend:", input$team_select)) +
      theme_minimal(base_size = 13)
  })

  output$team_history_table <- renderDT({
    selected_team_data() |>
      transmute(
        Season = year,
        Week = week,
        Team = team,
        Opponent = opposing_team,
        `Points For` = round(points_for, 2),
        `Points Against` = round(points_against, 2),
        Margin = round(margin, 2),
        Result = result
      ) |>
      datatable_clean(page_length = 20)
  })

  player_filtered <- reactive({
    req(input$player_year, input$player_week, input$player_team, input$player_pos)

    data <- players |> filter(year == input$player_year)

    if (input$player_week != "All Weeks") {
      data <- data |> filter(week == as.integer(input$player_week))
    }

    if (input$player_team != "All Teams") {
      data <- data |> filter(fantasy_team == input$player_team)
    }

    if (input$player_pos != "All Positions") {
      data <- data |> filter(pos == input$player_pos)
    }

    if (!is.null(input$player_search) && nzchar(input$player_search)) {
      data <- data |> filter(str_detect(str_to_lower(player_name), str_to_lower(input$player_search)))
    }

    data
  })

  output$top_players_plot <- renderPlot({
    plot_data <- player_filtered() |>
      group_by(player_name, pos) |>
      summarise(total_fpts = sum(fpts, na.rm = TRUE), .groups = "drop") |>
      slice_max(total_fpts, n = 15)

    validate(
      need(nrow(plot_data) > 0, "No player data found for these filters.")
    )

    plot_data |>
      ggplot(aes(x = reorder(player_name, total_fpts), y = total_fpts)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Fantasy Points", title = "Top Players") +
      theme_minimal(base_size = 13)
  })

  output$players_table <- renderDT({
    player_filtered() |>
      arrange(desc(fpts)) |>
      transmute(
        Season = year,
        Week = week,
        Player = player_name,
        `Fantasy Team` = fantasy_team,
        Position = pos,
        `NFL Team` = team,
        Slot = slot,
        Projection = round(proj, 2),
        Points = round(fpts, 2),
        `Over Projection` = round(fpts - proj, 2)
      ) |>
      datatable_clean(page_length = 25)
  })

  all_time_pair_games <- reactive({
    matchups |>
      mutate(pair_id = paste(pmin(team, opposing_team), pmax(team, opposing_team), week, year, sep = "__")) |>
      group_by(pair_id) |>
      summarise(
        Season = first(year),
        Week = first(week),
        Team = first(team),
        Opponent = first(opposing_team),
        `Team Points` = first(points_for),
        `Opponent Points` = first(points_against),
        Margin = abs(first(points_for) - first(points_against)),
        Winner = if_else(first(points_for) >= first(points_against), first(team), first(opposing_team)),
        Loser = if_else(first(points_for) < first(points_against), first(team), first(opposing_team)),
        .groups = "drop"
      )
  })

  output$records_cards <- renderUI({
    highest <- matchups |> slice_max(points_for, n = 1, with_ties = FALSE)
    lowest_win <- matchups |> filter(win == 1) |> slice_min(points_for, n = 1, with_ties = FALSE)
    closest <- all_time_pair_games() |> slice_min(Margin, n = 1, with_ties = FALSE)
    blowout <- all_time_pair_games() |> slice_max(Margin, n = 1, with_ties = FALSE)

    div(
      class = "metric-grid",
      card(
        "Highest Score Ever",
        paste0(highest$team, " — ", score_fmt(highest$points_for)),
        paste0("Week ", highest$week, ", ", highest$year)
      ),
      card(
        "Lowest Winning Score",
        paste0(lowest_win$team, " — ", score_fmt(lowest_win$points_for)),
        paste0("Week ", lowest_win$week, ", ", lowest_win$year)
      ),
      card(
        "Closest Game",
        paste0(closest$Team, " vs ", closest$Opponent),
        paste0("Margin: ", score_fmt(closest$Margin), " | Week ", closest$Week, ", ", closest$Season)
      ),
      card(
        "Biggest Blowout",
        paste0(blowout$Winner, " over ", blowout$Loser),
        paste0("Margin: ", score_fmt(blowout$Margin), " | Week ", blowout$Week, ", ", blowout$Season)
      )
    )
  })

  output$highest_scores_table <- renderDT({
    matchups |>
      arrange(desc(points_for)) |>
      transmute(
        Rank = row_number(),
        Season = year,
        Week = week,
        Team = team,
        Opponent = opposing_team,
        Points = round(points_for, 2),
        `Opponent Points` = round(points_against, 2),
        Result = result
      ) |>
      head(50) |>
      datatable_clean(page_length = 15)
  })

  output$closest_games_table <- renderDT({
    all_time_pair_games() |>
      arrange(Margin) |>
      mutate(Margin = round(Margin, 2)) |>
      head(50) |>
      datatable_clean(page_length = 15)
  })

  output$blowouts_table <- renderDT({
    all_time_pair_games() |>
      arrange(desc(Margin)) |>
      mutate(Margin = round(Margin, 2)) |>
      head(50) |>
      datatable_clean(page_length = 15)
  })

  output$all_time_records_table <- renderDT({
    matchups |>
      group_by(team) |>
      summarise(
        Wins = sum(win, na.rm = TRUE),
        Losses = sum(loss, na.rm = TRUE),
        `Points For` = sum(points_for, na.rm = TRUE),
        `Points Against` = sum(points_against, na.rm = TRUE),
        `Point Diff` = `Points For` - `Points Against`,
        `Avg Score` = mean(points_for, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(Record = make_record(Wins, Losses)) |>
      arrange(desc(Wins), desc(`Points For`)) |>
      transmute(
        Team = team,
        Record,
        Wins,
        Losses,
        `Points For` = round(`Points For`, 2),
        `Points Against` = round(`Points Against`, 2),
        `Point Diff` = round(`Point Diff`, 2),
        `Avg Score` = round(`Avg Score`, 2)
      ) |>
      datatable_clean(page_length = 20)
  })
}

shinyApp(ui = ui, server = server)
