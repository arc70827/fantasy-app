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

pct_fmt <- function(x) {
  ifelse(is.na(x), "—", scales::percent(x, accuracy = 0.1))
}

responsive_column_defs <- function(data) {
  definitions <- list(
    list(responsivePriority = 1, targets = 0)
  )

  if (ncol(data) >= 2) {
    definitions <- c(
      definitions,
      list(list(responsivePriority = 2, targets = 1))
    )
  }

  definitions
}

datatable_clean <- function(data, page_length = 15) {
  DT::datatable(
    data,
    rownames = FALSE,
    filter = "top",
    extensions = c("Buttons", "Responsive"),
    options = list(
      pageLength = page_length,
      responsive = TRUE,
      autoWidth = FALSE,
      scrollX = FALSE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel"),
      columnDefs = responsive_column_defs(data)
    ),
    class = "stripe hover compact nowrap"
  )
}

datatable_simple <- function(data, page_length = 5) {
  DT::datatable(
    data,
    rownames = FALSE,
    filter = "none",
    extensions = "Responsive",
    options = list(
      pageLength = page_length,
      responsive = TRUE,
      autoWidth = FALSE,
      scrollX = FALSE,
      dom = "t",
      ordering = FALSE,
      columnDefs = responsive_column_defs(data)
    ),
    class = "stripe hover compact nowrap"
  )
}

datatable_no_buttons <- function(data, page_length = 25) {
  DT::datatable(
    data,
    rownames = FALSE,
    filter = "top",
    extensions = "Responsive",
    options = list(
      pageLength = page_length,
      responsive = TRUE,
      autoWidth = FALSE,
      scrollX = FALSE,
      dom = "frtip",
      columnDefs = responsive_column_defs(data)
    ),
    class = "stripe hover compact nowrap"
  )
}

datatable_record <- function(data, page_length = 5) {
  DT::datatable(
    data,
    rownames = FALSE,
    filter = "none",
    extensions = "Responsive",
    options = list(
      pageLength = page_length,
      responsive = TRUE,
      autoWidth = FALSE,
      scrollX = FALSE,
      dom = "t",
      ordering = FALSE,
      columnDefs = responsive_column_defs(data)
    ),
    class = "stripe hover compact nowrap"
  )
}

format_record_table <- function(dt) {
  dt |>
    DT::formatStyle(
      columns = names(dt$x$data),
      target = "row",
      fontWeight = DT::styleEqual(1, "900")
    )
}

parse_season <- function(x) {
  x_chr <- stringr::str_squish(as.character(x))
  x_chr[x_chr %in% c("", "NA", "N/A", "Current", "Present", "present", "current")] <- NA_character_
  suppressWarnings(as.integer(x_chr))
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

if (!"matchup_type" %in% names(matchups)) {
  matchups <- matchups |>
    mutate(
      matchup_type = case_when(
        week <= 14 ~ "Regular Season",
        week == 15 ~ "Quarterfinals",
        week == 16 ~ "Semifinals",
        week == 17 ~ "Finals",
        TRUE ~ "Unknown"
      )
    )
} else {
  matchups <- matchups |>
    mutate(matchup_type = as.character(matchup_type))
}

players <- safe_read("data/fantasy_player_data.csv") |>
  mutate(
    week = as.integer(week),
    year = as.integer(year),
    proj = as.numeric(proj),
    fpts = as.numeric(fpts),
    fantasy_team = as.character(fantasy_team),
    player_name = as.character(player_name)
  )

team_names_raw <- safe_read("data/fantasy_name_data.csv")

# Handles both the old name table format:
# team_name, manager, year
# and the new name table format:
# team_name, manager, first_season, last_season
if ("year" %in% names(team_names_raw) && !"first_season" %in% names(team_names_raw)) {
  team_names <- team_names_raw |>
    mutate(
      first_season = as.integer(year),
      last_season = as.integer(year)
    ) |>
    select(team_name, manager, first_season, last_season)
} else {
  team_names <- team_names_raw |>
    mutate(
      first_season = parse_season(first_season),
      last_season = parse_season(last_season),
      last_season = if_else(is.na(last_season), max(c(matchups$year, players$year), na.rm = TRUE), last_season)
    ) |>
    select(team_name, manager, first_season, last_season)
}

team_names <- team_names |>
  mutate(
    team_name = as.character(team_name),
    manager = as.character(manager)
  )

resolve_manager_one <- function(team_name_value, year_value) {
  result <- team_names |>
    filter(
      team_name == team_name_value,
      year_value >= first_season,
      year_value <= last_season
    ) |>
    pull(manager)

  if (length(result) == 0 || is.na(result[1])) {
    return(as.character(team_name_value))
  }

  result[1]
}

resolve_team_name_one <- function(manager_value, year_value) {
  result <- team_names |>
    filter(
      manager == manager_value,
      year_value >= first_season,
      year_value <= last_season
    ) |>
    pull(team_name)

  if (length(result) == 0 || is.na(result[1])) {
    return(as.character(manager_value))
  }

  result[1]
}

matchups <- matchups |>
  mutate(
    manager = mapply(resolve_manager_one, team, year, USE.NAMES = FALSE),
    opposing_manager = mapply(resolve_manager_one, opposing_team, year, USE.NAMES = FALSE)
  )

players <- players |>
  mutate(
    manager = mapply(resolve_manager_one, fantasy_team, year, USE.NAMES = FALSE)
  )

years <- sort(unique(c(matchups$year, players$year, team_names$first_season, team_names$last_season)), decreasing = TRUE)
years <- years[!is.na(years)]

managers <- sort(unique(c(matchups$manager, matchups$opposing_manager, team_names$manager)))
all_teams <- sort(unique(c(matchups$team, matchups$opposing_team, players$fantasy_team)))
all_positions <- sort(unique(players$pos))
all_slots <- sort(unique(players$slot))
latest_year <- max(years, na.rm = TRUE)
latest_week <- matchups |>
  filter(year == latest_year) |>
  summarise(max_week = max(week, na.rm = TRUE)) |>
  pull(max_week)

# One row per fantasy matchup instead of one row per team.

infer_manager_finishes <- function() {
  finals_games <- pair_games |>
    filter(matchup_type %in% c("Championship", "Finals", "Third Place", "Third Place Game", "3rd Place"))

  if (nrow(finals_games) == 0) {
    return(tibble(year = integer(), manager = character(), team = character(), finish = integer(), finish_label = character()))
  }

  championship_games <- finals_games |>
    filter(matchup_type %in% c("Championship", "Finals")) |>
    group_by(year) |>
    slice_max(winning_score, n = 1, with_ties = FALSE) |>
    ungroup()

  third_place_games <- finals_games |>
    filter(matchup_type %in% c("Third Place", "Third Place Game", "3rd Place"))

  champ_finishes <- bind_rows(
    championship_games |>
      transmute(year, manager = winner, team = if_else(winner == manager_a, team_a, team_b), finish = 1L),
    championship_games |>
      transmute(year, manager = loser, team = if_else(loser == manager_a, team_a, team_b), finish = 2L)
  )

  third_finishes <- bind_rows(
    third_place_games |>
      transmute(year, manager = winner, team = if_else(winner == manager_a, team_a, team_b), finish = 3L),
    third_place_games |>
      transmute(year, manager = loser, team = if_else(loser == manager_a, team_a, team_b), finish = 4L)
  )

  bind_rows(champ_finishes, third_finishes) |>
    mutate(
      finish_label = case_when(
        finish == 1L ~ "1st",
        finish == 2L ~ "2nd",
        finish == 3L ~ "3rd",
        finish == 4L ~ "4th",
        TRUE ~ paste0(finish, "th")
      )
    ) |>
    distinct(year, manager, .keep_all = TRUE)
}

make_pair_games <- function(data) {
  data |>
    mutate(
      pair_key = paste(
        year,
        week,
        pmin(manager, opposing_manager),
        pmax(manager, opposing_manager),
        sep = "__"
      )
    ) |>
    group_by(pair_key) |>
    summarise(
      year = first(year),
      week = first(week),
      manager_a = first(manager),
      team_a = first(team),
      manager_b = first(opposing_manager),
      team_b = first(opposing_team),
      score_a = first(points_for),
      score_b = first(points_against),
      margin = abs(first(points_for) - first(points_against)),
      winner = if_else(first(points_for) >= first(points_against), first(manager), first(opposing_manager)),
      loser = if_else(first(points_for) < first(points_against), first(manager), first(opposing_manager)),
      winning_score = max(first(points_for), first(points_against), na.rm = TRUE),
      losing_score = min(first(points_for), first(points_against), na.rm = TRUE),
      matchup_type = first(matchup_type),
      .groups = "drop"
    ) |>
    mutate(
      matchup_label = paste0(manager_a, " vs ", manager_b),
      score_label = paste0(score_fmt(score_a), " - ", score_fmt(score_b))
    )
}

pair_games <- make_pair_games(matchups)
manager_finishes <- infer_manager_finishes()

h2h_summary <- function(manager_one, manager_two, through_year = Inf, through_week = Inf) {
  games <- matchups |>
    filter(
      (
        manager == manager_one & opposing_manager == manager_two
      ) |
        (
          manager == manager_two & opposing_manager == manager_one
        )
    ) |>
    filter(
      year < through_year |
        (year == through_year & week < through_week) |
        is.infinite(through_year)
    )

  if (nrow(games) == 0) {
    return(tibble(
      manager_one_wins = 0,
      manager_two_wins = 0,
      games = 0,
      manager_one_avg = NA_real_,
      manager_two_avg = NA_real_
    ))
  }

  tibble(
    manager_one_wins = sum(games$manager == manager_one & games$win == 1, na.rm = TRUE),
    manager_two_wins = sum(games$manager == manager_two & games$win == 1, na.rm = TRUE),
    games = nrow(games) / 2,
    manager_one_avg = mean(games$points_for[games$manager == manager_one], na.rm = TRUE),
    manager_two_avg = mean(games$points_for[games$manager == manager_two], na.rm = TRUE)
  )
}

# ---- UI pieces ----

card <- function(title, value, subtitle = NULL, class = "accent-blue") {
  div(
    class = paste("metric-card", class),
    div(class = "metric-title", title),
    div(class = "metric-value", value),
    if (!is.null(subtitle)) div(class = "metric-subtitle", subtitle)
  )
}

ui <- navbarPage(
  title = div(class = "brand-title", span(class = "brand-mark", "🏈"), " Fantasy League Hub"),
  id = "main_tabs",
  selected = "Dashboard",
  windowTitle = "Fantasy League Hub",

  header = tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1, viewport-fit=cover"
    ),
    tags$meta(name = "theme-color", content = "#0f172a"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "black-translucent"),
    tags$meta(name = "apple-mobile-web-app-title", content = "Fantasy Hub"),
    tags$link(rel = "manifest", href = "manifest.json"),
    tags$link(rel = "apple-touch-icon", sizes = "192x192", href = "icon-192.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "192x192", href = "icon-192.png"),
    tags$style(HTML("
      :root {
        --navy: #0f172a;
        --navy-2: #111827;
        --blue: #2563eb;
        --cyan: #06b6d4;
        --green: #16a34a;
        --gold: #f59e0b;
        --red: #dc2626;
        --purple: #7c3aed;
        --bg: #eef4ff;
        --card: #ffffff;
        --text: #0f172a;
        --muted: #64748b;
        --border: #dbeafe;
      }

      body {
        background:
          radial-gradient(circle at top left, rgba(37, 99, 235, 0.18), transparent 35%),
          radial-gradient(circle at top right, rgba(245, 158, 11, 0.16), transparent 30%),
          var(--bg);
        color: var(--text);
      }

      .navbar {
        border-radius: 0;
        margin-bottom: 0;
        border: none;
        background: linear-gradient(90deg, var(--navy), #1e3a8a);
        box-shadow: 0 6px 20px rgba(15, 23, 42, 0.25);
      }

      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a {
        color: #ffffff !important;
        font-weight: 700;
      }

      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:focus,
      .navbar-default .navbar-nav > .active > a:hover {
        background: rgba(255, 255, 255, 0.16) !important;
        color: #ffffff !important;
      }

      .navbar-default .navbar-nav > li > a:hover {
        background: rgba(255, 255, 255, 0.10) !important;
      }

      .brand-title {
        font-weight: 900;
        letter-spacing: 0.2px;
      }

      .brand-mark {
        margin-right: 4px;
      }

      .page-wrap {
        padding: 24px;
      }

      .section-card {
        background: rgba(255, 255, 255, 0.96);
        border-radius: 18px;
        padding: 22px;
        margin-bottom: 18px;
        border: 1px solid rgba(219, 234, 254, 0.95);
        box-shadow: 0 10px 28px rgba(15, 23, 42, 0.08);
      }

      .hero-card {
        background: linear-gradient(135deg, #0f172a, #1d4ed8);
        color: white;
        border: none;
      }

      .hero-card .muted {
        color: rgba(255,255,255,0.78);
      }

      .metric-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(215px, 1fr));
        gap: 14px;
        margin-bottom: 18px;
      }

      .metric-card {
        background: var(--card);
        border-radius: 18px;
        padding: 18px;
        box-shadow: 0 10px 26px rgba(15, 23, 42, 0.08);
        min-height: 120px;
        border: 1px solid rgba(226, 232, 240, 0.9);
        border-top: 6px solid var(--blue);
      }

      .metric-card.accent-green { border-top-color: var(--green); }
      .metric-card.accent-gold { border-top-color: var(--gold); }
      .metric-card.accent-red { border-top-color: var(--red); }
      .metric-card.accent-purple { border-top-color: var(--purple); }
      .metric-card.accent-cyan { border-top-color: var(--cyan); }

      .metric-title {
        color: var(--muted);
        font-size: 12px;
        font-weight: 800;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        margin-bottom: 8px;
      }

      .metric-value {
        color: var(--text);
        font-size: 25px;
        font-weight: 900;
        line-height: 1.1;
      }

      .score-number {
        color: #1d4ed8 !important;
      }

      .metric-subtitle {
        color: var(--muted);
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
        min-width: 190px;
        margin-bottom: 0;
      }

      h2, h3 {
        font-weight: 900;
      }

      .muted {
        color: var(--muted);
      }

      .recap-card {
        display: grid;
        grid-template-columns: 1fr 1fr;
        background: #ffffff;
        border: 4px solid #0f172a;
        border-radius: 10px;
        overflow: hidden;
        margin-bottom: 18px;
        box-shadow: 0 8px 22px rgba(15, 23, 42, 0.08);
      }

      .recap-team {
        min-height: 145px;
        padding: 24px;
        display: flex;
        flex-direction: column;
        justify-content: center;
      }

      .recap-team:first-child {
        border-right: 4px solid #0f172a;
      }

      .recap-team-name {
        font-size: 24px;
        font-weight: 900;
        color: #0f172a;
        line-height: 1.05;
        margin-bottom: 10px;
      }

      .recap-team-score {
        font-size: 30px;
        font-weight: 900;
        color: #1d4ed8;
        line-height: 1;
      }

      .recap-winner {
        background: linear-gradient(135deg, rgba(37, 99, 235, 0.08), rgba(245, 158, 11, 0.08));
      }

      .finish-card {
        background: #ffffff;
        border-radius: 18px;
        padding: 20px;
        box-shadow: 0 10px 26px rgba(15, 23, 42, 0.08);
        border: 1px solid rgba(226, 232, 240, 0.9);
        margin-bottom: 18px;
      }

      .finish-title {
        font-size: 22px;
        font-weight: 900;
        color: #0f172a;
        margin-bottom: 12px;
      }

      .finish-row {
        display: grid;
        grid-template-columns: 70px 1fr;
        gap: 12px;
        border-top: 1px solid #e5e7eb;
        padding: 12px 0;
        align-items: center;
      }

      .finish-rank {
        font-size: 24px;
        font-weight: 900;
        color: #1d4ed8;
      }

      .finish-detail {
        font-weight: 800;
        color: #0f172a;
      }

      .matchup-detail-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 18px;
      }

      .matchup-team-panel {
        background: #ffffff;
        border: 3px solid #0f172a;
        border-radius: 14px;
        overflow: hidden;
      }

      .matchup-team-header {
        background: linear-gradient(135deg, #0f172a, #1d4ed8);
        color: #ffffff;
        padding: 18px;
      }

      .matchup-team-title {
        font-size: 22px;
        font-weight: 900;
        margin-bottom: 4px;
      }

      .matchup-team-score {
        font-size: 30px;
        font-weight: 900;
      }

      .matchup-team-body {
        padding: 14px;
      }

      .rank-one {
        font-weight: 900 !important;
      }

      .record-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(360px, 1fr));
        gap: 18px;
      }

      .record-table-card {
        background: #ffffff;
        border-radius: 18px;
        padding: 18px;
        border: 1px solid rgba(226, 232, 240, 0.9);
        box-shadow: 0 10px 26px rgba(15, 23, 42, 0.08);
      }

      .record-table-card h3 {
        margin-top: 0;
      }

      .power-ranking-wrap table.dataTable {
        border-collapse: separate !important;
        border-spacing: 0 8px !important;
      }

      .power-ranking-wrap table.dataTable tbody tr {
        background: #ffffff !important;
        box-shadow: 0 4px 14px rgba(15, 23, 42, 0.08);
      }

      .power-ranking-wrap table.dataTable tbody td {
        font-weight: 700;
        padding-top: 14px !important;
        padding-bottom: 14px !important;
      }

      .section-title-row {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-bottom: 10px;
      }

      .section-title-row h3 {
        margin: 0;
      }

      .info-button {
        width: 26px;
        height: 26px;
        border-radius: 50%;
        padding: 0 !important;
        font-weight: 900;
        color: #ffffff !important;
        background: #2563eb !important;
        border: none !important;
        line-height: 26px;
        text-align: center;
      }

      .info-button:hover {
        background: #1d4ed8 !important;
      }

      /* Hide the Dashboard nav tab. The brand text is the home/dashboard link. */
      .navbar-nav > li > a[data-value='Dashboard'] {
        display: none !important;
      }

      .navbar-brand {
        cursor: pointer;
      }

      .dataTables_wrapper .dt-buttons .dt-button {
        background: #1d4ed8 !important;
        color: white !important;
        border: none !important;
        border-radius: 8px !important;
        padding: 5px 10px !important;
      }

      table.dataTable thead th {
        background: #1e3a8a !important;
        color: white !important;
      }

      table.dataTable tbody tr:hover {
        background-color: #dbeafe !important;
      }

      .app-loading-indicator {
        display: none;
        position: fixed;
        left: 50%;
        bottom: calc(18px + env(safe-area-inset-bottom));
        transform: translateX(-50%);
        align-items: center;
        gap: 10px;
        padding: 11px 16px;
        border-radius: 999px;
        background: rgba(15, 23, 42, 0.96);
        color: #ffffff;
        font-size: 14px;
        font-weight: 800;
        box-shadow: 0 12px 30px rgba(15, 23, 42, 0.28);
        z-index: 2500;
        pointer-events: none;
      }

      body.shiny-busy .app-loading-indicator {
        display: flex;
      }

      .app-loading-spinner {
        width: 18px;
        height: 18px;
        border: 3px solid rgba(255, 255, 255, 0.35);
        border-top-color: #ffffff;
        border-radius: 50%;
        animation: app-spin 0.8s linear infinite;
      }

      @keyframes app-spin {
        to { transform: rotate(360deg); }
      }

      table.dataTable > tbody > tr.child ul.dtr-details {
        display: block;
        width: 100%;
      }

      table.dataTable > tbody > tr.child span.dtr-title {
        min-width: 120px;
      }

      @media (max-width: 767px) {

        html,
        body {
          width: 100%;
          overflow-x: hidden;
        }

        body {
          font-size: 16px;
        }

        .navbar-brand {
          max-width: calc(100vw - 75px);
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }

        .navbar-toggle {
          margin-right: 12px;
        }

        .navbar-collapse {
          border-top: 1px solid rgba(255, 255, 255, 0.18);
          box-shadow: none;
        }

        .navbar-nav {
          margin-top: 0;
          margin-bottom: 0;
        }

        .navbar-default .navbar-nav > li > a {
          padding: 14px 20px;
          font-size: 16px;
        }

        .page-wrap {
          padding: 12px;
        }

        .section-card {
          padding: 16px;
          margin-bottom: 12px;
          border-radius: 14px;
        }

        .hero-card h2 {
          margin-top: 0;
          font-size: 26px;
        }

        .hero-card h4 {
          font-size: 16px;
          line-height: 1.4;
        }

        .metric-grid {
          grid-template-columns: 1fr;
          gap: 10px;
          margin-bottom: 12px;
        }

        .metric-card {
          min-height: 0;
          padding: 15px;
          border-radius: 14px;
        }

        .metric-value {
          font-size: 21px;
          overflow-wrap: anywhere;
        }

        .control-row {
          display: block;
          margin-bottom: 4px;
        }

        .control-row .form-group {
          width: 100%;
          min-width: 0;
          margin-bottom: 12px;
        }

        .control-row .selectize-control,
        .control-row .form-control {
          width: 100%;
        }

        .recap-card {
          grid-template-columns: 1fr;
          border-width: 3px;
          border-radius: 14px;
          margin-bottom: 12px;
        }

        .recap-team {
          min-height: 0;
          padding: 18px;
        }

        .recap-team:first-child {
          border-right: none;
          border-bottom: 3px solid #0f172a;
        }

        .recap-team-name {
          font-size: 20px;
        }

        .recap-team-score {
          font-size: 27px;
        }

        .matchup-detail-grid {
          grid-template-columns: 1fr;
          gap: 12px;
        }

        .record-grid {
          grid-template-columns: 1fr;
          gap: 12px;
        }

        .record-table-card {
          padding: 12px;
          border-radius: 14px;
        }

        .section-title-row {
          justify-content: space-between;
        }

        .info-button {
          flex: 0 0 auto;
        }

        #top_players_plot {
          height: 320px !important;
        }

        #manager_score_trend {
          height: 310px !important;
        }

        #manager_position_spider {
          height: 360px !important;
        }

        .shiny-plot-output {
          max-width: 100%;
        }

        .dataTables_wrapper {
          font-size: 13px;
        }

        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_length {
          float: none;
          text-align: left;
          margin-bottom: 8px;
        }

        .dataTables_wrapper .dataTables_filter input {
          width: calc(100% - 75px);
          max-width: none;
        }

        .dataTables_wrapper .dt-buttons {
          float: none;
          margin-bottom: 8px;
        }

        .dataTables_wrapper .dt-buttons .dt-button {
          margin-bottom: 5px;
        }

        table.dataTable thead th,
        table.dataTable tbody td {
          white-space: nowrap;
        }

        .app-loading-indicator {
          width: calc(100vw - 32px);
          justify-content: center;
          text-align: center;
        }
      }
    ")),
    tags$script(HTML("
      $(document).on('click', '.navbar-brand', function(e) {
        e.preventDefault();
        var dashboardTab = $('a[data-value=\"Dashboard\"]');
        if (dashboardTab.length) {
          dashboardTab.tab('show');
        }
      });

      if ('serviceWorker' in navigator) {
        window.addEventListener('load', function() {
          navigator.serviceWorker.register('service-worker.js').catch(function(error) {
            console.warn('Service worker registration failed:', error);
          });
        });
      }
    "))
  ),

  footer = div(
    class = "app-loading-indicator",
    role = "status",
    `aria-live` = "polite",
    div(class = "app-loading-spinner"),
    span("Updating league data...")
  ),

  tabPanel(
    "Dashboard",
    value = "Dashboard",
    div(
      class = "page-wrap",
      div(
        class = "section-card hero-card",
        h2("Last Week's Recap"),
        h4(textOutput("dashboard_week_label"))
      ),
      uiOutput("dashboard_cards"),
      div(
        class = "section-card",
        h3("Matchup Recap"),
        uiOutput("weekly_recap")
      ),
      div(
        class = "section-card",
        div(class = "section-title-row",
            h3("Power Rankings"),
            actionButton("power_info", "i", class = "info-button", title = "How Power Score works")
        ),
        div(class = "power-ranking-wrap", DTOutput("power_rankings_table"))
      )
    )
  ),

  tabPanel(
    "Managers",
    div(
      class = "page-wrap",
      div(
        class = "section-card hero-card",
        h2("Manager Hub"),
        div(
          class = "control-row",
          selectInput("manager_select", "Manager", choices = c("Select manager" = "", managers), selected = ""),
          uiOutput("manager_period_ui")
        )
      ),
      uiOutput("manager_content")
    )
  ),

  tabPanel(
    "Players",
    div(
      class = "page-wrap",
      div(
        class = "section-card hero-card",
        h2("Players"),
        p(class = "muted", "Search player performance by season, week, fantasy manager, and roster slot."),
        div(
          class = "control-row",
          uiOutput("player_year_ui"),
          uiOutput("player_week_ui"),
          uiOutput("player_manager_ui"),
          selectInput("player_slot", "Roster Slot", choices = c("All Slots", all_slots), selected = "All Slots"),
          textInput("player_search", "Player Search", placeholder = "Example: Josh Allen")
        )
      ),
      div(
        class = "section-card",
        h3("Top 3"),
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
    "Record Book",
    div(
      class = "page-wrap",
      div(
        class = "section-card hero-card",
        h2("Record Book"),
        p(class = "muted", "League records by season or all time.")
      ),
      div(
        class = "section-card",
        div(
          class = "control-row",
          selectInput("record_scope", "Record Scope", choices = c("All Time", years), selected = "All Time")
        )
      ),
      uiOutput("record_book_tables")
    )
  ),

  tabPanel(
    "History",
    div(
      class = "page-wrap",
      div(
        class = "section-card hero-card",
        h2("History"),
        p(class = "muted", "Every matchup in league history. Filter the archive, then click a matchup to view player-level data.")
      ),
      div(
        class = "section-card",
        div(
          class = "control-row",
          selectInput("history_year", "Season", choices = c("All Seasons", years), selected = "All Seasons"),
          uiOutput("history_week_ui"),
          selectInput("history_manager", "Manager", choices = c("All Managers", managers), selected = "All Managers")
        )
      ),
      div(
        class = "section-card",
        h3("Matchup Archive"),
        p(class = "muted", "Click a row to view the players from that matchup."),
        DTOutput("history_matchups_table")
      ),
      uiOutput("history_detail_ui")
    )
  )

)

# ---- Server ----

server <- function(input, output, session) {

  observeEvent(input$power_info, {
    showModal(
      modalDialog(
        title = "How Power Rankings Work",
        easyClose = TRUE,
        footer = modalButton("Close"),
        p("Power Score ranks the top teams using three ingredients:"),
        tags$ul(
          tags$li(strong("50% record:"), " teams with better win percentage get the biggest boost."),
          tags$li(strong("30% recent form:"), " teams scoring well over the last 3 games are rewarded."),
          tags$li(strong("20% season scoring:"), " teams with stronger overall scoring averages get a smaller boost.")
        ),
        p("The score is scaled from current-season league results, then combined into one ranking number.")
      )
    )
  })

  weeks_for_year <- function(data, year_value) {
    data |>
      filter(year == year_value) |>
      pull(week) |>
      unique() |>
      sort()
  }

  output$player_year_ui <- renderUI({
    selected_manager <- input$player_manager
    available_years <- players

    if (!is.null(selected_manager) && selected_manager != "All Managers") {
      available_years <- available_years |> filter(manager == selected_manager)
    }

    year_choices <- sort(unique(available_years$year), decreasing = TRUE)
    current_selection <- isolate(input$player_year)

    selected_year <- if (!is.null(current_selection) && current_selection %in% year_choices) {
      current_selection
    } else {
      max(year_choices, na.rm = TRUE)
    }

    selectInput("player_year", "Season", choices = year_choices, selected = selected_year)
  })

  output$player_manager_ui <- renderUI({
    selected_year <- input$player_year
    available_managers <- players

    if (!is.null(selected_year) && !is.na(selected_year)) {
      available_managers <- available_managers |> filter(year == selected_year)
    }

    manager_choices <- sort(unique(available_managers$manager))
    current_selection <- isolate(input$player_manager)

    selected_manager <- if (!is.null(current_selection) && current_selection %in% manager_choices) {
      current_selection
    } else {
      "All Managers"
    }

    selectInput("player_manager", "Manager", choices = c("All Managers", manager_choices), selected = selected_manager)
  })

  output$player_week_ui <- renderUI({
    req(input$player_year)

    weeks <- weeks_for_year(players, input$player_year)
    current_week <- isolate(input$player_week)

    selected_week <- if (is.null(current_week)) {
      if (length(weeks) > 0) max(weeks, na.rm = TRUE) else NA_integer_
    } else if (!is.null(current_week) && current_week %in% as.character(c("All Weeks", weeks))) {
      current_week
    } else if (!is.null(current_week) && suppressWarnings(as.integer(current_week)) %in% weeks) {
      as.integer(current_week)
    } else {
      if (length(weeks) > 0) max(weeks, na.rm = TRUE) else NA_integer_
    }

    selectInput("player_week", "Week", choices = c("All Weeks", weeks), selected = selected_week)
  })

  output$manager_period_ui <- renderUI({
    if (is.null(input$manager_select) || input$manager_select == "") {
      return(selectInput("manager_period", "Years / Team Name", choices = c("Select manager first" = ""), selected = ""))
    }

    manager_years <- sort(unique(matchups$year[matchups$manager == input$manager_select]), decreasing = TRUE)

    labels <- c("All Years")
    values <- c("All Years")

    if (length(manager_years) > 0) {
      year_labels <- vapply(
        manager_years,
        function(y) {
          tn <- resolve_team_name_one(input$manager_select, y)
          paste0(y, " — ", tn)
        },
        character(1)
      )

      labels <- c(labels, year_labels)
      values <- c(values, as.character(manager_years))
    }

    selectInput(
      "manager_period",
      "Years / Team Name",
      choices = stats::setNames(values, labels),
      selected = "All Years"
    )
  })

  dashboard_year <- reactive({
    matchups |>
      summarise(latest_year = max(year, na.rm = TRUE)) |>
      pull(latest_year)
  })

  dashboard_week <- reactive({
    matchups |>
      filter(year == dashboard_year()) |>
      summarise(latest_week = max(week, na.rm = TRUE)) |>
      pull(latest_week)
  })

  selected_week_pair_games <- reactive({
    pair_games |>
      filter(year == dashboard_year(), week == dashboard_week()) |>
      arrange(desc(winning_score))
  })

  output$dashboard_week_label <- renderText({
    paste0("Week ", dashboard_week(), ", ", dashboard_year())
  })

  season_matchups <- reactive({
    matchups |> filter(year == dashboard_year())
  })

  standings <- reactive({
    season_matchups() |>
      group_by(manager) |>
      summarise(
        wins = sum(win, na.rm = TRUE),
        losses = sum(loss, na.rm = TRUE),
        points_for = sum(points_for, na.rm = TRUE),
        points_against = sum(points_against, na.rm = TRUE),
        point_diff = points_for - points_against,
        avg_score = mean(points_for, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        record = make_record(wins, losses),
        win_pct = wins / pmax(wins + losses, 1)
      ) |>
      arrange(desc(wins), desc(points_for))
  })

  output$dashboard_cards <- renderUI({
    current_year <- dashboard_year()
    current_week <- dashboard_week()
    week_data <- matchups |> filter(year == current_year, week == current_week)
    pair_data <- selected_week_pair_games()

    validate(
      need(nrow(week_data) > 0, "No matchup data found for the most recent week.")
    )

    high_score <- week_data |> slice_max(points_for, n = 1, with_ties = FALSE)
    low_score <- week_data |> slice_min(points_for, n = 1, with_ties = FALSE)
    closest <- pair_data |> slice_min(margin, n = 1, with_ties = FALSE)
    blowout <- pair_data |> slice_max(margin, n = 1, with_ties = FALSE)
    league_avg <- mean(week_data$points_for, na.rm = TRUE)

    div(
      class = "metric-grid",
      card(
        "Highest Scorer",
        HTML(paste0(high_score$team, " — <span class='score-number'>", score_fmt(high_score$points_for), "</span>")),
        NULL,
        "accent-green"
      ),
      card(
        "Lowest Scorer",
        HTML(paste0(low_score$team, " — <span class='score-number'>", score_fmt(low_score$points_for), "</span>")),
        NULL,
        "accent-red"
      ),
      card(
        "Closest Matchup",
        paste0(closest$team_a, " vs ", closest$team_b),
        HTML(paste0("Margin: <span class='score-number'>", score_fmt(closest$margin), "</span>")),
        "accent-cyan"
      ),
      card(
        "Biggest Blowout",
        paste0(blowout$team_a, " vs ", blowout$team_b),
        HTML(paste0("Margin: <span class='score-number'>", score_fmt(blowout$margin), "</span>")),
        "accent-gold"
      ),
      card(
        "League Avg Score",
        HTML(paste0("<span class='score-number'>", score_fmt(league_avg), "</span>")),
        NULL,
        "accent-purple"
      )
    )
  })

  output$weekly_recap <- renderUI({
    games <- selected_week_pair_games()

    validate(
      need(nrow(games) > 0, "No matchup data found for the most recent week.")
    )

    recap_items <- lapply(seq_len(nrow(games)), function(i) {
      manager_a <- games[["manager_a"]][[i]]
      manager_b <- games[["manager_b"]][[i]]
      team_a <- games[["team_a"]][[i]]
      team_b <- games[["team_b"]][[i]]
      score_a <- games[["score_a"]][[i]]
      score_b <- games[["score_b"]][[i]]
      margin <- games[["margin"]][[i]]
      winner <- games[["winner"]][[i]]
      loser <- games[["loser"]][[i]]
      year_value <- games[["year"]][[i]]
      week_value <- games[["week"]][[i]]

      div(
        class = "recap-card",
        div(
          class = paste("recap-team", ifelse(score_a >= score_b, "recap-winner", "")),
          div(class = "recap-team-name", team_a),
          div(class = "recap-team-score", score_fmt(score_a))
        ),
        div(
          class = paste("recap-team", ifelse(score_b >= score_a, "recap-winner", "")),
          div(class = "recap-team-name", team_b),
          div(class = "recap-team-score", score_fmt(score_b))
        )
      )
    })

    tagList(recap_items)
  })

  power_rankings <- reactive({
    current_year <- dashboard_year()
    current_week <- dashboard_week()

    season_data <- matchups |>
      filter(year == current_year, week <= current_week)

    recent_start <- max(1, current_week - 2)

    recent_data <- season_data |>
      filter(week >= recent_start)

    base <- season_data |>
      group_by(manager) |>
      summarise(
        wins = sum(win, na.rm = TRUE),
        losses = sum(loss, na.rm = TRUE),
        points_for = sum(points_for, na.rm = TRUE),
        points_against = sum(points_against, na.rm = TRUE),
        avg_score = mean(points_for, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        win_pct = wins / pmax(wins + losses, 1),
        record = make_record(wins, losses)
      )

    recent <- recent_data |>
      group_by(manager) |>
      summarise(
        recent_avg = mean(points_for, na.rm = TRUE),
        .groups = "drop"
      )

    ranked <- base |>
      left_join(recent, by = "manager") |>
      mutate(
        recent_avg = if_else(is.na(recent_avg), avg_score, recent_avg),
        win_pct_score = dplyr::percent_rank(win_pct) * 100,
        recent_score = dplyr::percent_rank(recent_avg) * 100,
        season_score = dplyr::percent_rank(avg_score) * 100,
        win_pct_score = if_else(is.na(win_pct_score), 50, win_pct_score),
        recent_score = if_else(is.na(recent_score), 50, recent_score),
        season_score = if_else(is.na(season_score), 50, season_score),
        power_score = 0.50 * win_pct_score + 0.30 * recent_score + 0.20 * season_score
      ) |>
      arrange(desc(power_score), desc(points_for)) |>
      mutate(rank = row_number())

    ranked
  })

  output$power_rankings_table <- renderDT({
    power_rankings() |>
      slice_head(n = 5) |>
      mutate(
        team_name = mapply(resolve_team_name_one, manager, dashboard_year(), USE.NAMES = FALSE)
      ) |>
      transmute(
        Rank = rank,
        `Team Name` = team_name,
        Record = record,
        `Win %` = pct_fmt(win_pct),
        `Last 3 Avg` = round(recent_avg, 2),
        `Power Score` = round(power_score, 1)
      ) |>
      datatable_simple(page_length = 5)
  })

  selected_manager_data <- reactive({
    req(input$manager_select, input$manager_period)
    validate(need(input$manager_select != "", "Select a manager to view this tab."))

    data <- matchups |>
      filter(manager == input$manager_select)

    if (input$manager_period != "All Years") {
      data <- data |>
        filter(year == as.integer(input$manager_period))
    }

    data |>
      arrange(year, week)
  })

  output$manager_content <- renderUI({
    if (is.null(input$manager_select) || input$manager_select == "") {
      return(NULL)
    }

    tagList(
      uiOutput("manager_cards"),
      div(
        class = "section-card",
        h3("Weekly Score Trend"),
        plotOutput("manager_score_trend", height = "400px")
      ),
      div(
        class = "section-card",
        div(
          class = "matchup-detail-grid",
          div(
            h3("Positional Ranking Breakdown"),
            plotOutput("manager_position_spider", height = "480px")
          ),
          div(
            uiOutput("manager_finish_summary")
          )
        )
      )
    )
  })

  output$manager_cards <- renderUI({
    data <- selected_manager_data()

    validate(
      need(nrow(data) > 0, "No manager data found.")
    )

    wins <- sum(data$win, na.rm = TRUE)
    losses <- sum(data$loss, na.rm = TRUE)
    pf <- sum(data$points_for, na.rm = TRUE)
    pa <- sum(data$points_against, na.rm = TRUE)
    best_week <- data |> slice_max(points_for, n = 1, with_ties = FALSE)
    worst_week <- data |> slice_min(points_for, n = 1, with_ties = FALSE)

    if (input$manager_period == "All Years") {
      best_subtitle <- paste0(
        "Week ", best_week$week,
        ", ", best_week$year,
        " vs. ", best_week$opposing_manager
      )
      worst_subtitle <- paste0(
        "Week ", worst_week$week,
        ", ", worst_week$year,
        " vs. ", worst_week$opposing_manager
      )
    } else {
      best_subtitle <- paste0(
        "Week ", best_week$week,
        " vs. ", best_week$opposing_team
      )
      worst_subtitle <- paste0(
        "Week ", worst_week$week,
        " vs. ", worst_week$opposing_team
      )
    }

    div(
      class = "metric-grid",
      card("Record", make_record(wins, losses), NULL, "accent-blue"),
      card("Win Percentage", pct_fmt(wins / pmax(wins + losses, 1)), NULL, "accent-green"),
      card("Points For", score_fmt(pf), paste0("Average: ", score_fmt(mean(data$points_for, na.rm = TRUE))), "accent-purple"),
      card("Points Against", score_fmt(pa), paste0("Average: ", score_fmt(mean(data$points_against, na.rm = TRUE))), "accent-red"),
      card(
        "Best Week",
        score_fmt(best_week$points_for),
        best_subtitle,
        "accent-gold"
      ),
      card(
        "Worst Week",
        score_fmt(worst_week$points_for),
        worst_subtitle,
        "accent-cyan"
      )
    )
  })

  output$manager_score_trend <- renderPlot({
    trend_data <- selected_manager_data() |>
      arrange(year, week) |>
      mutate(game_index = row_number())

    if (input$manager_period == "All Years") {
      axis_breaks <- trend_data |>
        group_by(year) |>
        summarise(game_index = min(game_index), .groups = "drop")

      x_breaks <- axis_breaks$game_index
      x_labels <- as.character(axis_breaks$year)
      x_title <- "Season"
    } else {
      max_game <- max(trend_data$game_index, na.rm = TRUE)
      step <- ifelse(max_game <= 8, 1, ifelse(max_game <= 14, 2, 3))
      x_breaks <- seq(1, max_game, by = step)
      week_lookup <- trend_data |>
        select(game_index, week)

      x_labels <- paste0(
        "W",
        week_lookup$week[match(x_breaks, week_lookup$game_index)]
      )
      x_title <- "Week"
    }

    ggplot(trend_data, aes(x = game_index, y = points_for)) +
      geom_line(linewidth = 1.1, color = "#2563eb") +
      geom_point(size = 2.7, color = "#f59e0b") +
      scale_x_continuous(breaks = x_breaks, labels = x_labels) +
      labs(
        x = x_title,
        y = "Points For",
        title = paste("Score Trend:", input$manager_select)
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })

  output$manager_finish_summary <- renderUI({
    req(input$manager_select)
    validate(need(input$manager_select != "", ""))

    selected_finishes <- manager_finishes |>
      filter(manager == input$manager_select)

    if (input$manager_period == "All Years") {
      best_finishes <- selected_finishes |>
        arrange(finish, desc(year)) |>
        slice_head(n = 3)

      if (nrow(best_finishes) == 0) {
        return(div(class = "finish-card", div(class = "finish-title", "Best Finishes"), p(class = "muted", "No playoff finish data available yet.")))
      }

      finish_rows <- lapply(seq_len(nrow(best_finishes)), function(i) {
        div(
          class = "finish-row",
          div(class = "finish-rank", best_finishes$finish_label[[i]]),
          div(class = "finish-detail", paste0(best_finishes$year[[i]], " — ", best_finishes$team[[i]]))
        )
      })

      div(
        class = "finish-card",
        div(class = "finish-title", "Best Finishes"),
        tagList(finish_rows)
      )
    } else {
      selected_year <- as.integer(input$manager_period)
      finish <- selected_finishes |>
        filter(year == selected_year) |>
        slice_head(n = 1)

      if (nrow(finish) == 0) {
        return(div(class = "finish-card", div(class = "finish-title", "Season Finish"), p(class = "muted", "No playoff finish data available for this season.")))
      }

      div(
        class = "finish-card",
        div(class = "finish-title", "Season Finish"),
        div(class = "finish-row",
            div(class = "finish-rank", finish$finish_label[[1]]),
            div(class = "finish-detail", finish$team[[1]])
        )
      )
    }
  })

  output$manager_position_spider <- renderPlot({
    req(input$manager_select, input$manager_period)

    position_levels <- c("QB", "RB", "WR", "TE", "D/ST", "K")

    position_data <- players |>
      mutate(
        pos_clean = case_when(
          pos %in% c("QB", "RB", "WR", "TE", "K") ~ pos,
          pos %in% c("D/ST", "DST", "DEF", "D") ~ "D/ST",
          TRUE ~ as.character(pos)
        )
      ) |>
      filter(pos_clean %in% position_levels)

    if (input$manager_period != "All Years") {
      position_data <- position_data |>
        filter(year == as.integer(input$manager_period))
    }

    manager_position_totals <- position_data |>
      group_by(manager, pos_clean) |>
      summarise(points = sum(fpts, na.rm = TRUE), .groups = "drop")

    validate(
      need(nrow(manager_position_totals) > 0, "No player scoring data found for this period.")
    )

    manager_position_totals <- manager_position_totals |>
      group_by(pos_clean) |>
      mutate(
        rank = dense_rank(desc(points)),
        n_managers = n_distinct(manager)
      ) |>
      ungroup()

    selected_ranks <- tibble(pos_clean = position_levels) |>
      left_join(
        manager_position_totals |>
          filter(manager == input$manager_select) |>
          select(pos_clean, points, rank, n_managers),
        by = "pos_clean"
      ) |>
      mutate(
        points = if_else(is.na(points), 0, points),
        n_managers = if_else(is.na(n_managers), max(manager_position_totals$n_managers, na.rm = TRUE), n_managers),
        rank = if_else(is.na(rank), n_managers, rank),
        rank_score = if_else(n_managers <= 1, 1, (n_managers - rank + 1) / n_managers),
        rank_label = paste0("#", rank),
        pos_clean = factor(pos_clean, levels = position_levels)
      )

    angles <- seq(pi / 2, pi / 2 - 2 * pi + 2 * pi / length(position_levels), length.out = length(position_levels))

    selected_ranks <- selected_ranks |>
      mutate(
        angle = angles,
        x = rank_score * cos(angle),
        y = rank_score * sin(angle),
        label_x = 1.13 * cos(angle),
        label_y = 1.13 * sin(angle),
        rank_x = pmin(1.04, rank_score + 0.11) * cos(angle),
        rank_y = pmin(1.04, rank_score + 0.11) * sin(angle)
      )

    polygon_data <- bind_rows(selected_ranks, selected_ranks[1, ])

    grid_data <- lapply(c(0.25, 0.50, 0.75, 1.00), function(r) {
      tibble(
        level = r,
        angle = c(angles, angles[1]),
        x = r * cos(c(angles, angles[1])),
        y = r * sin(c(angles, angles[1]))
      )
    }) |>
      bind_rows()

    axis_data <- tibble(
      x = 0,
      y = 0,
      xend = cos(angles),
      yend = sin(angles)
    )

    ggplot() +
      geom_path(
        data = grid_data,
        aes(x = x, y = y, group = level),
        color = "#dbeafe",
        linewidth = 0.8
      ) +
      geom_segment(
        data = axis_data,
        aes(x = x, y = y, xend = xend, yend = yend),
        color = "#e5e7eb",
        linewidth = 0.8
      ) +
      geom_polygon(
        data = polygon_data,
        aes(x = x, y = y),
        fill = "#2563eb",
        alpha = 0.22,
        color = "#1d4ed8",
        linewidth = 1.3
      ) +
      geom_path(
        data = polygon_data,
        aes(x = x, y = y),
        color = "#1d4ed8",
        linewidth = 1.3
      ) +
      geom_point(
        data = selected_ranks,
        aes(x = x, y = y),
        color = "#f59e0b",
        size = 3.2
      ) +
      geom_text(
        data = selected_ranks,
        aes(x = label_x, y = label_y, label = paste0(pos_clean, "\n", rank_label)),
        fontface = "bold",
        color = "#0f172a",
        size = 4.3,
        lineheight = 0.9
      ) +
      coord_equal(xlim = c(-1.25, 1.25), ylim = c(-1.25, 1.25), clip = "off") +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme_void(base_size = 13) +
      theme(
        plot.margin = margin(20, 40, 20, 40)
      )
  })

  player_filtered <- reactive({
    req(input$player_year, input$player_week, input$player_manager, input$player_slot)

    data <- players |> filter(year == input$player_year)

    if (input$player_week != "All Weeks") {
      data <- data |> filter(week == as.integer(input$player_week))
    }

    if (input$player_manager != "All Managers") {
      data <- data |> filter(manager == input$player_manager)
    }

    if (input$player_slot != "All Slots") {
      data <- data |> filter(slot == input$player_slot)
    }

    if (!is.null(input$player_search) && nzchar(input$player_search)) {
      data <- data |> filter(str_detect(str_to_lower(player_name), str_to_lower(input$player_search)))
    }

    data
  })

  output$top_players_plot <- renderPlot({
    plot_data <- player_filtered() |>
      group_by(player_name, pos, slot) |>
      summarise(total_fpts = sum(fpts, na.rm = TRUE), .groups = "drop") |>
      slice_max(total_fpts, n = 3) |>
      arrange(desc(total_fpts)) |>
      mutate(
        rank = row_number(),
        podium_order = case_when(
          rank == 1 ~ 2,
          rank == 2 ~ 1,
          rank == 3 ~ 3
        ),
        rank_label = paste0("#", rank),
        display_label = stringr::str_wrap(player_name, width = 14)
      ) |>
      arrange(podium_order)

    validate(
      need(nrow(plot_data) > 0, "No player data found for these filters.")
    )

    max_points <- max(plot_data$total_fpts, na.rm = TRUE)

    ggplot(plot_data, aes(x = factor(podium_order), y = total_fpts)) +
      geom_col(aes(fill = factor(rank)), width = 0.72, show.legend = FALSE) +
      geom_text(
        aes(y = total_fpts + max_points * 0.06, label = rank_label),
        fontface = "bold",
        size = 8,
        color = "#0f172a"
      ) +
      geom_text(
        aes(y = total_fpts * 0.50, label = paste0(display_label, "\n", score_fmt(total_fpts), " pts")),
        fontface = "bold",
        size = 5.2,
        color = "#0f172a",
        lineheight = 0.95
      ) +
      scale_x_discrete(labels = NULL) +
      scale_y_continuous(limits = c(0, max_points * 1.18), expand = expansion(mult = c(0, 0.02))) +
      scale_fill_manual(values = c("1" = "#D4AF37", "2" = "#C0C0C0", "3" = "#CD7F32")) +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme_void(base_size = 13) +
      theme(
        plot.margin = margin(20, 20, 20, 20)
      )
  })

  output$players_table <- renderDT({
    player_filtered() |>
      arrange(desc(fpts)) |>
      transmute(
        Season = year,
        Week = week,
        Player = player_name,
        Manager = manager,
        `Fantasy Team` = fantasy_team,
        Position = pos,
        `NFL Team` = team,
        Slot = slot,
        Projection = round(proj, 2),
        Points = round(fpts, 2),
        `Over Projection` = round(fpts - proj, 2)
      ) |>
      datatable_no_buttons(page_length = 25)
  })











  record_scope_year <- reactive({
    if (is.null(input$record_scope) || input$record_scope == "All Time") {
      return(NA_integer_)
    }

    as.integer(input$record_scope)
  })

  record_pair_games <- reactive({
    data <- pair_games

    if (!is.na(record_scope_year())) {
      data <- data |> filter(year == record_scope_year())
    }

    data
  })

  record_matchups <- reactive({
    data <- matchups

    if (!is.na(record_scope_year())) {
      data <- data |> filter(year == record_scope_year())
    }

    data
  })

  record_players <- reactive({
    data <- players

    if (!is.na(record_scope_year())) {
      data <- data |> filter(year == record_scope_year())
    }

    data
  })

  record_limit <- reactive({
    if (is.na(record_scope_year())) 5 else 3
  })

  output$record_book_tables <- renderUI({
    if (is.na(record_scope_year())) {
      tagList(
        div(
          class = "record-grid",
          div(class = "record-table-card", h3("Single-Week Scores"), DTOutput("record_single_week")),
          div(class = "record-table-card", h3("Biggest Blowouts"), DTOutput("record_blowouts")),
          div(class = "record-table-card", h3("Closest Games"), DTOutput("record_closest")),
          div(class = "record-table-card", h3("Hospital"), p(class = "muted", "Most weeks with a player in an IR slot."), DTOutput("record_hospital")),
          div(class = "record-table-card", h3("Total Points"), DTOutput("record_total_points")),
          div(class = "record-table-card", h3("Total Wins"), DTOutput("record_total_wins")),
          div(class = "record-table-card", h3("Total Championships"), DTOutput("record_championships")),
          div(class = "record-table-card", h3("Points by Roster Slot"), DTOutput("record_slot_points"))
        )
      )
    } else {
      tagList(
        div(
          class = "record-grid",
          div(class = "record-table-card", h3("Single-Week Scores"), DTOutput("record_single_week")),
          div(class = "record-table-card", h3("Biggest Blowouts"), DTOutput("record_blowouts")),
          div(class = "record-table-card", h3("Closest Games"), DTOutput("record_closest"))
        )
      )
    }
  })

  output$record_single_week <- renderDT({
    record_matchups() |>
      arrange(desc(points_for)) |>
      transmute(
        Season = year,
        Week = week,
        Team = team,
        Manager = manager,
        Score = round(points_for, 2),
        Opponent = opposing_team
      ) |>
      slice_head(n = record_limit()) |>
      datatable_record(page_length = record_limit()) |>
      DT::formatStyle(
        columns = "Score",
        color = "#1d4ed8",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_blowouts <- renderDT({
    record_pair_games() |>
      arrange(desc(margin)) |>
      mutate(
        WinningTeam = if_else(score_a >= score_b, team_a, team_b),
        LosingTeam = if_else(score_a < score_b, team_a, team_b),
        WinningScore = pmax(score_a, score_b),
        LosingScore = pmin(score_a, score_b)
      ) |>
      transmute(
        Season = year,
        Week = week,
        `Winning Team` = WinningTeam,
        `Winning Score` = round(WinningScore, 2),
        `Losing Team` = LosingTeam,
        `Losing Score` = round(LosingScore, 2),
        Margin = round(margin, 2)
      ) |>
      slice_head(n = record_limit()) |>
      datatable_record(page_length = record_limit()) |>
      DT::formatStyle(
        columns = c("Winning Score", "Losing Score", "Margin"),
        color = "#1d4ed8",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_closest <- renderDT({
    record_pair_games() |>
      arrange(margin) |>
      mutate(
        WinningTeam = if_else(score_a >= score_b, team_a, team_b),
        LosingTeam = if_else(score_a < score_b, team_a, team_b),
        WinningScore = pmax(score_a, score_b),
        LosingScore = pmin(score_a, score_b)
      ) |>
      transmute(
        Season = year,
        Week = week,
        `Winning Team` = WinningTeam,
        `Winning Score` = round(WinningScore, 2),
        `Losing Team` = LosingTeam,
        `Losing Score` = round(LosingScore, 2),
        Margin = round(margin, 2)
      ) |>
      slice_head(n = record_limit()) |>
      datatable_record(page_length = record_limit()) |>
      DT::formatStyle(
        columns = c("Winning Score", "Losing Score", "Margin"),
        color = "#1d4ed8",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_hospital <- renderDT({
    record_players() |>
      filter(slot %in% c("IR", "Injured Reserve", "IL")) |>
      distinct(year, week, manager, fantasy_team) |>
      count(manager, fantasy_team, name = "IR Weeks") |>
      arrange(desc(`IR Weeks`)) |>
      transmute(
        Team = fantasy_team,
        Manager = manager,
        `IR Weeks`
      ) |>
      slice_head(n = 5) |>
      datatable_record(page_length = 5)
  }, server = FALSE)

  output$record_total_points <- renderDT({
    matchups |>
      group_by(manager) |>
      summarise(
        `Total Points` = sum(points_for, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(team_name = mapply(resolve_team_name_one, manager, latest_year, USE.NAMES = FALSE)) |>
      arrange(desc(`Total Points`)) |>
      transmute(
        Team = team_name,
        Manager = manager,
        `Total Points` = round(`Total Points`, 2)
      ) |>
      slice_head(n = 5) |>
      datatable_record(page_length = 5) |>
      DT::formatStyle(
        columns = "Total Points",
        color = "#1d4ed8",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_total_wins <- renderDT({
    matchups |>
      group_by(manager) |>
      summarise(
        Wins = sum(win, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(team_name = mapply(resolve_team_name_one, manager, latest_year, USE.NAMES = FALSE)) |>
      arrange(desc(Wins)) |>
      transmute(
        Team = team_name,
        Manager = manager,
        Wins
      ) |>
      slice_head(n = 5) |>
      datatable_record(page_length = 5) |>
      DT::formatStyle(
        columns = "Wins",
        color = "#1d4ed8",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_championships <- renderDT({
    manager_finishes |>
      filter(finish == 1L) |>
      count(manager, name = "Championships") |>
      mutate(team_name = mapply(resolve_team_name_one, manager, latest_year, USE.NAMES = FALSE)) |>
      arrange(desc(Championships), manager) |>
      transmute(
        Team = team_name,
        Manager = manager,
        Championships
      ) |>
      slice_head(n = 5) |>
      datatable_record(page_length = 5) |>
      DT::formatStyle(
        columns = "Championships",
        color = "#1d4ed8",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_slot_points <- renderDT({
    players |>
      group_by(manager, slot) |>
      summarise(
        Points = sum(fpts, na.rm = TRUE),
        .groups = "drop"
      ) |>
      group_by(slot) |>
      slice_max(Points, n = 1, with_ties = FALSE) |>
      ungroup() |>
      mutate(team_name = mapply(resolve_team_name_one, manager, latest_year, USE.NAMES = FALSE)) |>
      arrange(slot) |>
      transmute(
        Slot = slot,
        Team = team_name,
        Manager = manager,
        Points = round(Points, 2)
      ) |>
      datatable_record(page_length = 15) |>
      DT::formatStyle(
        columns = "Points",
        color = "#1d4ed8",
        fontWeight = "bold"
      )
  }, server = FALSE)


  history_week_choices <- reactive({
    data <- matchups

    if (!is.null(input$history_year) && input$history_year != "All Seasons") {
      data <- data |> filter(year == as.integer(input$history_year))
    }

    sort(unique(data$week))
  })

  output$history_week_ui <- renderUI({
    weeks_available <- history_week_choices()
    selectInput(
      "history_week",
      "Week",
      choices = c("All Weeks", weeks_available),
      selected = "All Weeks"
    )
  })

  history_matchups <- reactive({
    data <- pair_games

    if (!is.null(input$history_year) && input$history_year != "All Seasons") {
      data <- data |> filter(year == as.integer(input$history_year))
    }

    if (!is.null(input$history_week) && input$history_week != "All Weeks") {
      data <- data |> filter(week == as.integer(input$history_week))
    }

    if (!is.null(input$history_manager) && input$history_manager != "All Managers") {
      data <- data |> filter(manager_a == input$history_manager | manager_b == input$history_manager)
    }

    data |>
      mutate(
        winning_team = if_else(score_a >= score_b, team_a, team_b),
        winning_score = pmax(score_a, score_b),
        losing_team = if_else(score_a < score_b, team_a, team_b),
        losing_score = pmin(score_a, score_b)
      ) |>
      arrange(desc(year), desc(week), desc(winning_score)) |>
      mutate(matchup_id = row_number())
  })

  output$history_matchups_table <- renderDT({
    history_matchups() |>
      transmute(
        Season = year,
        Week = week,
        `Winning Team` = winning_team,
        `Winning Score` = round(winning_score, 2),
        `Losing Team` = losing_team,
        `Losing Score` = round(losing_score, 2)
      ) |>
      datatable_no_buttons(page_length = 15) |>
      DT::formatStyle(
        columns = c("Winning Score", "Losing Score"),
        color = "#1d4ed8",
        fontWeight = "bold"
      )
  }, server = FALSE)

  selected_history_matchup <- reactive({
    selected_row <- input$history_matchups_table_rows_selected

    if (is.null(selected_row) || length(selected_row) == 0) {
      return(NULL)
    }

    history_matchups()[selected_row[1], ]
  })

  output$history_detail_ui <- renderUI({
    matchup <- selected_history_matchup()

    if (is.null(matchup)) {
      return(NULL)
    }

    div(
      class = "section-card",
      h3("Matchup Detail"),
      p(class = "muted", paste0(matchup$matchup_type, " — Week ", matchup$week, ", ", matchup$year)),
      div(
        class = "matchup-detail-grid",
        div(
          class = "matchup-team-panel",
          div(
            class = "matchup-team-header",
            div(class = "matchup-team-title", paste0(matchup$manager_a, " — ", matchup$team_a)),
            div(class = "matchup-team-score", score_fmt(matchup$score_a))
          ),
          div(class = "matchup-team-body", DTOutput("history_team_a_players"))
        ),
        div(
          class = "matchup-team-panel",
          div(
            class = "matchup-team-header",
            div(class = "matchup-team-title", paste0(matchup$manager_b, " — ", matchup$team_b)),
            div(class = "matchup-team-score", score_fmt(matchup$score_b))
          ),
          div(class = "matchup-team-body", DTOutput("history_team_b_players"))
        )
      )
    )
  })

  roster_slot_order <- c("QB", "RB", "RB/WR", "WR", "TE", "FLEX", "D/ST", "K", "BE", "IR")

  matchup_player_table <- function(team_name_value, matchup) {
    players |>
      filter(
        year == matchup$year,
        week == matchup$week,
        fantasy_team == team_name_value
      ) |>
      mutate(slot_order = match(slot, roster_slot_order)) |>
      arrange(is.na(slot_order), slot_order, desc(fpts)) |>
      transmute(
        Player = player_name,
        Slot = slot,
        Projection = round(proj, 2),
        Points = round(fpts, 2)
      )
  }

  output$history_team_a_players <- renderDT({
    matchup <- selected_history_matchup()
    req(matchup)

    datatable_simple(matchup_player_table(matchup$team_a, matchup), page_length = 20) |>
      DT::formatStyle(
        columns = "Points",
        color = "#1d4ed8",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$history_team_b_players <- renderDT({
    matchup <- selected_history_matchup()
    req(matchup)

    datatable_simple(matchup_player_table(matchup$team_b, matchup), page_length = 20) |>
      DT::formatStyle(
        columns = "Points",
        color = "#1d4ed8",
        fontWeight = "bold"
      )
  }, server = FALSE)


}

shinyApp(ui = ui, server = server)
