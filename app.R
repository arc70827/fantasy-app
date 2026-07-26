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

datatable_clean <- function(data, page_length = 15, selection = "none") {
  DT::datatable(
    data,
    rownames = FALSE,
    selection = selection,
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
    class = "stripe compact nowrap"
  )
}

datatable_simple <- function(data, page_length = 5, selection = "none") {
  DT::datatable(
    data,
    rownames = FALSE,
    selection = selection,
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
    class = "stripe compact nowrap"
  )
}

datatable_no_buttons <- function(data, page_length = 25, selection = "none") {
  DT::datatable(
    data,
    rownames = FALSE,
    selection = selection,
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
    class = "stripe compact nowrap"
  )
}

datatable_record <- function(data, page_length = 5, selection = "none") {
  DT::datatable(
    data,
    rownames = FALSE,
    selection = selection,
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
    class = "stripe compact nowrap"
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
  title = div(class = "brand-title", "Fantasy League Hub"),
  id = "main_tabs",
  selected = "Dashboard",
  windowTitle = "Fantasy League Hub",

  header = tagList(
    tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1, viewport-fit=cover"
    ),
    tags$meta(name = "theme-color", content = "#9D0B1E"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "black-translucent"),
    tags$meta(name = "apple-mobile-web-app-title", content = "Fantasy Hub"),
    tags$link(rel = "manifest", href = "manifest.json"),
    tags$link(rel = "apple-touch-icon", sizes = "192x192", href = "img/icon-192.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "192x192", href = "img/icon-192.png"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@400;500;600;700;800;900&display=swap"
    ),
    tags$style(HTML("
      :root {
        --primary-red: #BE1C30;
        --dark-red: #9D0B1E;
        --near-black: #2B1E1E;
        --off-white: #E0E3E4;
        --charcoal: #4C4142;
        --surface: #ffffff;
        --surface-soft: rgba(255, 255, 255, 0.95);
        --muted: rgba(76, 65, 66, 0.80);
        --border: rgba(76, 65, 66, 0.22);
      }

      html {
        scroll-padding-top: 54px;
      }

      body {
        margin: 0;
        padding-top: calc(48px + env(safe-area-inset-top));
        background:
          linear-gradient(135deg, rgba(190, 28, 48, 0.08), transparent 34%),
          linear-gradient(315deg, rgba(43, 30, 30, 0.07), transparent 38%),
          var(--off-white);
        color: var(--near-black);
        font-family: 'Barlow Condensed', 'Arial Narrow', 'Roboto Condensed', Arial, sans-serif;
        font-size: 15px;
        font-weight: 500;
        line-height: 1.25;
      }

      a {
        color: var(--dark-red);
      }

      a:hover,
      a:focus {
        color: var(--primary-red);
      }

      h1, h2, h3, h4, h5, h6,
      .navbar-brand,
      .btn,
      label,
      table.dataTable thead th {
        font-family: 'Barlow Condensed', 'Arial Narrow', 'Roboto Condensed', Arial, sans-serif;
      }

      h2, h3 {
        color: var(--near-black);
        font-weight: 900;
        letter-spacing: 0.01em;
      }

      h2 {
        font-size: 24px;
        margin-top: 0;
        margin-bottom: 6px;
      }

      h3 {
        font-size: 19px;
        margin-top: 0;
        margin-bottom: 8px;
      }

      p {
        margin-bottom: 7px;
      }

      .navbar {
        position: fixed;
        top: 0;
        right: 0;
        left: 0;
        z-index: 1030;
        padding-top: env(safe-area-inset-top);
        min-height: 48px;
        margin-bottom: 0;
        border: none;
        border-radius: 0;
        background: linear-gradient(90deg, var(--dark-red), var(--near-black));
        box-shadow: 0 4px 14px rgba(43, 30, 30, 0.28);
      }

      .navbar > .container,
      .navbar > .container-fluid {
        position: relative;
        padding-right: 0;
        padding-left: 0;
      }

      .navbar-header {
        float: none !important;
        width: 100%;
      }

      .navbar-toggle {
        display: none !important;
      }

      .navbar-default .navbar-brand {
        float: none !important;
        display: flex;
        align-items: center;
        width: 100%;
        height: 48px;
        margin: 0 !important;
        padding: 0 14px;
        color: var(--off-white) !important;
        cursor: pointer;
        font-size: 18px;
        font-weight: 900;
        letter-spacing: 0.055em;
        text-transform: uppercase;
        user-select: none;
      }

      .navbar-default .navbar-brand:hover,
      .navbar-default .navbar-brand:focus {
        color: #ffffff !important;
        background: rgba(255, 255, 255, 0.06) !important;
      }

      .navbar-brand::after {
        content: '▾';
        margin-left: auto;
        font-size: 16px;
        transition: transform 0.18s ease;
      }

      .navbar.menu-open .navbar-brand::after {
        transform: rotate(180deg);
      }

      .brand-title {
        display: contents;
      }

      .navbar .navbar-collapse {
        position: absolute;
        top: 100%;
        right: 0;
        left: 0;
        max-height: calc(100vh - 48px);
        padding: 0;
        overflow-y: auto;
        border: none;
        background: var(--near-black);
        box-shadow: 0 12px 24px rgba(43, 30, 30, 0.32);
      }

      /* Keep the menu closed unless our own state class explicitly opens it.
         These selectors override Bootstrap regardless of its collapse state. */
      body .navbar .navbar-collapse,
      body .navbar .navbar-collapse.collapse,
      body .navbar .navbar-collapse.collapsing,
      body .navbar .navbar-collapse.in,
      body .navbar .navbar-collapse.show {
        display: none !important;
        height: 0 !important;
        min-height: 0 !important;
        overflow: hidden !important;
      }

      body .navbar.hub-menu-open .navbar-collapse,
      body .navbar.hub-menu-open .navbar-collapse.collapse,
      body .navbar.hub-menu-open .navbar-collapse.collapsing,
      body .navbar.hub-menu-open .navbar-collapse.in,
      body .navbar.hub-menu-open .navbar-collapse.show {
        display: block !important;
        height: auto !important;
        min-height: 0 !important;
        overflow-y: auto !important;
      }

      .navbar .navbar-nav {
        float: none !important;
        margin: 0 !important;
      }

      .navbar .navbar-nav > li {
        float: none !important;
      }

      .navbar-default .navbar-nav > li > a {
        padding: 10px 16px;
        border-top: 1px solid rgba(224, 227, 228, 0.10);
        color: var(--off-white) !important;
        font-size: 16px;
        font-weight: 700;
        letter-spacing: 0.02em;
      }

      .navbar-default .navbar-nav > li > a:hover,
      .navbar-default .navbar-nav > li > a:focus {
        background: rgba(190, 28, 48, 0.28) !important;
        color: #ffffff !important;
      }

      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:focus,
      .navbar-default .navbar-nav > .active > a:hover {
        background: var(--primary-red) !important;
        color: #ffffff !important;
      }

      .page-wrap {
        width: 100%;
        max-width: 1500px;
        margin: 0 auto;
        padding: 14px;
      }

      .section-card {
        margin-bottom: 10px;
        padding: 14px;
        border: 1px solid var(--border);
        border-radius: 10px;
        background: var(--surface-soft);
        box-shadow: 0 5px 16px rgba(43, 30, 30, 0.07);
      }

      .hero-card {
        padding: 13px 15px;
        border: none;
        background: linear-gradient(130deg, var(--near-black), var(--dark-red));
        color: #ffffff;
      }

      .hero-card h2,
      .hero-card h3 {
        color: #ffffff;
      }

      .hero-card h4 {
        margin: 0;
        font-size: 15px;
        font-weight: 600;
      }

      .hero-card p {
        margin: 2px 0 0;
      }

      .hero-card .muted {
        color: rgba(255, 255, 255, 0.78);
      }

      .metric-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
        gap: 8px;
        margin-bottom: 10px;
      }

      .metric-card {
        min-height: 86px;
        padding: 11px 12px;
        border: 1px solid var(--border);
        border-top: 4px solid var(--primary-red);
        border-radius: 10px;
        background: var(--surface);
        box-shadow: 0 4px 12px rgba(43, 30, 30, 0.07);
      }

      .metric-card.accent-blue,
      .metric-card.accent-red {
        border-top-color: var(--primary-red);
      }

      .metric-card.accent-green,
      .metric-card.accent-cyan {
        border-top-color: var(--dark-red);
      }

      .metric-card.accent-gold {
        border-top-color: var(--charcoal);
      }

      .metric-card.accent-purple {
        border-top-color: var(--near-black);
      }

      .metric-title {
        margin-bottom: 3px;
        color: var(--muted);
        font-size: 10.5px;
        font-weight: 800;
        letter-spacing: 0.06em;
        line-height: 1.1;
        text-transform: uppercase;
      }

      .metric-value {
        color: var(--near-black);
        font-size: 21px;
        font-weight: 900;
        line-height: 1.02;
        overflow-wrap: anywhere;
      }

      .score-number {
        color: var(--primary-red) !important;
      }

      .metric-subtitle {
        margin-top: 4px;
        color: var(--muted);
        font-size: 11.5px;
        line-height: 1.12;
      }

      .control-row {
        display: flex;
        align-items: end;
        flex-wrap: wrap;
        gap: 8px;
        margin-bottom: 8px;
      }

      .control-row > * {
        min-width: 0;
      }

      .control-row .form-group {
        min-width: 145px;
        margin-bottom: 0;
      }

      .form-group {
        margin-bottom: 8px;
      }

      .control-label,
      label {
        margin-bottom: 3px;
        color: var(--near-black);
        font-size: 12px;
        font-weight: 800;
        letter-spacing: 0.025em;
      }

      .form-control,
      .selectize-input {
        min-height: 34px;
        padding: 5px 8px;
        border-color: var(--border);
        border-radius: 7px;
        font-size: 14px;
        line-height: 1.2;
        box-shadow: none;
      }

      .form-control:focus,
      .selectize-input.focus {
        border-color: var(--primary-red);
        box-shadow: 0 0 0 2px rgba(190, 28, 48, 0.12);
      }

      .selectize-dropdown .active {
        background: var(--primary-red);
        color: #ffffff;
      }

      .muted {
        color: var(--muted);
      }

      .recap-card {
        display: grid;
        grid-template-columns: 1fr 1fr;
        margin-bottom: 9px;
        overflow: hidden;
        border: 2px solid var(--near-black);
        border-radius: 9px;
        background: #ffffff;
        box-shadow: 0 4px 12px rgba(43, 30, 30, 0.07);
      }

      .recap-team {
        display: flex;
        min-height: 100px;
        padding: 13px;
        flex-direction: column;
        justify-content: center;
      }

      .recap-team:first-child {
        border-right: 2px solid var(--near-black);
      }

      .recap-team-name {
        margin-bottom: 5px;
        color: var(--near-black);
        font-size: 19px;
        font-weight: 900;
        line-height: 1.02;
      }

      .recap-team-score {
        color: var(--primary-red);
        font-size: 25px;
        font-weight: 900;
        line-height: 1;
      }

      .recap-winner {
        background: linear-gradient(135deg, rgba(190, 28, 48, 0.10), rgba(224, 227, 228, 0.45));
      }

      .finish-card {
        margin-bottom: 10px;
        padding: 13px;
        border: 1px solid var(--border);
        border-radius: 10px;
        background: #ffffff;
        box-shadow: 0 4px 12px rgba(43, 30, 30, 0.07);
      }

      .finish-title {
        margin-bottom: 7px;
        color: var(--near-black);
        font-size: 19px;
        font-weight: 900;
      }

      .finish-row {
        display: grid;
        grid-template-columns: 52px 1fr;
        align-items: center;
        gap: 8px;
        padding: 7px 0;
        border-top: 1px solid var(--border);
      }

      .finish-rank {
        color: var(--primary-red);
        font-size: 20px;
        font-weight: 900;
      }

      .finish-detail {
        color: var(--near-black);
        font-weight: 800;
      }

      .matchup-detail-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 10px;
      }

      .matchup-team-panel {
        overflow: hidden;
        border: 2px solid var(--near-black);
        border-radius: 9px;
        background: #ffffff;
      }

      .matchup-team-header {
        padding: 10px 12px;
        background: linear-gradient(130deg, var(--near-black), var(--dark-red));
        color: #ffffff;
      }

      .matchup-team-title {
        margin-bottom: 2px;
        font-size: 18px;
        font-weight: 900;
        line-height: 1.05;
      }

      .matchup-team-score {
        font-size: 24px;
        font-weight: 900;
      }

      .matchup-team-body {
        padding: 7px;
      }

      .rank-one {
        font-weight: 900 !important;
      }

      .record-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
        gap: 10px;
      }

      .record-table-card {
        padding: 11px;
        border: 1px solid var(--border);
        border-radius: 10px;
        background: #ffffff;
        box-shadow: 0 4px 12px rgba(43, 30, 30, 0.07);
      }

      .record-table-card h3 {
        margin-top: 0;
      }

      .power-ranking-wrap table.dataTable {
        border-collapse: separate !important;
        border-spacing: 0 4px !important;
      }

      .power-ranking-wrap table.dataTable tbody tr {
        background: #ffffff !important;
        box-shadow: 0 2px 7px rgba(43, 30, 30, 0.06);
      }

      .power-ranking-wrap table.dataTable tbody td {
        padding-top: 7px !important;
        padding-bottom: 7px !important;
        font-weight: 700;
      }

      .section-title-row {
        display: flex;
        align-items: center;
        gap: 7px;
        margin-bottom: 6px;
      }

      .section-title-row h3 {
        margin: 0;
      }

      .info-button {
        width: 23px;
        height: 23px;
        padding: 0 !important;
        border: none !important;
        border-radius: 50%;
        background: var(--primary-red) !important;
        color: #ffffff !important;
        font-weight: 900;
        line-height: 23px;
        text-align: center;
      }

      .info-button:hover,
      .info-button:focus {
        background: var(--dark-red) !important;
      }

      .btn-primary,
      .btn-default {
        border-color: var(--dark-red) !important;
        background: var(--primary-red) !important;
        color: #ffffff !important;
        font-weight: 800;
      }

      .btn-primary:hover,
      .btn-primary:focus,
      .btn-default:hover,
      .btn-default:focus {
        border-color: var(--near-black) !important;
        background: var(--dark-red) !important;
        color: #ffffff !important;
      }

      .dataTables_wrapper {
        color: var(--near-black);
        font-size: 13px;
      }

      .dataTables_wrapper .dt-buttons .dt-button {
        margin-right: 4px;
        padding: 3px 7px !important;
        border: none !important;
        border-radius: 6px !important;
        background: var(--primary-red) !important;
        color: #ffffff !important;
        font-family: 'Barlow Condensed', 'Arial Narrow', sans-serif;
        font-size: 12px;
        font-weight: 800;
      }

      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select {
        min-height: 29px;
        padding: 3px 6px;
        border: 1px solid var(--border);
        border-radius: 6px;
      }

      table.dataTable {
        width: 100% !important;
      }

      table.dataTable thead th {
        padding: 6px 7px !important;
        border-bottom: none !important;
        background: var(--dark-red) !important;
        color: #ffffff !important;
        font-size: 12px;
        font-weight: 800;
        letter-spacing: 0.02em;
      }

      table.dataTable tbody td {
        padding: 5px 7px !important;
      }

      table.dataTable tbody tr,
      table.dataTable tbody td {
        cursor: default !important;
      }

      table.dataTable tbody tr.selected,
      table.dataTable tbody tr.selected > *,
      table.dataTable tbody tr.selected:hover,
      table.dataTable tbody tr.selected:hover > * {
        color: var(--near-black) !important;
        background: inherit !important;
        box-shadow: none !important;
      }

      #history_matchups_table table.dataTable tbody tr,
      #history_matchups_table table.dataTable tbody td {
        cursor: pointer !important;
      }

      #history_matchups_table table.dataTable tbody tr:hover > * {
        background: rgba(190, 28, 48, 0.08) !important;
      }

      #history_matchups_table table.dataTable tbody tr.selected > * {
        background: rgba(190, 28, 48, 0.15) !important;
        color: var(--near-black) !important;
        box-shadow: none !important;
      }

      table.dataTable > tbody > tr.child ul.dtr-details {
        display: block;
        width: 100%;
      }

      table.dataTable > tbody > tr.child span.dtr-title {
        min-width: 105px;
      }

      .modal-backdrop.in {
        opacity: 0.68;
      }

      .modal-content {
        overflow: hidden;
        border: 2px solid var(--dark-red);
        border-radius: 11px;
        box-shadow: 0 18px 44px rgba(43, 30, 30, 0.42);
      }

      .modal-header {
        padding: 9px 13px;
        border-bottom: none;
        background: linear-gradient(90deg, var(--dark-red), var(--near-black));
        color: #ffffff;
      }

      .modal-title {
        color: #ffffff;
        font-size: 20px;
        font-weight: 900;
      }

      .modal-header .close {
        color: #ffffff;
        opacity: 0.9;
        text-shadow: none;
      }

      .modal-body {
        max-height: calc(100vh - 145px);
        padding: 11px;
        overflow-y: auto;
        background: var(--off-white);
      }

      .modal-footer {
        padding: 7px 11px;
        border-top: 1px solid var(--border);
        background: #ffffff;
      }

      .app-loading-indicator {
        display: none;
        position: fixed;
        left: 50%;
        bottom: calc(12px + env(safe-area-inset-bottom));
        z-index: 2500;
        align-items: center;
        gap: 8px;
        padding: 8px 12px;
        transform: translateX(-50%);
        border-radius: 999px;
        background: rgba(43, 30, 30, 0.96);
        color: #ffffff;
        box-shadow: 0 10px 24px rgba(43, 30, 30, 0.28);
        font-size: 12px;
        font-weight: 800;
        pointer-events: none;
      }

      body.shiny-busy .app-loading-indicator {
        display: flex;
      }

      .app-loading-spinner {
        width: 15px;
        height: 15px;
        border: 2px solid rgba(255, 255, 255, 0.35);
        border-top-color: var(--primary-red);
        border-radius: 50%;
        animation: app-spin 0.8s linear infinite;
      }

      @keyframes app-spin {
        to { transform: rotate(360deg); }
      }

      /* Custom Fantasy League Hub menu. The generated Bootstrap navbar stays
         in the document only so Shiny can manage the tab panels. It is not shown. */
      .navbar {
        display: none !important;
      }

      .hub-nav {
        position: fixed;
        top: 0;
        right: 0;
        left: 0;
        z-index: 1100;
        padding-top: env(safe-area-inset-top);
        background: linear-gradient(90deg, var(--dark-red), var(--near-black));
        box-shadow: 0 4px 14px rgba(43, 30, 30, 0.28);
      }

      .hub-nav-bar {
        height: 48px;
      }

      .hub-nav-toggle {
        display: flex;
        align-items: center;
        justify-content: space-between;
        width: 100%;
        height: 48px;
        margin: 0;
        padding: 0 14px;
        border: 0;
        border-radius: 0;
        background: transparent;
        color: var(--off-white);
        font-family: 'Barlow Condensed', 'Arial Narrow', sans-serif;
        font-size: 18px;
        font-weight: 900;
        letter-spacing: 0.055em;
        text-align: left;
        text-transform: uppercase;
        cursor: pointer;
        box-shadow: none;
      }

      .hub-nav-toggle:hover,
      .hub-nav-toggle:focus,
      .hub-nav-toggle:active {
        outline: none;
        background: rgba(255, 255, 255, 0.06);
        color: #ffffff;
        box-shadow: none;
      }

      .hub-nav-chevron {
        flex: 0 0 auto;
        margin-left: 12px;
        font-size: 16px;
        transition: transform 0.18s ease;
      }

      .hub-nav.is-open .hub-nav-chevron {
        transform: rotate(180deg);
      }

      .hub-nav-menu {
        display: none;
        max-height: calc(100vh - 48px - env(safe-area-inset-top));
        overflow-y: auto;
        background: var(--near-black);
        box-shadow: 0 12px 24px rgba(43, 30, 30, 0.32);
      }

      .hub-nav.is-open .hub-nav-menu {
        display: block;
      }

      .hub-nav-item.btn {
        display: block;
        width: 100%;
        margin: 0;
        padding: 10px 16px;
        border: 0;
        border-top: 1px solid rgba(224, 227, 228, 0.10);
        border-radius: 0;
        background: transparent;
        color: var(--off-white);
        font-family: 'Barlow Condensed', 'Arial Narrow', sans-serif;
        font-size: 16px;
        font-weight: 700;
        letter-spacing: 0.02em;
        text-align: left;
        box-shadow: none;
      }

      .hub-nav-item.btn:hover,
      .hub-nav-item.btn:focus,
      .hub-nav-item.btn:active {
        outline: none;
        background: rgba(190, 28, 48, 0.28);
        color: #ffffff;
        box-shadow: none;
      }

      @media (max-width: 767px) {
        html,
        body {
          width: 100%;
          overflow-x: hidden;
        }

        body {
          padding-top: calc(46px + env(safe-area-inset-top));
          font-size: 14px;
        }

        .hub-nav-bar,
        .hub-nav-toggle {
          height: 46px;
        }

        .hub-nav-toggle {
          padding: 0 11px;
          font-size: 17px;
        }

        .hub-nav-menu {
          max-height: calc(100vh - 46px - env(safe-area-inset-top));
        }

        .hub-nav-item.btn {
          padding: 9px 13px;
          font-size: 15px;
        }

        .navbar-default .navbar-brand {
          height: 46px;
          padding: 0 11px;
          font-size: 17px;
        }

        .navbar .navbar-collapse {
          max-height: calc(100vh - 46px);
        }

        .navbar-default .navbar-nav > li > a {
          padding: 9px 13px;
          font-size: 15px;
        }

        .page-wrap {
          padding: 7px;
        }

        .section-card {
          margin-bottom: 7px;
          padding: 9px;
          border-radius: 8px;
        }

        .hero-card {
          padding: 9px 11px;
        }

        .hero-card h2 {
          margin-bottom: 3px;
          font-size: 20px;
        }

        .hero-card h4,
        .hero-card p {
          font-size: 12.5px;
          line-height: 1.15;
        }

        h3 {
          margin-bottom: 5px;
          font-size: 17px;
        }

        .metric-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 6px;
          margin-bottom: 7px;
        }

        .metric-card {
          min-height: 70px;
          padding: 8px;
          border-top-width: 3px;
          border-radius: 8px;
        }

        .metric-title {
          font-size: 9px;
        }

        .metric-value {
          font-size: 17.5px;
        }

        .metric-subtitle {
          margin-top: 2px;
          font-size: 10px;
        }

        .control-row {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 6px;
          margin-bottom: 4px;
        }

        .control-row .form-group,
        .control-row .shiny-html-output {
          width: 100%;
          min-width: 0;
          margin-bottom: 0;
        }

        .control-row > :only-child {
          grid-column: 1 / -1;
        }

        .control-label,
        label {
          margin-bottom: 2px;
          font-size: 10.5px;
        }

        .form-control,
        .selectize-input {
          min-height: 31px;
          padding: 4px 6px;
          font-size: 12.5px;
        }

        .recap-card {
          margin-bottom: 6px;
          border-width: 2px;
          border-radius: 7px;
        }

        .recap-team {
          min-height: 78px;
          padding: 8px;
        }

        .recap-team:first-child {
          border-right-width: 2px;
        }

        .recap-team-name {
          margin-bottom: 3px;
          font-size: 15.5px;
        }

        .recap-team-score {
          font-size: 21px;
        }

        .finish-card {
          margin-bottom: 7px;
          padding: 9px;
        }

        .finish-row {
          grid-template-columns: 43px 1fr;
          gap: 5px;
          padding: 5px 0;
        }

        .finish-rank {
          font-size: 17px;
        }

        .record-grid {
          grid-template-columns: 1fr;
          gap: 7px;
        }

        .record-table-card {
          padding: 7px;
          border-radius: 8px;
        }

        .section-title-row {
          justify-content: space-between;
        }

        #top_players_plot {
          height: 275px !important;
        }

        #manager_score_trend {
          height: 255px !important;
        }

        #manager_position_spider {
          height: 315px !important;
        }

        .shiny-plot-output {
          max-width: 100%;
        }

        .dataTables_wrapper {
          font-size: 11.5px;
        }

        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_length {
          float: none;
          margin-bottom: 4px;
          text-align: left;
        }

        .dataTables_wrapper .dataTables_filter input {
          width: calc(100% - 54px);
          max-width: none;
        }

        .dataTables_wrapper .dt-buttons {
          float: none;
          margin-bottom: 4px;
        }

        table.dataTable thead th,
        table.dataTable tbody td {
          padding: 4px 5px !important;
          white-space: nowrap;
        }

        .modal-dialog {
          width: calc(100% - 10px);
          margin: 5px;
        }

        .modal-body {
          max-height: calc(100vh - 116px);
          padding: 7px;
        }

        .matchup-detail-grid {
          grid-template-columns: 1fr;
          gap: 7px;
        }

        .matchup-team-header {
          padding: 8px 9px;
        }

        .matchup-team-title {
          font-size: 16px;
        }

        .matchup-team-score {
          font-size: 21px;
        }

        .matchup-team-body {
          padding: 4px;
        }

        .app-loading-indicator {
          width: calc(100vw - 24px);
          justify-content: center;
          text-align: center;
        }
      }

      @media (max-width: 360px) {
        .metric-grid {
          grid-template-columns: 1fr;
        }
      }
    ")),
    tags$script(HTML("
      (function() {
        function getHubNav() {
          return document.getElementById('hub_nav');
        }

        function getHubToggle() {
          return document.getElementById('hub_menu_toggle');
        }

        function closeHubMenu() {
          var nav = getHubNav();
          var toggle = getHubToggle();

          if (nav) nav.classList.remove('is-open');
          if (toggle) toggle.setAttribute('aria-expanded', 'false');
        }

        function initializeHubMenu() {
          closeHubMenu();

          if (document.documentElement.dataset.hubMenuBound === 'true') return;
          document.documentElement.dataset.hubMenuBound = 'true';

          document.addEventListener('click', function(event) {
            var nav = getHubNav();
            if (nav && !nav.contains(event.target)) closeHubMenu();
          });

          document.addEventListener('keydown', function(event) {
            if (event.key === 'Escape') closeHubMenu();
          });
        }

        document.addEventListener('DOMContentLoaded', initializeHubMenu);
        window.addEventListener('load', initializeHubMenu);

        if ('serviceWorker' in navigator) {
          window.addEventListener('load', function() {
            navigator.serviceWorker.register('service-worker.js').catch(function(error) {
              console.warn('Service worker registration failed:', error);
            });
          });
        }
      })();
    "))
    ),
    div(
      id = "hub_nav",
      class = "hub-nav",
      div(
        class = "hub-nav-bar",
        tags$button(
          id = "hub_menu_toggle",
          type = "button",
          class = "hub-nav-toggle",
          `aria-controls` = "hub_nav_menu",
          `aria-expanded` = "false",
          onclick = "var nav=document.getElementById('hub_nav'); var open=nav.classList.toggle('is-open'); this.setAttribute('aria-expanded', open ? 'true' : 'false');",
          span(class = "hub-nav-title", "Fantasy League Hub"),
          span(class = "hub-nav-chevron", `aria-hidden` = "true", "▾")
        )
      ),
      div(
        id = "hub_nav_menu",
        class = "hub-nav-menu",
        role = "menu",
        actionButton(
          "hub_nav_dashboard",
          "Dashboard",
          class = "hub-nav-item",
          onclick = "document.getElementById('hub_nav').classList.remove('is-open'); document.getElementById('hub_menu_toggle').setAttribute('aria-expanded','false');"
        ),
        actionButton(
          "hub_nav_managers",
          "Managers",
          class = "hub-nav-item",
          onclick = "document.getElementById('hub_nav').classList.remove('is-open'); document.getElementById('hub_menu_toggle').setAttribute('aria-expanded','false');"
        ),
        actionButton(
          "hub_nav_players",
          "Players",
          class = "hub-nav-item",
          onclick = "document.getElementById('hub_nav').classList.remove('is-open'); document.getElementById('hub_menu_toggle').setAttribute('aria-expanded','false');"
        ),
        actionButton(
          "hub_nav_record_book",
          "Record Book",
          class = "hub-nav-item",
          onclick = "document.getElementById('hub_nav').classList.remove('is-open'); document.getElementById('hub_menu_toggle').setAttribute('aria-expanded','false');"
        ),
        actionButton(
          "hub_nav_history",
          "History",
          class = "hub-nav-item",
          onclick = "document.getElementById('hub_nav').classList.remove('is-open'); document.getElementById('hub_menu_toggle').setAttribute('aria-expanded','false');"
        )
      )
    )
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
        p(class = "muted", "Every matchup in league history. Filter the archive, then tap a matchup to view player-level data.")
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
        p(class = "muted", "Tap a matchup to open its player details."),
        DTOutput("history_matchups_table")
      )
    )
  )

)

# ---- Server ----

server <- function(input, output, session) {

  observeEvent(input$hub_nav_dashboard, {
    updateNavbarPage(session, "main_tabs", selected = "Dashboard")
  }, ignoreInit = TRUE)

  observeEvent(input$hub_nav_managers, {
    updateNavbarPage(session, "main_tabs", selected = "Managers")
  }, ignoreInit = TRUE)

  observeEvent(input$hub_nav_players, {
    updateNavbarPage(session, "main_tabs", selected = "Players")
  }, ignoreInit = TRUE)

  observeEvent(input$hub_nav_record_book, {
    updateNavbarPage(session, "main_tabs", selected = "Record Book")
  }, ignoreInit = TRUE)

  observeEvent(input$hub_nav_history, {
    updateNavbarPage(session, "main_tabs", selected = "History")
  }, ignoreInit = TRUE)

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
      geom_line(linewidth = 1.1, color = "#BE1C30") +
      geom_point(size = 2.7, color = "#BE1C30") +
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
        color = "#E0E3E4",
        linewidth = 0.8
      ) +
      geom_segment(
        data = axis_data,
        aes(x = x, y = y, xend = xend, yend = yend),
        color = "#E0E3E4",
        linewidth = 0.8
      ) +
      geom_polygon(
        data = polygon_data,
        aes(x = x, y = y),
        fill = "#BE1C30",
        alpha = 0.22,
        color = "#BE1C30",
        linewidth = 1.3
      ) +
      geom_path(
        data = polygon_data,
        aes(x = x, y = y),
        color = "#BE1C30",
        linewidth = 1.3
      ) +
      geom_point(
        data = selected_ranks,
        aes(x = x, y = y),
        color = "#BE1C30",
        size = 3.2
      ) +
      geom_text(
        data = selected_ranks,
        aes(x = label_x, y = label_y, label = paste0(pos_clean, "\n", rank_label)),
        fontface = "bold",
        color = "#2B1E1E",
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
        color = "#2B1E1E"
      ) +
      geom_text(
        aes(y = total_fpts * 0.50, label = paste0(display_label, "\n", score_fmt(total_fpts), " pts")),
        fontface = "bold",
        size = 5.2,
        color = "#2B1E1E",
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
        color = "#BE1C30",
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
        color = "#BE1C30",
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
        color = "#BE1C30",
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
        color = "#BE1C30",
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
        color = "#BE1C30",
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
        color = "#BE1C30",
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
        color = "#BE1C30",
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
      datatable_no_buttons(page_length = 15, selection = "single") |>
      DT::formatStyle(
        columns = c("Winning Score", "Losing Score"),
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  selected_history_matchup <- reactiveVal(NULL)
  history_matchups_proxy <- DT::dataTableProxy("history_matchups_table")

  observeEvent(input$history_matchups_table_rows_selected, {
    selected_row <- input$history_matchups_table_rows_selected
    req(length(selected_row) > 0)

    matchup <- history_matchups() |>
      slice(selected_row[1])

    selected_history_matchup(matchup)

    showModal(
      modalDialog(
        title = "Matchup Detail",
        size = "l",
        easyClose = FALSE,
        footer = actionButton("close_history_modal", "Close", class = "btn-primary"),
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
    )
  }, ignoreInit = TRUE)

  observeEvent(input$close_history_modal, {
    removeModal()
    selected_history_matchup(NULL)
    DT::selectRows(history_matchups_proxy, NULL)
  }, ignoreInit = TRUE)

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
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$history_team_b_players <- renderDT({
    matchup <- selected_history_matchup()
    req(matchup)

    datatable_simple(matchup_player_table(matchup$team_b, matchup), page_length = 20) |>
      DT::formatStyle(
        columns = "Points",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)


}

shinyApp(ui = ui, server = server)
