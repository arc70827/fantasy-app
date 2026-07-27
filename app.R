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

ordinal_label <- function(x) {
  x <- as.integer(x)
  remainder_100 <- x %% 100L
  remainder_10 <- x %% 10L

  suffix <- ifelse(
    remainder_100 %in% 11:13,
    "th",
    ifelse(
      remainder_10 == 1L,
      "st",
      ifelse(remainder_10 == 2L, "nd", ifelse(remainder_10 == 3L, "rd", "th"))
    )
  )

  paste0(x, suffix)
}

matchup_html <- function(year, week, winning_team, winning_score, losing_team, losing_score) {
  paste0(
    year,
    " Week ",
    week,
    ": ",
    htmltools::htmlEscape(as.character(winning_team)),
    " <strong>",
    score_fmt(winning_score),
    "</strong> - ",
    htmltools::htmlEscape(as.character(losing_team)),
    " <strong>",
    score_fmt(losing_score),
    "</strong>"
  )
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


datatable_player_performance <- function(data, page_length = 25) {
  DT::datatable(
    data,
    rownames = FALSE,
    selection = "none",
    filter = "none",
    extensions = "Responsive",
    options = list(
      pageLength = page_length,
      responsive = TRUE,
      autoWidth = FALSE,
      scrollX = FALSE,
      dom = "rtip",
      columnDefs = responsive_column_defs(data)
    ),
    class = "stripe compact nowrap"
  )
}

datatable_record <- function(
  data,
  page_length = 5,
  selection = "none",
  escape = TRUE,
  highlight_leader = TRUE
) {
  table_options <- list(
    pageLength = page_length,
    responsive = TRUE,
    autoWidth = FALSE,
    scrollX = FALSE,
    dom = "t",
    ordering = FALSE,
    columnDefs = responsive_column_defs(data),
    initComplete = JS(
      "function(settings, json) {",
      "  var table = this.api();",
      "  window.setTimeout(function() {",
      "    table.columns.adjust();",
      "    if (table.responsive && table.responsive.recalc) table.responsive.recalc();",
      "  }, 0);",
      "}"
    )
  )

  if (isTRUE(highlight_leader)) {
    table_options$rowCallback <- JS(
      "function(row, data, displayNum) {",
      "  if (displayNum === 0) {",
      "    $(row).addClass('record-holder-row');",
      "  }",
      "}"
    )
  }

  DT::datatable(
    data,
    rownames = FALSE,
    selection = selection,
    filter = "none",
    escape = escape,
    extensions = "Responsive",
    width = "100%",
    options = table_options,
    class = "stripe compact record-datatable"
  )
}

datatable_history <- function(data, page_length = 15) {
  DT::datatable(
    data,
    rownames = FALSE,
    selection = "single",
    filter = "none",
    escape = FALSE,
    width = "100%",
    options = list(
      pageLength = page_length,
      lengthChange = FALSE,
      searching = FALSE,
      ordering = FALSE,
      autoWidth = FALSE,
      scrollX = FALSE,
      dom = "tip",
      columnDefs = list(
        list(targets = 0, className = "history-matchup-cell")
      ),
      initComplete = JS(
        "function(settings, json) {",
        "  var table = this.api();",
        "  window.setTimeout(function() { table.columns.adjust(); }, 0);",
        "}"
      )
    ),
    class = "stripe compact history-matchup-table"
  )
}

format_nonstarter_rows <- function(dt) {
  inactive_slots <- c("Bench", "BE", "IR", "Injured Reserve", "IL")

  dt |>
    DT::formatStyle(
      "Slot",
      target = "row",
      backgroundColor = DT::styleEqual(
        inactive_slots,
        rep("#D8D1D2", length(inactive_slots))
      ),
      color = DT::styleEqual(
        inactive_slots,
        rep("#665A5B", length(inactive_slots))
      ),
      fontStyle = DT::styleEqual(
        inactive_slots,
        rep("italic", length(inactive_slots))
      )
    )
}

record_scope_columns <- function(data, scope_year) {
  if (!is.na(scope_year) && "Season" %in% names(data)) {
    return(dplyr::select(data, -dplyr::all_of("Season")))
  }

  data
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
      content = "width=device-width, initial-scale=1, minimum-scale=1, maximum-scale=1, user-scalable=no, viewport-fit=cover"
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
        touch-action: pan-x pan-y;
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

      .hero-card label,
      .hero-card .control-label {
        color: #ffffff !important;
      }

      .hero-card {
        position: relative;
        z-index: 80;
        overflow: visible;
      }

      .hero-card .selectize-control {
        position: relative;
        z-index: 90;
      }

      .selectize-control.dropdown-active,
      .selectize-control.focus {
        z-index: 6100 !important;
      }

      .selectize-dropdown,
      body > .selectize-dropdown {
        z-index: 6200 !important;
      }

      .selectize-dropdown-content {
        max-height: min(60vh, 420px) !important;
        overscroll-behavior: contain;
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

      .one-line-card .metric-value {
        overflow: hidden;
        white-space: nowrap;
        text-overflow: clip;
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

      .season-finish-row {
        grid-template-columns: minmax(90px, auto) 1fr;
      }

      .season-finish-card {
        padding: 11px 13px;
      }

      .season-finish-inline {
        display: flex;
        align-items: baseline;
        min-width: 0;
        color: var(--near-black);
        font-size: 18px;
        font-weight: 900;
        line-height: 1.05;
        white-space: nowrap;
      }

      .season-finish-inline .finish-rank {
        color: inherit;
        font-size: inherit;
      }

      .finish-gold {
        border-color: #B8860B;
        background: linear-gradient(135deg, #FFF3B0, #E7C85F);
      }

      .finish-silver {
        border-color: #9AA0A6;
        background: linear-gradient(135deg, #F4F6F8, #C8CDD2);
      }

      .finish-bronze {
        border-color: #9A5B2E;
        background: linear-gradient(135deg, #F2D0B2, #C98B5B);
      }

      .championship-shrine {
        position: relative;
        margin-bottom: 10px;
        padding: 15px;
        overflow: hidden;
        border: 2px solid #C99A19;
        border-radius: 12px;
        background:
          radial-gradient(circle at top, rgba(255, 230, 132, 0.35), transparent 44%),
          linear-gradient(135deg, #FFF8D8, #ffffff 48%, #F4E4A1);
        box-shadow: 0 8px 22px rgba(43, 30, 30, 0.13);
      }

      .championship-shrine-title {
        margin-bottom: 10px;
        color: var(--near-black);
        font-size: 21px;
        font-weight: 900;
        letter-spacing: 0.05em;
        text-align: center;
        text-transform: uppercase;
      }

      .championship-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(145px, 1fr));
        gap: 9px;
      }

      .championship-card {
        padding: 11px 9px;
        border: 1px solid rgba(155, 108, 0, 0.35);
        border-radius: 10px;
        background: rgba(255, 255, 255, 0.82);
        text-align: center;
      }

      .championship-trophy {
        display: block;
        margin-bottom: 3px;
        font-size: 38px;
        line-height: 1;
        filter: drop-shadow(0 3px 2px rgba(43, 30, 30, 0.20));
      }

      .championship-year {
        color: var(--dark-red);
        font-size: 23px;
        font-weight: 900;
      }

      .championship-team {
        color: var(--near-black);
        font-size: 14px;
        font-weight: 800;
        line-height: 1.1;
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

      table.dataTable tbody tr.record-holder-row > td {
        background: rgba(212, 175, 55, 0.18) !important;
        font-size: inherit !important;
        font-weight: inherit !important;
      }

      .record-table-card .dataTables_wrapper,
      .record-table-card table.dataTable {
        width: 100% !important;
        max-width: 100% !important;
      }

      .record-table-card table.dataTable thead th,
      .record-table-card table.dataTable tbody td {
        white-space: normal !important;
        overflow-wrap: anywhere;
        word-break: normal;
      }

      #history_matchups_table .dataTables_filter,
      #history_matchups_table .dataTables_length {
        display: none !important;
      }

      #history_matchups_table table.dataTable,
      #history_matchups_table .dataTables_wrapper {
        width: 100% !important;
        max-width: 100% !important;
      }

      #history_matchups_table table.dataTable tbody td.history-matchup-cell {
        white-space: normal !important;
        overflow-wrap: anywhere;
        line-height: 1.18;
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

      #power_rankings_table table.dataTable tbody tr:first-child > td {
        font-size: 1.08em !important;
      }

      #power_rankings_table table.dataTable tbody td:last-child {
        font-weight: 900 !important;
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

      .modal {
        z-index: 2200 !important;
      }

      .modal-backdrop {
        z-index: 2100 !important;
      }

      .modal.in {
        display: flex !important;
        align-items: center;
        justify-content: center;
        padding: calc(54px + env(safe-area-inset-top)) 8px 8px;
      }

      .modal-dialog {
        width: min(920px, calc(100vw - 16px));
        margin: 0 auto;
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
        display: flex;
        position: fixed;
        inset: 0;
        z-index: 4000;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        gap: 13px;
        padding: 24px;
        background:
          radial-gradient(circle at center, rgba(255, 255, 255, 0.98), rgba(224, 227, 228, 0.98) 58%, rgba(190, 28, 48, 0.18)),
          var(--off-white);
        color: var(--near-black);
        opacity: 1;
        visibility: visible;
        transition: opacity 0.22s ease, visibility 0.22s ease;
        pointer-events: all;
      }

      html.app-ready .app-loading-indicator {
        opacity: 0;
        visibility: hidden;
        pointer-events: none;
      }

      html.app-ready body.shiny-busy .app-loading-indicator {
        opacity: 1;
        visibility: visible;
        pointer-events: all;
        transition-delay: 0.18s;
      }

      .app-loading-logo-shell {
        position: relative;
        width: 148px;
        height: 148px;
        padding: 10px;
        border-radius: 50%;
        background: #ffffff;
        box-shadow: 0 12px 34px rgba(43, 30, 30, 0.25);
      }

      .app-loading-logo {
        display: block;
        width: 100%;
        height: 100%;
        border-radius: 50%;
        object-fit: cover;
      }

      .app-loading-spinner {
        position: absolute;
        inset: -8px;
        border: 6px solid rgba(76, 65, 66, 0.22);
        border-top-color: var(--primary-red);
        border-right-color: var(--dark-red);
        border-radius: 50%;
        animation: app-spin 0.9s linear infinite;
      }

      .app-loading-label {
        font-size: 16px;
        font-weight: 900;
        letter-spacing: 0.05em;
        text-transform: uppercase;
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

        .best-starter-card {
          grid-column: 1 / -1;
        }

        .record-table-card table.dataTable thead th,
        .record-table-card table.dataTable tbody td,
        #history_matchups_table table.dataTable tbody td.history-matchup-cell {
          white-space: normal !important;
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

        .season-finish-row {
          grid-template-columns: minmax(86px, auto) 1fr;
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
          width: calc(100% - 4px);
          margin: 0;
        }

        .modal-body {
          max-height: calc(100vh - 116px);
          padding: 7px;
        }

        .matchup-detail-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 4px;
        }

        .matchup-team-panel {
          min-width: 0;
          border-width: 1px;
        }

        .matchup-team-header {
          padding: 6px;
        }

        .matchup-team-title {
          font-size: 13px;
          overflow-wrap: anywhere;
        }

        .matchup-team-score {
          font-size: 18px;
        }

        .matchup-team-body {
          padding: 2px;
        }

        #history_team_a_players .dataTables_wrapper,
        #history_team_b_players .dataTables_wrapper {
          font-size: 9px;
        }

        #history_team_a_players table.dataTable thead th,
        #history_team_a_players table.dataTable tbody td,
        #history_team_b_players table.dataTable thead th,
        #history_team_b_players table.dataTable tbody td {
          padding: 2px !important;
        }

        .championship-shrine {
          padding: 10px;
        }

        .championship-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 6px;
        }

        .championship-trophy {
          font-size: 31px;
        }

        .app-loading-logo-shell {
          width: 126px;
          height: 126px;
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

        function markAppReady() {
          document.documentElement.classList.add('app-ready');
        }

        function recalculateVisibleTables() {
          window.setTimeout(function() {
            if (!window.jQuery || !window.jQuery.fn || !window.jQuery.fn.dataTable) return;

            window.jQuery(window.jQuery.fn.dataTable.tables({ visible: true })).each(function() {
              var table = window.jQuery(this).DataTable();
              table.columns.adjust();
              if (table.responsive && table.responsive.recalc) table.responsive.recalc();
            });
          }, 120);
        }

        function registerTableMessageHandler() {
          if (document.documentElement.dataset.tableHandlerBound === 'true') return;

          if (window.Shiny && window.Shiny.addCustomMessageHandler) {
            window.Shiny.addCustomMessageHandler('recalculate_tables', recalculateVisibleTables);
            document.documentElement.dataset.tableHandlerBound = 'true';
          } else {
            window.setTimeout(registerTableMessageHandler, 50);
          }
        }

        function bindLoadingScreen() {
          if (window.jQuery) {
            window.jQuery(document).one('shiny:idle', markAppReady);
          } else {
            window.setTimeout(bindLoadingScreen, 50);
          }

          window.setTimeout(markAppReady, 15000);
        }

        function bindSelectizeToggleBehavior() {
          if (!window.jQuery) {
            window.setTimeout(bindSelectizeToggleBehavior, 50);
            return;
          }

          var $document = window.jQuery(document);
          $document.off('pointerdown.fantasyDropdownToggle', '.selectize-control .selectize-input');
          $document.on(
            'pointerdown.fantasyDropdownToggle',
            '.selectize-control .selectize-input',
            function(event) {
              var $control = window.jQuery(this).closest('.selectize-control');
              var selectElement = $control.siblings('select').get(0);
              var selectize = selectElement && selectElement.selectize;

              if (selectize && selectize.isOpen) {
                event.preventDefault();
                event.stopImmediatePropagation();
                selectize.close();
                selectize.blur();
                window.setTimeout(function() {
                  selectize.close();
                  selectize.blur();
                }, 0);
                return false;
              }
            }
          );
        }

        function initializeHubMenu() {
          closeHubMenu();
          bindLoadingScreen();
          bindSelectizeToggleBehavior();
          registerTableMessageHandler();

          if (document.documentElement.dataset.hubMenuBound === 'true') return;
          document.documentElement.dataset.hubMenuBound = 'true';

          document.addEventListener('click', function(event) {
            var nav = getHubNav();
            if (nav && !nav.contains(event.target)) closeHubMenu();
          });

          document.addEventListener('keydown', function(event) {
            if (event.key === 'Escape') closeHubMenu();
          });

          window.addEventListener('resize', recalculateVisibleTables);

          if (window.jQuery) {
            window.jQuery(document).on('shown.bs.tab', 'a[data-toggle=tab]', recalculateVisibleTables);
            window.jQuery(document).on('shiny:value', function(event) {
              var outputName = event && event.name ? event.name : '';
              if (outputName.indexOf('record_') === 0 || outputName === 'history_matchups_table') {
                recalculateVisibleTables();
              }
            });
          }
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
          "Home",
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
    div(
      class = "app-loading-logo-shell",
      tags$img(
        class = "app-loading-logo",
        src = "img/hornet-logo.png",
        alt = "Fantasy League Hub hornet logo"
      ),
      div(class = "app-loading-spinner")
    ),
    span(class = "app-loading-label", "Loading Fantasy League Hub")
  ),

  tabPanel(
    "Home",
    value = "Dashboard",
    div(
      class = "page-wrap",
      div(
        class = "section-card hero-card",
        h2(textOutput("dashboard_week_label", inline = TRUE))
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
          selectizeInput(
            "manager_select",
            "Manager",
            choices = c("Select manager" = "", managers),
            selected = "",
            options = list(
              dropdownParent = "body",
              maxOptions = length(managers) + 1
            )
          ),
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
          selectizeInput(
            "player_slot",
            "Roster Slot",
            choices = c("All Slots", "QB", "RB", "WR", "TE", "FLEX", "D/ST", "K", "Bench", "IR", "SLOT"),
            selected = "All Slots",
            options = list(dropdownParent = "body")
          ),
          textInput("player_search", "Player Search", placeholder = "Example: Josh Allen")
        )
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
          selectInput("record_scope", "Season", choices = c("All Time", years), selected = "All Time")
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
    session$sendCustomMessage("recalculate_tables", list())
  }, ignoreInit = TRUE)

  observeEvent(input$hub_nav_history, {
    updateNavbarPage(session, "main_tabs", selected = "History")
    session$sendCustomMessage("recalculate_tables", list())
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

    numeric_years <- sort(unique(available_years$year), decreasing = TRUE)
    year_choices <- c("All Years", as.character(numeric_years))
    current_selection <- isolate(input$player_year)

    selected_year <- if (!is.null(current_selection) && current_selection %in% year_choices) {
      current_selection
    } else if (length(numeric_years) > 0) {
      as.character(max(numeric_years, na.rm = TRUE))
    } else {
      "All Years"
    }

    selectizeInput(
      "player_year",
      "Season",
      choices = year_choices,
      selected = selected_year,
      options = list(dropdownParent = "body")
    )
  })

  output$player_manager_ui <- renderUI({
    selected_year <- input$player_year
    available_managers <- players

    if (!is.null(selected_year) && selected_year != "All Years") {
      available_managers <- available_managers |> filter(year == as.integer(selected_year))
    }

    manager_choices <- sort(unique(available_managers$manager))
    current_selection <- isolate(input$player_manager)

    selected_manager <- if (!is.null(current_selection) && current_selection %in% manager_choices) {
      current_selection
    } else {
      "All Managers"
    }

    selectizeInput(
      "player_manager",
      "Manager",
      choices = c("All Managers", manager_choices),
      selected = selected_manager,
      options = list(dropdownParent = "body")
    )
  })

  output$player_week_ui <- renderUI({
    req(input$player_year)

    weeks <- if (input$player_year == "All Years") {
      sort(unique(players$week))
    } else {
      weeks_for_year(players, as.integer(input$player_year))
    }
    current_week <- isolate(input$player_week)

    selected_week <- if (input$player_year == "All Years") {
      "All Weeks"
    } else if (is.null(current_week)) {
      if (length(weeks) > 0) max(weeks, na.rm = TRUE) else NA_integer_
    } else if (!is.null(current_week) && current_week %in% as.character(c("All Weeks", weeks))) {
      current_week
    } else if (!is.null(current_week) && suppressWarnings(as.integer(current_week)) %in% weeks) {
      as.integer(current_week)
    } else {
      if (length(weeks) > 0) max(weeks, na.rm = TRUE) else NA_integer_
    }

    selectizeInput(
      "player_week",
      "Week",
      choices = c("All Weeks", weeks),
      selected = selected_week,
      options = list(dropdownParent = "body")
    )
  })

  output$manager_period_ui <- renderUI({
    if (is.null(input$manager_select) || input$manager_select == "") {
      return(selectizeInput(
        "manager_period",
        "Years / Team Name",
        choices = c("Select manager first" = ""),
        selected = "",
        options = list(dropdownParent = "body")
      ))
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

    selectizeInput(
      "manager_period",
      "Years / Team Name",
      choices = stats::setNames(values, labels),
      selected = "All Years",
      options = list(
        dropdownParent = "body",
        maxOptions = length(values)
      )
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
    paste0(dashboard_year(), " Week ", dashboard_week(), " Recap")
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

    best_start <- players |>
      filter(year == current_year, week == current_week) |>
      filter(!str_to_lower(slot) %in% c("bench", "be", "ir", "injured reserve", "il")) |>
      slice_max(fpts, n = 1, with_ties = FALSE)

    best_start_card <- if (nrow(best_start) > 0) {
      card(
        "Best Starter",
        HTML(paste0("<span class='score-number'>", score_fmt(best_start$fpts[[1]]), "</span>")),
        paste0(best_start$player_name[[1]], " - ", best_start$fantasy_team[[1]]),
        "accent-gold best-starter-card"
      )
    } else {
      NULL
    }

    div(
      class = "metric-grid",
      card(
        "Highest Scorer",
        HTML(paste0("<span class='score-number'>", score_fmt(high_score$points_for[[1]]), "</span>")),
        high_score$team[[1]],
        "accent-green"
      ),
      card(
        "Lowest Scorer",
        HTML(paste0("<span class='score-number'>", score_fmt(low_score$points_for[[1]]), "</span>")),
        low_score$team[[1]],
        "accent-red"
      ),
      card(
        "League Avg Score",
        HTML(paste0("<span class='score-number'>", score_fmt(league_avg), "</span>")),
        NULL,
        "accent-purple"
      ),
      best_start_card,
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
    ranking_data <- power_rankings() |>
      arrange(desc(power_score), desc(points_for)) |>
      mutate(
        team_name = mapply(resolve_team_name_one, manager, dashboard_year(), USE.NAMES = FALSE)
      ) |>
      transmute(
        `Team Name` = team_name,
        Record = record,
        `Win %` = pct_fmt(win_pct),
        `Last 3 Avg` = round(recent_avg, 2),
        `Power Score` = round(power_score, 1)
      )

    ranking_data |>
      datatable_simple(page_length = max(1, nrow(ranking_data))) |>
      DT::formatStyle(
        columns = "Power Score",
        fontWeight = "900"
      )
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
    if (
      is.null(input$manager_select) || input$manager_select == "" ||
      is.null(input$manager_period) || input$manager_period == ""
    ) {
      return(NULL)
    }

    positional_panel <- div(
      class = "section-card",
      h3("Positional Ranking Breakdown"),
      plotOutput("manager_position_spider", height = "480px")
    )

    if (input$manager_period == "All Years") {
      return(tagList(
        uiOutput("manager_championship_shrine"),
        uiOutput("manager_cards"),
        positional_panel
      ))
    }

    tagList(
      uiOutput("manager_finish_summary"),
      uiOutput("manager_cards"),
      div(
        class = "section-card",
        h3("Weekly Scoring Trend"),
        plotOutput("manager_score_trend", height = "400px")
      ),
      positional_panel
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
      arrange(week)

    week_breaks <- sort(unique(trend_data$week))

    ggplot(trend_data, aes(x = week, y = points_for)) +
      geom_line(linewidth = 1.1, color = "#BE1C30") +
      geom_point(size = 2.7, color = "#BE1C30") +
      scale_x_continuous(breaks = week_breaks, labels = week_breaks) +
      labs(
        x = "Week",
        y = "Points For"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid = element_blank(),
        axis.line = element_line(color = "#4C4142", linewidth = 0.6),
        axis.ticks = element_line(color = "#4C4142", linewidth = 0.5)
      )
  })

  output$manager_championship_shrine <- renderUI({
    req(input$manager_select, input$manager_period)
    validate(need(input$manager_select != "", ""))

    if (input$manager_period != "All Years") {
      return(NULL)
    }

    championships <- manager_finishes |>
      filter(manager == input$manager_select, finish == 1L) |>
      arrange(desc(year))

    if (nrow(championships) == 0) {
      return(NULL)
    }

    championship_cards <- lapply(seq_len(nrow(championships)), function(i) {
      div(
        class = "championship-card",
        span(class = "championship-trophy", `aria-hidden` = "true", "🏆"),
        div(class = "championship-year", championships$year[[i]]),
        div(class = "championship-team", championships$team[[i]])
      )
    })

    div(
      class = "championship-shrine",
      div(class = "championship-shrine-title", "Championship Hall"),
      div(class = "championship-grid", tagList(championship_cards))
    )
  })

  output$manager_finish_summary <- renderUI({
    req(input$manager_select, input$manager_period)
    validate(need(input$manager_select != "", ""))

    if (input$manager_period == "All Years") {
      return(NULL)
    }

    selected_year <- as.integer(input$manager_period)
    finish <- manager_finishes |>
      filter(manager == input$manager_select, year == selected_year) |>
      slice_head(n = 1)

    team_name <- resolve_team_name_one(input$manager_select, selected_year)

    if (nrow(finish) > 0) {
      finish_label <- finish$finish_label[[1]]
      finish_team <- finish$team[[1]]
    } else {
      season_games <- matchups |>
        filter(manager == input$manager_select, year == selected_year) |>
        arrange(week)

      final_week <- if (nrow(season_games) > 0) max(season_games$week, na.rm = TRUE) else NA_integer_
      lost_quarterfinal <- any(season_games$week == 15 & season_games$loss == 1, na.rm = TRUE)

      finish_label <- case_when(
        lost_quarterfinal ~ "Quarterfinals",
        !is.na(final_week) && final_week == 14 ~ "Missed Playoffs",
        TRUE ~ "Unavailable"
      )
      finish_team <- team_name
    }

    finish_class <- case_when(
      finish_label == "1st" ~ "finish-gold",
      finish_label == "2nd" ~ "finish-silver",
      finish_label == "3rd" ~ "finish-bronze",
      TRUE ~ "finish-neutral"
    )

    div(
      class = paste("finish-card season-finish-card", finish_class),
      div(
        class = "season-finish-inline",
        span("Season Finish"),
        span(" - "),
        span(class = "finish-rank", finish_label)
      )
    )
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

    games_played <- matchups |>
      distinct(manager, year, week)

    if (input$manager_period != "All Years") {
      selected_year <- as.integer(input$manager_period)
      position_data <- position_data |>
        filter(year == selected_year)
      games_played <- games_played |>
        filter(year == selected_year)
    }

    games_played <- games_played |>
      count(manager, name = "games_played")

    manager_position_averages <- position_data |>
      group_by(manager, pos_clean) |>
      summarise(total_points = sum(fpts, na.rm = TRUE), .groups = "drop") |>
      left_join(games_played, by = "manager") |>
      mutate(
        games_played = pmax(coalesce(games_played, 0L), 1L),
        avg_points = total_points / games_played
      )

    validate(
      need(nrow(manager_position_averages) > 0, "No player scoring data found for this period.")
    )

    manager_position_averages <- manager_position_averages |>
      group_by(pos_clean) |>
      mutate(
        rank = dense_rank(desc(avg_points)),
        n_managers = n_distinct(manager)
      ) |>
      ungroup()

    selected_ranks <- tibble(pos_clean = position_levels) |>
      left_join(
        manager_position_averages |>
          filter(manager == input$manager_select) |>
          select(pos_clean, avg_points, rank, n_managers),
        by = "pos_clean"
      ) |>
      mutate(
        avg_points = if_else(is.na(avg_points), 0, avg_points),
        n_managers = if_else(
          is.na(n_managers),
          max(manager_position_averages$n_managers, na.rm = TRUE),
          n_managers
        ),
        rank = if_else(is.na(rank), n_managers, rank),
        rank_score = if_else(n_managers <= 1, 1, (n_managers - rank + 1) / n_managers),
        rank_label = ordinal_label(rank),
        detail_label = paste0(rank_label, ", ", score_fmt(avg_points), " pts/wk"),
        pos_clean = factor(pos_clean, levels = position_levels)
      )

    angles <- seq(
      pi / 2,
      pi / 2 - 2 * pi + 2 * pi / length(position_levels),
      length.out = length(position_levels)
    )

    selected_ranks <- selected_ranks |>
      mutate(
        angle = angles,
        x = rank_score * cos(angle),
        y = rank_score * sin(angle),
        label_x = 1.16 * cos(angle),
        label_y = 1.16 * sin(angle),
        subtitle_x = label_x,
        subtitle_y = label_y - 0.10
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
        aes(x = label_x, y = label_y, label = pos_clean),
        fontface = "bold",
        color = "#2B1E1E",
        size = 4.2
      ) +
      geom_text(
        data = selected_ranks,
        aes(x = subtitle_x, y = subtitle_y, label = detail_label),
        color = "#75696A",
        size = 3.0
      ) +
      coord_equal(xlim = c(-1.38, 1.38), ylim = c(-1.38, 1.38), clip = "off") +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme_void(base_size = 13) +
      theme(
        plot.margin = margin(24, 52, 24, 52)
      )
  })

  player_filtered <- reactive({
    req(input$player_year, input$player_week, input$player_manager, input$player_slot)

    data <- players

    if (input$player_year != "All Years") {
      data <- data |> filter(year == as.integer(input$player_year))
    }

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


  output$players_table <- renderDT({
    player_filtered() |>
      arrange(desc(fpts)) |>
      transmute(
        Game = paste0(year, " Week ", week),
        Position = pos,
        Player = player_name,
        `Fantasy Team` = fantasy_team,
        `NFL Team` = team,
        `Roster Slot` = slot,
        Proj = round(proj, 2),
        Pts = round(fpts, 2)
      ) |>
      datatable_player_performance(page_length = 25)
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
    5
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
          div(class = "record-table-card", h3("Career Points"), DTOutput("record_total_points")),
          div(class = "record-table-card", h3("Career Wins"), DTOutput("record_total_wins")),
          div(class = "record-table-card", h3("Championships"), DTOutput("record_championships")),
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
        Score = round(points_for, 2)
      ) |>
      slice_head(n = record_limit()) |>
      record_scope_columns(record_scope_year()) |>
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
        Matchup = matchup_html(
          year,
          week,
          WinningTeam,
          WinningScore,
          LosingTeam,
          LosingScore
        )
      ) |>
      slice_head(n = record_limit()) |>
      datatable_record(page_length = record_limit(), escape = FALSE)
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
        Matchup = matchup_html(
          year,
          week,
          WinningTeam,
          WinningScore,
          LosingTeam,
          LosingScore
        )
      ) |>
      slice_head(n = record_limit()) |>
      datatable_record(page_length = record_limit(), escape = FALSE)
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
        `Career Points` = sum(points_for, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(`Career Points`)) |>
      transmute(
        Manager = manager,
        `Career Points` = round(`Career Points`, 2)
      ) |>
      slice_head(n = 5) |>
      datatable_record(page_length = 5) |>
      DT::formatStyle(
        columns = "Career Points",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_total_wins <- renderDT({
    matchups |>
      group_by(manager) |>
      summarise(
        `Career Wins` = sum(win, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(`Career Wins`)) |>
      transmute(
        Manager = manager,
        `Career Wins`
      ) |>
      slice_head(n = 5) |>
      datatable_record(page_length = 5) |>
      DT::formatStyle(
        columns = "Career Wins",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_championships <- renderDT({
    manager_finishes |>
      filter(finish == 1L) |>
      count(manager, name = "Championships") |>
      arrange(desc(Championships), manager) |>
      transmute(
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
    slot_order <- c("QB", "RB", "WR", "TE", "FLEX", "D/ST", "K")

    players |>
      mutate(
        slot_clean = case_when(
          str_to_upper(slot) %in% c("DST", "DEF", "D") ~ "D/ST",
          TRUE ~ as.character(slot)
        )
      ) |>
      filter(slot_clean %in% slot_order) |>
      group_by(manager, slot_clean) |>
      summarise(
        Points = sum(fpts, na.rm = TRUE),
        .groups = "drop"
      ) |>
      group_by(slot_clean) |>
      slice_max(Points, n = 1, with_ties = FALSE) |>
      ungroup() |>
      mutate(slot_clean = factor(slot_clean, levels = slot_order)) |>
      arrange(slot_clean) |>
      transmute(
        Slot = as.character(slot_clean),
        Manager = manager,
        Points = round(Points, 2)
      ) |>
      datatable_record(page_length = 10, highlight_leader = FALSE) |>
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
        Matchup = matchup_html(
          year,
          week,
          winning_team,
          winning_score,
          losing_team,
          losing_score
        )
      ) |>
      datatable_history(page_length = 15)
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
              div(class = "matchup-team-title", matchup$team_a),
              div(class = "matchup-team-score", score_fmt(matchup$score_a))
            ),
            div(class = "matchup-team-body", DTOutput("history_team_a_players"))
          ),
          div(
            class = "matchup-team-panel",
            div(
              class = "matchup-team-header",
              div(class = "matchup-team-title", matchup$team_b),
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

  roster_slot_order <- c("QB", "RB", "RB/WR", "WR", "TE", "FLEX", "D/ST", "K")

  matchup_player_table <- function(team_name_value, matchup) {
    team_players <- players |>
      filter(
        year == matchup$year,
        week == matchup$week,
        fantasy_team == team_name_value
      ) |>
      mutate(
        display_slot = case_when(
          str_to_upper(slot) %in% c("BE", "BENCH") ~ "Bench",
          str_to_upper(slot) %in% c("IR", "INJURED RESERVE", "IL") ~ "IR",
          str_to_upper(slot) %in% c("DST", "DEF", "D") ~ "D/ST",
          TRUE ~ as.character(slot)
        ),
        is_starter = !display_slot %in% c("Bench", "IR"),
        slot_order = match(display_slot, roster_slot_order)
      )

    starters <- team_players |>
      filter(is_starter) |>
      arrange(is.na(slot_order), slot_order, desc(fpts))

    bench_players <- team_players |>
      filter(display_slot == "Bench") |>
      arrange(desc(fpts))

    ir_players <- team_players |>
      filter(display_slot == "IR") |>
      arrange(desc(fpts))

    display_columns <- function(data) {
      data |>
        transmute(
          Player = player_name,
          Slot = display_slot,
          Proj = round(proj, 2),
          Pts = round(fpts, 2)
        )
    }

    total_row <- tibble(
      Player = "Total",
      Slot = "",
      Proj = round(sum(starters$proj, na.rm = TRUE), 2),
      Pts = round(sum(starters$fpts, na.rm = TRUE), 2)
    )

    bind_rows(
      display_columns(starters),
      total_row,
      display_columns(bench_players),
      display_columns(ir_players)
    )
  }

  format_history_player_table <- function(data) {
    datatable_simple(data, page_length = max(1, nrow(data))) |>
      format_nonstarter_rows() |>
      DT::formatStyle(
        "Player",
        target = "row",
        backgroundColor = DT::styleEqual("Total", "#F4EFE7"),
        fontWeight = DT::styleEqual("Total", "900"),
        borderTop = DT::styleEqual("Total", "2px solid #4C4142"),
        borderBottom = DT::styleEqual("Total", "2px solid #4C4142")
      ) |>
      DT::formatStyle(
        columns = "Proj",
        color = "#8A7E7F"
      ) |>
      DT::formatStyle(
        columns = "Pts",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }

  output$history_team_a_players <- renderDT({
    matchup <- selected_history_matchup()
    req(matchup)

    matchup_player_table(matchup$team_a, matchup) |>
      format_history_player_table()
  }, server = FALSE)

  output$history_team_b_players <- renderDT({
    matchup <- selected_history_matchup()
    req(matchup)

    matchup_player_table(matchup$team_b, matchup) |>
      format_history_player_table()
  }, server = FALSE)



}

shinyApp(ui = ui, server = server)
