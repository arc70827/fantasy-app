# app.R
# Fantasy League Hub
#
# Expected folder structure:
# fantasy-app/
# ├── app.R
# └── data/
#     ├── fantasy_matchup_data.csv
#     ├── fantasy_manager_data.csv
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

signed_num_fmt <- function(x, digits = 1) {
  ifelse(
    is.na(x),
    "—",
    sprintf(paste0("%+.", digits, "f"), x)
  )
}

signed_pct_fmt <- function(x, digits = 1) {
  ifelse(
    is.na(x),
    "—",
    sprintf(paste0("%+.", digits, "f%%"), 100 * x)
  )
}


manager_image_slug <- function(manager_name) {
  manager_name |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "-") |>
    str_replace_all("(^-+|-+$)", "")
}

manager_initials <- function(manager_name) {
  parts <- str_split(str_squish(as.character(manager_name)), "\\s+")[[1]]
  parts <- parts[nzchar(parts)]

  if (length(parts) == 0) {
    return("?")
  }

  initials <- str_sub(parts, 1, 1)
  paste0(head(initials, 2), collapse = "") |>
    str_to_upper()
}

manager_headshot_src <- function(manager_name) {
  slug <- manager_image_slug(manager_name)
  candidates <- paste0(
    "img/managers/",
    slug,
    c(".png", ".PNG", ".jpg", ".JPG", ".jpeg", ".JPEG", ".webp", ".WEBP")
  )

  disk_paths <- file.path("www", candidates)
  existing <- candidates[file.exists(disk_paths)]

  if (length(existing) == 0) {
    return(NA_character_)
  }

  existing[[1]]
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
    selection = list(mode = "single", target = "row"),
    filter = "none",
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
        list(width = "29%", targets = 0),
        list(width = "14%", targets = 1),
        list(width = "39%", targets = 2),
        list(width = "18%", targets = 3, className = "dt-right")
      )
    ),
    class = "stripe compact player-performance-table"
  )
}

datatable_record <- function(
  data,
  page_length = 5,
  selection = "none",
  escape = TRUE,
  highlight_leader = TRUE,
  leader_rows = 1L
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

  leader_rows <- suppressWarnings(as.integer(leader_rows))
  if (is.na(leader_rows) || leader_rows < 1L) {
    leader_rows <- 1L
  }

  if (isTRUE(highlight_leader)) {
    table_options$rowCallback <- JS(
      "function(row, data, displayNum) {",
      paste0("  if (displayNum < ", leader_rows, ") {"),
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

leader_tie_count <- function(values, tolerance = 1e-9) {
  values <- as.numeric(values)
  values <- values[!is.na(values)]

  if (length(values) == 0) {
    return(0L)
  }

  as.integer(sum(abs(values - values[[1]]) <= tolerance))
}

record_rows_with_leader_ties <- function(data, value_col, n = 5L) {
  if (nrow(data) == 0 || !value_col %in% names(data)) {
    return(data)
  }

  leader_rows <- leader_tie_count(data[[value_col]])
  data |>
    slice_head(n = max(as.integer(n), leader_rows))
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

team_names <- safe_read("data/fantasy_manager_data.csv") |>
  mutate(
    manager = as.character(manager),
    espn_id = as.character(espn_id),
    team_name = as.character(team_name),
    start_year = as.integer(start_year),
    start_week = as.integer(start_week),
    end_year = as.integer(end_year),
    end_week = as.integer(end_week)
  )

required_manager_columns <- c(
  "manager",
  "espn_id",
  "team_name",
  "start_year",
  "start_week",
  "end_year",
  "end_week"
)
missing_manager_columns <- setdiff(required_manager_columns, names(team_names))

if (length(missing_manager_columns) > 0) {
  stop(
    "fantasy_manager_data.csv is missing required columns: ",
    paste(missing_manager_columns, collapse = ", "),
    call. = FALSE
  )
}

invalid_manager_ranges <- team_names |>
  filter(
    is.na(manager) |
      is.na(team_name) |
      is.na(start_year) |
      is.na(start_week) |
      is.na(end_year) |
      is.na(end_week) |
      start_week < 1 |
      start_week > 18 |
      end_week < 1 |
      end_week > 18 |
      start_year > end_year |
      (start_year == end_year & start_week > end_week)
  )

if (nrow(invalid_manager_ranges) > 0) {
  print(invalid_manager_ranges)
  stop("fantasy_manager_data.csv contains an invalid manager range.", call. = FALSE)
}

manager_range_contains <- function(
    start_year_value,
    start_week_value,
    end_year_value,
    end_week_value,
    year_value,
    week_value
) {
  starts_before_or_on <-
    start_year_value < year_value |
    (start_year_value == year_value & start_week_value <= week_value)

  ends_after_or_on <-
    end_year_value > year_value |
    (end_year_value == year_value & end_week_value >= week_value)

  starts_before_or_on & ends_after_or_on
}

resolve_manager_one <- function(team_name_value, year_value, week_value) {
  result <- team_names |>
    filter(
      team_name == team_name_value,
      manager_range_contains(
        start_year,
        start_week,
        end_year,
        end_week,
        year_value,
        week_value
      )
    ) |>
    distinct(manager) |>
    pull(manager)

  if (length(result) == 0 || is.na(result[1])) {
    return(as.character(team_name_value))
  }

  if (length(result) > 1) {
    stop(
      paste0(
        "More than one manager matches team ",
        team_name_value,
        " in ",
        year_value,
        " Week ",
        week_value,
        "."
      ),
      call. = FALSE
    )
  }

  result[1]
}

resolve_team_name_one <- function(manager_value, year_value, week_value = NA_integer_) {
  candidates <- team_names |>
    filter(
      manager == manager_value,
      start_year <= year_value,
      end_year >= year_value
    )

  if (!is.na(week_value)) {
    candidates <- candidates |>
      filter(
        manager_range_contains(
          start_year,
          start_week,
          end_year,
          end_week,
          year_value,
          week_value
        )
      )
  } else {
    candidates <- candidates |>
      arrange(
        desc(end_year),
        desc(end_week),
        desc(start_year),
        desc(start_week)
      )
  }

  result <- candidates |>
    pull(team_name)

  if (length(result) == 0 || is.na(result[1])) {
    return(as.character(manager_value))
  }

  result[1]
}

matchups <- matchups |>
  mutate(
    manager = mapply(resolve_manager_one, team, year, week, USE.NAMES = FALSE),
    opposing_manager = mapply(resolve_manager_one, opposing_team, year, week, USE.NAMES = FALSE)
  )

players <- players |>
  mutate(
    manager = mapply(resolve_manager_one, fantasy_team, year, week, USE.NAMES = FALSE)
  )

regular_season_matchups <- matchups |>
  filter(str_to_lower(str_squish(matchup_type)) == "regular season")

regular_season_players <- players |>
  filter(week <= 14)

years <- sort(
  unique(
    c(
      matchups$year,
      players$year,
      team_names$start_year,
      team_names$end_year
    )
  ),
  decreasing = TRUE
)
years <- years[!is.na(years)]

managers <- sort(
  unique(
    c(
      matchups$manager,
      matchups$opposing_manager,
      team_names$manager
    )
  )
)

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

playoff_years <- pair_games |>
  filter(str_to_lower(str_squish(matchup_type)) != "regular season") |>
  distinct(year) |>
  arrange(desc(year)) |>
  pull(year)

latest_playoff_year <- if (length(playoff_years) > 0) {
  max(playoff_years, na.rm = TRUE)
} else {
  latest_year
}

manager_season_finish_info <- function(manager_value, year_value) {
  finish <- manager_finishes |>
    filter(manager == manager_value, year == year_value) |>
    slice_head(n = 1)

  team_name <- resolve_team_name_one(manager_value, year_value)

  if (nrow(finish) > 0) {
    return(list(
      label = finish$finish_label[[1]],
      team = finish$team[[1]]
    ))
  }

  season_games <- matchups |>
    filter(manager == manager_value, year == year_value) |>
    arrange(week)

  postseason_games <- season_games |>
    filter(str_to_lower(str_squish(matchup_type)) != "regular season")

  lost_semifinal <- any(
    str_to_lower(str_squish(postseason_games$matchup_type)) == "semifinals" &
      postseason_games$loss == 1,
    na.rm = TRUE
  )

  lost_quarterfinal <- any(
    str_to_lower(str_squish(postseason_games$matchup_type)) == "quarterfinals" &
      postseason_games$loss == 1,
    na.rm = TRUE
  )

  made_postseason <- nrow(postseason_games) > 0

  season_max_week <- matchups |>
    filter(year == year_value) |>
    summarise(max_week = max(week, na.rm = TRUE)) |>
    pull(max_week)

  season_in_progress <- length(season_max_week) == 1 &&
    is.finite(season_max_week) &&
    season_max_week < 15

  finish_label <- case_when(
    lost_semifinal ~ "Semifinals",
    lost_quarterfinal ~ "Quarterfinals",
    nrow(season_games) > 0 && !made_postseason && season_in_progress ~ "In Progress",
    nrow(season_games) > 0 && !made_postseason ~ "Missed Playoffs",
    TRUE ~ "Unavailable"
  )

  list(label = finish_label, team = team_name)
}

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


# ---- Fantasy analytics ----

inactive_lineup_slots <- c(
  "BENCH", "BE", "BN", "IR", "INJURED RESERVE", "IL"
)

ir_lineup_slots <- c(
  "IR", "INJURED RESERVE", "IL"
)

normalize_fantasy_position <- function(x) {
  x_clean <- str_to_upper(str_squish(as.character(x)))

  case_when(
    x_clean %in% c("QB", "RB", "WR", "TE", "K") ~ x_clean,
    x_clean %in% c("D/ST", "DST", "DEF", "D") ~ "D/ST",
    TRUE ~ x_clean
  )
}

top_n_total <- function(values, n) {
  values <- as.numeric(values)
  values <- values[!is.na(values)]

  if (length(values) == 0 || n <= 0) {
    return(0)
  }

  sum(head(sort(values, decreasing = TRUE), n))
}

calculate_optimal_lineup_score <- function(roster) {
  roster <- roster |>
    mutate(
      pos_clean = normalize_fantasy_position(pos),
      slot_clean = str_to_upper(str_squish(as.character(slot))),
      fpts = as.numeric(fpts)
    )

  # IR players are not considered available for an optimal starting lineup.
  eligible <- roster |>
    filter(
      !slot_clean %in% ir_lineup_slots,
      !is.na(fpts),
      !is.na(pos_clean),
      nzchar(pos_clean)
    )

  qb_total <- top_n_total(eligible$fpts[eligible$pos_clean == "QB"], 1)
  dst_total <- top_n_total(eligible$fpts[eligible$pos_clean == "D/ST"], 1)
  kicker_total <- top_n_total(eligible$fpts[eligible$pos_clean == "K"], 1)

  skill_players <- eligible |>
    filter(pos_clean %in% c("RB", "WR", "TE")) |>
    mutate(skill_id = row_number())

  if (nrow(skill_players) == 0) {
    skill_total <- 0
  } else {
    skill_scores <- vapply(
      skill_players$skill_id,
      function(flex_id) {
        flex_score <- skill_players$fpts[
          skill_players$skill_id == flex_id
        ][1]

        remaining <- skill_players |>
          filter(skill_id != flex_id)

        flex_score +
          top_n_total(remaining$fpts[remaining$pos_clean == "RB"], 2) +
          top_n_total(remaining$fpts[remaining$pos_clean == "WR"], 2) +
          top_n_total(remaining$fpts[remaining$pos_clean == "TE"], 1)
      },
      numeric(1)
    )

    skill_total <- max(skill_scores, na.rm = TRUE)
  }

  qb_total + skill_total + dst_total + kicker_total
}

build_weekly_expected_wins <- function(matchup_data) {
  matchup_data |>
    group_by(year, week) |>
    mutate(
      expected_wins = vapply(
        points_for,
        function(score_value) {
          valid_scores <- points_for[!is.na(points_for)]

          if (is.na(score_value) || length(valid_scores) <= 1) {
            return(NA_real_)
          }

          teams_beaten <- sum(valid_scores < score_value)
          tied_teams <- max(sum(valid_scores == score_value) - 1, 0)

          (teams_beaten + 0.5 * tied_teams) / (length(valid_scores) - 1)
        },
        numeric(1)
      ),
      actual_win_value = case_when(
        win == 1 ~ 1,
        loss == 1 ~ 0,
        TRUE ~ 0.5
      ),
      xw_delta = actual_win_value - expected_wins
    ) |>
    ungroup()
}

build_weekly_lineup_analytics <- function(player_data) {
  player_data |>
    group_by(manager, fantasy_team, year, week) |>
    group_modify(
      ~ {
        roster <- .x |>
          mutate(
            slot_clean = str_to_upper(str_squish(as.character(slot))),
            pos_clean = normalize_fantasy_position(pos)
          )

        starters <- roster |>
          filter(!slot_clean %in% inactive_lineup_slots)

        actual_starter_points <- sum(starters$fpts, na.rm = TRUE)

        # Historical 2021 data contains starters only, so there is no honest
        # way to reconstruct an optimal bench decision for those weeks.
        has_full_roster_data <- any(
          roster$slot_clean %in% c("BENCH", "BE", "BN")
        )

        optimal_points <- if (has_full_roster_data) {
          calculate_optimal_lineup_score(roster)
        } else {
          NA_real_
        }

        lineup_efficiency <- if (
          is.na(optimal_points) ||
          optimal_points == 0
        ) {
          NA_real_
        } else {
          min(actual_starter_points / optimal_points, 1)
        }

        points_left_on_bench <- if (is.na(optimal_points)) {
          NA_real_
        } else {
          max(optimal_points - actual_starter_points, 0)
        }

        projection_starters <- starters |>
          filter(!is.na(proj))

        projection_weeks_available <- nrow(projection_starters) > 0

        projected_starter_points <- if (projection_weeks_available) {
          sum(projection_starters$proj, na.rm = TRUE)
        } else {
          NA_real_
        }

        projection_actual_points <- if (projection_weeks_available) {
          sum(projection_starters$fpts, na.rm = TRUE)
        } else {
          NA_real_
        }

        projection_delta <- if (projection_weeks_available) {
          projection_actual_points - projected_starter_points
        } else {
          NA_real_
        }

        projection_pct <- if (
          is.na(projected_starter_points) ||
          projected_starter_points == 0
        ) {
          NA_real_
        } else {
          projection_delta / projected_starter_points
        }

        tibble(
          actual_starter_points = actual_starter_points,
          optimal_points = optimal_points,
          lineup_efficiency = lineup_efficiency,
          points_left_on_bench = points_left_on_bench,
          projected_starter_points = projected_starter_points,
          projection_actual_points = projection_actual_points,
          projection_delta = projection_delta,
          projection_pct = projection_pct
        )
      }
    ) |>
    ungroup()
}

safe_mean_numeric <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  mean(x)
}

aggregate_fantasy_analytics <- function(expected_scope, lineup_scope) {
  expected_summary <- expected_scope |>
    group_by(manager) |>
    summarise(
      xw_games = sum(!is.na(expected_wins)),
      expected_wins_raw = sum(expected_wins, na.rm = TRUE),
      actual_wins_raw = sum(actual_win_value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      expected_wins = if_else(
        xw_games > 0,
        expected_wins_raw,
        NA_real_
      ),
      actual_wins = if_else(
        xw_games > 0,
        actual_wins_raw,
        NA_real_
      ),
      xw_delta = actual_wins - expected_wins
    ) |>
    select(
      manager,
      xw_games,
      expected_wins,
      actual_wins,
      xw_delta
    )

  lineup_summary <- lineup_scope |>
    group_by(manager) |>
    summarise(
      leff_weeks = sum(!is.na(lineup_efficiency)),
      leff_actual_points = sum(
        if_else(
          !is.na(lineup_efficiency),
          actual_starter_points,
          0
        ),
        na.rm = TRUE
      ),
      leff_optimal_points = sum(
        if_else(
          !is.na(lineup_efficiency),
          optimal_points,
          0
        ),
        na.rm = TRUE
      ),
      points_left_per_week = safe_mean_numeric(
        points_left_on_bench[
          !is.na(lineup_efficiency)
        ]
      ),
      pp_weeks = sum(!is.na(projection_delta)),
      pp_points_per_week = safe_mean_numeric(projection_delta),
      pp_projected_points = sum(
        if_else(
          !is.na(projection_delta),
          projected_starter_points,
          0
        ),
        na.rm = TRUE
      ),
      pp_actual_points = sum(
        if_else(
          !is.na(projection_delta),
          projection_actual_points,
          0
        ),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    mutate(
      lineup_efficiency = if_else(
        leff_weeks > 0 & leff_optimal_points != 0,
        leff_actual_points / leff_optimal_points,
        NA_real_
      ),
      projection_pct = if_else(
        pp_weeks > 0 & pp_projected_points != 0,
        (pp_actual_points - pp_projected_points) / pp_projected_points,
        NA_real_
      )
    ) |>
    select(
      manager,
      leff_weeks,
      lineup_efficiency,
      points_left_per_week,
      pp_weeks,
      pp_points_per_week,
      projection_pct
    )

  full_join(expected_summary, lineup_summary, by = "manager")
}

build_win_streaks <- function(matchup_data) {
  matchup_data |>
    filter(win == 1) |>
    arrange(manager, year, week) |>
    group_by(manager, year) |>
    mutate(
      new_streak = row_number() == 1L | week != lag(week) + 1L,
      streak_id = cumsum(coalesce(new_streak, TRUE))
    ) |>
    group_by(manager, year, streak_id) |>
    summarise(
      team_name = if (n_distinct(team) == 1L) {
        first(team)
      } else {
        paste(unique(team), collapse = " / ")
      },
      start_week = min(week, na.rm = TRUE),
      end_week = max(week, na.rm = TRUE),
      streak_weeks = n(),
      .groups = "drop"
    ) |>
    arrange(desc(streak_weeks), desc(year), start_week, team_name)
}

weekly_expected_wins <- build_weekly_expected_wins(regular_season_matchups)
weekly_lineup_analytics <- build_weekly_lineup_analytics(players)

season_team_lookup <- matchups |>
  arrange(manager, year, desc(week)) |>
  group_by(manager, year) |>
  slice_head(n = 1) |>
  ungroup() |>
  transmute(
    manager,
    year,
    team_name = team
  )

season_fantasy_analytics <- sort(unique(matchups$year)) |>
  lapply(
    function(season_year) {
      aggregate_fantasy_analytics(
        expected_scope = weekly_expected_wins |>
          filter(year == season_year),
        lineup_scope = weekly_lineup_analytics |>
          filter(year == season_year)
      ) |>
        mutate(year = season_year)
    }
  ) |>
  bind_rows() |>
  left_join(
    season_team_lookup,
    by = c("manager", "year")
  )

career_fantasy_analytics <- aggregate_fantasy_analytics(
  expected_scope = weekly_expected_wins,
  lineup_scope = weekly_lineup_analytics
) |>
  left_join(
    matchups |>
      distinct(manager, year) |>
      count(manager, name = "seasons"),
    by = "manager"
  )

# ---- Release notes ----

app_release_version <- "1.1"

app_release_intro <- paste(
  "Introducing new metrics designed to show how lucky or skilled",
  "different fantasy managers may be."
)

app_release_metric_notes <- list(
  list(
    label = "Expected Wins (XW)",
    text = paste(
      "Shows how many games a manager would be expected to win based on how",
      "their weekly score compared with every other team in the league.",
      "XW uses regular-season games only."
    )
  ),
  list(
    label = "Lineup Efficiency (LEff)",
    text = paste(
      "Shows how efficiently a manager set their lineup by comparing actual",
      "starter points with the highest-scoring legal lineup available from",
      "that week's roster."
    )
  ),
  list(
    label = "Projection Performance (PP)",
    text = paste(
      "Shows how a manager's starters performed relative to ESPN projections,",
      "measured as the average points per week above or below projection."
    )
  )
)

app_release_feature_notes <- c(
  "New Record Book leaderboards show career and single-season historical rankings for XW, LEff, and PP, along with a new Win Streak tracker. Primary career records now use regular-season games only, with playoff-inclusive leaderboards shown separately.",
  "The new Playoffs tab preserves the history of every playoff matchup in league history, including brackets, matchup scores, byes, and each season's champion.",
  "Manager headshots are now included throughout the app, including manager pages, matchup recaps, and playoff brackets."
)

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

      .metric-playoff-note {
        display: inline-block;
        margin-top: 3px;
        color: rgba(76, 65, 66, 0.62);
        font-size: 10.5px;
        font-weight: 500;
      }

      .manager-profile-header {
        display: flex;
        align-items: center;
        gap: 12px;
        margin-top: 10px;
      }

      .manager-avatar,
      .manager-avatar-fallback {
        width: 82px;
        height: 82px;
        flex: 0 0 82px;
        border: 3px solid rgba(190, 28, 48, 0.18);
        border-radius: 50%;
        box-shadow: 0 5px 14px rgba(43, 30, 30, 0.10);
      }

      .manager-avatar {
        display: block;
        object-fit: cover;
        object-position: center;
      }

      .manager-avatar-fallback {
        display: flex;
        align-items: center;
        justify-content: center;
        background: rgba(224, 227, 228, 0.62);
        color: var(--near-black);
        font-size: 24px;
        font-weight: 900;
        letter-spacing: 0.04em;
      }

      .manager-profile-name {
        color: rgba(255, 255, 255, 0.96);
        font-size: 22px;
        font-weight: 900;
        line-height: 1.05;
      }

      .manager-profile-team {
        margin-top: 4px;
        color: rgba(255, 255, 255, 0.72);
        font-size: 12px;
        font-weight: 700;
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

      select.form-control {
        width: 100%;
        min-height: 34px;
        padding-right: 28px;
        cursor: pointer;
        background-color: #ffffff;
      }

      .player-performance-table {
        width: 100% !important;
        table-layout: fixed;
      }

      .player-performance-table thead th,
      .player-performance-table tbody td {
        white-space: normal !important;
        overflow-wrap: anywhere;
      }

      .player-performance-table tbody tr {
        cursor: pointer;
      }

      .player-performance-table tbody td:last-child {
        color: var(--primary-red);
        font-weight: 900;
      }

      .player-detail-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 8px;
      }

      .player-detail-item {
        padding: 9px 10px;
        border: 1px solid var(--border);
        border-radius: 8px;
        background: rgba(224, 227, 228, 0.34);
      }

      .player-detail-label {
        margin-bottom: 2px;
        color: var(--muted);
        font-size: 10px;
        font-weight: 900;
        letter-spacing: 0.055em;
        text-transform: uppercase;
      }

      .player-detail-value {
        color: var(--near-black);
        font-size: 16px;
        font-weight: 800;
        line-height: 1.1;
        overflow-wrap: anywhere;
      }

      .player-detail-value.player-detail-points {
        color: var(--primary-red);
        font-size: 20px;
        font-weight: 900;
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
        align-items: center;
        flex-direction: row;
        gap: 10px;
      }

      .recap-team:first-child {
        border-right: 2px solid var(--near-black);
      }

      .recap-avatar,
      .recap-avatar-fallback {
        width: 46px;
        height: 46px;
        flex: 0 0 46px;
        border: 2px solid rgba(43, 30, 30, 0.16);
        border-radius: 50%;
      }

      .recap-avatar {
        display: block;
        object-fit: cover;
        object-position: center;
      }

      .recap-avatar-fallback {
        display: flex;
        align-items: center;
        justify-content: center;
        background: rgba(224, 227, 228, 0.86);
        color: var(--near-black);
        font-size: 14px;
        font-weight: 900;
      }

      .recap-team-copy {
        min-width: 0;
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

      .playoff-bracket {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 12px;
        align-items: start;
      }

      .playoff-round {
        min-width: 0;
      }

      .playoff-round-title {
        margin: 0 0 8px;
        color: var(--near-black);
        font-size: 16px;
        font-weight: 900;
        letter-spacing: 0.035em;
        text-align: center;
        text-transform: uppercase;
      }

      .playoff-matchup-card,
      .playoff-bye-card {
        margin-bottom: 9px;
        overflow: hidden;
        border: 1px solid var(--border);
        border-radius: 9px;
        background: #ffffff;
        box-shadow: 0 3px 10px rgba(43, 30, 30, 0.07);
      }

      .playoff-team-row {
        display: grid;
        grid-template-columns: 30px minmax(0, 1fr) auto;
        gap: 7px;
        align-items: center;
        padding: 8px 9px;
      }

      .playoff-team-row + .playoff-team-row {
        border-top: 1px solid var(--border);
      }

      .playoff-team-row.playoff-winner {
        background: linear-gradient(135deg, rgba(190, 28, 48, 0.10), rgba(224, 227, 228, 0.35));
      }

      .playoff-avatar,
      .playoff-avatar-fallback {
        width: 30px;
        height: 30px;
        border: 1px solid rgba(43, 30, 30, 0.14);
        border-radius: 50%;
      }

      .playoff-avatar {
        display: block;
        object-fit: cover;
        object-position: center;
      }

      .playoff-avatar-fallback {
        display: flex;
        align-items: center;
        justify-content: center;
        background: rgba(224, 227, 228, 0.82);
        color: var(--near-black);
        font-size: 10px;
        font-weight: 900;
      }

      .playoff-team-copy {
        min-width: 0;
      }

      .playoff-team-name {
        overflow: hidden;
        color: var(--near-black);
        font-size: 13px;
        font-weight: 900;
        line-height: 1.02;
        text-overflow: ellipsis;
        white-space: nowrap;
      }

      .playoff-manager-name {
        margin-top: 2px;
        overflow: hidden;
        color: var(--muted);
        font-size: 10px;
        font-weight: 600;
        line-height: 1.05;
        text-overflow: ellipsis;
        white-space: nowrap;
      }

      .playoff-score {
        color: var(--primary-red);
        font-size: 17px;
        font-weight: 900;
      }

      .playoff-bye-card {
        display: grid;
        grid-template-columns: 30px minmax(0, 1fr) auto;
        gap: 7px;
        align-items: center;
        padding: 8px 9px;
        border-style: dashed;
      }

      .playoff-bye-label {
        color: var(--muted);
        font-size: 10px;
        font-weight: 900;
        letter-spacing: 0.05em;
        text-transform: uppercase;
      }

      .playoff-final-block + .playoff-final-block {
        margin-top: 15px;
      }

      .playoff-final-subtitle {
        margin: 0 0 7px;
        color: var(--muted);
        font-size: 11px;
        font-weight: 900;
        letter-spacing: 0.04em;
        text-align: center;
        text-transform: uppercase;
      }

      .playoff-champion-section {
        padding: 0;
        overflow: hidden;
      }

      .playoff-champion-card {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 6px;
        padding: 24px 18px;
        text-align: center;
        background: linear-gradient(145deg, #f8df7a, #d7aa2d);
        border: 1px solid #b78916;
        box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.48);
      }

      .playoff-champion-avatar,
      .playoff-champion-avatar-fallback {
        width: 92px;
        height: 92px;
        border: 3px solid rgba(43, 30, 30, 0.78);
        border-radius: 50%;
      }

      .playoff-champion-avatar {
        object-fit: cover;
        object-position: center;
      }

      .playoff-champion-avatar-fallback {
        display: flex;
        align-items: center;
        justify-content: center;
        background: rgba(255, 255, 255, 0.42);
        color: var(--near-black);
        font-size: 28px;
        font-weight: 900;
      }

      .playoff-champion-manager {
        margin-top: 3px;
        color: var(--near-black);
        font-size: 22px;
        font-weight: 900;
        line-height: 1.05;
      }

      .playoff-champion-team {
        color: rgba(43, 30, 30, 0.72);
        font-size: 13px;
        font-weight: 700;
      }

      .playoff-champion-label {
        margin-top: 5px;
        color: var(--near-black);
        font-size: 13px;
        font-weight: 900;
        letter-spacing: 0.08em;
        text-transform: uppercase;
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

      .section-subtitle {
        margin: -4px 0 8px;
        color: var(--muted);
        font-size: 13px;
        font-weight: 600;
        letter-spacing: 0.01em;
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

      .record-book-subsection {
        margin-top: 16px;
        padding-top: 14px;
        border-top: 2px solid rgba(76, 65, 66, 0.16);
      }

      .record-book-subsection > h3 {
        margin: 0 0 2px;
      }

      .record-book-subsection > .muted {
        margin: 0 0 10px;
      }

      table.dataTable tbody tr.record-holder-row > td {
        background: rgba(212, 175, 55, 0.18) !important;
        font-size: inherit !important;
      }

      table.dataTable tbody tr.record-holder-row > td:last-child,
      table.dataTable tbody tr.record-holder-row > td:last-child strong {
        font-weight: 900 !important;
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

      .power-ranking-actions {
        display: flex;
        justify-content: center;
        margin-top: 8px;
      }

      .power-ranking-toggle {
        min-width: 110px;
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
        display: flex;
        align-items: stretch;
        height: 48px;
      }

      .hub-nav-toggle {
        display: flex;
        flex: 1 1 auto;
        align-items: center;
        justify-content: space-between;
        min-width: 0;
        width: auto;
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

      .hub-update-button.btn {
        position: relative;
        display: flex;
        flex: 0 0 48px;
        align-items: center;
        justify-content: center;
        width: 48px;
        height: 48px;
        margin: 0;
        padding: 0;
        border: 0;
        border-left: 1px solid rgba(224, 227, 228, 0.10);
        border-radius: 0;
        background: transparent;
        color: var(--off-white);
        font-size: 17px;
        box-shadow: none;
      }

      .hub-update-button.btn:hover,
      .hub-update-button.btn:focus,
      .hub-update-button.btn:active {
        outline: none;
        background: rgba(255, 255, 255, 0.08);
        color: #ffffff;
        box-shadow: none;
      }

      .hub-update-dot {
        position: absolute;
        top: 9px;
        right: 9px;
        display: none;
        width: 8px;
        height: 8px;
        border: 2px solid var(--near-black);
        border-radius: 50%;
        background: #FFCA28;
        box-sizing: content-box;
      }

      .hub-update-button.has-update .hub-update-dot {
        display: block;
      }

      .release-version {
        display: inline-block;
        margin-bottom: 8px;
        padding: 4px 8px;
        border-radius: 999px;
        background: rgba(190, 28, 48, 0.10);
        color: var(--dark-red);
        font-size: 12px;
        font-weight: 900;
        letter-spacing: 0.04em;
        text-transform: uppercase;
      }

      .release-notes {
        margin: 4px 0 0;
        padding-left: 20px;
      }

      .release-notes li {
        margin-bottom: 8px;
      }

      .analytics-table-wrap {
        width: 100%;
        overflow-x: hidden;
      }

      #league_analytics_table table.dataTable tbody td:nth-child(2),
      #league_analytics_table table.dataTable tbody td:nth-child(3),
      #league_analytics_table table.dataTable tbody td:nth-child(4) {
        font-weight: 800;
      }

      #league_analytics_table table.dataTable tbody td:nth-child(2) {
        color: var(--dark-red);
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

        .hub-update-button.btn {
          flex-basis: 46px;
          width: 46px;
          height: 46px;
        }

        .hub-update-dot {
          top: 8px;
          right: 8px;
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
          gap: 6px;
        }

        .recap-avatar,
        .recap-avatar-fallback {
          width: 34px;
          height: 34px;
          flex-basis: 34px;
        }

        .recap-avatar-fallback {
          font-size: 10px;
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

        .playoff-bracket {
          grid-template-columns: 1fr;
          gap: 10px;
        }

        .playoff-round {
          padding-bottom: 2px;
        }

        .playoff-round-title {
          margin-bottom: 6px;
          font-size: 14px;
          text-align: left;
        }

        .playoff-team-name {
          font-size: 12.5px;
        }

        .playoff-score {
          font-size: 16px;
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
          height: 390px !important;
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

        .championship-grid.championship-grid-odd .championship-card:last-child {
          grid-column: 1 / -1;
          width: min(100%, 220px);
          justify-self: center;
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

        function updateReleaseNotificationState() {
          var button = document.getElementById('whats_new_button');
          if (!button) return;

          var version = button.getAttribute('data-version') || '';
          var seenVersion = null;

          try {
            seenVersion = window.localStorage.getItem('fantasyHubSeenRelease');
          } catch (error) {
            seenVersion = null;
          }

          button.classList.toggle('has-update', seenVersion !== version);
        }

        function markReleaseSeen() {
          var button = document.getElementById('whats_new_button');
          if (!button) return;

          var version = button.getAttribute('data-version') || '';

          try {
            window.localStorage.setItem('fantasyHubSeenRelease', version);
          } catch (error) {
            // If local storage is unavailable, the modal still works.
          }

          button.classList.remove('has-update');
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

        function initializeHubMenu() {
          closeHubMenu();
          bindLoadingScreen();
          registerTableMessageHandler();
          updateReleaseNotificationState();

          if (document.documentElement.dataset.hubMenuBound === 'true') return;
          document.documentElement.dataset.hubMenuBound = 'true';

          document.addEventListener('click', function(event) {
            var updateButton = event.target && event.target.closest
              ? event.target.closest('#whats_new_button')
              : null;

            if (updateButton) markReleaseSeen();

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
        ),
        actionButton(
          "whats_new_button",
          label = tagList(
            tags$span(
              class = "glyphicon glyphicon-bell",
              `aria-hidden` = "true"
            ),
            tags$span(
              class = "hub-update-dot",
              `aria-hidden` = "true"
            )
          ),
          class = "hub-update-button",
          title = "What's New",
          `aria-label` = "What's New",
          `data-version` = app_release_version
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
          "hub_nav_playoffs",
          "Playoffs",
          class = "hub-nav-item",
          onclick = "document.getElementById('hub_nav').classList.remove('is-open'); document.getElementById('hub_menu_toggle').setAttribute('aria-expanded','false');"
        ),
        actionButton(
          "hub_nav_history",
          "Matchup Archive",
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
        div(class = "power-ranking-wrap", DTOutput("power_rankings_table")),
        div(
          class = "power-ranking-actions",
          actionButton(
            "power_rankings_toggle",
            "Show More",
            class = "btn-primary power-ranking-toggle"
          )
        )
      ),
      div(
        class = "section-card",
        div(
          class = "section-title-row",
          h3("Fantasy Luck Analytics"),
          actionButton(
            "analytics_info",
            "i",
            class = "info-button",
            title = "How Fantasy Luck Analytics work"
          )
        ),
        div(
          class = "section-subtitle",
          "XW = Expected Wins • LEff = Lineup Efficiency • PP = Projection Performance"
        ),
        div(
          class = "analytics-table-wrap",
          DTOutput("league_analytics_table")
        ),
        div(
          class = "power-ranking-actions",
          actionButton(
            "league_analytics_toggle",
            "Show More",
            class = "btn-primary power-ranking-toggle"
          )
        )
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
          selectInput(
            "manager_select",
            "Manager",
            choices = c("Select manager" = "", managers),
            selected = "",
            selectize = FALSE
          ),
          uiOutput("manager_period_ui")
        ),
        uiOutput("manager_avatar_ui")
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
          selectInput(
            "player_slot",
            "Roster Slot",
            choices = c("All Slots", "QB", "RB", "WR", "TE", "FLEX", "D/ST", "K", "Bench", "IR"),
            selected = "All Slots",
            selectize = FALSE
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
        p(
          class = "muted",
          "* Career Wins, Win Streak, Career Points, Points by Roster Slot, and Expected Wins use regular-season games only. Playoff-inclusive versions of the affected career records appear at the bottom. Career and single-season LEff and PP use all eligible games."
        )
      ),
      div(
        class = "section-card",
        div(
          class = "control-row",
          selectInput("record_scope", "Season", choices = c("All Time", years), selected = "All Time", selectize = FALSE)
        )
      ),
      uiOutput("record_book_tables")
    )
  ),

  tabPanel(
    "Playoffs",
    div(
      class = "page-wrap",
      div(
        class = "section-card hero-card",
        h2("Playoffs"),
        p(class = "muted", "Select a season to view the full playoff bracket and matchup scores.")
      ),
      div(
        class = "section-card",
        div(
          class = "control-row",
          selectInput(
            "playoffs_year",
            "Season",
            choices = playoff_years,
            selected = latest_playoff_year,
            selectize = FALSE
          )
        )
      ),
      div(
        class = "section-card",
        uiOutput("playoff_bracket")
      ),
      uiOutput("playoff_champion")
    )
  ),

  tabPanel(
    title = "Matchup Archive",
    value = "History",
    div(
      class = "page-wrap",
      div(
        class = "section-card hero-card",
        h2("Matchup Archive")
      ),
      div(
        class = "section-card",
        div(
          class = "control-row",
          selectInput("history_year", "Season", choices = c("All Seasons", years), selected = "All Seasons", selectize = FALSE),
          uiOutput("history_week_ui"),
          selectInput("history_manager", "Manager", choices = c("All Managers", managers), selected = "All Managers", selectize = FALSE)
        )
      ),
      div(
        class = "section-card",
        h3("Past Matchups"),
        p(class = "muted", "Tap on a matchup to explore more"),
        DTOutput("history_matchups_table")
      )
    )
  )

)

# ---- Server ----

server <- function(input, output, session) {

  show_all_power_rankings <- reactiveVal(FALSE)
  show_all_league_analytics <- reactiveVal(FALSE)

  latest_player_year <- max(players$year, na.rm = TRUE)
  latest_player_week <- max(
    players$week[players$year == latest_player_year],
    na.rm = TRUE
  )

  reset_tab_state <- function() {
    show_all_power_rankings(FALSE)
    show_all_league_analytics(FALSE)
    updateActionButton(session, "power_rankings_toggle", label = "Show More")
    updateActionButton(session, "league_analytics_toggle", label = "Show More")

    updateSelectInput(session, "manager_select", selected = "")
    updateSelectInput(session, "manager_period", selected = "")

    updateSelectInput(
      session,
      "player_year",
      selected = as.character(latest_player_year)
    )
    updateSelectInput(
      session,
      "player_week",
      selected = as.character(latest_player_week)
    )
    updateSelectInput(session, "player_manager", selected = "All Managers")
    updateSelectInput(session, "player_slot", selected = "All Slots")
    updateTextInput(session, "player_search", value = "")

    updateSelectInput(session, "record_scope", selected = "All Time")
    updateSelectInput(session, "playoffs_year", selected = as.character(latest_playoff_year))

    updateSelectInput(session, "history_year", selected = "All Seasons")
    updateSelectInput(session, "history_week", selected = "All Weeks")
    updateSelectInput(session, "history_manager", selected = "All Managers")
  }

  observeEvent(input$hub_nav_dashboard, {
    reset_tab_state()
    updateNavbarPage(session, "main_tabs", selected = "Dashboard")
  }, ignoreInit = TRUE)

  observeEvent(input$hub_nav_managers, {
    reset_tab_state()
    updateNavbarPage(session, "main_tabs", selected = "Managers")
  }, ignoreInit = TRUE)

  observeEvent(input$hub_nav_players, {
    reset_tab_state()
    updateNavbarPage(session, "main_tabs", selected = "Players")
  }, ignoreInit = TRUE)

  observeEvent(input$hub_nav_record_book, {
    reset_tab_state()
    updateNavbarPage(session, "main_tabs", selected = "Record Book")
    session$sendCustomMessage("recalculate_tables", list())
  }, ignoreInit = TRUE)

  observeEvent(input$hub_nav_playoffs, {
    reset_tab_state()
    updateNavbarPage(session, "main_tabs", selected = "Playoffs")
  }, ignoreInit = TRUE)

  observeEvent(input$hub_nav_history, {
    reset_tab_state()
    updateNavbarPage(session, "main_tabs", selected = "History")
    session$sendCustomMessage("recalculate_tables", list())
  }, ignoreInit = TRUE)

  observeEvent(input$whats_new_button, {
    showModal(
      modalDialog(
        title = "What's New",
        easyClose = TRUE,
        footer = modalButton("Close"),
        div(
          class = "release-version",
          paste0("Fantasy Football Hub v", app_release_version)
        ),
        tags$p(
          class = "release-intro",
          app_release_intro
        ),
        tags$ul(
          class = "release-notes release-metric-notes",
          lapply(
            app_release_metric_notes,
            function(note) {
              tags$li(
                strong(paste0(note$label, ": ")),
                note$text
              )
            }
          )
        ),
        tags$p(
          class = "release-home-note",
          "All three metrics are displayed in the Fantasy Luck Analytics section on the Home page and on individual manager pages."
        ),
        tags$ul(
          class = "release-notes",
          lapply(app_release_feature_notes, tags$li)
        )
      )
    )
  })

  show_fantasy_analytics_info <- function() {
    showModal(
      modalDialog(
        title = "How Fantasy Luck Analytics Work",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tags$div(
          tags$p(
            strong("Expected Wins (XW): "),
            "Regular-season games only. Each week, your score is compared with every other recorded team score from that week. ",
            "XW is the share of those teams your score would have beaten, with ties counting as half a win. ",
            "Weekly XW values are added across the selected period."
          ),
          tags$p(
            strong("Lineup Efficiency (LEff): "),
            "Your actual starter points divided by the highest scoring legal lineup that could have been made from your roster that week. ",
            "IR players are excluded from the optimal lineup. Historical weeks without full bench data are shown as unavailable rather than estimated."
          ),
          tags$p(
            strong("Projection Performance (PP): "),
            "The average number of points per week your starters scored above or below their ESPN projections. ",
            "Positive PP means your starters outperformed projection; negative PP means they underperformed."
          ),
          tags$p(
            class = "muted",
            "XW uses regular-season games only. LEff and PP remain descriptive lineup and projection measures across the selected period. The three indicators are not combined into one overall luck score."
          )
        )
      )
    )
  }

  observeEvent(input$analytics_info, {
    show_fantasy_analytics_info()
  })

  observeEvent(input$manager_analytics_info, {
    show_fantasy_analytics_info()
  })

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
        p("All three ingredients use regular-season results only. The score is scaled from current-season league results, then combined into one ranking number.")
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

    selectInput(
      "player_year",
      "Season",
      choices = year_choices,
      selected = selected_year,
      selectize = FALSE
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

    selectInput(
      "player_manager",
      "Manager",
      choices = c("All Managers", manager_choices),
      selected = selected_manager,
      selectize = FALSE
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

    selectInput(
      "player_week",
      "Week",
      choices = c("All Weeks", weeks),
      selected = selected_week,
      selectize = FALSE
    )
  })

  output$manager_period_ui <- renderUI({
    if (is.null(input$manager_select) || input$manager_select == "") {
      return(selectInput(
        "manager_period",
        "Years / Team Name",
        choices = c("Select manager first" = ""),
        selected = "",
        selectize = FALSE
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

    selectInput(
      "manager_period",
      "Years / Team Name",
      choices = stats::setNames(values, labels),
      selected = "All Years",
      selectize = FALSE
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
    regular_season_matchups |> filter(year == dashboard_year())
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
        "League Average Score",
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

      recap_avatar <- function(manager_name) {
        src <- manager_headshot_src(manager_name)

        if (!is.na(src)) {
          tags$img(
            class = "recap-avatar",
            src = src,
            alt = paste0(manager_name, " headshot")
          )
        } else {
          div(
            class = "recap-avatar-fallback",
            manager_initials(manager_name)
          )
        }
      }

      div(
        class = "recap-card",
        div(
          class = paste("recap-team", ifelse(score_a >= score_b, "recap-winner", "")),
          recap_avatar(manager_a),
          div(
            class = "recap-team-copy",
            div(class = "recap-team-name", team_a),
            div(class = "recap-team-score", score_fmt(score_a))
          )
        ),
        div(
          class = paste("recap-team", ifelse(score_b >= score_a, "recap-winner", "")),
          recap_avatar(manager_b),
          div(
            class = "recap-team-copy",
            div(class = "recap-team-name", team_b),
            div(class = "recap-team-score", score_fmt(score_b))
          )
        )
      )
    })

    tagList(recap_items)
  })

  power_rankings <- reactive({
    current_year <- dashboard_year()
    current_week <- dashboard_week()

    season_data <- regular_season_matchups |>
      filter(year == current_year, week <= current_week)

    recent_data <- season_data |>
      arrange(manager, desc(week)) |>
      group_by(manager) |>
      slice_head(n = 3) |>
      ungroup()

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

  observeEvent(input$power_rankings_toggle, {
    show_all_power_rankings(!show_all_power_rankings())
    updateActionButton(
      session,
      "power_rankings_toggle",
      label = if (show_all_power_rankings()) "Show Less" else "Show More"
    )
  }, ignoreInit = TRUE)

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

    displayed_rankings <- if (show_all_power_rankings()) {
      ranking_data
    } else {
      ranking_data |> slice_head(n = 5)
    }

    displayed_rankings |>
      datatable_simple(page_length = max(1, nrow(displayed_rankings))) |>
      DT::formatStyle(
        columns = "Power Score",
        fontWeight = "900"
      )
  })

  league_analytics <- reactive({
    current_year <- dashboard_year()
    current_week <- dashboard_week()

    expected_scope <- weekly_expected_wins |>
      filter(
        year == current_year,
        week <= current_week
      )

    lineup_scope <- weekly_lineup_analytics |>
      filter(
        year == current_year,
        week <= current_week
      )

    aggregate_fantasy_analytics(
      expected_scope = expected_scope,
      lineup_scope = lineup_scope
    ) |>
      mutate(
        team_name = mapply(
          resolve_team_name_one,
          manager,
          current_year,
          current_week,
          USE.NAMES = FALSE
        )
      ) |>
      arrange(
        desc(expected_wins),
        desc(lineup_efficiency),
        desc(pp_points_per_week),
        team_name
      )
  })

  observeEvent(input$league_analytics_toggle, {
    show_all_league_analytics(!show_all_league_analytics())
    updateActionButton(
      session,
      "league_analytics_toggle",
      label = if (show_all_league_analytics()) "Show Less" else "Show More"
    )
  }, ignoreInit = TRUE)

  output$league_analytics_table <- renderDT({
    analytics_data <- league_analytics() |>
      transmute(
        `Team Name` = team_name,
        XW = if_else(
          is.na(expected_wins),
          "—",
          sprintf("%.2f", expected_wins)
        ),
        LEff = if_else(
          is.na(lineup_efficiency),
          "—",
          sprintf("%.1f%%", 100 * lineup_efficiency)
        ),
        PP = if_else(
          is.na(pp_points_per_week),
          "—",
          sprintf("%+.1f", pp_points_per_week)
        )
      )

    displayed_analytics <- if (show_all_league_analytics()) {
      analytics_data
    } else {
      analytics_data |> slice_head(n = 5)
    }

    displayed_analytics |>
      datatable_simple(page_length = max(1, nrow(displayed_analytics)))
  })

  output$playoff_bracket <- renderUI({
    req(input$playoffs_year)

    selected_year <- as.integer(input$playoffs_year)

    postseason <- pair_games |>
      filter(
        year == selected_year,
        str_to_lower(str_squish(matchup_type)) != "regular season"
      ) |>
      mutate(
        round_key = case_when(
          str_to_lower(str_squish(matchup_type)) == "quarterfinals" ~ "quarterfinals",
          str_to_lower(str_squish(matchup_type)) == "semifinals" ~ "semifinals",
          str_to_lower(str_squish(matchup_type)) %in% c("finals", "championship") ~ "finals",
          str_to_lower(str_squish(matchup_type)) %in% c("third place", "third place game", "3rd place") ~ "third_place",
          TRUE ~ "other"
        )
      )

    validate(need(nrow(postseason) > 0, "No playoff data is available for this season."))

    bracket_avatar <- function(manager_name) {
      src <- manager_headshot_src(manager_name)

      if (!is.na(src)) {
        tags$img(
          class = "playoff-avatar",
          src = src,
          alt = paste0(manager_name, " headshot")
        )
      } else {
        div(
          class = "playoff-avatar-fallback",
          manager_initials(manager_name)
        )
      }
    }

    team_row <- function(manager_name, team_name, score, is_winner = FALSE) {
      div(
        class = paste("playoff-team-row", if (is_winner) "playoff-winner" else ""),
        bracket_avatar(manager_name),
        div(
          class = "playoff-team-copy",
          div(class = "playoff-team-name", team_name),
          div(class = "playoff-manager-name", manager_name)
        ),
        div(class = "playoff-score", score_fmt(score))
      )
    }

    matchup_card <- function(game_row) {
      manager_a <- game_row$manager_a[[1]]
      manager_b <- game_row$manager_b[[1]]
      score_a <- game_row$score_a[[1]]
      score_b <- game_row$score_b[[1]]

      div(
        class = "playoff-matchup-card",
        team_row(
          manager_a,
          game_row$team_a[[1]],
          score_a,
          identical(manager_a, game_row$winner[[1]])
        ),
        team_row(
          manager_b,
          game_row$team_b[[1]],
          score_b,
          identical(manager_b, game_row$winner[[1]])
        )
      )
    }

    round_cards <- function(round_data) {
      if (nrow(round_data) == 0) {
        return(NULL)
      }

      tagList(lapply(seq_len(nrow(round_data)), function(i) {
        matchup_card(round_data[i, , drop = FALSE])
      }))
    }

    quarterfinals <- postseason |>
      filter(round_key == "quarterfinals") |>
      arrange(desc(winning_score))

    semifinals <- postseason |>
      filter(round_key == "semifinals") |>
      arrange(desc(winning_score))

    finals <- postseason |>
      filter(round_key == "finals") |>
      arrange(desc(winning_score))

    third_place <- postseason |>
      filter(round_key == "third_place") |>
      arrange(desc(winning_score))

    semifinal_participants <- unique(c(semifinals$manager_a, semifinals$manager_b))
    quarterfinal_participants <- unique(c(quarterfinals$manager_a, quarterfinals$manager_b))
    bye_managers <- setdiff(semifinal_participants, quarterfinal_participants)

    bye_cards <- if (length(bye_managers) > 0) {
      tagList(lapply(bye_managers, function(manager_name) {
        semifinal_game <- semifinals |>
          filter(manager_a == manager_name | manager_b == manager_name) |>
          slice_head(n = 1)

        team_name <- if (nrow(semifinal_game) > 0 && semifinal_game$manager_a[[1]] == manager_name) {
          semifinal_game$team_a[[1]]
        } else if (nrow(semifinal_game) > 0) {
          semifinal_game$team_b[[1]]
        } else {
          resolve_team_name_one(manager_name, selected_year)
        }

        div(
          class = "playoff-bye-card",
          bracket_avatar(manager_name),
          div(
            class = "playoff-team-copy",
            div(class = "playoff-team-name", team_name),
            div(class = "playoff-manager-name", manager_name)
          ),
          div(class = "playoff-bye-label", "Bye")
        )
      }))
    } else {
      NULL
    }

    finals_content <- tagList(
      if (nrow(finals) > 0) {
        div(
          class = "playoff-final-block",
          div(class = "playoff-final-subtitle", "Championship"),
          round_cards(finals)
        )
      },
      if (nrow(third_place) > 0) {
        div(
          class = "playoff-final-block",
          div(class = "playoff-final-subtitle", "Third Place"),
          round_cards(third_place)
        )
      }
    )

    div(
      class = "playoff-bracket",
      div(
        class = "playoff-round",
        h3(class = "playoff-round-title", "Quarterfinals"),
        bye_cards,
        round_cards(quarterfinals)
      ),
      div(
        class = "playoff-round",
        h3(class = "playoff-round-title", "Semifinals"),
        round_cards(semifinals)
      ),
      div(
        class = "playoff-round",
        h3(class = "playoff-round-title", "Finals"),
        finals_content
      )
    )
  })

  output$playoff_champion <- renderUI({
    req(input$playoffs_year)

    selected_year <- as.integer(input$playoffs_year)

    championship_game <- pair_games |>
      filter(
        year == selected_year,
        str_to_lower(str_squish(matchup_type)) %in% c("finals", "championship")
      ) |>
      arrange(desc(week)) |>
      slice_head(n = 1)

    if (nrow(championship_game) == 0) {
      return(NULL)
    }

    champion_manager <- championship_game$winner[[1]]
    champion_team <- if (
      identical(champion_manager, championship_game$manager_a[[1]])
    ) {
      championship_game$team_a[[1]]
    } else {
      championship_game$team_b[[1]]
    }

    champion_src <- manager_headshot_src(champion_manager)

    champion_avatar <- if (!is.na(champion_src)) {
      tags$img(
        class = "playoff-champion-avatar",
        src = champion_src,
        alt = paste0(champion_manager, " headshot")
      )
    } else {
      div(
        class = "playoff-champion-avatar-fallback",
        manager_initials(champion_manager)
      )
    }

    div(
      class = "section-card playoff-champion-section",
      div(
        class = "playoff-champion-card",
        champion_avatar,
        div(class = "playoff-champion-manager", champion_manager),
        div(class = "playoff-champion-team", champion_team),
        div(
          class = "playoff-champion-label",
          paste0(selected_year, " Champion")
        )
      )
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

  output$manager_avatar_ui <- renderUI({
    if (is.null(input$manager_select) || input$manager_select == "") {
      return(NULL)
    }

    manager_name <- input$manager_select

    manager_years <- matchups$year[
      matchups$manager == manager_name &
        !is.na(matchups$year)
    ]

    selected_year <- if (
      is.null(input$manager_period) ||
      input$manager_period == "" ||
      input$manager_period == "All Years"
    ) {
      if (length(manager_years) > 0) {
        max(manager_years)
      } else {
        latest_year
      }
    } else {
      as.integer(input$manager_period)
    }

    show_team_name <- !is.null(input$manager_period) &&
      input$manager_period != "" &&
      input$manager_period != "All Years"

    team_name <- if (show_team_name) {
      resolve_team_name_one(manager_name, selected_year)
    } else {
      NULL
    }

    headshot_src <- manager_headshot_src(manager_name)

    avatar <- if (!is.na(headshot_src)) {
      tags$img(
        class = "manager-avatar",
        src = headshot_src,
        alt = paste0(manager_name, " headshot")
      )
    } else {
      div(
        class = "manager-avatar-fallback",
        manager_initials(manager_name)
      )
    }

    div(
      class = "manager-profile-header",
      avatar,
      div(
        div(class = "manager-profile-name", manager_name),
        if (!is.null(team_name)) div(class = "manager-profile-team", team_name)
      )
    )
  })

  output$manager_content <- renderUI({
    if (
      is.null(input$manager_select) || input$manager_select == "" ||
      is.null(input$manager_period) || input$manager_period == ""
    ) {
      return(NULL)
    }

    analytics_panel <- div(
      class = "section-card",
      div(
        class = "section-title-row",
        h3("Fantasy Luck Analytics"),
        actionButton(
          "manager_analytics_info",
          "i",
          class = "info-button",
          title = "How Fantasy Luck Analytics work"
        )
      ),
      div(
        class = "section-subtitle",
        "Expected Wins (XW) • Lineup Efficiency (LEff) • Projection Performance (PP)"
      ),
      uiOutput("manager_analytics_cards")
    )

    positional_panel <- div(
      class = "section-card",
      h3("Positional Ranking Breakdown"),
      div(class = "section-subtitle", "Points Per Week by Position"),
      plotOutput("manager_position_spider", height = "480px")
    )

    if (input$manager_period == "All Years") {
      return(tagList(
        uiOutput("manager_championship_shrine"),
        uiOutput("manager_cards"),
        analytics_panel,
        positional_panel
      ))
    }

    tagList(
      uiOutput("manager_finish_summary"),
      uiOutput("manager_cards"),
      analytics_panel,
      div(
        class = "section-card",
        h3("Weekly Scoring Trend"),
        plotOutput("manager_score_trend", height = "400px")
      ),
      positional_panel
    )
  })

  output$manager_analytics_cards <- renderUI({
    req(input$manager_select, input$manager_period)
    validate(need(input$manager_select != "", "Select a manager to view analytics."))

    expected_scope <- weekly_expected_wins |>
      filter(manager == input$manager_select)

    lineup_scope <- weekly_lineup_analytics |>
      filter(manager == input$manager_select)

    if (input$manager_period != "All Years") {
      selected_year <- as.integer(input$manager_period)
      expected_scope <- expected_scope |> filter(year == selected_year)
      lineup_scope <- lineup_scope |> filter(year == selected_year)
    }

    analytics <- aggregate_fantasy_analytics(
      expected_scope = expected_scope,
      lineup_scope = lineup_scope
    )

    validate(need(nrow(analytics) > 0, "No fantasy analytics are available for this period."))

    row <- analytics |> slice_head(n = 1)

    xw_value <- if (is.na(row$expected_wins[[1]])) {
      "—"
    } else {
      sprintf("%.2f", row$expected_wins[[1]])
    }

    xw_subtitle <- if (is.na(row$expected_wins[[1]])) {
      "Expected wins unavailable for this period"
    } else {
      paste0(
        "Regular season • Actual wins: ", sprintf("%.1f", row$actual_wins[[1]]),
        " • Schedule delta: ", signed_num_fmt(row$xw_delta[[1]], 2)
      )
    }

    leff_value <- if (is.na(row$lineup_efficiency[[1]])) {
      "—"
    } else {
      sprintf("%.1f%%", 100 * row$lineup_efficiency[[1]])
    }

    leff_subtitle <- if (is.na(row$lineup_efficiency[[1]])) {
      "Full roster data unavailable for this period"
    } else {
      paste0(
        score_fmt(row$points_left_per_week[[1]]),
        " pts/week left on bench"
      )
    }

    pp_value <- if (is.na(row$pp_points_per_week[[1]])) {
      "—"
    } else {
      paste0(signed_num_fmt(row$pp_points_per_week[[1]], 1), " pts/wk")
    }

    pp_subtitle <- if (is.na(row$projection_pct[[1]])) {
      "Projection data unavailable for this period"
    } else {
      paste0(signed_pct_fmt(row$projection_pct[[1]], 1), " vs projection")
    }

    div(
      class = "metric-grid",
      card(
        "Expected Wins (XW)",
        xw_value,
        xw_subtitle,
        "accent-blue"
      ),
      card(
        "Lineup Efficiency (LEff)",
        leff_value,
        leff_subtitle,
        "accent-green"
      ),
      card(
        "Projection Performance (PP)",
        pp_value,
        pp_subtitle,
        "accent-purple"
      )
    )
  })

  output$manager_cards <- renderUI({
    data_all <- selected_manager_data()

    validate(
      need(nrow(data_all) > 0, "No manager data found.")
    )

    data_regular <- data_all |>
      filter(str_to_lower(str_squish(matchup_type)) == "regular season")

    validate(
      need(nrow(data_regular) > 0, "No regular-season manager data found.")
    )

    wins <- sum(data_regular$win, na.rm = TRUE)
    losses <- sum(data_regular$loss, na.rm = TRUE)
    pf <- sum(data_regular$points_for, na.rm = TRUE)
    pa <- sum(data_regular$points_against, na.rm = TRUE)

    wins_all <- sum(data_all$win, na.rm = TRUE)
    losses_all <- sum(data_all$loss, na.rm = TRUE)
    pf_all <- sum(data_all$points_for, na.rm = TRUE)
    pa_all <- sum(data_all$points_against, na.rm = TRUE)

    regular_win_pct <- wins / pmax(wins + losses, 1)
    all_win_pct <- wins_all / pmax(wins_all + losses_all, 1)

    regular_pf_avg <- mean(data_regular$points_for, na.rm = TRUE)
    regular_pa_avg <- mean(data_regular$points_against, na.rm = TRUE)
    all_pf_avg <- mean(data_all$points_for, na.rm = TRUE)
    all_pa_avg <- mean(data_all$points_against, na.rm = TRUE)

    show_playoff_note <- any(
      str_to_lower(str_squish(data_all$matchup_type)) != "regular season",
      na.rm = TRUE
    )

    best_week <- data_all |> slice_max(points_for, n = 1, with_ties = FALSE)
    worst_week <- data_all |> slice_min(points_for, n = 1, with_ties = FALSE)

    record_subtitle <- if (show_playoff_note) {
      tags$span(
        class = "metric-playoff-note",
        paste0("*Incl playoffs: ", make_record(wins_all, losses_all))
      )
    } else {
      NULL
    }

    win_pct_subtitle <- if (show_playoff_note) {
      tags$span(
        class = "metric-playoff-note",
        paste0("*Incl playoffs: ", pct_fmt(all_win_pct))
      )
    } else {
      NULL
    }

    pf_subtitle <- if (show_playoff_note) {
      tagList(
        paste0("Average: ", score_fmt(regular_pf_avg)),
        tags$br(),
        tags$span(
          class = "metric-playoff-note",
          paste0(
            "*Incl playoffs: ", score_fmt(pf_all),
            " total • ", score_fmt(all_pf_avg), " avg"
          )
        )
      )
    } else {
      paste0("Average: ", score_fmt(regular_pf_avg))
    }

    pa_subtitle <- if (show_playoff_note) {
      tagList(
        paste0("Average: ", score_fmt(regular_pa_avg)),
        tags$br(),
        tags$span(
          class = "metric-playoff-note",
          paste0(
            "*Incl playoffs: ", score_fmt(pa_all),
            " total • ", score_fmt(all_pa_avg), " avg"
          )
        )
      )
    } else {
      paste0("Average: ", score_fmt(regular_pa_avg))
    }

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
      card("Record", make_record(wins, losses), record_subtitle, "accent-blue"),
      card("Win Percentage", pct_fmt(regular_win_pct), win_pct_subtitle, "accent-green"),
      card("Points For", score_fmt(pf), pf_subtitle, "accent-purple"),
      card("Points Against", score_fmt(pa), pa_subtitle, "accent-red"),
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
      div(class = "championship-shrine-title", "Trophy Case"),
      div(
        class = paste(
          "championship-grid",
          if (nrow(championships) %% 2L == 1L) "championship-grid-odd" else ""
        ),
        tagList(championship_cards)
      )
    )
  })

  output$manager_finish_summary <- renderUI({
    req(input$manager_select, input$manager_period)
    validate(need(input$manager_select != "", ""))

    if (input$manager_period == "All Years") {
      return(NULL)
    }

    selected_year <- as.integer(input$manager_period)
    finish_info <- manager_season_finish_info(input$manager_select, selected_year)
    finish_label <- finish_info$label

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

    inactive_roster_slots <- c(
      "BENCH", "BE", "BN", "IR", "INJURED RESERVE", "IL", "SLOT"
    )

    position_data <- regular_season_players |>
      mutate(
        pos_clean = case_when(
          pos %in% c("QB", "RB", "WR", "TE", "K") ~ pos,
          pos %in% c("D/ST", "DST", "DEF", "D") ~ "D/ST",
          TRUE ~ as.character(pos)
        ),
        roster_slot_clean = str_to_upper(str_squish(as.character(slot)))
      ) |>
      filter(
        pos_clean %in% position_levels,
        !roster_slot_clean %in% inactive_roster_slots
      )

    games_played <- regular_season_matchups |>
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
        detail_label = paste0(rank_label, ", ", score_fmt(avg_points)),
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
        label_x = 1.09 * cos(angle),
        label_y = 1.09 * sin(angle),
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
      coord_equal(xlim = c(-1.23, 1.23), ylim = c(-1.23, 1.23), clip = "off") +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme_void(base_size = 13) +
      theme(
        plot.margin = margin(8, 16, 8, 16)
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


  player_performance_data <- reactive({
    player_filtered() |>
      arrange(desc(fpts), desc(year), desc(week), player_name) |>
      transmute(
        Season = year,
        Week = week,
        Game = paste0(year, " Week ", week),
        Position = pos,
        Player = player_name,
        `Fantasy Team` = fantasy_team,
        `NFL Team` = team,
        `Roster Slot` = slot,
        Proj = round(proj, 2),
        Pts = round(fpts, 2)
      )
  })

  output$players_table <- renderDT({
    player_performance_data() |>
      transmute(
        Game,
        Position,
        Player,
        Points = Pts
      ) |>
      datatable_player_performance(page_length = 25) |>
      DT::formatStyle(
        columns = "Points",
        color = "#BE1C30",
        fontWeight = "900"
      )
  })

  selected_player_performance <- reactiveVal(NULL)
  players_table_proxy <- DT::dataTableProxy("players_table")

  observeEvent(input$players_table_rows_selected, {
    selected_row <- input$players_table_rows_selected
    req(length(selected_row) > 0)

    player_row <- player_performance_data() |>
      slice(selected_row[1])

    selected_player_performance(player_row)

    detail_item <- function(label, value, value_class = NULL) {
      div(
        class = "player-detail-item",
        div(class = "player-detail-label", label),
        div(class = paste("player-detail-value", value_class), value)
      )
    }

    showModal(
      modalDialog(
        title = player_row$Player[[1]],
        size = "m",
        easyClose = FALSE,
        footer = actionButton("close_player_modal", "Close", class = "btn-primary"),
        div(
          class = "player-detail-grid",
          detail_item("Season", player_row$Season[[1]]),
          detail_item("Week", player_row$Week[[1]]),
          detail_item("Fantasy Team", player_row$`Fantasy Team`[[1]]),
          detail_item("NFL Team", player_row$`NFL Team`[[1]]),
          detail_item("Position", player_row$Position[[1]]),
          detail_item("Roster Slot", player_row$`Roster Slot`[[1]]),
          detail_item("Proj", score_fmt(player_row$Proj[[1]])),
          detail_item("Pts", score_fmt(player_row$Pts[[1]]), "player-detail-points")
        )
      )
    )
  }, ignoreInit = TRUE)

  observeEvent(input$close_player_modal, {
    removeModal()
    selected_player_performance(NULL)
    DT::selectRows(players_table_proxy, NULL)
  }, ignoreInit = TRUE)





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

  record_limit <- reactive({
    5
  })

  output$record_book_tables <- renderUI({
    if (is.na(record_scope_year())) {
      tagList(
        div(
          class = "record-grid",
          div(class = "record-table-card", h3("Championships"), DTOutput("record_championships")),
          div(class = "record-table-card", h3("Career Wins*"), DTOutput("record_total_wins")),
          div(class = "record-table-card", h3("Win Streak*"), DTOutput("record_win_streak")),
          div(class = "record-table-card", h3("Single-Week Scores"), DTOutput("record_single_week")),
          div(class = "record-table-card", h3("Career Points*"), DTOutput("record_total_points")),
          div(class = "record-table-card", h3("Points by Roster Slot*"), DTOutput("record_slot_points")),
          div(class = "record-table-card", h3("Biggest Blowouts"), DTOutput("record_blowouts")),
          div(class = "record-table-card", h3("Closest Games"), DTOutput("record_closest")),
          div(
            class = "record-table-card",
            h3("Career Expected Wins (XW)*"),
            p(class = "muted", "Most expected wins accumulated across a manager's career."),
            DTOutput("record_xw_career")
          ),
          div(
            class = "record-table-card",
            h3("Career Lineup Efficiency (LEff)"),
            p(class = "muted", "Best lineup efficiency across all eligible career weeks."),
            DTOutput("record_leff_career")
          ),
          div(
            class = "record-table-card",
            h3("Career Projection Performance (PP)"),
            p(class = "muted", "Best average points per week versus projection across a manager's career."),
            DTOutput("record_pp_career")
          ),
          div(
            class = "record-table-card",
            h3("Hospital"),
            p(class = "muted", "Most weeks with at least one player in an IR slot, cumulative across all seasons."),
            DTOutput("record_hospital")
          )
        ),
        div(
          class = "record-book-subsection",
          h3("Single-Season Fantasy Luck Analytics"),
          p(
            class = "muted",
            "The best individual-season performances in Expected Wins, Lineup Efficiency, and Projection Performance."
          ),
          div(
            class = "record-grid",
            div(
              class = "record-table-card",
              h3("Expected Wins (XW)*"),
              p(class = "muted", "Best single-season expected wins."),
              DTOutput("record_xw")
            ),
            div(
              class = "record-table-card",
              h3("Lineup Efficiency (LEff)"),
              p(class = "muted", "Best single-season lineup efficiency."),
              DTOutput("record_leff")
            ),
            div(
              class = "record-table-card",
              h3("Projection Performance (PP)"),
              p(class = "muted", "Best single-season performance versus projection."),
              DTOutput("record_pp")
            )
          )
        ),
        div(
          class = "record-book-subsection",
          h3("Including Playoffs"),
          p(
            class = "muted",
            "These versions include both regular-season and postseason games."
          ),
          div(
            class = "record-grid",
            div(class = "record-table-card", h3("Career Points"), DTOutput("record_total_points_all_games")),
            div(class = "record-table-card", h3("Career Wins"), DTOutput("record_total_wins_all_games")),
            div(class = "record-table-card", h3("Points by Roster Slot"), DTOutput("record_slot_points_all_games"))
          )
        )
      )
    } else {
      tagList(
        div(
          class = "record-grid",
          div(class = "record-table-card", h3("Win Streak*"), DTOutput("record_win_streak")),
          div(class = "record-table-card", h3("Single-Week Scores"), DTOutput("record_single_week")),
          div(class = "record-table-card", h3("Biggest Blowouts"), DTOutput("record_blowouts")),
          div(class = "record-table-card", h3("Closest Games"), DTOutput("record_closest"))
        ),
        div(
          class = "record-book-subsection",
          h3("Fantasy Luck Analytics"),
          p(
            class = "muted",
            "Expected Wins, Lineup Efficiency, and Projection Performance for the selected season."
          ),
          div(
            class = "record-grid",
            div(
              class = "record-table-card",
              h3("Expected Wins (XW)*"),
              DTOutput("record_xw")
            ),
            div(
              class = "record-table-card",
              h3("Lineup Efficiency (LEff)"),
              DTOutput("record_leff")
            ),
            div(
              class = "record-table-card",
              h3("Projection Performance (PP)"),
              DTOutput("record_pp")
            )
          )
        )
      )
    }
  })

  output$record_single_week <- renderDT({
    ranked <- record_matchups() |>
      arrange(desc(points_for), desc(year), desc(week), team) |>
      mutate(
        leader_value = points_for
      ) |>
      record_rows_with_leader_ties("leader_value", record_limit())

    leader_rows <- leader_tie_count(ranked$leader_value)

    display <- ranked |>
      transmute(
        Season = year,
        Week = week,
        Team = team,
        Manager = manager,
        Score = round(points_for, 2)
      ) |>
      record_scope_columns(record_scope_year())

    display |>
      datatable_record(
        page_length = max(record_limit(), nrow(display)),
        leader_rows = leader_rows
      ) |>
      DT::formatStyle(
        columns = "Score",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_blowouts <- renderDT({
    ranked <- record_pair_games() |>
      arrange(desc(margin), desc(year), desc(week)) |>
      mutate(
        WinningTeam = if_else(score_a >= score_b, team_a, team_b),
        LosingTeam = if_else(score_a < score_b, team_a, team_b),
        WinningScore = pmax(score_a, score_b),
        LosingScore = pmin(score_a, score_b),
        leader_value = margin
      ) |>
      record_rows_with_leader_ties("leader_value", record_limit())

    leader_rows <- leader_tie_count(ranked$leader_value)

    ranked |>
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
      datatable_record(
        page_length = max(record_limit(), nrow(ranked)),
        escape = FALSE,
        leader_rows = leader_rows
      )
  }, server = FALSE)

  output$record_closest <- renderDT({
    ranked <- record_pair_games() |>
      arrange(margin, desc(year), desc(week)) |>
      mutate(
        WinningTeam = if_else(score_a >= score_b, team_a, team_b),
        LosingTeam = if_else(score_a < score_b, team_a, team_b),
        WinningScore = pmax(score_a, score_b),
        LosingScore = pmin(score_a, score_b),
        leader_value = margin
      ) |>
      record_rows_with_leader_ties("leader_value", record_limit())

    leader_rows <- leader_tie_count(ranked$leader_value)

    ranked |>
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
      datatable_record(
        page_length = max(record_limit(), nrow(ranked)),
        escape = FALSE,
        leader_rows = leader_rows
      )
  }, server = FALSE)

  output$record_hospital <- renderDT({
    ranked <- players |>
      mutate(
        slot_clean = str_to_upper(str_squish(as.character(slot)))
      ) |>
      filter(
        slot_clean %in% c("IR", "INJURED RESERVE", "IL"),
        !is.na(manager),
        nzchar(manager)
      ) |>
      distinct(year, week, manager) |>
      count(manager, name = "IR Weeks") |>
      arrange(desc(`IR Weeks`), manager) |>
      mutate(
        leader_value = `IR Weeks`
      ) |>
      record_rows_with_leader_ties("leader_value", 5L)

    leader_rows <- leader_tie_count(ranked$leader_value)

    ranked |>
      transmute(
        Manager = manager,
        `IR Weeks`
      ) |>
      datatable_record(
        page_length = max(5L, nrow(ranked)),
        leader_rows = leader_rows
      )
  }, server = FALSE)

  output$record_total_points <- renderDT({
    ranked <- regular_season_matchups |>
      group_by(manager) |>
      summarise(
        `Career Points` = sum(points_for, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(`Career Points`), manager) |>
      mutate(
        leader_value = `Career Points`
      ) |>
      record_rows_with_leader_ties("leader_value", 5L)

    leader_rows <- leader_tie_count(ranked$leader_value)

    ranked |>
      transmute(
        Manager = manager,
        `Career Points` = round(`Career Points`, 2)
      ) |>
      datatable_record(
        page_length = max(5L, nrow(ranked)),
        leader_rows = leader_rows
      ) |>
      DT::formatStyle(
        columns = "Career Points",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_total_wins <- renderDT({
    ranked <- regular_season_matchups |>
      group_by(manager) |>
      summarise(
        `Career Wins` = sum(win, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(`Career Wins`), manager) |>
      mutate(
        leader_value = `Career Wins`
      ) |>
      record_rows_with_leader_ties("leader_value", 5L)

    leader_rows <- leader_tie_count(ranked$leader_value)

    ranked |>
      transmute(
        Manager = manager,
        `Career Wins`
      ) |>
      datatable_record(
        page_length = max(5L, nrow(ranked)),
        leader_rows = leader_rows
      ) |>
      DT::formatStyle(
        columns = "Career Wins",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_championships <- renderDT({
    ranked <- manager_finishes |>
      filter(finish == 1L) |>
      count(manager, name = "Championships") |>
      arrange(desc(Championships), manager) |>
      mutate(
        leader_value = Championships
      ) |>
      record_rows_with_leader_ties("leader_value", 5L)

    leader_rows <- leader_tie_count(ranked$leader_value)

    ranked |>
      transmute(
        Manager = manager,
        Championships
      ) |>
      datatable_record(
        page_length = max(5L, nrow(ranked)),
        leader_rows = leader_rows
      ) |>
      DT::formatStyle(
        columns = "Championships",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_slot_points <- renderDT({
    slot_order <- c("QB", "RB", "WR", "TE", "FLEX", "D/ST", "K")

    regular_season_players |>
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
      slice_max(Points, n = 1, with_ties = TRUE) |>
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

  output$record_win_streak <- renderDT({
    streaks <- build_win_streaks(regular_season_matchups)

    if (!is.na(record_scope_year())) {
      streaks <- streaks |>
        filter(year == record_scope_year())
    }

    ranked <- streaks |>
      arrange(desc(streak_weeks), desc(year), start_week, team_name) |>
      mutate(
        leader_value = streak_weeks
      ) |>
      record_rows_with_leader_ties("leader_value", record_limit())

    leader_rows <- leader_tie_count(ranked$leader_value)

    display <- ranked |>
      transmute(
        Season = year,
        `Team Name` = team_name,
        Streak = streak_weeks,
        Weeks = if_else(
          start_week == end_week,
          paste0("Week ", start_week),
          paste0("Weeks ", start_week, "–", end_week)
        )
      ) |>
      record_scope_columns(record_scope_year())

    display |>
      datatable_record(
        page_length = max(record_limit(), nrow(display)),
        leader_rows = leader_rows
      ) |>
      DT::formatStyle(
        columns = "Streak",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_xw_career <- renderDT({
    ranked <- career_fantasy_analytics |>
      filter(!is.na(expected_wins)) |>
      arrange(desc(expected_wins), manager) |>
      mutate(
        leader_value = expected_wins
      ) |>
      record_rows_with_leader_ties("leader_value", record_limit())

    leader_rows <- leader_tie_count(ranked$leader_value)

    display <- ranked |>
      transmute(
        Manager = manager,
        Seasons = seasons,
        XW = round(expected_wins, 2)
      )

    display |>
      datatable_record(
        page_length = max(record_limit(), nrow(display)),
        leader_rows = leader_rows
      ) |>
      DT::formatStyle(
        columns = "XW",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_leff_career <- renderDT({
    ranked <- career_fantasy_analytics |>
      filter(!is.na(lineup_efficiency)) |>
      arrange(desc(lineup_efficiency), manager) |>
      mutate(
        leader_value = lineup_efficiency
      ) |>
      record_rows_with_leader_ties("leader_value", record_limit())

    leader_rows <- leader_tie_count(ranked$leader_value)

    display <- ranked |>
      transmute(
        Manager = manager,
        Seasons = seasons,
        LEff = lineup_efficiency
      )

    display |>
      datatable_record(
        page_length = max(record_limit(), nrow(display)),
        leader_rows = leader_rows
      ) |>
      DT::formatPercentage(
        columns = "LEff",
        digits = 1
      ) |>
      DT::formatStyle(
        columns = "LEff",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_pp_career <- renderDT({
    ranked <- career_fantasy_analytics |>
      filter(!is.na(pp_points_per_week)) |>
      arrange(desc(pp_points_per_week), manager) |>
      mutate(
        leader_value = pp_points_per_week
      ) |>
      record_rows_with_leader_ties("leader_value", record_limit())

    leader_rows <- leader_tie_count(ranked$leader_value)

    display <- ranked |>
      transmute(
        Manager = manager,
        Seasons = seasons,
        PP = sprintf("%+.1f", pp_points_per_week)
      )

    display |>
      datatable_record(
        page_length = max(record_limit(), nrow(display)),
        leader_rows = leader_rows
      ) |>
      DT::formatStyle(
        columns = "PP",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_xw <- renderDT({
    ranked <- season_fantasy_analytics |>
      filter(!is.na(expected_wins))

    if (!is.na(record_scope_year())) {
      ranked <- ranked |>
        filter(year == record_scope_year())
    }

    ranked <- ranked |>
      arrange(desc(expected_wins), desc(year), team_name, manager) |>
      mutate(
        leader_value = expected_wins
      ) |>
      record_rows_with_leader_ties("leader_value", record_limit())

    leader_rows <- leader_tie_count(ranked$leader_value)

    display <- ranked |>
      transmute(
        Season = year,
        Team = coalesce(team_name, manager),
        Manager = manager,
        XW = round(expected_wins, 2)
      ) |>
      record_scope_columns(record_scope_year())

    display |>
      datatable_record(
        page_length = max(record_limit(), nrow(display)),
        leader_rows = leader_rows
      ) |>
      DT::formatStyle(
        columns = "XW",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_leff <- renderDT({
    ranked <- season_fantasy_analytics |>
      filter(!is.na(lineup_efficiency))

    if (!is.na(record_scope_year())) {
      ranked <- ranked |>
        filter(year == record_scope_year())
    }

    ranked <- ranked |>
      arrange(desc(lineup_efficiency), desc(year), team_name, manager) |>
      mutate(
        leader_value = lineup_efficiency
      ) |>
      record_rows_with_leader_ties("leader_value", record_limit())

    leader_rows <- leader_tie_count(ranked$leader_value)

    display <- ranked |>
      transmute(
        Season = year,
        Team = coalesce(team_name, manager),
        Manager = manager,
        LEff = lineup_efficiency
      ) |>
      record_scope_columns(record_scope_year())

    display |>
      datatable_record(
        page_length = max(record_limit(), nrow(display)),
        leader_rows = leader_rows
      ) |>
      DT::formatPercentage(
        columns = "LEff",
        digits = 1
      ) |>
      DT::formatStyle(
        columns = "LEff",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_pp <- renderDT({
    ranked <- season_fantasy_analytics |>
      filter(!is.na(pp_points_per_week))

    if (!is.na(record_scope_year())) {
      ranked <- ranked |>
        filter(year == record_scope_year())
    }

    ranked <- ranked |>
      arrange(desc(pp_points_per_week), desc(year), team_name, manager) |>
      mutate(
        leader_value = pp_points_per_week
      ) |>
      record_rows_with_leader_ties("leader_value", record_limit())

    leader_rows <- leader_tie_count(ranked$leader_value)

    display <- ranked |>
      transmute(
        Season = year,
        Team = coalesce(team_name, manager),
        Manager = manager,
        PP = sprintf("%+.1f", pp_points_per_week)
      ) |>
      record_scope_columns(record_scope_year())

    display |>
      datatable_record(
        page_length = max(record_limit(), nrow(display)),
        leader_rows = leader_rows
      ) |>
      DT::formatStyle(
        columns = "PP",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_total_points_all_games <- renderDT({
    ranked <- matchups |>
      group_by(manager) |>
      summarise(
        `Career Points` = sum(points_for, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(`Career Points`), manager) |>
      mutate(
        leader_value = `Career Points`
      ) |>
      record_rows_with_leader_ties("leader_value", 5L)

    leader_rows <- leader_tie_count(ranked$leader_value)

    ranked |>
      transmute(
        Manager = manager,
        `Career Points` = round(`Career Points`, 2)
      ) |>
      datatable_record(
        page_length = max(5L, nrow(ranked)),
        leader_rows = leader_rows
      ) |>
      DT::formatStyle(
        columns = "Career Points",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_total_wins_all_games <- renderDT({
    ranked <- matchups |>
      group_by(manager) |>
      summarise(
        `Career Wins` = sum(win, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(`Career Wins`), manager) |>
      mutate(
        leader_value = `Career Wins`
      ) |>
      record_rows_with_leader_ties("leader_value", 5L)

    leader_rows <- leader_tie_count(ranked$leader_value)

    ranked |>
      transmute(
        Manager = manager,
        `Career Wins`
      ) |>
      datatable_record(
        page_length = max(5L, nrow(ranked)),
        leader_rows = leader_rows
      ) |>
      DT::formatStyle(
        columns = "Career Wins",
        color = "#BE1C30",
        fontWeight = "bold"
      )
  }, server = FALSE)

  output$record_slot_points_all_games <- renderDT({
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
      slice_max(Points, n = 1, with_ties = TRUE) |>
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
      selected = "All Weeks",
      selectize = FALSE
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
