# scripts/build_app_cache.R
#
# Builds the prepared data object used by app.R.
# This script is intended to run before deployment, not when a Shiny worker starts.

required_packages <- c(
  "dplyr", "readr", "stringr", "janitor"
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

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(janitor)
})

cache_schema_version <- 1L
cache_path <- file.path("data", "app_cache.rds")

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

message("Reading production fantasy data...")

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

message("Resolving historical manager ownership...")

matchups <- matchups |>
  mutate(
    manager = mapply(
      resolve_manager_one,
      team,
      year,
      week,
      USE.NAMES = FALSE
    ),
    opposing_manager = mapply(
      resolve_manager_one,
      opposing_team,
      year,
      week,
      USE.NAMES = FALSE
    )
  )

players <- players |>
  mutate(
    manager = mapply(
      resolve_manager_one,
      fantasy_team,
      year,
      week,
      USE.NAMES = FALSE
    )
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

all_teams <- sort(
  unique(
    c(
      matchups$team,
      matchups$opposing_team,
      players$fantasy_team
    )
  )
)

all_positions <- sort(unique(players$pos))
all_slots <- sort(unique(players$slot))
latest_year <- max(years, na.rm = TRUE)

latest_week <- matchups |>
  filter(year == latest_year) |>
  summarise(max_week = max(week, na.rm = TRUE)) |>
  pull(max_week)

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
      winner = if_else(
        first(points_for) >= first(points_against),
        first(manager),
        first(opposing_manager)
      ),
      loser = if_else(
        first(points_for) < first(points_against),
        first(manager),
        first(opposing_manager)
      ),
      winning_score = max(
        first(points_for),
        first(points_against),
        na.rm = TRUE
      ),
      losing_score = min(
        first(points_for),
        first(points_against),
        na.rm = TRUE
      ),
      matchup_type = first(matchup_type),
      .groups = "drop"
    ) |>
    mutate(
      matchup_label = paste0(manager_a, " vs ", manager_b),
      score_label = paste0(score_fmt(score_a), " - ", score_fmt(score_b))
    )
}

pair_games <- make_pair_games(matchups)

infer_manager_finishes <- function() {
  finals_games <- pair_games |>
    filter(
      matchup_type %in% c(
        "Championship",
        "Finals",
        "Third Place",
        "Third Place Game",
        "3rd Place"
      )
    )

  if (nrow(finals_games) == 0) {
    return(
      tibble(
        year = integer(),
        manager = character(),
        team = character(),
        finish = integer(),
        finish_label = character()
      )
    )
  }

  championship_games <- finals_games |>
    filter(matchup_type %in% c("Championship", "Finals")) |>
    group_by(year) |>
    slice_max(winning_score, n = 1, with_ties = FALSE) |>
    ungroup()

  third_place_games <- finals_games |>
    filter(
      matchup_type %in% c(
        "Third Place",
        "Third Place Game",
        "3rd Place"
      )
    )

  champ_finishes <- bind_rows(
    championship_games |>
      transmute(
        year,
        manager = winner,
        team = if_else(winner == manager_a, team_a, team_b),
        finish = 1L
      ),
    championship_games |>
      transmute(
        year,
        manager = loser,
        team = if_else(loser == manager_a, team_a, team_b),
        finish = 2L
      )
  )

  third_finishes <- bind_rows(
    third_place_games |>
      transmute(
        year,
        manager = winner,
        team = if_else(winner == manager_a, team_a, team_b),
        finish = 3L
      ),
    third_place_games |>
      transmute(
        year,
        manager = loser,
        team = if_else(loser == manager_a, team_a, team_b),
        finish = 4L
      )
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

inactive_lineup_slots <- c(
  "BENCH",
  "BE",
  "BN",
  "IR",
  "INJURED RESERVE",
  "IL"
)

ir_lineup_slots <- c(
  "IR",
  "INJURED RESERVE",
  "IL"
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

  eligible <- roster |>
    filter(
      !slot_clean %in% ir_lineup_slots,
      !is.na(fpts),
      !is.na(pos_clean),
      nzchar(pos_clean)
    )

  qb_total <- top_n_total(
    eligible$fpts[eligible$pos_clean == "QB"],
    1
  )

  dst_total <- top_n_total(
    eligible$fpts[eligible$pos_clean == "D/ST"],
    1
  )

  kicker_total <- top_n_total(
    eligible$fpts[eligible$pos_clean == "K"],
    1
  )

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
          top_n_total(
            remaining$fpts[remaining$pos_clean == "RB"],
            2
          ) +
          top_n_total(
            remaining$fpts[remaining$pos_clean == "WR"],
            2
          ) +
          top_n_total(
            remaining$fpts[remaining$pos_clean == "TE"],
            1
          )
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
          tied_teams <- max(
            sum(valid_scores == score_value) - 1,
            0
          )

          (
            teams_beaten +
              0.5 * tied_teams
          ) / (
            length(valid_scores) - 1
          )
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
            slot_clean = str_to_upper(
              str_squish(as.character(slot))
            ),
            pos_clean = normalize_fantasy_position(pos)
          )

        starters <- roster |>
          filter(!slot_clean %in% inactive_lineup_slots)

        actual_starter_points <- sum(
          starters$fpts,
          na.rm = TRUE
        )

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
          max(
            optimal_points - actual_starter_points,
            0
          )
        }

        projection_starters <- starters |>
          filter(!is.na(proj))

        projection_weeks_available <- nrow(
          projection_starters
        ) > 0

        projected_starter_points <- if (
          projection_weeks_available
        ) {
          sum(
            projection_starters$proj,
            na.rm = TRUE
          )
        } else {
          NA_real_
        }

        projection_actual_points <- if (
          projection_weeks_available
        ) {
          sum(
            projection_starters$fpts,
            na.rm = TRUE
          )
        } else {
          NA_real_
        }

        projection_delta <- if (
          projection_weeks_available
        ) {
          projection_actual_points -
            projected_starter_points
        } else {
          NA_real_
        }

        projection_pct <- if (
          is.na(projected_starter_points) ||
          projected_starter_points == 0
        ) {
          NA_real_
        } else {
          projection_delta /
            projected_starter_points
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

aggregate_fantasy_analytics <- function(
    expected_scope,
    lineup_scope
) {
  expected_summary <- expected_scope |>
    group_by(manager) |>
    summarise(
      xw_games = sum(!is.na(expected_wins)),
      expected_wins_raw = sum(
        expected_wins,
        na.rm = TRUE
      ),
      actual_wins_raw = sum(
        actual_win_value,
        na.rm = TRUE
      ),
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
      pp_points_per_week = safe_mean_numeric(
        projection_delta
      ),
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
        leff_weeks > 0 &
          leff_optimal_points != 0,
        leff_actual_points /
          leff_optimal_points,
        NA_real_
      ),
      projection_pct = if_else(
        pp_weeks > 0 &
          pp_projected_points != 0,
        (
          pp_actual_points -
            pp_projected_points
        ) /
          pp_projected_points,
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

  full_join(
    expected_summary,
    lineup_summary,
    by = "manager"
  )
}

build_win_streaks <- function(matchup_data) {
  matchup_data |>
    filter(win == 1) |>
    arrange(manager, year, week) |>
    group_by(manager, year) |>
    mutate(
      new_streak =
        row_number() == 1L |
        week != lag(week) + 1L,
      streak_id = cumsum(
        coalesce(new_streak, TRUE)
      )
    ) |>
    group_by(manager, year, streak_id) |>
    summarise(
      team_name = if (n_distinct(team) == 1L) {
        first(team)
      } else {
        paste(
          unique(team),
          collapse = " / "
        )
      },
      start_week = min(week, na.rm = TRUE),
      end_week = max(week, na.rm = TRUE),
      streak_weeks = n(),
      .groups = "drop"
    ) |>
    arrange(
      desc(streak_weeks),
      desc(year),
      start_week,
      team_name
    )
}

message("Building fantasy analytics...")

weekly_expected_wins <- build_weekly_expected_wins(
  regular_season_matchups
)

weekly_lineup_analytics <- build_weekly_lineup_analytics(
  players
)

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

season_fantasy_analytics <- sort(
  unique(matchups$year)
) |>
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

win_streaks <- build_win_streaks(
  regular_season_matchups
)

app_cache <- list(
  cache_schema_version = cache_schema_version,
  generated_at_utc = format(
    Sys.time(),
    tz = "UTC",
    usetz = TRUE
  ),
  matchups = matchups,
  players = players,
  team_names = team_names,
  regular_season_matchups = regular_season_matchups,
  regular_season_players = regular_season_players,
  years = years,
  managers = managers,
  all_teams = all_teams,
  all_positions = all_positions,
  all_slots = all_slots,
  latest_year = latest_year,
  latest_week = latest_week,
  pair_games = pair_games,
  manager_finishes = manager_finishes,
  playoff_years = playoff_years,
  latest_playoff_year = latest_playoff_year,
  weekly_expected_wins = weekly_expected_wins,
  weekly_lineup_analytics = weekly_lineup_analytics,
  season_fantasy_analytics = season_fantasy_analytics,
  career_fantasy_analytics = career_fantasy_analytics,
  win_streaks = win_streaks
)

required_cache_objects <- c(
  "matchups",
  "players",
  "team_names",
  "regular_season_matchups",
  "regular_season_players",
  "years",
  "managers",
  "all_teams",
  "all_positions",
  "all_slots",
  "latest_year",
  "latest_week",
  "pair_games",
  "manager_finishes",
  "playoff_years",
  "latest_playoff_year",
  "weekly_expected_wins",
  "weekly_lineup_analytics",
  "season_fantasy_analytics",
  "career_fantasy_analytics",
  "win_streaks"
)

missing_cache_objects <- setdiff(
  required_cache_objects,
  names(app_cache)
)

if (length(missing_cache_objects) > 0) {
  stop(
    "Cache build failed. Missing objects: ",
    paste(missing_cache_objects, collapse = ", "),
    call. = FALSE
  )
}

if (nrow(app_cache$matchups) == 0) {
  stop("Cache build failed. No matchup rows were produced.", call. = FALSE)
}

if (nrow(app_cache$players) == 0) {
  stop("Cache build failed. No player rows were produced.", call. = FALSE)
}

if (length(app_cache$years) == 0) {
  stop("Cache build failed. No seasons were produced.", call. = FALSE)
}

dir.create(
  dirname(cache_path),
  recursive = TRUE,
  showWarnings = FALSE
)

temporary_cache_path <- tempfile(
  pattern = "app_cache_",
  tmpdir = dirname(cache_path),
  fileext = ".rds"
)

saveRDS(
  app_cache,
  temporary_cache_path,
  compress = FALSE,
  version = 3
)

cache_check <- readRDS(temporary_cache_path)

if (
  is.null(cache_check$cache_schema_version) ||
  as.integer(cache_check$cache_schema_version) != cache_schema_version
) {
  unlink(temporary_cache_path)
  stop(
    "Cache validation failed after writing the RDS file.",
    call. = FALSE
  )
}

if (file.exists(cache_path)) {
  unlink(cache_path)
}

if (!file.rename(temporary_cache_path, cache_path)) {
  copied <- file.copy(
    temporary_cache_path,
    cache_path,
    overwrite = TRUE
  )
  unlink(temporary_cache_path)

  if (!isTRUE(copied)) {
    stop(
      "Could not move the completed cache into place.",
      call. = FALSE
    )
  }
}

cache_size_mb <- file.info(cache_path)$size / 1024^2

message(
  "App cache built successfully: ",
  cache_path
)

message(
  "Cache size: ",
  sprintf("%.2f MB", cache_size_mb)
)

message(
  "Latest data: ",
  latest_year,
  " Week ",
  latest_week
)
