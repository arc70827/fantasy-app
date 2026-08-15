# ============================================================
# ESPN FANTASY FOOTBALL WEEKLY UPDATER v4 - FIXED 2021 VALIDATION
#
# Purpose:
#   Pull one completed fantasy week from ESPN, validate it,
#   update the three production CSV files, and safely support
#   changing fantasy team names and new managers.
#
# Production files:
#   data/fantasy_matchup_data.csv
#   data/fantasy_player_data.csv
#   data/fantasy_manager_data.csv
#
# Safety:
#   Nothing is written unless every validation passes.
#
# Local test example:
#
#   Sys.setenv(
#     TARGET_YEAR = "2025",
#     TARGET_WEEK = "17",
#     DRY_RUN = "true"
#   )
#   source("scripts/update_weekly.R")
#
# Scheduled production mode:
#   Leave TARGET_YEAR and TARGET_WEEK blank.
#   Set DRY_RUN=false in GitHub Actions.
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

library(httr2)
library(jsonlite)
library(tidyverse)


# ============================================================
# 2. SETTINGS
# ============================================================

league_id <- "254169010"

data_dir <- Sys.getenv(
  "DATA_DIR",
  unset = "data"
)

matchup_file <- file.path(
  data_dir,
  "fantasy_matchup_data.csv"
)

player_file <- file.path(
  data_dir,
  "fantasy_player_data.csv"
)

manager_file <- file.path(
  data_dir,
  "fantasy_manager_data.csv"
)

final_fantasy_week <- 17L

target_year_env <- Sys.getenv("TARGET_YEAR", unset = "")
target_week_env <- Sys.getenv("TARGET_WEEK", unset = "")
dry_run_env <- Sys.getenv("DRY_RUN", unset = "true")

dry_run <- tolower(trimws(dry_run_env)) %in%
  c("true", "t", "1", "yes", "y")


# ============================================================
# 3. LOOKUP TABLES
# ============================================================

POSITION_MAP <- c(
  `1`  = "QB",
  `2`  = "RB",
  `3`  = "WR",
  `4`  = "TE",
  `5`  = "K",
  `16` = "D/ST"
)

SLOT_MAP <- c(
  `0`  = "QB",
  `1`  = "TQB",
  `2`  = "RB",
  `3`  = "RB/WR",
  `4`  = "WR",
  `5`  = "WR/TE",
  `6`  = "TE",
  `7`  = "OP",
  `16` = "D/ST",
  `17` = "K",
  `20` = "Bench",
  `21` = "IR",
  `23` = "FLEX"
)

PRO_TEAM_MAP <- c(
  `0`  = "FA",
  `1`  = "ATL",
  `2`  = "BUF",
  `3`  = "CHI",
  `4`  = "CIN",
  `5`  = "CLE",
  `6`  = "DAL",
  `7`  = "DEN",
  `8`  = "DET",
  `9`  = "GB",
  `10` = "TEN",
  `11` = "IND",
  `12` = "KC",
  `13` = "LV",
  `14` = "LAR",
  `15` = "MIA",
  `16` = "MIN",
  `17` = "NE",
  `18` = "NO",
  `19` = "NYG",
  `20` = "NYJ",
  `21` = "PHI",
  `22` = "ARI",
  `23` = "PIT",
  `24` = "LAC",
  `25` = "SF",
  `26` = "SEA",
  `27` = "TB",
  `28` = "WSH",
  `29` = "CAR",
  `30` = "JAX",
  `33` = "BAL",
  `34` = "HOU"
)


# ============================================================
# 4. SAFE HELPERS
# ============================================================

safe_list <- function(x, name) {

  if (
    is.null(x) ||
    !is.list(x) ||
    is.null(x[[name]])
  ) {
    return(list())
  }

  x[[name]]
}


safe_chr <- function(x) {

  if (
    is.null(x) ||
    length(x) == 0
  ) {
    return(NA_character_)
  }

  value <- as.character(x[[1]])

  if (
    length(value) == 0
  ) {
    return(NA_character_)
  }

  value
}


safe_num <- function(x) {

  if (
    is.null(x) ||
    length(x) == 0
  ) {
    return(NA_real_)
  }

  suppressWarnings(
    as.numeric(x[[1]])
  )
}


map_lookup <- function(ids, lookup_table) {

  if (
    length(ids) == 0 ||
    is.na(ids)
  ) {
    return(NA_character_)
  }

  out <- lookup_table[
    as.character(ids)
  ]

  if (
    length(out) == 0 ||
    is.na(out)
  ) {
    return(NA_character_)
  }

  unname(out)
}


read_required_csv <- function(path) {

  if (
    !file.exists(path)
  ) {
    stop(
      paste0(
        "Required file not found: ",
        path
      )
    )
  }

  readr::read_csv(
    path,
    show_col_types = FALSE
  )
}


parse_optional_integer <- function(x, label) {

  if (
    is.null(x) ||
    !nzchar(trimws(x))
  ) {
    return(NA_integer_)
  }

  value <- suppressWarnings(
    as.integer(trimws(x))
  )

  if (
    is.na(value)
  ) {
    stop(
      paste0(
        label,
        " must be an integer."
      )
    )
  }

  value
}


infer_current_fantasy_season <- function() {

  eastern_year <- as.integer(
    format(
      Sys.time(),
      tz = "America/New_York",
      format = "%Y"
    )
  )

  eastern_month <- as.integer(
    format(
      Sys.time(),
      tz = "America/New_York",
      format = "%m"
    )
  )

  if (
    eastern_month <= 2
  ) {
    return(
      eastern_year - 1L
    )
  }

  eastern_year
}


period_is_after <- function(
    year_a,
    week_a,
    year_b,
    week_b
) {

  year_a > year_b ||
    (
      year_a == year_b &&
      week_a > week_b
    )
}


period_is_immediately_after <- function(
    target_year,
    target_week,
    prior_year,
    prior_week
) {

  same_season_next_week <-
    target_year == prior_year &&
    target_week == prior_week + 1L

  next_season_week_one <-
    target_year == prior_year + 1L &&
    target_week == 1L &&
    prior_week == final_fantasy_week

  same_season_next_week ||
    next_season_week_one
}


range_contains_period <- function(
    start_year,
    start_week,
    end_year,
    end_week,
    target_year,
    target_week
) {

  starts_before_or_on <-
    start_year < target_year ||
    (
      start_year == target_year &&
      start_week <= target_week
    )

  ends_after_or_on <-
    end_year > target_year ||
    (
      end_year == target_year &&
      end_week >= target_week
    )

  starts_before_or_on &&
    ends_after_or_on
}


# ============================================================
# 5. ESPN AUTHENTICATION
# ============================================================

swid <- Sys.getenv("ESPN_SWID", unset = "")
espn_s2 <- Sys.getenv("ESPN_S2", unset = "")

if (
  !nzchar(swid)
) {
  stop(
    "ESPN_SWID was not found in the environment."
  )
}

if (
  !nzchar(espn_s2)
) {
  stop(
    "ESPN_S2 was not found in the environment."
  )
}

espn_cookie <- paste0(
  "SWID=",
  swid,
  "; espn_s2=",
  espn_s2
)


# ============================================================
# 6. ESPN REQUEST HELPERS
# ============================================================

build_espn_request <- function(
    season,
    views,
    scoring_period = NULL,
    use_history = FALSE
) {

  if (
    use_history
  ) {

    url <- paste0(
      "https://lm-api-reads.fantasy.espn.com/",
      "apis/v3/games/ffl/",
      "leagueHistory/",
      league_id
    )

    req <- request(
      url
    ) |>
      req_url_query(
        seasonId = season
      )

  } else {

    url <- paste0(
      "https://lm-api-reads.fantasy.espn.com/",
      "apis/v3/games/ffl/",
      "seasons/",
      season,
      "/segments/0/leagues/",
      league_id
    )

    req <- request(
      url
    )
  }

  req <- req |>
    req_url_query(
      view = views,
      .multi = "explode"
    )

  if (
    !is.null(scoring_period)
  ) {

    req <- req |>
      req_url_query(
        scoringPeriodId = scoring_period
      )
  }

  req |>
    req_headers(
      Cookie = espn_cookie
    )
}


perform_espn_request <- function(req) {

  tryCatch(
    req_perform(req),
    httr2_http_404 = function(e) NULL
  )
}


espn_get <- function(
    season,
    views,
    scoring_period = NULL
) {

  current_req <- build_espn_request(
    season = season,
    views = views,
    scoring_period = scoring_period,
    use_history = FALSE
  )

  current_resp <- perform_espn_request(
    current_req
  )

  if (
    !is.null(current_resp)
  ) {

    resp_check_status(
      current_resp
    )

    return(
      resp_body_json(
        current_resp,
        simplifyVector = FALSE
      )
    )
  }

  history_req <- build_espn_request(
    season = season,
    views = views,
    scoring_period = scoring_period,
    use_history = TRUE
  )

  history_resp <- perform_espn_request(
    history_req
  )

  if (
    is.null(history_resp)
  ) {
    return(NULL)
  }

  resp_check_status(
    history_resp
  )

  body <- resp_body_json(
    history_resp,
    simplifyVector = FALSE
  )

  if (
    length(body) == 0
  ) {
    return(NULL)
  }

  body[[1]]
}


# ============================================================
# 7. ESPN TEAM SNAPSHOT
# ============================================================

build_team_snapshot <- function(data) {

  members <- safe_list(
    data,
    "members"
  )

  member_lookup <- tibble(

    owner_id =
      map_chr(
        members,
        ~ safe_chr(
          .x$id
        )
      ),

    owner_handle =
      map_chr(
        members,
        function(member) {

          display_name <- safe_chr(
            member$displayName
          )

          if (
            !is.na(display_name) &&
            nzchar(display_name)
          ) {
            return(
              display_name
            )
          }

          fallback_name <- str_squish(
            paste(
              safe_chr(
                member$firstName
              ),
              safe_chr(
                member$lastName
              )
            )
          )

          if (
            nzchar(fallback_name)
          ) {
            return(
              fallback_name
            )
          }

          NA_character_
        }
      )
  )

  teams <- safe_list(
    data,
    "teams"
  )

  team_snapshot <- map_dfr(
    teams,
    function(team) {

      team_name <- safe_chr(
        team$name
      )

      if (
        is.na(team_name) ||
        !nzchar(team_name)
      ) {

        team_name <- str_squish(
          paste(
            safe_chr(
              team$location
            ),
            safe_chr(
              team$nickname
            )
          )
        )
      }

      owner_id <- safe_chr(
        team$primaryOwner
      )

      owner_handle <- member_lookup$owner_handle[
        member_lookup$owner_id ==
          owner_id
      ]

      if (
        length(owner_handle) == 0
      ) {
        owner_handle <- NA_character_
      } else {
        owner_handle <- owner_handle[[1]]
      }

      tibble(
        team_id =
          as.integer(
            safe_num(
              team$id
            )
          ),

        team_name =
          as.character(
            team_name
          ),

        owner_id =
          as.character(
            owner_id
          ),

        owner_handle =
          as.character(
            owner_handle
          )
      )
    }
  ) |>
    arrange(
      team_id
    )

  if (
    nrow(team_snapshot) == 0
  ) {
    stop(
      "ESPN returned no fantasy teams."
    )
  }

  if (
    any(
      is.na(
        team_snapshot$team_id
      )
    ) ||
    any(
      is.na(
        team_snapshot$team_name
      )
    ) ||
    any(
      !nzchar(
        team_snapshot$team_name
      )
    ) ||
    any(
      is.na(
        team_snapshot$owner_id
      )
    ) ||
    any(
      !nzchar(
        team_snapshot$owner_id
      )
    )
  ) {
    stop(
      "ESPN returned a team with a missing team ID, team name, or owner ID."
    )
  }

  duplicate_team_names <- team_snapshot |>
    count(
      team_name,
      name = "Rows"
    ) |>
    filter(
      Rows > 1
    )

  if (
    nrow(duplicate_team_names) > 0
  ) {

    print(
      duplicate_team_names,
      n = Inf
    )

    stop(
      paste0(
        "Two current fantasy teams have the same team name. ",
        "The app uses team name as a historical key, so the weekly update was stopped."
      )
    )
  }

  team_snapshot
}


# ============================================================
# 8. ESPN SCHEDULE
# ============================================================

build_schedule_table <- function(data) {

  schedule <- safe_list(
    data,
    "schedule"
  )

  map_dfr(
    schedule,
    function(matchup) {

      home <- safe_list(
        matchup,
        "home"
      )

      away <- safe_list(
        matchup,
        "away"
      )

      if (
        length(home) == 0 ||
        length(away) == 0 ||
        is.null(home$teamId) ||
        is.null(away$teamId)
      ) {
        return(
          tibble()
        )
      }

      tibble(
        week =
          as.integer(
            safe_num(
              matchup$matchupPeriodId
            )
          ),

        matchup_id =
          as.integer(
            safe_num(
              matchup$id
            )
          ),

        playoff_tier =
          safe_chr(
            matchup$playoffTierType
          ),

        winner =
          safe_chr(
            matchup$winner
          ),

        home_team_id =
          as.integer(
            safe_num(
              home$teamId
            )
          ),

        home_points =
          safe_num(
            home$totalPoints
          ),

        away_team_id =
          as.integer(
            safe_num(
              away$teamId
            )
          ),

        away_points =
          safe_num(
            away$totalPoints
          )
      )
    }
  ) |>
    filter(
      !is.na(week),
      week >= 1,
      week <= final_fantasy_week
    )
}


week_is_complete <- function(
    schedule_table,
    week
) {

  week_rows <- schedule_table |>
    filter(
      .data$week == !!week
    )

  if (
    nrow(week_rows) == 0
  ) {
    return(FALSE)
  }

  winner_values <- toupper(
    replace_na(
      week_rows$winner,
      ""
    )
  )

  all(
    nzchar(
      winner_values
    ) &
      winner_values !=
        "UNDECIDED"
  )
}


prepare_target_games <- function(
    schedule_table,
    target_week
) {

  week_rows <- schedule_table |>
    filter(
      week == target_week
    )

  if (
    nrow(week_rows) == 0
  ) {
    stop(
      paste0(
        "ESPN returned no schedule rows for Week ",
        target_week,
        "."
      )
    )
  }

  if (
    target_week <= 14
  ) {

    return(
      week_rows |>
        mutate(
          matchup_type =
            "Regular Season"
        )
    )
  }

  if (
    target_week == 15
  ) {

    kept <- week_rows |>
      filter(
        playoff_tier ==
          "WINNERS_BRACKET"
      ) |>
      mutate(
        matchup_type =
          "Quarterfinals"
      )

    if (
      nrow(kept) == 0
    ) {
      stop(
        "Could not identify the Week 15 winners bracket."
      )
    }

    return(
      kept
    )
  }

  if (
    target_week == 16
  ) {

    kept <- week_rows |>
      filter(
        playoff_tier ==
          "WINNERS_BRACKET"
      ) |>
      mutate(
        matchup_type =
          "Semifinals"
      )

    if (
      nrow(kept) == 0
    ) {
      stop(
        "Could not identify the Week 16 winners bracket."
      )
    }

    return(
      kept
    )
  }

  semifinal_games <- schedule_table |>
    filter(
      week == 16,
      playoff_tier ==
        "WINNERS_BRACKET"
    ) |>
    mutate(

      semifinal_winner_id =
        case_when(

          toupper(
            replace_na(
              winner,
              ""
            )
          ) ==
            "HOME" ~
            home_team_id,

          toupper(
            replace_na(
              winner,
              ""
            )
          ) ==
            "AWAY" ~
            away_team_id,

          home_points >
            away_points ~
            home_team_id,

          away_points >
            home_points ~
            away_team_id,

          TRUE ~
            NA_integer_
        ),

      semifinal_loser_id =
        case_when(

          toupper(
            replace_na(
              winner,
              ""
            )
          ) ==
            "HOME" ~
            away_team_id,

          toupper(
            replace_na(
              winner,
              ""
            )
          ) ==
            "AWAY" ~
            home_team_id,

          home_points >
            away_points ~
            away_team_id,

          away_points >
            home_points ~
            home_team_id,

          TRUE ~
            NA_integer_
        )
    )

  finalist_ids <- unique(
    na.omit(
      semifinal_games$semifinal_winner_id
    )
  )

  third_place_ids <- unique(
    na.omit(
      semifinal_games$semifinal_loser_id
    )
  )

  if (
    length(finalist_ids) != 2 ||
    length(third_place_ids) != 2
  ) {
    stop(
      paste0(
        "Could not identify exactly two finalists and two third place teams ",
        "from the Week 16 semifinals."
      )
    )
  }

  kept <- week_rows |>
    mutate(

      matchup_type =
        case_when(

          home_team_id %in%
            finalist_ids &
            away_team_id %in%
            finalist_ids ~
            "Finals",

          home_team_id %in%
            third_place_ids &
            away_team_id %in%
            third_place_ids ~
            "Third Place",

          TRUE ~
            "Consolation"
        )
    ) |>
    filter(
      matchup_type !=
        "Consolation"
    )

  if (
    nrow(kept) != 2
  ) {

    print(
      kept,
      n = Inf
    )

    stop(
      "Week 17 did not produce exactly a championship game and a third place game."
    )
  }

  kept
}


# ============================================================
# 9. PLAYER STAT HELPERS
# ============================================================

get_stat_entry <- function(
    player,
    week,
    stat_source_id
) {

  stats <- safe_list(
    player,
    "stats"
  )

  if (
    length(stats) == 0
  ) {
    return(NULL)
  }

  for (
    stat in stats
  ) {

    source_id <- safe_num(
      stat$statSourceId
    )

    scoring_period <- safe_num(
      stat$scoringPeriodId
    )

    if (
      !is.na(source_id) &&
      !is.na(scoring_period) &&
      source_id == stat_source_id &&
      scoring_period == week
    ) {
      return(stat)
    }
  }

  NULL
}


get_stat_total <- function(
    player,
    week,
    stat_source_id
) {

  stat_entry <- get_stat_entry(
    player = player,
    week = week,
    stat_source_id = stat_source_id
  )

  if (
    is.null(stat_entry)
  ) {
    return(0)
  }

  value <- safe_num(
    stat_entry$appliedTotal
  )

  if (
    is.na(value)
  ) {
    return(0)
  }

  value
}


get_actual <- function(
    player,
    week
) {

  get_stat_total(
    player = player,
    week = week,
    stat_source_id = 0
  )
}


get_projection <- function(
    player,
    week
) {

  get_stat_total(
    player = player,
    week = week,
    stat_source_id = 1
  )
}


# ============================================================
# 10. BUILD TARGET MATCHUP DATA
# ============================================================

build_target_matchups <- function(
    target_games,
    team_snapshot,
    season,
    target_week
) {

  home_rows <- target_games |>
    transmute(
      team_id =
        home_team_id,

      points_for =
        home_points,

      opp_team_id =
        away_team_id,

      points_against =
        away_points,

      matchup_type
    )

  away_rows <- target_games |>
    transmute(
      team_id =
        away_team_id,

      points_for =
        away_points,

      opp_team_id =
        home_team_id,

      points_against =
        home_points,

      matchup_type
    )

  internal <- bind_rows(
    home_rows,
    away_rows
  )

  if (
    any(
      is.na(
        internal$points_for
      )
    ) ||
    any(
      is.na(
        internal$points_against
      )
    )
  ) {
    stop(
      "At least one completed matchup is missing a team score."
    )
  }

  output <- internal |>
    left_join(
      team_snapshot |>
        select(
          team_id,
          team_name
        ),
      by =
        "team_id"
    ) |>
    left_join(
      team_snapshot |>
        select(
          opp_team_id =
            team_id,
          opposing_team =
            team_name
        ),
      by =
        "opp_team_id"
    ) |>
    mutate(
      Win =
        as.integer(
          points_for >
            points_against
        ),

      Loss =
        as.integer(
          points_for <
            points_against
        ),

      Week =
        as.integer(
          target_week
        ),

      Year =
        as.integer(
          season
        )
    ) |>
    select(
      Team =
        team_name,

      `Points For` =
        points_for,

      `Opposing Team` =
        opposing_team,

      `Points Against` =
        points_against,

      Win,

      Loss,

      Week,

      Year,

      `Matchup Type` =
        matchup_type
    ) |>
    arrange(
      Team
    )

  if (
    any(
      is.na(
        output$Team
      )
    ) ||
    any(
      is.na(
        output$`Opposing Team`
      )
    )
  ) {
    stop(
      "A matchup team ID could not be mapped to a fantasy team name."
    )
  }

  list(
    internal = internal,
    output = output
  )
}


# ============================================================
# 11. BUILD TARGET PLAYER DATA
# ============================================================

build_target_players <- function(
    roster_data,
    team_snapshot,
    eligible_team_ids,
    season,
    target_week
) {

  teams <- safe_list(
    roster_data,
    "teams"
  )

  selected_teams <- keep(
    teams,
    function(team) {

      team_id <- as.integer(
        safe_num(
          team$id
        )
      )

      team_id %in%
        eligible_team_ids
    }
  )

  returned_team_ids <- map_int(
    selected_teams,
    ~ as.integer(
      safe_num(
        .x$id
      )
    )
  )

  missing_roster_team_ids <- setdiff(
    eligible_team_ids,
    returned_team_ids
  )

  extra_roster_team_ids <- setdiff(
    returned_team_ids,
    eligible_team_ids
  )

  if (
    length(missing_roster_team_ids) > 0 ||
    length(extra_roster_team_ids) > 0
  ) {

    cat(
      "\nExpected eligible team IDs:\n"
    )

    print(
      sort(
        eligible_team_ids
      )
    )

    cat(
      "\nReturned eligible roster team IDs:\n"
    )

    print(
      sort(
        returned_team_ids
      )
    )

    stop(
      "The weekly roster response did not contain exactly the expected fantasy teams."
    )
  }

  internal <- map_dfr(
    selected_teams,
    function(team) {

      team_id <- as.integer(
        safe_num(
          team$id
        )
      )

      entries <- safe_list(
        safe_list(
          team,
          "roster"
        ),
        "entries"
      )

      if (
        length(entries) == 0
      ) {
        return(
          tibble()
        )
      }

      map_dfr(
        entries,
        function(entry) {

          player_pool <- safe_list(
            entry,
            "playerPoolEntry"
          )

          player <- safe_list(
            player_pool,
            "player"
          )

          player_name <- safe_chr(
            player$fullName
          )

          if (
            is.na(player_name) ||
            !nzchar(
              str_trim(
                player_name
              )
            ) ||
            player_name ==
              "Player Name"
          ) {
            return(
              tibble()
            )
          }

          player_id <- as.integer(
            safe_num(
              player$id
            )
          )

          actual_position_id <- safe_num(
            player$defaultPositionId
          )

          lineup_slot_id <- safe_num(
            entry$lineupSlotId
          )

          pro_team_id <- safe_num(
            player$proTeamId
          )

          pos <- map_lookup(
            actual_position_id,
            POSITION_MAP
          )

          slot <- map_lookup(
            lineup_slot_id,
            SLOT_MAP
          )

          nfl_team <- map_lookup(
            pro_team_id,
            PRO_TEAM_MAP
          )

          tibble(
            team_id =
              team_id,

            player_id =
              player_id,

            player_name =
              as.character(
                player_name
              ),

            pos =
              pos,

            nfl_team =
              nfl_team,

            slot =
              slot,

            proj =
              get_projection(
                player = player,
                week = target_week
              ),

            fpts =
              get_actual(
                player = player,
                week = target_week
              )
          )
        }
      )
    }
  )

  if (
    nrow(internal) == 0
  ) {
    stop(
      "ESPN returned no player rows for the target week."
    )
  }

  bad_mappings <- internal |>
    filter(
      is.na(pos) |
      is.na(nfl_team) |
      is.na(slot)
    )

  if (
    nrow(bad_mappings) > 0
  ) {

    print(
      bad_mappings,
      n = Inf
    )

    stop(
      paste0(
        "An ESPN position, NFL team, or lineup slot ID was not recognized. ",
        "Update the lookup tables before writing files."
      )
    )
  }

  duplicate_player_ids <- internal |>
    count(
      team_id,
      player_id,
      name = "Rows"
    ) |>
    filter(
      Rows > 1
    )

  if (
    nrow(duplicate_player_ids) > 0
  ) {

    print(
      duplicate_player_ids,
      n = Inf
    )

    stop(
      "The same ESPN player ID appeared more than once on the same fantasy roster."
    )
  }

  output <- internal |>
    left_join(
      team_snapshot |>
        select(
          team_id,
          team_name
        ),
      by =
        "team_id"
    ) |>
    transmute(
      `Player Name` =
        player_name,

      `Fantasy Team` =
        team_name,

      POS =
        pos,

      TEAM =
        nfl_team,

      SLOT =
        slot,

      PROJ =
        round(
          replace_na(
            proj,
            0
          ),
          1
        ),

      FPTS =
        round(
          replace_na(
            fpts,
            0
          ),
          2
        ),

      Week =
        as.integer(
          target_week
        ),

      Year =
        as.integer(
          season
        )
    ) |>
    arrange(
      `Fantasy Team`,
      SLOT,
      `Player Name`
    )

  if (
    any(
      is.na(
        output$`Fantasy Team`
      )
    )
  ) {
    stop(
      "A roster team ID could not be mapped to a fantasy team name."
    )
  }

  list(
    internal = internal,
    output = output
  )
}


# ============================================================
# 12. UPDATE MANAGER DATABASE
# ============================================================

make_placeholder_manager <- function(
    owner_id,
    owner_handle
) {

  handle <- str_squish(
    replace_na(
      as.character(
        owner_handle
      ),
      ""
    )
  )

  if (
    nzchar(handle)
  ) {
    return(
      paste0(
        "REVIEW: ",
        handle
      )
    )
  }

  short_id <- str_replace_all(
    as.character(
      owner_id
    ),
    "[{}]",
    ""
  )

  short_id <- substr(
    short_id,
    1,
    8
  )

  paste0(
    "REVIEW: ",
    short_id
  )
}


update_manager_database <- function(
    manager_data,
    team_snapshot,
    season,
    target_week
) {

  manager_data <- manager_data |>
    mutate(
      Manager =
        as.character(
          Manager
        ),

      `ESPN ID` =
        as.character(
          `ESPN ID`
        ),

      `Team Name` =
        as.character(
          `Team Name`
        ),

      `Start Year` =
        as.integer(
          `Start Year`
        ),

      `Start Week` =
        as.integer(
          `Start Week`
        ),

      `End Year` =
        as.integer(
          `End Year`
        ),

      `End Week` =
        as.integer(
          `End Week`
        )
    )

  expected_columns <- c(
    "Manager",
    "ESPN ID",
    "Team Name",
    "Start Year",
    "Start Week",
    "End Year",
    "End Week"
  )

  if (
    !identical(
      names(
        manager_data
      ),
      expected_columns
    )
  ) {
    stop(
      "fantasy_manager_data.csv has the wrong schema."
    )
  }

  changes <- list()

  for (
    row_index in seq_len(
      nrow(
        team_snapshot
      )
    )
  ) {

    current_team <- team_snapshot[
      row_index,
    ]

    owner_id <- as.character(
      current_team$owner_id
    )

    current_team_name <- as.character(
      current_team$team_name
    )

    owner_handle <- as.character(
      current_team$owner_handle
    )

    owner_rows <- which(
      !is.na(
        manager_data$`ESPN ID`
      ) &
      manager_data$`ESPN ID` ==
        owner_id
    )

    if (
      length(owner_rows) == 0
    ) {

      placeholder_manager <- make_placeholder_manager(
        owner_id = owner_id,
        owner_handle = owner_handle
      )

      new_row <- tibble(
        Manager =
          placeholder_manager,

        `ESPN ID` =
          owner_id,

        `Team Name` =
          current_team_name,

        `Start Year` =
          as.integer(
            season
          ),

        `Start Week` =
          as.integer(
            target_week
          ),

        `End Year` =
          as.integer(
            season
          ),

        `End Week` =
          as.integer(
            target_week
          )
      )

      manager_data <- bind_rows(
        manager_data,
        new_row
      )

      changes[[length(changes) + 1L]] <- tibble(
        Change =
          "NEW MANAGER",

        Manager =
          placeholder_manager,

        `ESPN ID` =
          owner_id,

        `Team Name` =
          current_team_name,

        Year =
          season,

        Week =
          target_week
      )

      next
    }

    owner_manager_names <- unique(
      manager_data$Manager[
        owner_rows
      ]
    )

    owner_manager_names <- owner_manager_names[
      !is.na(
        owner_manager_names
      ) &
      nzchar(
        owner_manager_names
      )
    ]

    if (
      length(owner_manager_names) != 1
    ) {

      print(
        manager_data[
          owner_rows,
        ],
        n = Inf
      )

      stop(
        paste0(
          "ESPN ID ",
          owner_id,
          " does not map to exactly one manager name."
        )
      )
    }

    manager_name <- owner_manager_names[[1]]

    covering_rows <- owner_rows[
      map_lgl(
        owner_rows,
        function(index) {

          range_contains_period(
            start_year =
              manager_data$`Start Year`[[index]],

            start_week =
              manager_data$`Start Week`[[index]],

            end_year =
              manager_data$`End Year`[[index]],

            end_week =
              manager_data$`End Week`[[index]],

            target_year =
              season,

            target_week =
              target_week
          )
        }
      )
    ]

    if (
      length(covering_rows) > 1
    ) {

      print(
        manager_data[
          covering_rows,
        ],
        n = Inf
      )

      stop(
        paste0(
          "Multiple manager database rows cover ESPN ID ",
          owner_id,
          " for ",
          season,
          " Week ",
          target_week,
          "."
        )
      )
    }

    if (
      length(covering_rows) == 1
    ) {

      covering_index <- covering_rows[[1]]

      stored_team_name <- manager_data$`Team Name`[
        covering_index
      ]

      if (
        identical(
          stored_team_name,
          current_team_name
        )
      ) {
        next
      }

      stop(
        paste0(
          "Manager database conflict for ",
          manager_name,
          " in ",
          season,
          " Week ",
          target_week,
          ". The database says '",
          stored_team_name,
          "' but ESPN currently says '",
          current_team_name,
          "'. No files were changed."
        )
      )
    }

    owner_table <- manager_data[
      owner_rows,
    ] |>
      mutate(
        original_index =
          owner_rows
      ) |>
      arrange(
        desc(
          `End Year`
        ),
        desc(
          `End Week`
        )
      )

    latest_row <- owner_table[
      1,
    ]

    latest_end_year <- latest_row$`End Year`[[1]]
    latest_end_week <- latest_row$`End Week`[[1]]
    latest_team_name <- latest_row$`Team Name`[[1]]
    latest_index <- latest_row$original_index[[1]]

    if (
      !period_is_after(
        year_a = season,
        week_a = target_week,
        year_b = latest_end_year,
        week_b = latest_end_week
      )
    ) {

      stop(
        paste0(
          "The target period occurs before the latest manager record for ESPN ID ",
          owner_id,
          ". No files were changed."
        )
      )
    }

    if (
      period_is_immediately_after(
        target_year = season,
        target_week = target_week,
        prior_year = latest_end_year,
        prior_week = latest_end_week
      ) &&
      identical(
        latest_team_name,
        current_team_name
      )
    ) {

      manager_data$`End Year`[
        latest_index
      ] <- as.integer(
        season
      )

      manager_data$`End Week`[
        latest_index
      ] <- as.integer(
        target_week
      )

      changes[[length(changes) + 1L]] <- tibble(
        Change =
          "EXTENDED RANGE",

        Manager =
          manager_name,

        `ESPN ID` =
          owner_id,

        `Team Name` =
          current_team_name,

        Year =
          season,

        Week =
          target_week
      )

      next
    }

    new_row <- tibble(
      Manager =
        manager_name,

      `ESPN ID` =
        owner_id,

      `Team Name` =
        current_team_name,

      `Start Year` =
        as.integer(
          season
        ),

      `Start Week` =
        as.integer(
          target_week
        ),

      `End Year` =
        as.integer(
          season
        ),

      `End Week` =
        as.integer(
          target_week
        )
    )

    manager_data <- bind_rows(
      manager_data,
      new_row
    )

    changes[[length(changes) + 1L]] <- tibble(
      Change =
        if_else(
          latest_team_name ==
            current_team_name,
          "NEW RANGE AFTER GAP",
          "TEAM NAME CHANGE"
        ),

      Manager =
        manager_name,

      `ESPN ID` =
        owner_id,

      `Team Name` =
        current_team_name,

      Year =
        season,

      Week =
        target_week
    )
  }

  manager_data <- manager_data |>
    arrange(
      `Start Year`,
      `Start Week`,
      Manager,
      `Team Name`
    )

  change_table <- bind_rows(
    changes
  )

  list(
    data = manager_data,
    changes = change_table
  )
}


# ============================================================
# 13. MANAGER DATABASE VALIDATION
# ============================================================

validate_manager_database <- function(
    manager_data
) {

  bad_ranges <- manager_data |>
    filter(
      is.na(
        Manager
      ) |
      !nzchar(
        Manager
      ) |
      is.na(
        `Team Name`
      ) |
      !nzchar(
        `Team Name`
      ) |
      is.na(
        `Start Year`
      ) |
      is.na(
        `Start Week`
      ) |
      is.na(
        `End Year`
      ) |
      is.na(
        `End Week`
      ) |
      `Start Week` < 1 |
      `Start Week` >
        final_fantasy_week |
      `End Week` < 1 |
      `End Week` >
        final_fantasy_week |
      `Start Year` >
        `End Year` |
      (
        `Start Year` ==
          `End Year` &
        `Start Week` >
          `End Week`
      )
    )

  if (
    nrow(bad_ranges) > 0
  ) {

    print(
      bad_ranges,
      n = Inf
    )

    stop(
      "fantasy_manager_data.csv contains an invalid range."
    )
  }

  known_ids <- manager_data |>
    filter(
      !is.na(
        `ESPN ID`
      ),
      nzchar(
        `ESPN ID`
      )
    ) |>
    distinct(
      `ESPN ID`,
      Manager
    ) |>
    count(
      `ESPN ID`,
      name = "Manager_Count"
    ) |>
    filter(
      Manager_Count >
        1
    )

  if (
    nrow(known_ids) > 0
  ) {

    print(
      known_ids,
      n = Inf
    )

    stop(
      "At least one ESPN ID maps to more than one manager name."
    )
  }

  expanded <- map_dfr(
    seq_len(
      nrow(
        manager_data
      )
    ),
    function(index) {

      row <- manager_data[
        index,
      ]

      years <- seq.int(
        row$`Start Year`[[1]],
        row$`End Year`[[1]]
      )

      crossing(
        Year =
          years,
        Week =
          seq_len(
            final_fantasy_week
          )
      ) |>
        filter(
          map2_lgl(
            Year,
            Week,
            ~ range_contains_period(
              start_year =
                row$`Start Year`[[1]],

              start_week =
                row$`Start Week`[[1]],

              end_year =
                row$`End Year`[[1]],

              end_week =
                row$`End Week`[[1]],

              target_year =
                .x,

              target_week =
                .y
            )
          )
        ) |>
        mutate(
          Manager =
            row$Manager[[1]],

          `ESPN ID` =
            row$`ESPN ID`[[1]],

          `Team Name` =
            row$`Team Name`[[1]]
        )
    }
  )

  overlapping_ids <- expanded |>
    filter(
      !is.na(
        `ESPN ID`
      ),
      nzchar(
        `ESPN ID`
      )
    ) |>
    count(
      `ESPN ID`,
      Year,
      Week,
      name = "Rows"
    ) |>
    filter(
      Rows >
        1
    )

  if (
    nrow(overlapping_ids) > 0
  ) {

    print(
      overlapping_ids,
      n = Inf
    )

    stop(
      "At least one ESPN ID has overlapping manager ranges."
    )
  }

  overlapping_team_names <- expanded |>
    group_by(
      `Team Name`,
      Year,
      Week
    ) |>
    summarise(
      Manager_Count =
        n_distinct(
          Manager
        ),
      .groups =
        "drop"
    ) |>
    filter(
      Manager_Count >
        1
    )

  if (
    nrow(overlapping_team_names) > 0
  ) {

    print(
      overlapping_team_names,
      n = Inf
    )

    stop(
      paste0(
        "The same fantasy team name maps to more than one manager ",
        "in the same year and week."
      )
    )
  }

  invisible(TRUE)
}


# ============================================================
# 14. SCORE RECONCILIATION
#
# This deliberately does NOT require a fixed number of starters.
#
# A manager may leave one or more starting slots empty.
# The rule is only:
#
# ESPN official team score
# =
# sum of every player ESPN marks in an active lineup slot
#
# Bench and IR are excluded.
# ============================================================

validate_week_scores <- function(
    matchup_internal,
    player_internal,
    target_games
) {

  eligible_team_ids <- sort(
    unique(
      c(
        target_games$home_team_id,
        target_games$away_team_id
      )
    )
  )

  roster_team_ids <- sort(
    unique(
      player_internal$team_id
    )
  )

  if (
    !identical(
      eligible_team_ids,
      roster_team_ids
    )
  ) {

    cat(
      "\nEligible matchup team IDs:\n"
    )

    print(
      eligible_team_ids
    )

    cat(
      "\nRoster team IDs:\n"
    )

    print(
      roster_team_ids
    )

    stop(
      "The player pull did not return every eligible matchup team."
    )
  }

  starter_totals <- player_internal |>
    filter(
      !slot %in%
        c(
          "Bench",
          "IR"
        )
    ) |>
    group_by(
      team_id
    ) |>
    summarise(
      starter_fpts =
        sum(
          fpts,
          na.rm = TRUE
        ),
      .groups =
        "drop"
    )

  score_check <- matchup_internal |>
    select(
      team_id,
      points_for
    ) |>
    left_join(
      starter_totals,
      by =
        "team_id"
    ) |>
    mutate(
      starter_fpts =
        replace_na(
          starter_fpts,
          0
        ),

      difference =
        round(
          starter_fpts -
            points_for,
          6
        )
    )

  score_failures <- score_check |>
    filter(
      abs(
        difference
      ) >
        0.01
    )

  if (
    nrow(score_failures) > 0
  ) {

    print(
      score_failures,
      n = Inf
    )

    stop(
      paste0(
        "Active player FPTS do not equal ESPN matchup Points For. ",
        "No files were changed."
      )
    )
  }

  invisible(
    score_check
  )
}


# ============================================================
# 15. FINAL CSV VALIDATION
# ============================================================

validate_final_data <- function(
    matchup_data,
    player_data,
    manager_data
) {

  expected_matchup_columns <- c(
    "Team",
    "Points For",
    "Opposing Team",
    "Points Against",
    "Win",
    "Loss",
    "Week",
    "Year",
    "Matchup Type"
  )

  expected_player_columns <- c(
    "Player Name",
    "Fantasy Team",
    "POS",
    "TEAM",
    "SLOT",
    "PROJ",
    "FPTS",
    "Week",
    "Year"
  )

  expected_manager_columns <- c(
    "Manager",
    "ESPN ID",
    "Team Name",
    "Start Year",
    "Start Week",
    "End Year",
    "End Week"
  )

  if (
    !identical(
      names(
        matchup_data
      ),
      expected_matchup_columns
    )
  ) {
    stop(
      "The final matchup data has the wrong schema."
    )
  }

  if (
    !identical(
      names(
        player_data
      ),
      expected_player_columns
    )
  ) {
    stop(
      "The final player data has the wrong schema."
    )
  }

  if (
    !identical(
      names(
        manager_data
      ),
      expected_manager_columns
    )
  ) {
    stop(
      "The final manager data has the wrong schema."
    )
  }

  duplicate_matchups <- matchup_data |>
    count(
      Year,
      Week,
      Team,
      name = "Rows"
    ) |>
    filter(
      Rows !=
        1
    )

  if (
    nrow(duplicate_matchups) > 0
  ) {

    print(
      duplicate_matchups,
      n = Inf
    )

    stop(
      "Duplicate directional matchup keys found."
    )
  }

  duplicate_players <- player_data |>
    count(
      Year,
      Week,
      `Fantasy Team`,
      `Player Name`,
      name = "Rows"
    ) |>
    filter(
      Rows !=
        1
    )

  if (
    nrow(duplicate_players) > 0
  ) {

    print(
      duplicate_players,
      n = Inf
    )

    stop(
      "Duplicate player, fantasy team, and week keys found."
    )
  }

  # ----------------------------------------------------------
  # PLAYER COMPLETENESS VALIDATION
  #
  # 2021 is preserved legacy data. ESPN no longer exposes that
  # season for this league, and the manually saved 2021 rows
  # intentionally contain missing historical TEAM / PROJ values.
  # There is also at least one preserved "Empty" lineup row with
  # no POS.
  #
  # Therefore:
  #
  #   Every season must have the core fields needed by the app.
  #
  #   2022 onward must additionally have complete POS, TEAM,
  #   SLOT, PROJ, and FPTS values because those seasons were
  #   rebuilt / scraped from ESPN.
  # ----------------------------------------------------------

  bad_core_player_rows <- player_data |>
    filter(
      is.na(
        `Player Name`
      ) |
      !nzchar(
        `Player Name`
      ) |
      is.na(
        `Fantasy Team`
      ) |
      !nzchar(
        `Fantasy Team`
      ) |
      is.na(
        SLOT
      ) |
      is.na(
        FPTS
      ) |
      is.na(
        Week
      ) |
      is.na(
        Year
      )
    )

  if (
    nrow(bad_core_player_rows) > 0
  ) {

    print(
      bad_core_player_rows,
      n = Inf
    )

    stop(
      "The final player data contains missing core required values."
    )
  }

  bad_scraped_player_rows <- player_data |>
    filter(
      Year >= 2022,
      is.na(
        POS
      ) |
      is.na(
        TEAM
      ) |
      is.na(
        SLOT
      ) |
      is.na(
        PROJ
      ) |
      is.na(
        FPTS
      )
    )

  if (
    nrow(bad_scraped_player_rows) > 0
  ) {

    print(
      bad_scraped_player_rows,
      n = Inf
    )

    stop(
      paste0(
        "The 2022-or-later player data contains missing scraped values. ",
        "No files were changed."
      )
    )
  }

  if (
    any(
      matchup_data$`Matchup Type` ==
        "Consolation"
    )
  ) {
    stop(
      "Consolation games leaked into the final matchup data."
    )
  }

  validate_manager_database(
    manager_data
  )

  invisible(TRUE)
}


# ============================================================
# 16. COMPARE TARGET WEEK WITH EXISTING CSV
# ============================================================

count_row_differences <- function(
    old_data,
    new_data
) {

  all_columns <- names(
    new_data
  )

  old_only <- anti_join(
    old_data,
    new_data,
    by =
      all_columns
  )

  new_only <- anti_join(
    new_data,
    old_data,
    by =
      all_columns
  )

  nrow(old_only) +
    nrow(new_only)
}


# ============================================================
# 17. ATOMIC FILE WRITE
# ============================================================

write_all_files_safely <- function(
    matchup_data,
    player_data,
    manager_data
) {

  temp_matchup <- tempfile(
    fileext = ".csv"
  )

  temp_player <- tempfile(
    fileext = ".csv"
  )

  temp_manager <- tempfile(
    fileext = ".csv"
  )

  backup_matchup <- tempfile(
    fileext = ".csv"
  )

  backup_player <- tempfile(
    fileext = ".csv"
  )

  backup_manager <- tempfile(
    fileext = ".csv"
  )

  write_csv(
    matchup_data,
    temp_matchup
  )

  write_csv(
    player_data,
    temp_player
  )

  write_csv(
    manager_data,
    temp_manager,
    na = ""
  )

  backup_ok <- c(
    file.copy(
      matchup_file,
      backup_matchup,
      overwrite = TRUE
    ),

    file.copy(
      player_file,
      backup_player,
      overwrite = TRUE
    ),

    file.copy(
      manager_file,
      backup_manager,
      overwrite = TRUE
    )
  )

  if (
    !all(
      backup_ok
    )
  ) {
    stop(
      "Could not create temporary backups. No files were changed."
    )
  }

  replace_ok <- c(
    file.copy(
      temp_matchup,
      matchup_file,
      overwrite = TRUE
    ),

    file.copy(
      temp_player,
      player_file,
      overwrite = TRUE
    ),

    file.copy(
      temp_manager,
      manager_file,
      overwrite = TRUE
    )
  )

  if (
    !all(
      replace_ok
    )
  ) {

    file.copy(
      backup_matchup,
      matchup_file,
      overwrite = TRUE
    )

    file.copy(
      backup_player,
      player_file,
      overwrite = TRUE
    )

    file.copy(
      backup_manager,
      manager_file,
      overwrite = TRUE
    )

    stop(
      paste0(
        "A file write failed. The original CSV files were restored."
      )
    )
  }

  invisible(TRUE)
}


# ============================================================
# 18. MAIN
# ============================================================

main <- function() {

  cat(
    "\n============================================\n"
  )

  cat(
    "ESPN WEEKLY FANTASY UPDATE\n"
  )

  cat(
    "============================================\n\n"
  )

  existing_matchups <- read_required_csv(
    matchup_file
  )

  existing_players <- read_required_csv(
    player_file
  )

  existing_managers <- read_required_csv(
    manager_file
  )

  season_override <- parse_optional_integer(
    target_year_env,
    "TARGET_YEAR"
  )

  week_override <- parse_optional_integer(
    target_week_env,
    "TARGET_WEEK"
  )

  if (
    is.na(
      season_override
    )
  ) {

    season <- infer_current_fantasy_season()

  } else {

    season <- season_override
  }

  if (
    !is.na(
      week_override
    ) &&
    (
      week_override < 1 ||
      week_override >
        final_fantasy_week
    )
  ) {
    stop(
      paste0(
        "TARGET_WEEK must be between 1 and ",
        final_fantasy_week,
        "."
      )
    )
  }

  existing_season_weeks <- existing_matchups |>
    filter(
      Year ==
        season
    ) |>
    pull(
      Week
    ) |>
    as.integer() |>
    unique() |>
    sort()

  if (
    is.na(
      week_override
    )
  ) {

    if (
      length(
        existing_season_weeks
      ) == 0
    ) {

      target_week <- 1L

    } else {

      target_week <- max(
        existing_season_weeks,
        na.rm = TRUE
      ) + 1L
    }

  } else {

    target_week <- week_override
  }

  if (
    target_week >
      final_fantasy_week
  ) {

    cat(
      "Season ",
      season,
      " is already complete through Week ",
      final_fantasy_week,
      ".\n"
    )

    cat(
      "No files were changed.\n"
    )

    return(
      invisible(
        NULL
      )
    )
  }

  cat(
    "Season: ",
    season,
    "\n",
    sep = ""
  )

  cat(
    "Target week: ",
    target_week,
    "\n",
    sep = ""
  )

  cat(
    "Dry run: ",
    dry_run,
    "\n",
    sep = ""
  )

  cat(
    "Data directory: ",
    data_dir,
    "\n\n",
    sep = ""
  )

  season_data <- espn_get(
    season = season,
    views = c(
      "mTeam",
      "mMatchupScore",
      "mSettings"
    )
  )

  if (
    is.null(
      season_data
    )
  ) {

    cat(
      "ESPN has no league data available for season ",
      season,
      ".\n",
      sep = ""
    )

    cat(
      "No files were changed.\n"
    )

    return(
      invisible(
        NULL
      )
    )
  }

  team_snapshot <- build_team_snapshot(
    season_data
  )

  schedule_table <- build_schedule_table(
    season_data
  )

  if (
    !week_is_complete(
      schedule_table = schedule_table,
      week = target_week
    )
  ) {

    cat(
      "Week ",
      target_week,
      " is not complete in ESPN yet.\n",
      sep = ""
    )

    cat(
      "No files were changed.\n"
    )

    return(
      invisible(
        NULL
      )
    )
  }

  cat(
    "PASS: ESPN marks Week ",
    target_week,
    " as complete.\n",
    sep = ""
  )

  target_games <- prepare_target_games(
    schedule_table = schedule_table,
    target_week = target_week
  )

  eligible_team_ids <- sort(
    unique(
      c(
        target_games$home_team_id,
        target_games$away_team_id
      )
    )
  )

  matchup_result <- build_target_matchups(
    target_games = target_games,
    team_snapshot = team_snapshot,
    season = season,
    target_week = target_week
  )

  roster_data <- espn_get(
    season = season,
    views =
      "mRoster",
    scoring_period =
      target_week
  )

  if (
    is.null(
      roster_data
    )
  ) {
    stop(
      "ESPN returned no roster data for the target week."
    )
  }

  player_result <- build_target_players(
    roster_data = roster_data,
    team_snapshot = team_snapshot,
    eligible_team_ids = eligible_team_ids,
    season = season,
    target_week = target_week
  )

  validate_week_scores(
    matchup_internal = matchup_result$internal,
    player_internal = player_result$internal,
    target_games = target_games
  )

  cat(
    "PASS: active player FPTS equal ESPN matchup totals.\n"
  )

  manager_result <- update_manager_database(
    manager_data = existing_managers,
    team_snapshot = team_snapshot,
    season = season,
    target_week = target_week
  )

  updated_managers <- manager_result$data

  validate_manager_database(
    updated_managers
  )

  cat(
    "PASS: manager database is valid.\n"
  )

  old_target_matchups <- existing_matchups |>
    filter(
      Year ==
        season,
      Week ==
        target_week
    )

  old_target_players <- existing_players |>
    filter(
      Year ==
        season,
      Week ==
        target_week
    )

  matchup_difference_count <- count_row_differences(
    old_data = old_target_matchups,
    new_data = matchup_result$output
  )

  player_difference_count <- count_row_differences(
    old_data = old_target_players,
    new_data = player_result$output
  )

  updated_matchups <- existing_matchups |>
    filter(
      !(
        Year ==
          season &
        Week ==
          target_week
      )
    ) |>
    bind_rows(
      matchup_result$output
    ) |>
    arrange(
      Year,
      Week,
      Team
    )

  updated_players <- existing_players |>
    filter(
      !(
        Year ==
          season &
        Week ==
          target_week
      )
    ) |>
    bind_rows(
      player_result$output
    ) |>
    arrange(
      Year,
      Week,
      `Fantasy Team`,
      SLOT,
      `Player Name`
    )

  validate_final_data(
    matchup_data = updated_matchups,
    player_data = updated_players,
    manager_data = updated_managers
  )

  cat(
    "PASS: final combined CSV data passed validation.\n\n"
  )

  cat(
    "Target matchup rows: ",
    nrow(
      matchup_result$output
    ),
    "\n",
    sep = ""
  )

  cat(
    "Target player rows: ",
    nrow(
      player_result$output
    ),
    "\n",
    sep = ""
  )

  cat(
    "Matchup row differences versus existing target week: ",
    matchup_difference_count,
    "\n",
    sep = ""
  )

  cat(
    "Player row differences versus existing target week: ",
    player_difference_count,
    "\n",
    sep = ""
  )

  if (
    nrow(
      manager_result$changes
    ) > 0
  ) {

    cat(
      "\nManager database changes:\n"
    )

    print(
      manager_result$changes,
      n = Inf
    )

  } else {

    cat(
      "\nManager database changes: none\n"
    )
  }

  if (
    dry_run
  ) {

    cat(
      "\n============================================\n"
    )

    cat(
      "DRY RUN COMPLETE\n"
    )

    cat(
      "All checks passed. No files were changed.\n"
    )

    cat(
      "============================================\n"
    )

    return(
      invisible(
        list(
          matchup_data =
            matchup_result$output,

          player_data =
            player_result$output,

          manager_data =
            updated_managers
        )
      )
    )
  }

  write_all_files_safely(
    matchup_data = updated_matchups,
    player_data = updated_players,
    manager_data = updated_managers
  )

  cat(
    "\n============================================\n"
  )

  cat(
    "WEEKLY UPDATE COMPLETE\n"
  )

  cat(
    "Updated ",
    season,
    " Week ",
    target_week,
    ".\n",
    sep = ""
  )

  cat(
    "All three CSV files were written successfully.\n"
  )

  cat(
    "============================================\n"
  )

  invisible(
    list(
      matchup_data =
        matchup_result$output,

      player_data =
        player_result$output,

      manager_data =
        updated_managers
    )
  )
}


weekly_update_result <- main()
