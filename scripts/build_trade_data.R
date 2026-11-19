# ============================================================
# BUILD TRADE DATA
#
# Turns a season's raw ESPN communication probe output into
# data/fantasy_trade_data.csv, with manager and player names.
#
# Inputs:
#   probe_output/espn_communication_<season>.json  (from probe_espn_trades.R)
#   data/fantasy_manager_data.csv                  (ESPN ID -> manager)
#
# Player names come from ESPN's public player list, which needs no login.
#
# Run from the project folder:
#   Rscript scripts/build_trade_data.R
# ============================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(readr)
})

source("scripts/parse_trades.R")

season <- as.integer(Sys.getenv("TRADE_SEASON", unset = "2026"))
# Paths can be overridden for the automated update, which writes to a
# temporary file first.
probe_path <- Sys.getenv(
  "TRADE_JSON",
  unset = file.path("probe_output", sprintf("espn_communication_%d.json", season))
)
manager_path <- file.path("data", "fantasy_manager_data.csv")
out_path <- Sys.getenv("TRADE_OUT", unset = file.path("data", "fantasy_trade_data.csv"))

if (!file.exists(probe_path)) {
  stop("Missing ", probe_path, ". Run scripts/probe_espn_trades.R first.", call. = FALSE)
}

# ---- Moves from the raw feed ----

moves <- parse_espn_moves(probe_path, season)

# ---- Team number -> owner, from owner-initiated adds and drops ----
# Each team's own add and drop messages carry that owner's ESPN ID as author.

owner_by_team <- moves |>
  filter(move %in% c("add", "drop")) |>
  mutate(team = ifelse(move == "add", gaining_team, losing_team)) |>
  filter(!is.na(team)) |>
  distinct(team, author)

conflicts <- owner_by_team |>
  count(team) |>
  filter(n > 1)

if (nrow(conflicts) > 0) {
  stop("Some team numbers map to more than one owner: ",
       paste(conflicts$team, collapse = ", "), call. = FALSE)
}

# ---- Owner -> manager name ----

managers <- read_csv(manager_path, show_col_types = FALSE) |>
  transmute(espn_id = `ESPN ID`, manager = Manager) |>
  filter(!is.na(espn_id), nzchar(espn_id)) |>
  distinct(espn_id, .keep_all = TRUE)

owner_names <- owner_by_team |>
  left_join(managers, by = c("author" = "espn_id")) |>
  select(team, manager)

team_name <- function(team) {
  if_else(is.na(team), NA_character_,
          if_else(team == 0L, "Waivers",
                  owner_names$manager[match(team, owner_names$team)]))
}

# ---- Player number -> name, from ESPN's public player list ----

player_ids <- unique(moves$player_id)

fetch_player_names <- function() {
  filter_header <- toJSON(
    list(players = list(limit = 20000)),
    auto_unbox = TRUE
  )

  resp <- request(
    sprintf(
      "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/%d/players",
      season
    )
  ) |>
    req_url_query(scoringPeriodId = 0, view = "players_wl") |>
    req_headers(`x-fantasy-filter` = as.character(filter_header)) |>
    req_perform()

  players <- resp_body_json(resp, simplifyVector = TRUE)
  tibble(player_id = as.integer(players$id), player_name = players$fullName)
}

names_table <- fetch_player_names()

missing_names <- setdiff(player_ids, names_table$player_id)
if (length(missing_names) > 0) {
  warning("No name found for ", length(missing_names), " player IDs, for example ",
          paste(head(missing_names, 5), collapse = ", "))
}

# ---- Final table: one row per player movement ----

trade_rows <- moves |>
  filter(move %in% c("trade_move", "trade_drop")) |>
  left_join(names_table, by = "player_id") |>
  mutate(
    trade_id = topic_id,
    trade_date = as.Date(timestamp),
    from_team = losing_team,
    to_team = gaining_team,
    from_manager = team_name(losing_team),
    to_manager = team_name(gaining_team),
    move = recode(move, trade_move = "traded", trade_drop = "dropped_after_trade")
  ) |>
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S", tz = "America/New_York")) |>
  select(season, trade_id, trade_date, timestamp, move,
         player_id, player_name, from_team, from_manager, to_team, to_manager) |>
  arrange(timestamp, trade_id, player_name)

write_csv(trade_rows, out_path)

message("Wrote ", nrow(trade_rows), " trade rows to ", out_path)
