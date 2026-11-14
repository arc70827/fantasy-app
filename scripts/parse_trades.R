# ============================================================
# ESPN TRADE PARSER
#
# Turns the raw league communication JSON (written by
# scripts/probe_espn_trades.R) into one row per player movement.
#
# Message types, confirmed against the Recent Activity page:
#   178  add                 gaining team = to
#   179  drop                losing team  = to
#   180  add from waivers    gaining team = to   (from = 0, the waiver pool)
#   181  drop from waiver add losing team = to
#   244  player traded       gaining team = to, losing team = from
#   245  drop forced by trade losing team = from
#
# Nothing here writes to data/. Output goes where the caller says.
#
# Example:
#   source("scripts/parse_trades.R")
#   moves <- parse_espn_moves("probe_output/espn_communication_2026.json", 2026)
#   trades <- summarize_trades(moves)
# ============================================================

library(jsonlite)
library(dplyr)

move_labels <- c(
  "178" = "add",
  "179" = "drop",
  "180" = "waiver_add",
  "181" = "waiver_drop",
  "244" = "trade_move",
  "245" = "trade_drop"
)

# Reads every topic and message, keeping only transaction types.
parse_espn_moves <- function(json_path, season) {
  body <- fromJSON(json_path, simplifyVector = FALSE)

  rows <- list()

  for (topic in body$topics) {
    for (msg in topic$messages) {
      type_code <- as.character(msg$messageTypeId)

      if (!type_code %in% names(move_labels)) {
        next
      }

      from_team <- if (is.null(msg$from)) NA_integer_ else as.integer(msg$from)
      to_team <- if (is.null(msg$to)) NA_integer_ else as.integer(msg$to)

      # Which team gains and which loses depends on the message type.
      gaining <- switch(
        type_code,
        "178" = to_team,
        "180" = to_team,
        "244" = to_team,
        NA_integer_
      )
      losing <- switch(
        type_code,
        "179" = to_team,
        "181" = to_team,
        "244" = from_team,
        "245" = from_team,
        NA_integer_
      )

      rows[[length(rows) + 1]] <- data.frame(
        season = season,
        topic_id = topic$id,
        message_id = msg$id,
        timestamp = as.POSIXct(
          msg$date / 1000,
          origin = "1970-01-01",
          tz = "America/New_York"
        ),
        message_type = type_code,
        move = unname(move_labels[type_code]),
        gaining_team = gaining,
        losing_team = losing,
        player_id = as.integer(msg$targetId),
        author = if (is.null(msg$author)) NA_character_ else msg$author,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame())
  }

  bind_rows(rows) |>
    arrange(timestamp)
}

# One row per trade (a topic that contains type 244 messages).
summarize_trades <- function(moves) {
  trade_moves <- moves |>
    filter(message_type == "244")

  trade_moves |>
    group_by(season, topic_id) |>
    summarise(
      trade_time = min(timestamp),
      teams = paste(
        sort(unique(c(gaining_team, losing_team))),
        collapse = " <-> "
      ),
      players_moved = n(),
      .groups = "drop"
    ) |>
    arrange(trade_time)
}
