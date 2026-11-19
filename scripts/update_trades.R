# ============================================================
# UPDATE TRADE DATA
#
# Fetches the season's full trade feed from ESPN and rebuilds
# data/fantasy_trade_data.csv.
#
# Safety:
#   - Nothing is written if the fetch fails or returns no trades.
#   - The new file must not have fewer rows than the current one,
#     so a partial fetch can't erase trade history.
#   - DRY_RUN=true validates everything but leaves the CSV unchanged.
#
# Environment:
#   ESPN_SWID, ESPN_S2   ESPN login cookies (repository secrets in CI)
#   TRADE_SEASON         season to update, for example 2026
#   DRY_RUN              "true" to validate only
# ============================================================

source("scripts/espn_trade_fetch.R")

season <- as.integer(Sys.getenv("TRADE_SEASON", unset = "2026"))
swid <- Sys.getenv("ESPN_SWID", unset = "")
espn_s2 <- Sys.getenv("ESPN_S2", unset = "")
dry_run <- tolower(trimws(Sys.getenv("DRY_RUN", unset = "true"))) %in%
  c("true", "t", "1", "yes", "y")

if (!nzchar(swid) || !nzchar(espn_s2)) {
  stop("ESPN_SWID and ESPN_S2 must be set.", call. = FALSE)
}

production_csv <- file.path("data", "fantasy_trade_data.csv")
raw_json <- tempfile(fileext = ".json")
candidate_csv <- tempfile(fileext = ".csv")

message("Fetching trade feed for ", season, "...")
body <- fetch_trade_pages(season, swid, espn_s2)

if (length(body$topics) == 0) {
  stop("ESPN returned no trade topics for ", season, ". Nothing was changed.", call. = FALSE)
}

writeLines(toJSON(body, auto_unbox = TRUE), raw_json)

# Build the candidate CSV from the raw feed.
Sys.setenv(
  TRADE_SEASON = season,
  TRADE_JSON = raw_json,
  TRADE_OUT = candidate_csv
)
source("scripts/build_trade_data.R", local = new.env())

new_rows <- nrow(read.csv(candidate_csv))
old_rows <- if (file.exists(production_csv)) nrow(read.csv(production_csv)) else 0

message("Trade rows: current file ", old_rows, ", new feed ", new_rows)

if (new_rows == 0) {
  stop("The rebuilt trade table is empty. Nothing was changed.", call. = FALSE)
}

if (new_rows < old_rows) {
  stop(
    "The new trade table has fewer rows (", new_rows, ") than the current file (",
    old_rows, "). Nothing was changed.",
    call. = FALSE
  )
}

if (dry_run) {
  message("DRY_RUN is true. Validation passed; the CSV was not changed.")
} else {
  file.copy(candidate_csv, production_csv, overwrite = TRUE)
  message("Updated ", production_csv, " with ", new_rows, " rows.")
}
