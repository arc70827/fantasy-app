# ============================================================
# ESPN TRADE PROBE
#
# Purpose:
#   Read-only check of ESPN's league communication feed for one
#   season, so the trade message format can be confirmed before
#   any production code depends on it.
#
# Writes nothing to data/. Raw responses go to probe_output/
# (gitignored).
#
# Run locally (PowerShell), with your own cookies:
#
#   $env:ESPN_SWID = "{your-swid-including-braces}"
#   $env:ESPN_S2   = "your-espn_s2-value"
#   $env:PROBE_YEAR = "2025"
#   Rscript scripts/probe_espn_trades.R
#
# Never paste the cookie values into a chat or commit them.
# ============================================================

library(httr2)
library(jsonlite)

league_id <- "254169010"

season <- as.integer(Sys.getenv("PROBE_YEAR", unset = "2025"))
swid <- Sys.getenv("ESPN_SWID", unset = "")
espn_s2 <- Sys.getenv("ESPN_S2", unset = "")

if (!nzchar(swid) || !nzchar(espn_s2)) {
  stop("Set ESPN_SWID and ESPN_S2 in the environment first.", call. = FALSE)
}

out_dir <- "probe_output"
dir.create(out_dir, showWarnings = FALSE)

# Filter taken from the ffscrapr package's ESPN transactions code.
# Unverified for this league; the probe exists to confirm it.
espn_filter <- toJSON(
  list(
    filterType = list(value = "ACTIVITY_TRANSACTIONS"),
    limit = 1000,
    limitPerMessageSet = list(value = 1000),
    filterIncludeMessageTypeIds = list(
      value = c(178, 179, 180, 181, 239, 244)
    ),
    sortMessageDate = list(sortPriority = 1, sortAsc = FALSE)
  ),
  auto_unbox = TRUE
)

url <- sprintf(
  "https://fantasy.espn.com/apis/v3/games/ffl/seasons/%d/segments/0/leagues/%s/communication/",
  season,
  league_id
)

resp <- request(url) |>
  req_url_query(view = "kona_league_communication") |>
  req_headers(
    Cookie = paste0("SWID=", swid, "; espn_s2=", espn_s2),
    `x-fantasy-filter` = as.character(espn_filter)
  ) |>
  req_error(is_error = function(resp) FALSE) |>
  req_perform()

cat("HTTP status:", resp_status(resp), "\n")

if (resp_status(resp) != 200) {
  cat("Response body (first 500 chars):\n")
  cat(substr(resp_body_string(resp), 1, 500), "\n")
  stop("ESPN did not return data. Check the cookies and season.", call. = FALSE)
}

raw_text <- resp_body_string(resp)
raw_path <- file.path(out_dir, sprintf("espn_communication_%d.json", season))
writeLines(raw_text, raw_path)
cat("Raw response saved to:", raw_path, "\n\n")

body <- fromJSON(raw_text, simplifyVector = FALSE)

cat("Top-level keys:", paste(names(body), collapse = ", "), "\n")

topics <- body$topics
if (is.null(topics)) {
  stop("No 'topics' field in response. See the raw file.", call. = FALSE)
}
cat("Topics returned:", length(topics), "\n\n")

# Flatten every message so message types can be counted.
messages <- list()
for (topic in topics) {
  for (msg in topic$messages) {
    messages[[length(messages) + 1]] <- list(
      topic_date = topic$date,
      msg = msg
    )
  }
}
cat("Messages returned:", length(messages), "\n\n")

type_ids <- vapply(
  messages,
  function(m) {
    tid <- m$msg$messageTypeId
    if (is.null(tid)) NA_character_ else as.character(tid)
  },
  character(1)
)

cat("Message type counts:\n")
print(sort(table(type_ids), decreasing = TRUE))
cat("\n")

# Show the field names used by each message type seen.
cat("Fields seen per message type:\n")
for (tid in unique(type_ids)) {
  idx <- which(type_ids == tid)[1]
  cat(
    "  type", tid, ":",
    paste(names(messages[[idx]]$msg), collapse = ", "),
    "\n"
  )
}
cat("\n")

# Print one full example of each message type, truncated for readability.
cat("One example per type (structure only):\n")
for (tid in unique(type_ids)) {
  idx <- which(type_ids == tid)[1]
  cat("\n--- messageTypeId", tid, "---\n")
  str(messages[[idx]]$msg, max.level = 3, give.attr = FALSE)
}

cat("\nDone. Send back the output above (no cookies), and the file:\n")
cat(normalizePath(raw_path, mustWork = FALSE), "\n")
