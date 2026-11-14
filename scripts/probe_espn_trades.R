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

# Filter copied from the x-fantasy-filter header that ESPN's own
# Recent Activity page sends, reduced to transactions only.
# filterType.value must be a list of type names, not a single string.
page_size <- 100

# The league history endpoint nests the filter under "communication";
# the season endpoint takes "topics" at the top level.
make_filter <- function(offset, history = FALSE) {
  topics <- list(
    topics = list(
      filterType = list(value = list("ACTIVITY_TRANSACTIONS")),
      limit = page_size,
      limitPerMessageSet = list(value = page_size),
      offset = offset,
      sortMessageDate = list(sortPriority = 1, sortAsc = FALSE)
    )
  )

  if (history) {
    topics <- list(communication = topics)
  }

  toJSON(topics, auto_unbox = TRUE)
}

# Same host the weekly updater uses. fantasy.espn.com returns a web page
# for this path, so it isn't used here.
url <- sprintf(
  "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/%d/segments/0/leagues/%s/communication/",
  season,
  league_id
)

# Fetch every page. ESPN returns at most page_size topics per request,
# so keep asking with a larger offset until a page comes back short.
# Past seasons may only be served from ESPN's league history endpoint,
# which the weekly updater uses for 2022 through 2024.
history_url <- sprintf(
  "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/leagueHistory/%s?seasonId=%d",
  league_id,
  season
)

send_request <- function(target_url, offset, history = FALSE) {
  request(target_url) |>
    req_url_query(view = "kona_league_communication") |>
    req_headers(
      Cookie = paste0("SWID=", swid, "; espn_s2=", espn_s2),
      `x-fantasy-filter` = as.character(make_filter(offset, history))
    ) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
}

fetch_page <- function(offset) {
  resp <- send_request(url, offset)

  if (resp_status(resp) == 404) {
    cat("Season endpoint returned 404. Trying league history endpoint.\n")
    resp <- send_request(history_url, offset, history = TRUE)
  }

  cat("Offset", offset, "-> HTTP", resp_status(resp), "\n")

  if (resp_status(resp) != 200) {
    cat("Response body (first 500 chars):\n")
    cat(substr(resp_body_string(resp), 1, 500), "\n")
    stop("ESPN did not return data. Check the cookies and season.", call. = FALSE)
  }

  text <- resp_body_string(resp)

  # A web page here means the request didn't reach the API.
  if (grepl("^\\s*<", text)) {
    stop(
      "ESPN returned a web page instead of league data. ",
      "Check that the cookies are current.",
      call. = FALSE
    )
  }

  fromJSON(text, simplifyVector = FALSE)
}

all_topics <- list()
offset <- 0

repeat {
  page <- fetch_page(offset)
  n <- length(page$topics)
  all_topics <- c(all_topics, page$topics)
  if (n < page_size) break
  offset <- offset + page_size
}

body <- list(topics = all_topics)

raw_path <- file.path(out_dir, sprintf("espn_communication_%d.json", season))
writeLines(toJSON(body, auto_unbox = TRUE), raw_path)
cat("All pages saved to:", raw_path, "\n\n")

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
