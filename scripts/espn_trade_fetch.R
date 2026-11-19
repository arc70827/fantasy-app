# ============================================================
# ESPN TRADE FETCH
#
# Downloads every page of a season's league communication feed
# (transactions, trades, and forced drops) and returns it as a list
# with a "topics" element. Shared by update_trades.R and the probe.
#
# Requires the httr2 and jsonlite packages.
# ============================================================

library(httr2)
library(jsonlite)

trade_league_id <- "254169010"
trade_page_size <- 100

# Filter copied from the x-fantasy-filter header that ESPN's own
# Recent Activity page sends, reduced to transactions only.
# The season endpoint takes "topics" at the top level; the league
# history endpoint nests it under "communication".
make_trade_filter <- function(offset, history = FALSE) {
  topics <- list(
    topics = list(
      filterType = list(value = list("ACTIVITY_TRANSACTIONS")),
      limit = trade_page_size,
      limitPerMessageSet = list(value = trade_page_size),
      offset = offset,
      sortMessageDate = list(sortPriority = 1, sortAsc = FALSE)
    )
  )

  if (history) {
    topics <- list(communication = topics)
  }

  toJSON(topics, auto_unbox = TRUE)
}

# Returns list(topics = <all topics for the season>).
# Uses the season endpoint if it exists, otherwise the league history
# endpoint. An empty season returns an empty topics list.
fetch_trade_pages <- function(season, swid, espn_s2) {
  cookie <- paste0("SWID=", swid, "; espn_s2=", espn_s2)

  season_url <- sprintf(
    "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/%d/segments/0/leagues/%s/communication/",
    season,
    trade_league_id
  )

  history_url <- sprintf(
    "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/leagueHistory/%s?seasonId=%d",
    trade_league_id,
    season
  )

  send_request <- function(target_url, offset, history) {
    request(target_url) |>
      req_url_query(view = "kona_league_communication") |>
      req_headers(
        Cookie = cookie,
        `x-fantasy-filter` = as.character(make_trade_filter(offset, history))
      ) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
  }

  use_history <- FALSE
  all_topics <- list()
  offset <- 0

  repeat {
    resp <- if (use_history) {
      send_request(history_url, offset, TRUE)
    } else {
      send_request(season_url, offset, FALSE)
    }

    if (!use_history && resp_status(resp) == 404) {
      use_history <- TRUE
      resp <- send_request(history_url, offset, TRUE)
    }

    if (resp_status(resp) != 200) {
      stop(
        "ESPN returned HTTP ", resp_status(resp),
        " for season ", season, ". Check the cookies and season.",
        call. = FALSE
      )
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

    page <- fromJSON(text, simplifyVector = FALSE)

    if (is.null(page$topics) && !is.null(page$communication)) {
      page$topics <- page$communication$topics
    }

    topics <- page$topics
    all_topics <- c(all_topics, topics)

    if (length(topics) < trade_page_size) {
      break
    }

    offset <- offset + trade_page_size
  }

  list(topics = all_topics)
}
