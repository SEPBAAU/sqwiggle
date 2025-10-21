# aflw_build_outputs.R
# Build complete AFLW fixtures/results CSV using fitzRoy (CRAN).
# Keep it minimal so it runs cleanly; we can add Elo later.

options(warn = 1)
suppressPackageStartupMessages({
  library(fitzRoy)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
})

yrs <- 2017:(year(Sys.Date()) + 1)

get_year <- function(y) {
  tryCatch(fetch_results(season = y, comp = "AFLW"), error = function(e) NULL)
}

raw <- lapply(yrs, get_year) |> bind_rows()
if (is.null(raw) || nrow(raw) == 0) stop("No AFLW rows returned.")

nm <- names(raw)
pick <- function(...) { x <- intersect(c(...), nm); if (length(x)) x[1] else NA }

date_col <- pick("Date","date")
round_col <- pick("Round","round")
season_col <- pick("Season","season")
home_col <- pick("Home.Team","home_team")
away_col <- pick("Away.Team","away_team")
hpts_col <- pick("Home.Points","home_points")
apts_col <- pick("Away.Points","away_points")
venue_col <- pick("Venue","venue","Ground","ground")

fx <- raw |>
  transmute(
    Date      = as.character(.data[[date_col]]),
    Round     = as.character(if (!is.na(round_col)) .data[[round_col]] else NA),
    Season    = as.character(if (!is.na(season_col)) .data[[season_col]] else year(ymd(.data[[date_col]]))),
    HomeTeam  = str_squish(as.character(.data[[home_col]])),
    AwayTeam  = str_squish(as.character(.data[[away_col]])),
    HomeScore = suppressWarnings(as.integer(.data[[hpts_col]])),
    AwayScore = suppressWarnings(as.integer(.data[[apts_col]])),
    Venue     = str_squish(as.character(if (!is.na(venue_col)) .data[[venue_col]] else NA))
  ) |>
  mutate(
    EventId = str_replace_all(paste0(Season, "_", Round, "_", HomeTeam, "_", AwayTeam), "[^A-Za-z0-9_]+", "_")
  ) |>
  arrange(Date, HomeTeam, AwayTeam) |>
  distinct(EventId, .keep_all = TRUE)

dir.create("data", showWarnings = FALSE)
write.csv(fx, "data/aflw_fixtures_all.csv", row.names = FALSE)

# OPTIONAL: quick-and-dirty upcoming placeholder (no Elo yet)
upcoming <- fx |> filter(is.na(HomeScore) & is.na(AwayScore) & Date >= as.character(Sys.Date()))
if (nrow(upcoming)) {
  write.csv(
    upcoming |> select(Date, Season, Round, HomeTeam, AwayTeam, EventId),
    "data/aflw_predictions_upcoming.csv",
    row.names = FALSE
  )
}
