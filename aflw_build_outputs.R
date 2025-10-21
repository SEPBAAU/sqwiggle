# aflw_build_outputs.R
# Build complete AFLW fixtures/results + upcoming predictions as CSVs (free).
# Depends on fitzRoy (data) + elo (ratings).
options(warn = 1)

suppressPackageStartupMessages(library(fitzRoy))
suppressPackageStartupMessages(library(elo))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(lubridate))

# 1) Fetch all AFLW results 2017 → next year (future fixtures sometimes appear)
yrs <- 2017:(year(Sys.Date()) + 1)
get_y <- function(y) tryCatch(fetch_results(season = y, comp = "AFLW"), error = function(e) NULL)
raw <- lapply(yrs, get_y) |> bind_rows()
stopifnot(nrow(raw) > 0)

# 2) Normalise to sheet-friendly columns
nm <- names(raw); pick <- function(...) {x <- intersect(c(...), nm); if (length(x)) x[1] else NA}
date_col <- pick("Date","date"); round_col <- pick("Round","round")
season_col <- pick("Season","season"); home_col <- pick("Home.Team","home_team")
away_col <- pick("Away.Team","away_team"); hpts_col <- pick("Home.Points","home_points")
apts_col <- pick("Away.Points","away_points"); venue_col <- pick("Venue","venue","Ground","ground")

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

# 3) ELO training on completed games (simple, sensible defaults)
params <- list(K = 24, HFA = 40, BASE = 1500)
hist <- fx |> filter(!is.na(HomeScore), !is.na(AwayScore)) |> arrange(Date)
teams <- sort(unique(c(hist$HomeTeam, hist$AwayTeam)))
ratings <- setNames(rep(params$BASE, length(teams)), teams)

for (i in seq_len(nrow(hist))) {
  g <- hist[i,]
  Rh <- (ratings[[g$HomeTeam]] %||% params$BASE) + params$HFA
  Ra <-  ratings[[g$AwayTeam]] %||% params$BASE
  pH <- 1 / (1 + 10 ^ ((Ra - Rh)/400))
  aH <- ifelse(g$HomeScore > g$AwayScore, 1, ifelse(g$HomeScore == g$AwayScore, 0.5, 0))
  ratings[[g$HomeTeam]] <- (ratings[[g$HomeTeam]] %||% params$BASE) + params$K * (aH - pH)
  ratings[[g$AwayTeam]] <- (ratings[[g$AwayTeam]] %||% params$BASE) + params$K * ((1 - aH) - (1 - pH))
}

# 4) Score upcoming fixtures (no scores yet)
today <- as.character(Sys.Date())
upcoming <- fx |> filter(is.na(HomeScore) & is.na(AwayScore) & Date >= today)

preds <- upcoming |>
  rowwise() |>
  mutate(
    EloHome = ratings[[HomeTeam]] %||% params$BASE,
    EloAway = ratings[[AwayTeam]] %||% params$BASE,
    P_Home  = 1 / (1 + 10 ^ (((EloAway) - (EloHome + params$HFA))/400)),
    P_Away  = 1 - P_Home,
    FairOdds_H = round(1 / P_Home, 2),
    FairOdds_A = round(1 / P_Away, 2)
  ) |>
  ungroup() |>
  select(Date, Season, Round, HomeTeam, AwayTeam, P_Home, FairOdds_H, P_Away, FairOdds_A, EventId)

write.csv(preds, "data/aflw_predictions_upcoming.csv", row.names = FALSE)

message("Wrote: fixtures=", nrow(fx), " | upcoming=", nrow(preds))
