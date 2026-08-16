# Branford Fantasy Football Hub

A mobile focused R Shiny application for exploring the history, statistics, records, managers, players, matchups, and weekly results of the Branford fantasy football league.

The application uses historical league data beginning in 2021 and automatically updates from ESPN during the fantasy football season.

## Repository Structure

### Application

`app.R`

The main R Shiny application.

The app reads the production CSV files from the `data` folder and provides the user interface, calculations, rankings, player history, manager history, matchup archive, record book, and other league statistics.

### Production Data

`data/fantasy_matchup_data.csv`

Contains fantasy team matchup results by season and week.

`data/fantasy_player_data.csv`

Contains weekly fantasy player data including fantasy team, NFL position, NFL team, lineup slot, projection, fantasy points, week, and season.

`data/fantasy_manager_data.csv`

Maps real managers to ESPN owner IDs and fantasy team names over time.

Manager records use:

* Manager
* ESPN ID
* Team Name
* Start Year
* Start Week
* End Year
* End Week

The ESPN ID is the permanent machine identity for a manager. Fantasy team names are allowed to change over time.

## Historical Data

The league database begins with the 2021 season.

### 2021

The 2021 data was manually preserved because the league's ESPN historical data is no longer available through the current ESPN endpoint.

Some 2021 player fields such as historical NFL team and projection are intentionally unavailable.

The updater preserves the 2021 data rather than attempting to reconstruct it.

### 2022 and Later

Historical seasons beginning in 2022 were rebuilt and validated against ESPN.

The automated system is used for new weekly data going forward.

## Weekly ESPN Updater

`scripts/update_weekly.R`

This script retrieves completed fantasy football weeks from ESPN and safely updates the production database.

The updater uses the ESPN league ID stored in the script and authenticates using repository secrets.

It performs several validations before any production data is written.

### Weekly Process

On an automatic run, the updater:

1. Determines the current fantasy season.

2. Determines the next missing fantasy week.

3. Checks whether ESPN considers that week complete.

4. Pulls the relevant matchup and roster information from ESPN.

5. Pulls each player's exact weekly fantasy points.

6. Pulls projections and current NFL team information.

7. Identifies active lineup slots while allowing legitimately empty starting positions.

8. Independently sums active player fantasy points.

9. Compares those totals with ESPN's official matchup totals.

10. Updates manager and fantasy team name ranges.

11. Validates the complete production dataset.

12. Replaces the target week's data using an upsert rather than a blind append.

No CSV files are written unless all required validations pass.

## Late ESPN Stat Corrections

Each automatic weekly run also rechecks the four most recently imported fantasy weeks.

If ESPN has made a late scoring correction, the updater can correct:

* Player fantasy points
* Team matchup scores
* Points for
* Points against
* Win and loss results when necessary

The correction process does not rewrite historical roster identity, lineup slots, fantasy team names, projections, or NFL team history merely because ESPN later displays those values differently.

If no late correction is found, nothing is changed.

## Manager and Team Name Handling

Managers are identified internally by ESPN owner ID rather than fantasy team name.

This allows a manager to change fantasy team names without losing historical identity.

For example:

```text
Manager     ESPN ID     Team Name       Start       End
John Smith  {ABC123}    The Crushers    2026 W1     2026 W6
John Smith  {ABC123}    Smith Happens   2026 W7     2027 W4
```

If the same manager keeps the same team name, the existing range is extended.

Week 1 of a new season is considered consecutive with Week 17 of the previous fantasy season, so an unchanged team name can continue across seasons.

If the manager changes the fantasy team name, the old range remains intact and a new range is created.

## New Managers

If the updater encounters an ESPN owner ID that does not already exist in the manager database, it automatically creates a new manager entry.

The temporary manager name is written in a form similar to:

```text
REVIEW: ESPNHandle
```

After the first update, manually replace only the `Manager` value with the person's real name.

Do not alter the ESPN ID.

Once the real manager name is saved alongside that ESPN ID, future weekly updates retain the real name automatically.

## Playoffs

The stored fantasy schedule includes:

* Weeks 1 through 14: Regular Season
* Week 15: Quarterfinals
* Week 16: Semifinals
* Week 17: Finals and Third Place

Consolation games are excluded from the production matchup and player datasets.

## GitHub Actions

### Automatic Fantasy Data Update

`.github/workflows/update_fantasy_data.yml`

The workflow runs automatically every Tuesday morning during the year.

Normal automatic operation requires no year or week input.

The workflow:

1. Checks out the repository.
2. Sets up R.
3. Runs the ESPN weekly updater.
4. Validates the ESPN data.
5. Checks for late stat corrections.
6. Updates the production CSV files if necessary.
7. Commits changed CSV files to `main`.
8. Deploys the updated Shiny application when production data changed.

If there are no new completed weeks and no stat corrections, no commit or deployment is created.

The workflow can also be launched manually from the GitHub Actions interface.

For testing, use the `Validate only and do not change files` option.

## Shiny Deployment

`.github/workflows/deploy-shiny.yml`

Normal code changes pushed manually to `main` trigger the Shiny deployment workflow.

This allows application development to be tested locally before it reaches production.

Typical development process:

1. Make changes locally.
2. Run and test the Shiny app locally.
3. Continue editing until satisfied.
4. Commit the changes.
5. Push to `main`.
6. GitHub Actions deploys the new version to shinyapps.io.

The weekly ESPN workflow performs its own deployment after automated data changes.

## GitHub Repository Secrets

The following GitHub Actions repository secrets are required.

### ESPN

```text
ESPN_SWID
ESPN_S2
```

These provide authenticated access to the ESPN fantasy league.

### shinyapps.io

```text
SHINYAPPS_ACCOUNT
SHINYAPPS_TOKEN
SHINYAPPS_SECRET
SHINYAPPS_APP_NAME
```

Never place the actual secret values in this README, `app.R`, the weekly updater, or any committed repository file.

## Local Development

The repository can be run locally from R or Positron.

Example:

```r
setwd("C:/Users/adamc/Downloads/fantasy-app")

shiny::runApp()
```

Changes should normally be tested locally before being committed to `main`.

## Testing the Weekly Updater

A specific historical week can be tested without changing production data.

Example:

```r
setwd("C:/Users/adamc/Downloads/fantasy-app")

Sys.setenv(
  TARGET_YEAR = "2025",
  TARGET_WEEK = "17",
  DRY_RUN = "true",
  DATA_DIR = "data"
)

source("scripts/update_weekly.R")
```

For automatic mode, leave `TARGET_WEEK` unset.

Always use `DRY_RUN = "true"` when experimenting against the production `data` folder.

## Safety Philosophy

The updater is intentionally conservative.

If ESPN returns unexpected data, a matchup is incomplete, player totals do not reconcile with official ESPN matchup totals, an unknown mapping appears, duplicate records are detected, or another validation fails, the updater stops before changing production files.

The goal is to prefer a failed update over silently introducing incorrect historical data.

## Expected Maintenance

During a normal fantasy football season, weekly data updates should require no manual work.

Possible future manual maintenance includes:

* Replacing a new manager's temporary `REVIEW:` name after the manager first appears.
* Updating the application when new features are desired.
* Updating the ESPN integration if ESPN changes its private fantasy API.
* Refreshing ESPN authentication secrets if they expire.
* Reviewing GitHub Actions if GitHub changes its workflow environment.
* Checking before each season that the scheduled workflow remains enabled.

Otherwise, the application and weekly data process are designed to operate automatically.