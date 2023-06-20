#' Get Players
#'
#' Collects only bio/roster info, use template to create grades function if desired
#'
#' @param season season to retrieve
#' @param league one of `c('nfl', 'ncaa')`, defaults to nfl
#' @param ... parameters to pass to the `httr::GET()` request
#'
#' @export
pff_players <- function(season, league = "nfl", ...){

  stopifnot(
    season <= nflreadr::get_current_season(roster = TRUE),
    league %in% c("nfl", "ncaa")
  )

  season_grid <- expand.grid(
    season = season,
    league = league
  )

  teams <- purrr::pmap_dfr(
    season_grid,
    purrr::possibly(pff_teams, otherwise = "data.frame"),
    .progress = glue::glue("Retrieving team IDs for {nrow(season_grid)} seasons")
  ) |>
    dplyr::select(season, team_id, league)

  out <- purrr::pmap_dfr(
    teams,
    purrr::possibly(.pff_players_by_team, otherwise = data.frame()),
    .progress = glue::glue("Retrieving players for {nrow(teams)} teams")
  )

  return(out)
}

.pff_players_by_team <- function(season, team_id, league = "nfl") {

  stopifnot(
    season <= nflreadr::get_current_season(roster = TRUE),
    !is.na(as.numeric(team_id)),
    league %in% c("nfl", "ncaa")
  )

  req <- .pff_request(
    endpoint = glue::glue("teams/{team_id}/roster"),
    query = list(league = league, season = season)
  )

  out <- req |>
    getElement("team_players") |>
    tidyr::unpack(cols = dplyr::where(is.data.frame), names_sep = "_") |>
    dplyr::mutate(season = .env$season,
                  league = .env$league,
                  dplyr::across(dplyr::where(is.character), ~dplyr::na_if(.x, ""))) |>
    dplyr::select(
      dplyr::any_of(
        c(
          "season",
          "position",
          "player_id" = "id",
          "player_name" = "name",
          "age",
          "team_id" = "franchise_id",
          "team_name",
          "team_slug",
          "status",
          "college",
          "height",
          "weight",
          "jersey_number",
          "slug"
        )
      ),
      dplyr::starts_with("draft"),
      "league"
    )

  return(out)
}
