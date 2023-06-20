#' Get teams from PFF API
#'
#' Collects whatever is available from the "premium" endpoint without authentication
#'
#' @param season season to retrieve
#' @param league one of `c('nfl', 'ncaa')`, defaults to nfl
#'
#' @export
pff_teams <- function(season = nflreadr::get_current_season(roster = TRUE),
                      league = "nfl",
                      ...){

  stopifnot(
    season <= nflreadr::get_current_season(roster = TRUE),
    league %in% c("nfl", "ncaa")
  )

  req <- .pff_request(
    endpoint = "teams/overview",
    query = list(season = season, league = league),
    base_url = "https://premium.pff.com/api/v1",
    ...
  )

  teams <- req |>
    getElement("team_overview") |>
    dplyr::select(
      dplyr::any_of(c(
        "team_id" = "franchise_id",
        "team_name" = "name",
        "team_abbreviation" = "abbreviation"
      ))
    ) |>
    dplyr::mutate(season = .env$season, league = .env$league)

  return(teams)
}
