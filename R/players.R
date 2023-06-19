#' Get Players
#'
#' Collects only bio/roster info, use template to create grades function if desired
#'
#' @param season season to retrieve
#' @param league one of `c('nfl', 'ncaa')`, defaults to nfl
#'
#' @export
pff_players <- function(season, league = "nfl"){

  stopifnot(
    season <= nflreadr::get_current_season(roster = TRUE),
    league %in% c("nfl", "ncaa")
  )

  pos_grid <- expand.grid(
    season = season,
    position = c('QB', 'WR', 'HB', 'FB', 'TE', 'C', 'G', 'T', 'CB', 'S', 'LB', 'DI', 'ED', 'K', 'P'),
    league = league
  )

  out <- purrr::pmap_dfr(
    pos_grid,
    purrr::possibly(.pff_players_by_position, otherwise = data.frame(), quiet = FALSE)
  )
  return(out)
}

.pff_players_by_position <- function(season, position, league = "nfl") {
  stopifnot(
    season <= nflreadr::get_current_season(roster = TRUE),
    position %in% c('QB', 'WR', 'HB', 'FB', 'TE', 'C', 'G', 'T', 'CB', 'S', 'LB', 'DI', 'ED', 'K', 'P'),
    league %in% c("nfl", "ncaa")
  )

  req <- .pff_request(
    endpoint = "nfl/grades",
    query = list(league = league, season = season, position = position)
  )

  out <- req |>
    getElement("players") |>
    tidyr::unpack(cols = dplyr::where(is.data.frame),names_sep = "_") |>
    dplyr::mutate(season = season) |>
    dplyr::select(
      dplyr::any_of(
        c(
          "season",
          "position",
          "pff_id" = "id",
          "name",
          "age",
          "franchise_id",
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
      dplyr::starts_with("draft")
    )

  return(out)
}
