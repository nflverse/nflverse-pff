#' Scrape all PFF players and save to PFF repo release
#'
#' @export
scrape_players_to_release <- function(){

  seasons <- nflreadr::get_current_season(roster = TRUE):2016

  players <- purrr::map_dfr(
    seasons,
    purrr::possibly(~pff_players)
  ) |>
    dplyr::group_by(pff_id) |>
    tidyr::fill(-c(age,season), .direction = "downup") |>
    dplyr::ungroup()

  nflversedata::nflverse_save(
    data_frame = players,
    file_name = "pff_players",
    nflverse_type = "pff_players",
    release_tag = "pff"
  )
}
