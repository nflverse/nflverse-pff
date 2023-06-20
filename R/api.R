#' pff request function
#'
#' Interpolates endpoint and query parameters to the default base URL and then
#' passes it to `.api_request`
#'
#' @param endpoint character: path to access
#' @param query a list to be appended as HTTP query parameters (e.g. ?x=x&y=y)
#' @param base_url base url, defaults to "https://www.pff.com/api"
#' @param ... arguments passed to `.api_request()`
#'
#' @export
.pff_request <- function(endpoint, query, base_url = "https://www.pff.com/api", ...){

  url <- httr::modify_url(
    url = glue::glue("{base_url}/{endpoint}"),
    query = query
  )

  resp <- .api_request(url, ...)

  resp_contents <- list()

  if(length(resp) > 0) resp_contents <- jsonlite::fromJSON(resp)

  out <- structure(
    resp_contents,
    url = attr(resp, "url"),
    status_code = attr(resp, "status_code"),
    class = c("nflversepff_request", "list")
  )

  return(out)
}

#' base request function
#'
#' Formats api request to the PFF API with retry
#'
#' @param url Full URL to be scraped.
#' @param ... arguments to pass to httr request function
#' @param verb character: a curl verb supported by httr, e.g. GET, POST, PUT, DELETE
#' this is passed to `httr::RETRY()`. Defaults to \code{"GET"}
#' @param retry_times Maximum number of attempts. Defaults to \code{3}.
#' @param user_agent Request user agent, defaults to option `nflversepff.user_agent`
#' @param log_url url for print logging, defaults to `url`
#' @param verbose whether to print out URLs, default = FALSE
#'
#' @export
.api_request <- function(url,
                         ...,
                         verb = "GET",
                         user_agent = getOption("nflversepff.user_agent"),
                         retry_delay = 1L,
                         retry_times = 3L,
                         log_url = url,
                         verbose = getOption("nflversepff.verbose", FALSE)
                         ){

  if(is.null(user_agent)) {
    user_agent <- glue::glue(
      "nflverse/nflverse-pff {packageVersion('nflversepff')} ",
      "<https://github.com/nflverse/nflverse-pff>"
    )
  }

  stopifnot(
    is.character(url) && length(url) == 1,
    is.character(user_agent) && length(user_agent) == 1,
    is.numeric(retry_times),
    is.numeric(retry_delay),
    verbose %in% c(TRUE, FALSE)
  )

  if(verbose) cli::cli_alert_info("Retrieving {log_url}")

  resp <- httr::RETRY(
    verb = verb,
    url = url,
    pause_base = retry_delay,
    pause_min = retry_delay,
    times = retry_times,
    httr::user_agent(user_agent),
    ...
  )

  out <- httr::content(resp, as = "text")

  if(httr::http_error(resp)){
    cli::cli_warn("ERROR: {httr::http_status(resp)$message} \n URL:{log_url}")
    out <- character()
  }

  out <- structure(
    out,
    url = url,
    status_code = httr::status_code(resp),
    response = resp,
    class = c("nflversepff_request", class(out))
  )

  return(out)
}
