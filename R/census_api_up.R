#' Check if census API is working
#'
#' @param timeout_sec The number of seconds until it times out and returns FALSE
#'
#' @returns TRUE if the census API is working and FALSE if it is not working
#' @export
#'
#' @examples
#' census_api_up()
census_api_up <- function(timeout_sec = 5) {
  test_url <- "https://api.census.gov/data/2020/dec/pl?get=NAME&for=us:1"

  resp <- tryCatch(
    httr::GET(test_url, httr::timeout(timeout_sec)),
    error = function(e) NULL
  )

  if (is.null(resp)) return(FALSE)

  httr::status_code(resp) == 200
}
