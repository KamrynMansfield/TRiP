# count_weekdays.R --------------------------------------------------------
# Utility used to convert between monthly ridership totals and average weekday
# ridership. It is called in make_model_data_frame() (to build upt_avg before
# modeling) and again in forecast_ridership() (to turn forecast averages back
# into monthly totals).

#' Add a count of weekdays in each month
#'
#' Takes a data frame that has a year column and a month column and appends a
#' `weekdays_in_month` column giving the number of Monday-through-Friday days in
#' that year/month combination. Note that this is a raw weekday count; it does
#' not exclude holidays.
#'
#' @param df A data frame containing a year column and a month column.
#' @param year_col The name of the four-digit year column, as a string.
#'   Defaults to `"year"`.
#' @param month_col The name of the numeric month column (1-12), as a string.
#'   Defaults to `"month"`.
#'
#' @returns The input data frame with one extra integer column,
#'   `weekdays_in_month`.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(year = c(2024, 2024), month = c(1, 2))
#' count_weekdays(df)
#' }
#'
#' @keywords internal
#' @noRd
# count number of weekdays in a given month/year combo
count_weekdays <- function(df, year_col = "year", month_col = "month") {
  # Ensure necessary columns exist
  if (!all(c(year_col, month_col) %in% names(df))) {
    stop("Both specified year and month columns must exist in the dataframe.")
  }

  # Use mapply to compute weekdays for each row
  # (vectorized over the year and month columns, one result per row)
  df$weekdays_in_month <- mapply(function(y, m) {
    # First and last day of the month
    # last day = (first day of the following month) - 1 day
    first_day <- as.Date(sprintf("%04d-%02d-01", y, m))
    last_day <- as.Date(format(seq(first_day, length = 2, by = "1 month")[2] - 1, "%Y-%m-%d"))

    # Generate all dates in the month
    all_days <- seq(first_day, last_day, by = "day")

    # Count weekdays (Monday = 1, ..., Sunday = 7)
    # NOTE: weekdays() is locale dependent, so this assumes an English locale.
    sum(!(weekdays(all_days, abbreviate = FALSE) %in% c("Saturday", "Sunday")))
  }, df[[year_col]], df[[month_col]])

  return(df)
}
