# count number of weekdays in a given month/year combo
count_weekdays <- function(df, year_col = "year", month_col = "month") {
  # Ensure necessary columns exist
  if (!all(c(year_col, month_col) %in% names(df))) {
    stop("Both specified year and month columns must exist in the dataframe.")
  }

  # Use mapply to compute weekdays for each row
  df$weekdays_in_month <- mapply(function(y, m) {
    # First and last day of the month
    first_day <- as.Date(sprintf("%04d-%02d-01", y, m))
    last_day <- as.Date(format(seq(first_day, length = 2, by = "1 month")[2] - 1, "%Y-%m-%d"))

    # Generate all dates in the month
    all_days <- seq(first_day, last_day, by = "day")

    # Count weekdays (Monday = 1, ..., Sunday = 7)
    sum(!(weekdays(all_days, abbreviate = FALSE) %in% c("Saturday", "Sunday")))
  }, df[[year_col]], df[[month_col]])

  return(df)
}
