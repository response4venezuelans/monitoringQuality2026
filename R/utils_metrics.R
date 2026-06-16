get_total_activities <- function(dataframe) {
  nrow(dataframe)
}

get_total_activities_to_review <- function(dataframe, column_name) {
  dataframe |>
    summarise(total = sum(.data[[column_name]], na.rm = TRUE)) |>
    pull(total)
}

get_percentage_activities <- function(total_errors, total_activities) {
  if (total_activities == 0) return(0)
  round((total_errors / total_activities) * 100, 2)
}
