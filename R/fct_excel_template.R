# Returns TRUE if the dataframe column structure matches the template
check_dataframe_structure <- function(dataframe, template_file_path, sheet = 1) {
  template_colnames <- read_excel(template_file_path, sheet = sheet) |> names()
  identical(names(dataframe), template_colnames)
}

# Replaces spaces and parentheses in column names with dots
rename_columns <- function(dataframe) {
  dataframe |>
    rename_with(~ str_replace_all(.x, c(" " = ".", "\\(" = ".", "\\)" = ".")))
}

add_platform_column <- function(df) {
  df |>
    mutate(Platform = case_when(
      Country.Country == "Brazil"   ~ "Brazil",
      Country.Country == "Chile"    ~ "Chile",
      Country.Country == "Colombia" ~ "Colombia",
      Country.Country == "Ecuador"  ~ "Ecuador",
      Country.Country == "Peru"     ~ "Peru",
      Country.Country %in% c("Aruba", "Curacao", "Guyana", "Dominican Republic", "Trinidad and Tobago") ~ "Caribbean",
      Country.Country %in% c("Costa Rica", "Mexico", "Panama") ~ "Central America and Mexico",
      Country.Country %in% c("Argentina", "Paraguay", "Uruguay", "Bolivia") ~ "Southern Cone",
      .default = NA_character_
    ))
}

addIndicatorType <- function(df, indicatordf) {
  df |>
    left_join(
      indicatordf |> select(Sector, Indicator, Indicator.Type),
      by = c("Indicator.Sector" = "Sector", "Indicator.Indicator" = "Indicator")
    ) |>
    rename(Indicator.Indicator.Type = Indicator.Type)
}

addCountryISOCodes <- function(df, countryDF) {
  df |>
    left_join(countryDF, by = c("Country.Country" = "Country", "Country.Admin1" = "Admin1")) |>
    rename(Country.countryISO = countryISO, Country.Admin1ISOCode = Admin1ISOCode)
}

# Excel uploads don't carry ActivityInfo's calc_new_agd/calc_new_pop_type
# fields, so recompute the same AGD.Sum.Calculated/PopType.Sum.Calculated
# totals qa_check() expects, using the same column groups ActivityInfo sums.
add_calculated_sums <- function(df) {
  df |>
    rowwise() |>
    mutate(
      AGD.Sum.Calculated     = sum(c_across(all_of(qa_agd_columns)),        na.rm = TRUE),
      PopType.Sum.Calculated = sum(c_across(all_of(qa_population_columns)), na.rm = TRUE)
    ) |>
    ungroup()
}
