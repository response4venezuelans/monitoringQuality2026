# Returns 1 if output is zero for non-Direct Assistance indicators
is_valid_output <- function(indicator_type, output_data_column) {
  if_else(
    indicator_type != "Direct Assistance" & !is.na(output_data_column) & output_data_column == 0,
    1L, 0L
  )
}

# Returns 1 if total monthly beneficiaries is missing or zero for types that require it
is_valid_total_beneficiaries_of_month <- function(indicator_type, total_monthly_beneficiaries_data_column) {
  optional_types <- c("Infrastructure", "Mechanism/Advocacy", "Other", "Campaign")
  if_else(
    !indicator_type %in% optional_types &
      (is.na(total_monthly_beneficiaries_data_column) | total_monthly_beneficiaries_data_column == 0),
    1L, 0L
  )
}

# Returns 1 if new beneficiaries is invalid for the given indicator type
is_valid_new_beneficiaries_of_month <- function(indicator_type, new_beneficiaries_of_month) {
  if_else(
    (indicator_type == "Direct Assistance" &
      (is.na(new_beneficiaries_of_month) | new_beneficiaries_of_month < 0)) |
      (indicator_type != "Direct Assistance" &
        !is.na(new_beneficiaries_of_month)),
    1L, 0L
  )
}

# Returns 1 if population type disaggregation sum doesn't match new beneficiaries (Direct Assistance only)
is_valid_population_type_disaggregation <- function(indicator_type, population_columns, new_beneficiaries_of_month) {
  case_when(
    indicator_type == "Direct Assistance" &
      sum(population_columns, na.rm = TRUE) != new_beneficiaries_of_month ~ 1L,
    .default = 0L
  )
}

# Returns 1 if AGD disaggregation sum doesn't match new beneficiaries (Direct Assistance only)
is_valid_age_gender_disaggregation <- function(indicator_type, population_columns, new_beneficiaries_of_month) {
  case_when(
    indicator_type == "Direct Assistance" &
      sum(population_columns, na.rm = TRUE) != new_beneficiaries_of_month ~ 1L,
    .default = 0L
  )
}

# Returns 1 if CVA is "Yes" but value is missing/zero or mechanism is invalid
is_valid_cva <- function(cva_column, cva_value, cva_type) {
  valid_cva_types <- c(
    "Beneficiary Bank Account", "Mobile Money", "Prepaid Card",
    "Cash Collection Over the Counter (OTC)", "Direct Cash (Cash in Hand)",
    "Cardless ATM Withdrawal", "Voucher", "Other"
  )
  cva_value_num <- suppressWarnings(as.numeric(cva_value))
  case_when(
    is.na(cva_column) | cva_column != "Yes"                                        ~ 0L,
    is.na(cva_value_num) | cva_value_num <= 0 | !cva_type %in% valid_cva_types     ~ 1L,
    .default = 0L
  )
}

# Returns 1 if the admin0/admin1 combination is not in the reference table
check_admin_validity <- function(reported_admin0, reported_admin1, valid_admin_table) {
  tibble(Country_Country = reported_admin0, Country_Admin1 = reported_admin1) |>
    left_join(valid_admin_table, by = c("Country_Country" = "Country", "Country_Admin1" = "Admin1")) |>
    mutate(valid = if_else(is.na(countryISO) | is.na(Admin1ISOCode), 1L, 0L)) |>
    pull(valid)
}

# Returns 1 if the sector/indicator combination is not in the reference table
check_indicator_validity <- function(reported_sector, reported_indicator, valid_indicator_table) {
  tibble(Sector = reported_sector, Indicator = reported_indicator) |>
    left_join(valid_indicator_table, by = c("Sector", "Indicator")) |>
    mutate(valid = if_else(is.na(CODE), 1L, 0L)) |>
    pull(valid)
}

# Returns 1 if Direct Assistance records in a sector have all-zero AGD counts
is_valid_agd_sector_specific <- function(sector_column, sector_name, indicator_type_column, population_columns) {
  case_when(
    indicator_type_column == "Direct Assistance" &
      str_detect(sector_column, fixed(sector_name)) &
      sum(population_columns, na.rm = TRUE) == 0 ~ 1L,
    .default = 0L
  )
}
