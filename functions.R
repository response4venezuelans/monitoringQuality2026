library(activityinfo)
library(httr2)
library(dplyr)
library(stringr)
library(readxl)

activityInfoToken(Sys.getenv("ACTIVITYINFOTOKEN"), prompt = FALSE)

fetch_ai_form <- function(form_id) {
  request(paste0("https://www.activityinfo.org/resources/query/v43/form/", form_id)) |>
    req_auth_bearer_token(Sys.getenv("ACTIVITYINFOTOKEN")) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE, flatten = TRUE) |>
    as_tibble()
}

getDataFromAI <- function(typeOfRequest, filterRequest) {
  data <- fetch_ai_form("cov6wkemnorsfs62i") |>
    rename(
      Record.ID                            = `_id`,
      Country.Country                      = `country.Country`,
      Country.Admin1                       = `country.Admin1`,
      Appealing.organisation.Name          = `appealing_org.Name`,
      Implementation.Set.up                = implementation_setup,
      Implementing.partner.Name            = `implementing_org.Name`,
      Month                                = month,
      Indicator.Sector                     = `indicator.sector`,
      Indicator.Indicator                  = `indicator.indicator_simplified`,
      Activity.Name                        = activity,
      Activity.Description                 = activity_description,
      RMRP.Activity                        = rmrp_activity,
      CVA                                  = cva,
      Value..in.USD.                       = cva_value,
      Delivery.mechanism                   = cva_mechanism,
      Quantity.of.output                   = output,
      Total.monthly.beneficiaries          = total_monthly_beneficiaries,
      Refugees.and.Migrants.IN.DESTINATION = new_indestination,
      Refugees.and.Migrants.IN.TRANSIT     = new_intransit,
      Host.Communities.Beneficiaries       = new_hostcomm,
      Refugees.and.Migrants.PENDULARS      = new_pendulars,
      Colombian.Returnees                  = new_col_returnees,
      Women.under.18                       = new_women_under18,
      Men.under.18                         = new_men_under18,
      Women.above.18                       = new_women_above18,
      Men.above.18                         = new_men_above18,
      Other.under.18                       = new_other_under18,
      Other.above.18                       = new_other_above18,
      Platform                             = platform,
      indicator_ref                        = indicator
    ) |>
    select(
      -any_of(c("_lastEditTime", "_recordStatus", "intro_review",
                "appealing_org", "implementing_org", "country")),
      -starts_with("calc_")
    ) |>
    mutate(New.beneficiaries.of.the.month = Total.monthly.beneficiaries) |>
    left_join(indicators_2026_types, by = "indicator_ref") |>
    rename(Indicator.Indicator.Type = indicator_type) |>
    select(-indicator_ref)

  if (typeOfRequest == "country" && filterRequest != "All") {
    data <- data |> filter(Country.Country == filterRequest)
  } else if (typeOfRequest == "partner" && filterRequest != "All") {
    data <- data |> filter(Appealing.organisation.Name == filterRequest)
  }

  data
}

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
    (indicator_type %in% c("Direct Assistance", "Capacity Building") &
      (is.na(new_beneficiaries_of_month) | new_beneficiaries_of_month < 0)) |
      (!indicator_type %in% c("Direct Assistance", "Capacity Building") &
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
  if_else(
    cva_column == "Yes",
    if_else(cva_value <= 0 | !cva_type %in% valid_cva_types, 1L, 0L),
    0L
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
