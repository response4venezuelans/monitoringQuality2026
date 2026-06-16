fetch_ai_form <- function(form_id) {
  request(str_glue("https://www.activityinfo.org/resources/query/v43/form/{form_id}")) |>
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
