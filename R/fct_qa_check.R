qa_check <- function(data) {
  pop_cols   <- c("Refugees.and.Migrants.IN.DESTINATION", "Refugees.and.Migrants.IN.TRANSIT",
                  "Host.Communities.Beneficiaries", "Refugees.and.Migrants.PENDULARS", "Colombian.Returnees")
  agd_cols   <- c("Women.under.18", "Men.under.18", "Women.above.18", "Men.above.18", "Other.under.18", "Other.above.18")
  youth_cols <- c("Women.under.18", "Men.under.18", "Other.under.18")

  data |>
    mutate(
      QA_output = is_valid_output(Indicator.Indicator.Type, Quantity.of.output),
      QA_TotalMonthlyBeneficiaries = is_valid_total_beneficiaries_of_month(
        Indicator.Indicator.Type, Total.monthly.beneficiaries
      ),
      QA_NewBeneficiariesMonth = is_valid_new_beneficiaries_of_month(
        Indicator.Indicator.Type, New.beneficiaries.of.the.month
      )
    ) |>
    rowwise() |>
    mutate(
      tmp_pop_sum   = sum(c_across(all_of(pop_cols)),   na.rm = TRUE),
      tmp_agd_sum   = sum(c_across(all_of(agd_cols)),   na.rm = TRUE),
      tmp_youth_sum = sum(c_across(all_of(youth_cols)), na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
      QA_check_population_disaggregation = case_when(
        Indicator.Indicator.Type == "Direct Assistance" &
          tmp_pop_sum != New.beneficiaries.of.the.month ~ 1L,
        .default = 0L
      ),
      QA_check_AGD = case_when(
        Indicator.Indicator.Type == "Direct Assistance" &
          tmp_agd_sum != New.beneficiaries.of.the.month ~ 1L,
        .default = 0L
      ),
      QA_Education = case_when(
        Indicator.Indicator.Type == "Direct Assistance" &
          str_detect(Indicator.Sector, fixed("Education")) &
          tmp_youth_sum == 0 ~ 1L,
        .default = 0L
      ),
      QA_ChildProtection = case_when(
        Indicator.Indicator.Type == "Direct Assistance" &
          str_detect(Indicator.Sector, fixed("Child Protection")) &
          tmp_youth_sum == 0 ~ 1L,
        .default = 0L
      )
    ) |>
    select(-tmp_pop_sum, -tmp_agd_sum, -tmp_youth_sum) |>
    mutate(
      QA_valid_cva = is_valid_cva(CVA, Value..in.USD., Delivery.mechanism),
      QA_admin     = check_admin_validity(Country.Country, Country.Admin1, countryListDF),
      QA_indicator = check_indicator_validity(Indicator.Sector, Indicator.Indicator, indicatorDF)
    ) |>
    rowwise() |>
    mutate(QA_sum = as.integer(sum(c_across(starts_with("QA_")), na.rm = TRUE) > 0)) |>
    ungroup()
}
