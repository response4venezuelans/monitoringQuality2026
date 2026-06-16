# Breakdown columns that ActivityInfo itself sums into AGD.Sum.Calculated /
# PopType.Sum.Calculated (kept here so fct_excel_template.R can recompute the
# same totals for uploaded files, which don't carry ActivityInfo's calc columns)
qa_population_columns <- c("Refugees.and.Migrants.IN.DESTINATION", "Refugees.and.Migrants.IN.TRANSIT",
                            "Host.Communities.Beneficiaries", "Refugees.and.Migrants.PENDULARS", "Colombian.Returnees")
qa_agd_columns   <- c("Women.under.18", "Men.under.18", "Women.above.18", "Men.above.18", "Other.under.18", "Other.above.18")
qa_youth_columns <- c("Women.under.18", "Men.under.18", "Other.under.18")

qa_check <- function(data) {
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
      tmp_youth_sum = sum(c_across(all_of(qa_youth_columns)), na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
      QA_check_population_disaggregation = case_when(
        Indicator.Indicator.Type == "Direct Assistance" &
          PopType.Sum.Calculated != New.beneficiaries.of.the.month ~ 1L,
        .default = 0L
      ),
      QA_check_AGD = case_when(
        Indicator.Indicator.Type == "Direct Assistance" &
          AGD.Sum.Calculated != New.beneficiaries.of.the.month ~ 1L,
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
    select(-tmp_youth_sum) |>
    mutate(
      QA_valid_cva = is_valid_cva(CVA, Value..in.USD., Delivery.mechanism),
      QA_admin     = check_admin_validity(Country.Country, Country.Admin1, countryListDF),
      QA_indicator = check_indicator_validity(Indicator.Sector, Indicator.Indicator, indicatorDF)
    ) |>
    rowwise() |>
    mutate(QA_sum = as.integer(sum(c_across(starts_with("QA_")), na.rm = TRUE) > 0)) |>
    ungroup()
}
