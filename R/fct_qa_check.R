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
      # New beneficiaries of the month is no longer separately reported: it's
      # only trustworthy when ActivityInfo's own AGD and population-type
      # breakdown sums agree with each other (Direct Assistance only).
      New.beneficiaries.of.the.month = case_when(
        Indicator.Indicator.Type == "Direct Assistance" &
          !is.na(PopType.Sum.Calculated) & !is.na(AGD.Sum.Calculated) &
          PopType.Sum.Calculated == AGD.Sum.Calculated ~ PopType.Sum.Calculated,
        .default = NA_real_
      )
    ) |>
    mutate(
      QA_output = is_valid_output(Indicator.Indicator.Type, Quantity.of.output),
      QA_TotalMonthlyBeneficiaries = is_valid_total_beneficiaries_of_month(
        Indicator.Indicator.Type, Total.monthly.beneficiaries
      ),
      QA_NewBeneficiariesMonth = is_valid_new_beneficiaries_of_month(
        Indicator.Indicator.Type, New.beneficiaries.of.the.month
      ),
      QA_NewBeneficiariesExceedsTotal = case_when(
        Indicator.Indicator.Type == "Direct Assistance" &
          !is.na(New.beneficiaries.of.the.month) &
          New.beneficiaries.of.the.month > Total.monthly.beneficiaries ~ 1L,
        .default = 0L
      )
    ) |>
    mutate(
      tmp_youth_sum = rowSums(across(all_of(qa_youth_columns)), na.rm = TRUE)
    ) |>
    mutate(
      # Cross-check: ActivityInfo's AGD breakdown sum and population-type
      # breakdown sum should always agree with each other for Direct
      # Assistance records; both flags surface the same mismatch.
      QA_check_population_disaggregation = case_when(
        Indicator.Indicator.Type == "Direct Assistance" &
          PopType.Sum.Calculated != AGD.Sum.Calculated ~ 1L,
        .default = 0L
      ),
      QA_check_AGD = case_when(
        Indicator.Indicator.Type == "Direct Assistance" &
          PopType.Sum.Calculated != AGD.Sum.Calculated ~ 1L,
        .default = 0L
      ),
      # Zero new beneficiaries this month is valid on its own (everyone was
      # already supported in a previous month), so only flag when there ARE
      # new beneficiaries but none of them are recorded as under-18.
      QA_Education = case_when(
        Indicator.Indicator.Type == "Direct Assistance" &
          str_detect(Indicator.Sector, fixed("Education")) &
          New.beneficiaries.of.the.month > 0 &
          tmp_youth_sum == 0 ~ 1L,
        .default = 0L
      ),
      QA_ChildProtection = case_when(
        Indicator.Indicator.Type == "Direct Assistance" &
          str_detect(Indicator.Sector, fixed("Child Protection")) &
          New.beneficiaries.of.the.month > 0 &
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
    mutate(QA_sum = as.integer(rowSums(across(starts_with("QA_")), na.rm = TRUE) > 0))
}
