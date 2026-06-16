library(shiny)
library(activityinfo)
library(bslib)
library(DT)
library(waiter)
library(readxl)
library(writexl)
library(httr2)
library(dplyr)
library(stringr)

# Allow file uploads up to 50MB on Posit Connect (default is too restrictive and returns 403)
options(shiny.maxRequestSize = 50 * 1024^2)

activityInfoToken(Sys.getenv("ACTIVITYINFOTOKEN"), prompt = FALSE)

# queryTable() (used below) calls the activityinfo package's internal httr
# GET/POST helpers, which expose no timeout parameter of their own. A process-
# wide httr timeout ensures a slow/unresponsive ActivityInfo API aborts the
# underlying curl transfer instead of hanging app startup indefinitely (Posit
# Connect kills the whole worker if startup doesn't finish within its own
# init timeout, which is what caused a previous 504 Gateway Time-out).
httr::set_config(httr::timeout(30))

# Needed immediately below to fetch startup reference data; everything else in
# R/ is auto-sourced by Shiny after global.R runs, so it doesn't need this.
source("R/fct_activityinfo.R")

# Get from Activity Info data Lists
# Monitoring framework
# Admin list
# Organization list
# Country list

countryListDF <- tryCatch(
  queryTable("cnkrge1m07falxuu7o",
             "Country" = "c8u26b8kxeqpy0k4",
             "Admin1" = "c3ns3zikxeqq4h95",
             "Admin1ISOCode" = "cl3sspjkxeqq8yq6",
             "countryISO" = "c1u8kphm4vqtemz2"),
  error = function(e) {
    message("ERROR fetching country list: ", conditionMessage(e))
    tibble(Country = character(0), Admin1 = character(0),
           Admin1ISOCode = character(0), countryISO = character(0))
  }
)

countryList <-  unique(countryListDF$Country)
# Include All to get all data
countryList <- c(countryList, "All")

df_partnersDF <- tryCatch(
  fetch_ai_form("ccopwnzmjrensxpf96"),
  error = function(e) {
    message("ERROR fetching partners form: ", conditionMessage(e))
    tibble(Name = character(0))
  }
)
partnerList <- c(unique(df_partnersDF$Name), "All")

indicators_ref_2026 <- tryCatch(
  fetch_ai_form("c17x28umnqepjii1ho"),
  error = function(e) {
    message("ERROR fetching 2026 indicator reference: ", conditionMessage(e))
    tibble(`_id` = character(0), sector = character(0),
           indicator_simplified = character(0), indictatortype = character(0))
  }
)

indicators_2026_types <- indicators_ref_2026 |>
  select(`_id`, indictatortype) |>
  rename(indicator_ref = `_id`, indicator_type = indictatortype)

indicatorDF <- tryCatch(
  indicators_ref_2026 |>
    rename(
      CODE           = `_id`,
      Sector         = sector,
      Indicator      = indicator_simplified,
      Indicator.Type = indictatortype
    ),
  error = function(e) {
    message("ERROR building indicatorDF from 2026 reference: ", conditionMessage(e))
    tibble(CODE = character(0), Sector = character(0),
           Indicator = character(0), Indicator.Type = character(0))
  }
)
