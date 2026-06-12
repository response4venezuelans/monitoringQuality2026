library(shiny)
library(activityinfo)
library(httr2)
library(dplyr)
library(bslib)
library(gridlayout)
library(DT)
source("functions.R")

#Define Custom Theme
# Define custom theme using the provided color palette
R4Vtheme <- bs_theme(
  bg = "#f0f2f5",  # Light gray background
  fg = "#132A3E",  # Dark blue foreground
  primary = "#00AAAD",  # Teal primary color
  secondary = "#902857",  # Dark pink secondary color
  success = "#72BF44",  # Green success color
  info = "#129ABF",  # Light blue info color
  warning = "#FEBE10",  # Yellow warning color
  danger = "#e83f54",  # Red danger color
  base_font = font_google("Inter"),
  code_font = font_google("JetBrains Mono")
)

activityInfoToken(Sys.getenv("ACTIVITYINFOTOKEN"), prompt = FALSE)
# Get from Activity Info data Lists
# Monitoring framework
# Admin list
# Organization list
# Country list

countryListDF <- queryTable("cnkrge1m07falxuu7o",
                 "Country" = "c8u26b8kxeqpy0k4",
                 "Admin1" = "c3ns3zikxeqq4h95",
                 "Admin1ISOCode" = "cl3sspjkxeqq8yq6",
                 "countryISO" = "c1u8kphm4vqtemz2")

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

activities_icon <-
  HTML(
    '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-bar-chart-line-fill" viewBox="0 0 16 16">
  <path d="M11 2a1 1 0 0 1 1-1h2a1 1 0 0 1 1 1v12h.5a.5.5 0 0 1 0 1H.5a.5.5 0 0 1 0-1H1v-3a1 1 0 0 1 1-1h2a1 1 0 0 1 1 1v3h1V7a1 1 0 0 1 1-1h2a1 1 0 0 1 1 1v7h1z"/>
</svg>'
  )
error_icon <-
  HTML(
    '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-bandaid" viewBox="0 0 16 16">
  <path d="M14.121 1.879a3 3 0 0 0-4.242 0L8.733 3.026l4.261 4.26 1.127-1.165a3 3 0 0 0 0-4.242M12.293 8 8.027 3.734 3.738 8.031 8 12.293zm-5.006 4.994L3.03 8.737 1.879 9.88a3 3 0 0 0 4.241 4.24l.006-.006 1.16-1.121ZM2.679 7.676l6.492-6.504a4 4 0 0 1 5.66 5.653l-1.477 1.529-5.006 5.006-1.523 1.472a4 4 0 0 1-5.653-5.66l.001-.002 1.505-1.492z"/>
  <path d="M5.56 7.646a.5.5 0 1 1-.706.708.5.5 0 0 1 .707-.708Zm1.415-1.414a.5.5 0 1 1-.707.707.5.5 0 0 1 .707-.707M8.39 4.818a.5.5 0 1 1-.708.707.5.5 0 0 1 .707-.707Zm0 5.657a.5.5 0 1 1-.708.707.5.5 0 0 1 .707-.707ZM9.803 9.06a.5.5 0 1 1-.707.708.5.5 0 0 1 .707-.707Zm1.414-1.414a.5.5 0 1 1-.706.708.5.5 0 0 1 .707-.708ZM6.975 9.06a.5.5 0 1 1-.707.708.5.5 0 0 1 .707-.707ZM8.39 7.646a.5.5 0 1 1-.708.708.5.5 0 0 1 .707-.708Zm1.413-1.414a.5.5 0 1 1-.707.707.5.5 0 0 1 .707-.707"/>
</svg>'
  )
percent_icon <-
  HTML(
    '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-percent" viewBox="0 0 16 16">
  <path d="M13.442 2.558a.625.625 0 0 1 0 .884l-10 10a.625.625 0 1 1-.884-.884l10-10a.625.625 0 0 1 .884 0M4.5 6a1.5 1.5 0 1 1 0-3 1.5 1.5 0 0 1 0 3m0 1a2.5 2.5 0 1 0 0-5 2.5 2.5 0 0 0 0 5m7 6a1.5 1.5 0 1 1 0-3 1.5 1.5 0 0 1 0 3m0 1a2.5 2.5 0 1 0 0-5 2.5 2.5 0 0 0 0 5"/>
</svg>'
  )
