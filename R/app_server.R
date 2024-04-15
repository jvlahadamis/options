#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
#'
#'
app_server <- function(input, output, session) {

  # all servers modules go here
  r <- reactiveValues()

  # user manual
  mod_manual_server("manual_1")

  # small r sector
  mod_sector_server("sector_1", r = r)

  # small r month
  mod_month_server("month_1", r = r)

  # small r month select and "go"
  mod_general_inputs_server("general_inputs_1", r = r)

  # Earnings Calendar # done
  mod_calendar_server("calendar_1", r = r)

  # IV chart
  mod_implied_vol_percentile_server("implied_vol_percentile_1", r = r)

  # Option Chain
  mod_option_chain_server("option_chain_1", r = r)

  mod_option_chain_display_server("option_chain_display_1", r = r)
}



