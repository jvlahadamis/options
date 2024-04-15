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
  mod_maunal_server("maunal_1")

  # small r sector
  mod_name_of_module3_test_server("name_of_module3_test_1", r = r)

  # small r month
  mod_month_server("month_1", r = r)

  # small r month select and "go"
  mod_general_inputs_server("general_inputs_1", r = r)

  # Earnings Calendar
  mod_name_of_module1_server("name_of_module1_1", r = r)

  # IV chart
  mod_implied_vol_percentile_server("implied_vol_percentile_1", r = r)

  mod_greeks_server("greeks_1", r = r)


}



