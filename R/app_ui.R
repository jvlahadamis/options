#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
#'
#'
#'
app_ui <- function(request) {
  shiny::fluidPage(
    theme = bslib::bs_theme(bootswatch = "cyborg"),
    ############################################################
    # Options + Earnings                                       #
    shiny::titlePanel("Options + Earnings"),                   #
    ## Select a sector                                         #
    mod_name_of_module3_test_ui("name_of_module3_test_1"),     #
    ## Select a month                                          #
    mod_month_ui("month_1"),                                   #
    ############################################################


    shiny::tabsetPanel(type = "tabs",
                       shiny::tabPanel("User Manual", mod_maunal_ui("maunal_1")),
                       shiny::tabPanel("Earnings Calendar", mod_name_of_module1_ui("name_of_module1_1")),
                       shiny::tabPanel("Option Implied Volatility Percentile",mod_implied_vol_percentile_ui("implied_vol_percentile_1")),
                       shiny::tabPanel("Option Chain", mod_greeks_ui("greeks_1"))
  ))
  }

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "optionsearnings"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}




