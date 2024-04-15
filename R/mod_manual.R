#' maunal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_manual_ui <- function(id){
  ns <- NS(id)



  shiny::tagList(
  shiny::tabPanel(title = "User Manual",
                  # shiny::sidebarLayout(

                    # shiny::sidebarPanel(
                  shiny::br(),
                  shiny::br(),
                  shinycssloaders::withSpinner(shiny::uiOutput(ns("text_output"))))
  )}



#' maunal Server Functions
#'
#' @noRd
mod_manual_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    output$text_output <- shiny::renderUI(
      shiny::div(
        shiny::h1(strong("This app is designed to assist traders in placing options trades during
                  earnings season.")),
        shiny::br(),
        shiny::br(),
        shiny::h2("The app consists of three main tabs which are described below."),
        shiny::br(),
        shiny::h3("1.  Earnings Calendar"),

            shiny::h4("This tab displays the earnings calendar, by sector and month for
            companies in the S&P 500. Simply choose a sector at the top along
            with the month you wish to view.

            Note: only companies with confirmed earnings releases are displayed."),
        shiny::br(),
        shiny::br(),
        shiny::h3("2.  Percentile of Options implied Volatility"),
         shiny::h4("This tab displays an important risk metric - the current percentile of
          the option implied volatility -
          measured as percentage of time the options implied volatlilty has
          been greater than it's current value over the past 252 trading days.
          The Implied Volatility percentile can help traders assess position
          directionality, position size, and the overall volatility picture for
          options on companies in the given sector."),
        shiny::br(),
        shiny::br(),
        shiny::h3("3.  Options Chain for Friday after earnings release"),

          shiny::h4("This tab pulls the option chain for the Friday immediately following each company's earnings release. When the
                    action button is clicked, the option chain for the currently selected sector and will be pulled."),
        shiny::br(),
        shiny::br(),
        shiny::br()

        )
    )})}








## To be copied in the UI
# mod_manual_ui("manual_1")

## To be copied in the server
# mod_manual_server("manual_1")
