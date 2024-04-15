#' month UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_month_ui <- function(id){
  ns <- NS(id)




    monthing <- c("April", "May")

    shiny::tagList(
      shiny::radioButtons(ns("month_inp"),
                          "Select Month",
                          choices = monthing,
                          selected = "April",
                          inline = TRUE))
      }


######################################################

#' month Server Functions
#'
#' @noRd
mod_month_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$month_inp, {


      r$month <- input$month_inp


  })
})}








## To be copied in the UI
# mod_month_ui("month_1")

## To be copied in the server
# mod_month_server("month_1")
