#' option_chain_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#' @import purrr
#' @import shiny
#' @import tidyr
#' @import rvest
#' @import tibble
#' @import tidyquant
#' @import quantmod
#' @import shinycssloaders
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
mod_option_chain_display_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::tagList(

      title = "Options Chain - Week of Earnings",

      shiny::sidebarLayout(

        shiny::sidebarPanel(

          shiny::actionButton(ns("generator"),"Get Options Chain"),
          width = 5),

        shiny::mainPanel(
          shinycssloaders::withSpinner(DT::DTOutput(ns("option_chain")))
        )))

  )
}

#' option_chain_display Server Functions
#'
#' @noRd
mod_option_chain_display_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns




    shiny::observeEvent(input$generator, {


      output$option_chain <- DT::renderDT({

        r$output_table
      })



    })



  })
}

## To be copied in the UI
# mod_option_chain_display_ui("option_chain_display_1")

## To be copied in the server
# mod_option_chain_display_server("option_chain_display_1")
