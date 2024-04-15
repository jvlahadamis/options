#' name_of_module3_test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList


mod_name_of_module3_test_ui <- function(id){
  ns <- NS(id)


    sectoring <- c(BatchGetSymbols::GetSP500Stocks() %>%
      dplyr::select(SEC.filings) %>%
      unique())

shiny::tagList(

      shiny::radioButtons(ns("sector_inp"),
                          "Select Sector",
                          choices = sectoring$SEC.filings,
                          selected = "Communication Services",
                          inline = TRUE)
      )


}


#################################################################

#' name_of_module3_test Server Functions
#'
#' @noRd
mod_name_of_module3_test_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    shiny::observeEvent(input$sector_inp,{
      r$sector <- input$sector_inp})
    })
}


## To be copied in the UI
# mod_name_of_module3_test_ui("name_of_module3_test_1")

## To be copied in the server
# mod_name_of_module3_test_server("name_of_module3_test_1")
















