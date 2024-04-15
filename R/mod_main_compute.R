#' main_compute UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_main_compute_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' main_compute Server Functions
#'
#' @noRd 
mod_main_compute_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_main_compute_ui("main_compute_1")
    
## To be copied in the server
# mod_main_compute_server("main_compute_1")
