#' name_of_module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#' @import purrr
#' @import shiny
#' @import tidyr
#' @import rvest
mod_calendar_ui <- function(id){


  ns <- NS(id) # namespace


  shiny::tagList(title = "",
                      #shiny::sidebarLayout(


                        shiny::mainPanel(



                        # Calendar Output
                          # shiny::mainPanel(
                      shiny::br(),
                      shinycssloaders::withSpinner(shiny::plotOutput(ns("calendar"),
                                                            width = "130%",
                                                            height = 900))))


  }



#' name_of_module1 Server Functions
#'
#' @noRd
mod_calendar_server <- function(id, r){
  moduleServer(id, function(input, output, session){

    ns <- session$ns


    day <- reactive({
    # browser()
      r$go()$monthday %>% as.double()

    })

    # companies reporting
    companies_ <- reactive({
     # browser()
      r$go()$data %>%
        purrr::map(.f = ~ paste0((.x %>% as.data.frame())$act_symbol, "\n") %>%
                     stringr::str_c(collapse = ""))

    })


    # calendar
    output$calendar <- shiny::renderPlot({


      # browser()
      calendR::calendR(

        year = stringr::str_sub(Sys.Date(), start = 1, end = 4) %>% as.double(),

        month = r$month_select() %>%
          dplyr::filter(Sector %in% r$sector) %>%
          dplyr::mutate(monthday = date %>%
                          stringr::str_sub(start = 9, end = 10)) %>%
          dplyr::filter(month %in% r$month) %>%
          dplyr::slice(1) %>%
          dplyr::select(month_number) %>% as.double(),

        months.col = "white",

        # subtitle
        subtitle = r$sector,
        subtitle.col = "white",
        subtitle.size = 20,

        orientation = "l",
        weeknames.col = "white",
        title.col = "white",
        title.size = 30,
        days.col = "black",
        day.size = 5,
        bg.col = "black",
        lwd = 1,
        lty = 1,
        font.family = "Roboto",
        font.style = "bold",
        start = "S",
        special.days = r$go()$monthday %>% as.double(),
        special.col = "lightblue",
        text = companies_(),
        text.pos = r$go()$monthday %>% as.double(),
        text.size = 5,
        text.col = "#000000",

        margin = 0.5)
    })

  })}


## To be copied in the UI
# mod_calendar_ui("calendar_1")

## To be copied in the server
# mod_calendar_server("calendar_1")



