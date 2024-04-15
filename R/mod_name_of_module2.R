#' name_of_module2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_name_of_module2_ui <- function(id){


  ns <- NS(id) # namespace
      shiny::tabPanel(title = "Small r Earnings Calendar + Percentile(Options IV)",
                      shiny::sidebarLayout(
                        shiny::sidebarPanel(),



                          plotly::plotlyOutput(ns("ivrank"),
                                               height = 700,
                                               width = "175%")))
}



#' name_of_module1 Server Functions
#'
#' @noRd
mod_name_of_module2_server <- function(id, r){
  moduleServer(id, function(input, output, session){

    ns <- session$ns

    ##############################################################################################################################
    calendar <- reactive({
      # browser()
      df %>% dplyr::filter(date> "2024-01-01") %>%
        dplyr::filter(act_symbol %in% sp500$Tickers) %>%
        tidyr::drop_na() %>%
        merge(sp500, by.x = "act_symbol", by.y = "Tickers") %>%
        dplyr::rename("Sector" = "SEC.filings")

    })

    month_select <- reactive({
      # browser()
      calendar() %>% dplyr::mutate(month = format(date, "%B")) %>%
        dplyr::mutate(month_number = stringr::str_sub(date, start = 6, end = 7)) %>%
        dplyr::mutate(month_number = dplyr::case_when(month_number %>%
                                                        stringr::str_sub(start = 1, end = 1) == "0" ~ month_number
                                                      %>% stringr::str_sub(start = 2, end = 2),
                                                      TRUE ~ month_number))

    })

    go <- reactive({
      # browser()
      r$month_select() %>%
        dplyr::filter(Sector == r$sector) %>%
        dplyr::mutate(monthday = date %>%
                        stringr::str_sub(start = 9, end = 10)) %>%
        dplyr::filter(month %in% input$month_input) %>%
        dplyr::group_by(monthday) %>%
        dplyr::select(act_symbol, monthday) %>%
        tidyr::nest()})

    # day of earnings

    day <- reactive({
      #  browser()
      go()$monthday %>% as.double()

    })

    # companies reporting
    companies_ <- reactive({

      # browser()
      go()$data %>%
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
          dplyr::filter(month %in% input$month_input) %>%
          dplyr::slice(1) %>%
          dplyr::select(month_number) %>% as.double(),
        months.col = "white",
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
        start = "M",
        special.days = go()$monthday %>% as.double(),
        special.col = "lightblue",
        text = companies_(),
        text.pos = go()$monthday %>% as.double(),
        text.size = 4,
        text.col = "#000000",

        margin = 0.5)
    })

    ############################################################################################################################

    #df <- readr::read_csv("https://www.dolthub.com/csv/jvlahadamis/earnings/master/earnings_calendar?include_bom=0")
    # df %>% saveRDS("savedf")
    #vol <- readr::read_csv("https://www.dolthub.com/csv/post-no-preference/options/master/volatility_history?include_bom=0")

    #vol <- readRDS(system.file("inst", "voldf.rds", package = "yourgolem"))
    #df <- readRDS(system.file("inst", "savedf.rds", package = "yourgolem"))
    df <- readRDS("savedf.rds")
    vol <- readRDS("voldf.rds")
    sp500 <- BatchGetSymbols::GetSP500Stocks() %>% dplyr::select(Tickers, SEC.filings)


    t <- reactive({

      t <- vol %>%
        dplyr::arrange(desc(date)) %>%
        dplyr::filter(date > Sys.Date() - 365) %>%
        dplyr::filter(act_symbol %in% sp500$Tickers) %>%
        dplyr::group_by(act_symbol)
      fill_current_iv <- t %>%
        dplyr::select(date, iv_current) %>%
        tidyr::pivot_wider(names_from = "act_symbol", values_from = "iv_current") %>%
        dplyr::slice(1) %>%
        tidyr::pivot_longer(-date, names_to = "act_symbol",
                            values_to = "current_iv") %>%
        dplyr::select(act_symbol, current_iv) %>%
        dplyr::group_by(act_symbol)
      t %>%
        merge(fill_current_iv, by.x = "act_symbol", by.y = "act_symbol") %>%
        dplyr::mutate(count_of = dplyr::case_when(iv_current < current_iv ~ 1,  TRUE ~0)) %>%
        merge(sp500, by.x = "act_symbol", by.y = "Tickers")
    })

    # iv rank
    output$ivrank <- plotly::renderPlotly({


      go() %>% tidyr::unnest()

      t() %>%
        dplyr::filter(act_symbol %in% (go() %>% tidyr::unnest())$act_symbol) %>%
        dplyr::group_by(act_symbol) %>%
        dplyr::filter(SEC.filings %in% r$sector) %>%
        dplyr::summarise(iv_percentile = sum(count_of)/252) %>%
        plotly::plot_ly(
          x = ~act_symbol,
          y = ~iv_percentile,
          name  = ~act_symbol,
          color = ~act_symbol) %>%
        #type
        plotly::add_trace() %>%
        # layout and format
        plotly::layout(
          title = list(text = paste0("Implied Volatility Percentile
                                     Aggregate of Option Contracts for ", r$sector, " companies reporting in ",
                                     r$month),
                       y = 0.9,
                       x = 20),

          xaxis = list(title = list(text = "Company")),

          yaxis = list(title = list(text = "IV Percentile")))

    })



  })}












## To be copied in the UI
# mod_name_of_module2_ui("name_of_module2_1")

## To be copied in the server
# mod_name_of_module2_server("name_of_module2_1")




