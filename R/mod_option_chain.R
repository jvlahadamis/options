#' greeks UI Function
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
#'
#'
#' @noRd
mod_option_chain_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::tagList(

      title = "Options Chain - Friday Following Earnings Release",

      shiny::sidebarLayout(

        shiny::sidebarPanel(

          shiny::actionButton(ns("generator"),"Get Options Chain"),
          width = 5),

        shiny::mainPanel(
          shinycssloaders::withSpinner(DT::DTOutput(ns("option_chain")))
        )))

  )
}


mod_option_chain_server <- function(id, r){
  moduleServer(id, function(input, output, session){

    ns <- session$ns



    shiny::observeEvent(input$generator, {


    prices <- reactive({
      # # browser()
      tidyquant::tq_get(x = r$get_links()$act_symbol,
                                get = "stock.prices",
    ) %>% dplyr::filter(date == max(date)) %>%
      dplyr::select(symbol, adjusted)

      })


## use curl::curl
    date_codes <- reactive({
      Sys.sleep(20)

      z <- reactive({r$get_links()})

      n <- reactive({r$get_links()$links %>%
      purrr::map(.f = ~ rvest::read_html(.x) %>% rvest::html_nodes("option") %>% as.character())})

      u <- reactive({n() %>% purrr::map(.f = ~tibble::as_tibble(.x)) %>%
        purrr::map(.f = ~tidyr::nest(.x)) %>%
        dplyr::bind_rows()})

      z() %>% dplyr::mutate(date_codes = u()) %>% tidyr::unnest(cols = c(date_codes)) %>% tidyr::unnest(cols = c(data))

    })

    step_1 <- reactive({
      # # browser()
      date_codes() %>%
      dplyr::rename(options_string = value)
      })

    options_dates <- reactive({
      step_1() %>%
        dplyr::mutate(options_date = options_string %>% qdapRegex::ex_between(">", "<"))
      })

      ############################################################################

    t <- reactive({
      # browser()
        r$go() %>% tidyr::unnest(cols = c(data)) %>%
        dplyr::ungroup() %>%
        dplyr::select(act_symbol) %>%
        merge(r$month_select_mod(), by.x ="act_symbol", by.y = "act_symbol") %>%
        dplyr::select(act_symbol, date) %>%
        dplyr::mutate(earnings_date_plus_seven = as.Date(date) + 4) %>%
        dplyr::mutate(date = as.Date(date)) %>%
        dplyr::mutate(earnings_date = date)

    })

    snipe <- reactive({

      options_dates() %>%
      dplyr::mutate(options_date = options_date %>% stringr::str_replace_all(",", "")) %>%
      dplyr::mutate(options_date = as.Date(options_date, format = "%B %d %Y")) %>%
        merge(t(), by.x = "act_symbol", by.y ="act_symbol") %>%
        dplyr::group_by(act_symbol) %>%
      dplyr::filter(options_date >= earnings_date & options_date <= earnings_date_plus_seven) %>%
      dplyr::mutate(options_date = as.character(options_date)) %>% as.list()

      })

      get_links_filtered <- reactive({

        r$get_links() %>% dplyr::filter(act_symbol %in% snipe()$act_symbol)

      })

    work <- reactive({
      # browser()
      purrr::map2(.x = snipe()$act_symbol,
                  .y = snipe()$options_date,
                  .f = ~quantmod::getOptionChain(Symbols = .x,
                                                 Exp = .y))
      })

    vol <- reactive({
      # browser()
      r$vol %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::filter(date > Sys.Date()-7) %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::filter(act_symbol %in% get_links_filtered()$act_symbol) %>%
      dplyr::select(date,
                    act_symbol,
                    hv_current,
                    hv_year_high,
                    hv_year_low,
                    iv_current,
                    iv_year_high,
                    iv_year_low,
                    iv_week_ago)

    })

    call <- reactive({
      # browser()
      check <- work() %>%
        purrr::map(.f = ~ dplyr::bind_rows(.x[]$calls)) %>%
        purrr::map(.f = ~tibble::as_tibble(.x)) %>%
        purrr::map(.f = ~tidyr::nest(.x)) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(act_symbol = get_links_filtered()$act_symbol) %>%
        tidyr::unnest(cols = c(data)) %>%
        dplyr::mutate(expiry = as.Date(Expiration)) %>%
        dplyr::mutate(call_or_put = as.character("call")) %>%
      dplyr::select(act_symbol,
                    call_or_put,
                    expiry,
                    Strike,
                    Last,
                    Bid,
                    Ask,
                    Vol,
                    OI,
                    ITM) %>%
      tidyr::drop_na() %>%
      merge(vol(), by.x = "act_symbol", by.y = "act_symbol") %>%
      dplyr::select(1:12) %>%
      merge(prices(), by.x = "act_symbol",
            by.y = "symbol") %>%
      dplyr::mutate(DTE = difftime(expiry, date)) %>%
      dplyr::mutate(DTE = DTE %>% stringr::str_replace_all("days", "")) %>%
      dplyr::mutate(DTE = as.double(DTE)) %>%
      dplyr::mutate(dividend = 0)

    })

    puts <- reactive({
      # browser()
      work() %>%
        purrr::map(.f = ~ dplyr::bind_rows(.x[]$puts)) %>%
        purrr::map(.f = ~tibble::as_tibble(.x)) %>%
        purrr::map(.f = ~tidyr::nest(.x)) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(act_symbol = get_links_filtered()$act_symbol) %>%
        tidyr::unnest(cols = c(data)) %>%
        dplyr::mutate(expiry = as.Date(Expiration)) %>%
        dplyr::mutate(call_or_put = as.character("put")) %>%
        dplyr::select(act_symbol,
                      call_or_put,
                      expiry,
                      Strike,
                      Last,
                      Bid,
                      Ask,
                      Vol,
                      OI,
                      ITM) %>%
        tidyr::drop_na() %>%
        merge(vol(), by.x = "act_symbol", by.y = "act_symbol") %>%
        dplyr::select(1:12) %>%
        merge(prices(), by.x = "act_symbol", by.y = "symbol") %>%
        dplyr::mutate(DTE = difftime(expiry, date)) %>%
        dplyr::mutate(DTE = DTE %>% stringr::str_replace_all("days", "")) %>%
        dplyr::mutate(DTE = as.double(DTE)) %>%
        dplyr::mutate(dividend = 0)

    })


    output_table <- reactive({call() %>% dplyr::bind_rows(puts()) %>%
        dplyr::select(act_symbol,
                      call_or_put,
                      expiry,
                      Strike,
                      Last,
                      Bid,
                      Ask,
                      Vol,
                      OI,
                      ITM) %>%
        tidyr::drop_na() %>%
        merge(vol(), by.x = "act_symbol",
              by.y = "act_symbol") %>%
        dplyr::select(1:12) %>%
        merge(prices(), by.x = "act_symbol",
              by.y = "symbol") %>%
        dplyr::mutate(DTE = difftime(expiry, date)) %>%
        dplyr::mutate(DTE = DTE %>% stringr::str_replace_all("days", "")) %>%
        dplyr::mutate(DTE = as.double(DTE)) %>%
        dplyr::mutate(dividend = 0) %>%
        dplyr::select(1:12, 14) %>%
        dplyr::rename(Ticker = act_symbol,
                      CallPut = call_or_put,
                      Expiry = expiry,
                      historical_vol = hv_current,
                      Days_to_Expiry = DTE)

      #%>% shiny::bindEvent(input$generator)

    })


    output$option_chain <- DT::renderDT({
      #shiny::req(output_table())

      shiny::isolate(output_table())
    })


  }, ignoreNULL = TRUE)


})
}


## To be copied in the UI
# mod_option_chain_ui("option_chain_1")

## To be copied in the server
# mod_option_chain_server("option_chain_1")








