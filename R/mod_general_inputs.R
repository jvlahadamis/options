#' general_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#'
#' @noRd
#' @import dplyr
#' @import purrr
#' @import BatchGetSymbols
#' @import shiny
#' @import tidyr
#' @import rvest
#' @import tibble
#' @import tidyquant
mod_general_inputs_server <- function(id, r){

  moduleServer(id, function(input, output, session){

    ns <- session$ns


    sp500 <- BatchGetSymbols::GetSP500Stocks() %>%
      dplyr::select(Tickers, SEC.filings)

    r$sp500 <- reactive({sp500})


    df <- readr::read_csv("https://www.dolthub.com/csv/jvlahadamis/earnings/master/earnings_calendar?include_bom=0")


    r$vol <- readr::read_csv("https://www.dolthub.com/csv/post-no-preference/options/master/volatility_history?include_bom=0")



    # saveRDS(df, "savedf")
    # saveRDS(vol, "voldf")
    #df <- readRDS("savedf")
    # r$vol <- readRDS("voldf")


    tolisten <- reactive({list(r$sector, r$month)})

    shiny::observeEvent(tolisten(), {

      calendar <- reactive({
       # browser()
      df %>% dplyr::filter(date> "2024-01-01") %>%
        dplyr::filter(act_symbol %in% r$sp500()$Tickers) %>%
        tidyr::drop_na() %>%
        merge(r$sp500(), by.x = "act_symbol", by.y = "Tickers") %>%
        dplyr::rename("Sector" = "SEC.filings")})


    r$month_select <- reactive({
      # browser()
      calendar() %>% dplyr::mutate(month = format(date, "%B")) %>%
        dplyr::mutate(month_number = stringr::str_sub(date, start = 6, end = 7)) %>%
        dplyr::mutate(month_number = dplyr::case_when(month_number %>%
                                                        stringr::str_sub(start = 1, end = 1) == "0" ~ month_number
                                                      %>% stringr::str_sub(start = 2, end = 2),
                                                      TRUE ~ month_number))})



      r$month_select_mod <- reactive({
        # browser()
        calendar() %>% dplyr::mutate(month = format(date, "%B")) %>%
          dplyr::mutate(month_number = stringr::str_sub(date, start = 6, end = 7)) %>%
          dplyr::mutate(month_number = dplyr::case_when(month_number %>%
                                                          stringr::str_sub(start = 1, end = 1) == "0" ~ month_number
                                                        %>% stringr::str_sub(start = 2, end = 2),
                                                        TRUE ~ month_number)) %>%
          dplyr::filter(Sector %in% r$sector) %>%
          dplyr::filter(month %in% r$month) %>%
          dplyr::filter(date > Sys.Date())})



  step_1 <- reactive({
    # # browser()
    date_codes() %>%
      dplyr::rename(options_string = value)})


  options_dates <- reactive({
    step_1() %>%
      dplyr::mutate(options_date = options_string %>% qdapRegex::ex_between(">", "<"))})


    r$go <- reactive({
      # browser()
      calendar() %>% dplyr::mutate(month = format(date, "%B")) %>%
        dplyr::mutate(month_number = stringr::str_sub(date, start = 6, end = 7)) %>%
        dplyr::mutate(month_number = dplyr::case_when(month_number %>%
                                                        stringr::str_sub(start = 1, end = 1) == "0" ~ month_number
                                                      %>% stringr::str_sub(start = 2, end = 2),
                                                      TRUE ~ month_number)) %>%
        dplyr::filter(Sector == r$sector) %>%
        dplyr::mutate(monthday = date %>%
                        stringr::str_sub(start = 9, end = 10)) %>%
        dplyr::filter(month %in% r$month) %>%
        dplyr::group_by(monthday) %>%
        dplyr::select(act_symbol, monthday) %>%
        tidyr::nest()})




    r$get_links <- reactive({
      # browser()
      calendar() %>% dplyr::mutate(month = format(date, "%B")) %>%
        dplyr::mutate(month_number = stringr::str_sub(date, start = 6, end = 7)) %>%
        dplyr::mutate(month_number = dplyr::case_when(month_number %>%
                                                        stringr::str_sub(start = 1, end = 1) == "0" ~ month_number
                                                      %>% stringr::str_sub(start = 2, end = 2),
                                                      TRUE ~ month_number)) %>%
        dplyr::filter(Sector == r$sector) %>%
        dplyr::mutate(monthday = date %>%
                        stringr::str_sub(start = 9, end = 10)) %>%
        dplyr::filter(month %in% r$month) %>%
        dplyr::group_by(monthday) %>%
        dplyr::select(act_symbol, monthday) %>%
        dplyr::ungroup() %>%
        dplyr::select(act_symbol) %>%
        merge(r$month_select_mod(), by.x ="act_symbol", by.y = "act_symbol") %>%
        dplyr::select(act_symbol, date) %>%
        dplyr::mutate(links =paste0("https://ca.finance.yahoo.com/quote/",act_symbol,"/options"))})

    }
  )}


  )}



## To be copied in the UI
# mod_general_inputs_ui("general_inputs_1")

## To be copied in the server
# mod_general_inputs_server("general_inputs_1")
