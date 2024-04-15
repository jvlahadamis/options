#' implied_vol_percentile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_implied_vol_percentile_ui <- function(id){
  ns <- NS(id)

  shiny::tagList(
    shiny::br(),
    shiny::br(),
    # type = (),
    title = "",
                    #shiny::sidebarLayout(
                     # shiny::sidebarPanel(),
                      # Calendar Output
                      #shiny::mainPanel(
                        shiny::br(),
                    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("ivrank"),
                                             height = 800,
                                             width = "80%")),
    )
    }

#' implied_vol_percentile Server Functions
#'
#' @noRd
mod_implied_vol_percentile_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    vol <- reactive({r$vol})

    t <- reactive({

      t <- vol() %>%
        dplyr::arrange(desc(date)) %>%
        dplyr::filter(date > Sys.Date() - 365) %>%
        dplyr::filter(act_symbol %in% r$sp500()$Tickers) %>%
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
        merge(r$sp500(), by.x = "act_symbol", by.y = "Tickers")

    })

    # iv rank
    output$ivrank <- plotly::renderPlotly({



      t <- list(
        family = "Roboto",
        size = 10,
        color = "black")

t1 <- list(
  family = "Roboto",
  color = "black",
  size = 20)

      t2 <- list(
        family = "Roboto",
        size = 12,
        color = "black")

      t()  %>%
        dplyr::filter(act_symbol %in% (r$go() %>% tidyr::unnest())$act_symbol) %>%
        dplyr::group_by(act_symbol) %>%
        dplyr::filter(SEC.filings %in% r$sector) %>%
        dplyr::summarise(iv_percentile = sum(count_of)/252) %>%


        plotly::plot_ly(
          x = ~act_symbol,
          y = ~iv_percentile,
          name  = ~act_symbol,
          color = ~act_symbol) %>%


        plotly::add_trace() %>%

        plotly::layout(
          title = list(text = paste0("Implied Volatility Percentile of Option Contracts for ", r$sector, " companies reporting in ", r$month),
                       y = 0.95,
                       x = 10,
                       font = t1), font = t,

          xaxis = list(title = list(text = "Company", font = t2)),

          yaxis = list(title = list(text = "IV Percentile", font = t2)),
          plot_bgcolor = "#e5ecf6")

    })


  })
}








## To be copied in the UI
# mod_implied_vol_percentile_ui("implied_vol_percentile_1")

## To be copied in the server
# mod_implied_vol_percentile_server("implied_vol_percentile_1")






