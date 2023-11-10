#' UI UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#'
#'
#' @importFrom shiny NS tagList
#' @import skimr
mod_UI_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput("dataset",
                "Select a dataset",
                choices = c("Fatty acid"= "Fattyacid",
                            "Milk"= "Milk",
                            "Feed" = "Feed"),
                selected = "fattyacid"
    ),
    tabBox(title = "",
           width = 12,
           collapsible = TRUE,
           maximizable = TRUE,
           elevation = 1,
           solidHeader = TRUE,
           status = "lightblue",
           side = "right",
           type = "tabs",
           tabPanel("Metadata",mod_metadata_ui(ns("metadata_1"))
           ),
           tabPanel("Data", mod_data_ui(ns("data_1"))
           ),
           tabPanel("Plot", mod_plot_ui(ns("plot_1"))
           )
    )
  )
}

#' UI Server Functions
#'
#' @noRd
#' @import DT
mod_UI_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_metadata_server("metadata_1")

  })
}

## To be copied in the UI
# mod_UI_ui("UI_1")

## To be copied in the server
# mod_UI_server("UI_1")
