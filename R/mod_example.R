#' example UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @import dplyr
mod_example_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("dataset"),
                "Select a dataset",
                choices = c("Fatty acid"= "Fattyacid",
                            "Milk"= "Milk",
                            "Feed" = "Feed"),
                selected = "Fattyacid"
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
           tabPanel("Metadata", mod_metadata_ui(ns("metadata_1"))
           ),
           tabPanel("Data", mod_data_ui(ns("data_1"))
           ),
           tabPanel("Plot", mod_plot_ui(ns("plot_1"))
           )
    )
  )
}

#' example Server Functions
#'
#' @noRd
mod_example_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    exampleData <- reactive({
      input$dataset
    })

    mod_metadata_server("metadata_1", exampleData)

    mod_data_server("data_1", exampleData)

    mod_plot_server("plot_1", exampleData)
  })
}

## To be copied in the UI
# mod_example_ui("example_1")

## To be copied in the server
# mod_example_server("example_1")
