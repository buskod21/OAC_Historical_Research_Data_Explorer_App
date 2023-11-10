#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import skimr
#' @import dplyr
mod_data_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      column(12,
             box(title = "Raw Data",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 elevation = 3,
                 width = 12,
                 collapsed = F,
                 DT::dataTableOutput(ns("rawtable"))),
             box(title = "Summary statistics",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 elevation = 3,
                 width = 12,
                 collapsed = T,
                 verbatimTextOutput(ns("summary")))
      )
    )
  )
}

#' data Server Functions
#'
#' @noRd
mod_data_server <- function(id, exampleData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Render and output raw data and summary

    ## Input and read data

    data <- reactive({
      req(exampleData())
      path <- file.path("data", paste0(exampleData(), ".rda"))

      # Load the .rda
      if (file.exists(path)) {
        df <- load(path)
        df <- get(df)

        #Optionally, you can modify the data here (e.g., converting columns to factors)
        df <- df %>%
        mutate(across(1:5, as.factor))

        return(df)
        } else {
          # Handle the case where the file doesn't exist
        return(NULL)
      }
    })


    output$rawtable <- DT::renderDataTable({
    DT::datatable(data(),
              rownames = FALSE,
              options = list(dom = "tp",
                             autoWidth = FALSE,
                             scrollX = TRUE))
  })


    #output summary statistics
    output$summary <- renderPrint({
      skim(data())
    })

  })
}

## To be copied in the UI
# mod_data_ui("data_1")

## To be copied in the server
# mod_data_server("data_1")
