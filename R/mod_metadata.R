#' metadata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT datatable renderDataTable dataTableOutput
mod_metadata_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             box(title = "Description of the Dataset",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 elevation = 3,
                 width = 12,
                 collapsed = F,
                 dataTableOutput(ns("meta"))),
             box(title = "Data Schema",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 elevation = 3,
                 width = 12,
                 collapsed = T,
                 dataTableOutput(ns("schema")))
      )
    )
  )
}

#' metadata Server Functions
#'
#' @noRd
mod_metadata_server <- function(id, exampleData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    metaData <- reactive({

      req(exampleData)

      filename <- list.files("data", pattern = paste0(exampleData(),"Meta"))

      if (length(filename) == 0) {

        # Handle the case when no matching file is found
        print("File not found")
        return(NULL)  # Or handle the error as needed

        loaded_data <- load(file.path("data", filename))
      }

      # Assuming there's only one matching file, load the data
      loaded_data <- load(file.path("data", filename))

      # Extract the loaded data from the environment
      loaded_data <- get(loaded_data)

      return(loaded_data)
    })



    #Render a data table for metadata
    output$meta <- renderDataTable({
      datatable(metaData()[1:7,],
                rownames = FALSE,
                colnames = c("Keys", "Values"),
                options = list(dom = "tp",
                               autoWidth = FALSE,
                               scrollX = TRUE))
    })


    # Render a data table for data schema
    output$schema <- renderDataTable({
      datatable(metaData()[8:13,],
                rownames = FALSE,
                colnames = c("Keys", "Values"),
                options = list(dom = "tp",
                               autoWidth = FALSE,
                               scrollX = TRUE))
    })

  })
}

## To be copied in the UI
# mod_metadata_ui("metadata_1")

## To be copied in the server
# mod_metadata_server("metadata_1")
