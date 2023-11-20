#' plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggplot2
mod_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    # The plot tab with option to select X and Y variable.
    #The X and Y will be updated after loading the data
    fluidRow(
      column(3,
             box(title = "Select Plot Type",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = F,
                 elevation = 3,
                 width = 12,
                 awesomeRadio(
                   inputId = ns("button"),
                   label = "Plot type",
                   choices = c("Boxplot","Scatter plot"),
                   selected = "Boxplot",
                   inline = TRUE
                 )),

             box(title = "Select Variables",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = F,
                 elevation = 3,
                 width = 12,
                 selectInput(ns("Xvar"),
                             "X",
                             choices = ""),
                 selectInput(ns("Yvar"),
                             "Y",
                             choices =""))),
      column(9,
             box(title = "Plot Area",
                 status = "white",
                 solidHeader = TRUE,
                 collapsible = F,
                 elevation = 3,
                 width = 12,
                 plotOutput(ns("plot"))
             )
      )
    )
  )
}

#' plot Server Functions
#'
#' @noRd
mod_plot_server <- function(id, exampleData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data_plot <- reactive({

      req(exampleData())

      path <- file.path("inst/extdata/data", paste0(exampleData(), ".rda"))

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

    # update selectInput for Xvar based on selected data
    observe({
      req(data_plot())
      updateSelectInput(session,
                        inputId = 'Xvar',
                        label = 'X',
                        choices = colnames(data_plot()))
    })

    # update selectInput for Yvar based on selected data
    observe({
      req(data_plot())
      Ychoices <- subset(colnames(data_plot()), !(colnames(data_plot()) %in% input$Xvar))
      updateSelectInput(session,
                        inputId = 'Yvar',
                        label = 'Y',
                        choices = Ychoices)
    })

    # Create a reactive plot
    output$plot <- renderPlot({
      plot_type <- input$button

      if (plot_type == "Boxplot") {
        data_plot() %>%
          ggplot(aes_string(x = input$Xvar, y = input$Yvar, group = input$Xvar, color = input$Xvar)) +
          geom_boxplot(size = 1) +
          theme_classic()
      } else if (plot_type == "Scatter plot") {
        data_plot() %>%
          ggplot(aes_string(x = input$Xvar, y = input$Yvar, color = input$Xvar)) +
          geom_point(size = 3) +
          theme_classic()
      }

    })


  })
}

## To be copied in the UI
# mod_plot_ui("plot_1")

## To be copied in the server
# mod_plot_server("plot_1")
