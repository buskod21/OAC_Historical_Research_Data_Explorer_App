# Home tab content module

homeTab_UI <- function(id) {
  ns <- NS(id) # Create a namespace for the module

  tagList(
    tags$head(tags$style(HTML("
      /* Externalize your CSS into a file for scalability */
      @keyframes fadeIn {
        from { opacity: 0; }
        to { opacity: 1; }
      }
      .animated-box {
        animation: fadeIn 5s ease-in-out infinite alternate;
      }
    "))),

    fluidRow(
      column(
        width = 12,
        tags$div(
          style = "display: flex; justify-content: space-between;
          flex-wrap: nowrap; align-items: stretch; gap: 10px;",
          valueBoxOutput(ns("total_dataverses"), width = 2.2),
          valueBoxOutput(ns("total_studies"), width = 2.2),
          valueBoxOutput(ns("unique_authors"), width = 2.2),
          valueBoxOutput(ns("unique_keywords"), width = 2.2),
          valueBoxOutput(ns("total_file"), width = 2.2)
        )
      )
    ),

    br(),
    tags$div(
      style = " padding: 20px;
    border-radius: 10px;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
    text-align: center;
    max-width: 80%;
    margin: auto;
    font-size: 20px;
    line-height: 1.6;
    color: #333333;",
      tags$h4(tags$b("About the re-usable data explorer App (RED-X)")),
      tags$hr(style = "border: 2px solid #3c8dbc; width: 250px; margin: auto;"),

      br(),

      p("The reusable data explorer app, developed by Agri-food Data Canada,
        allows researchers to explore historical data in the Borealis repository.
        It provides detailed study insights and data analysis tools to help researchers
        understand past research and enhance project efficiency.")),

    br(), br(),

    tags$div(
      style = " background-color: lightgrey;
    padding: 20px;
    border-radius: 10px;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
    text-align: center;
    max-width: 50%;
    margin: auto;
    font-size: 20px;
    line-height: 1.6;
    color: #333333;",
      tags$h4(tags$b("Mission of Agri-Food data Canada")),
      tags$hr(style = "border: 2px solid #3c8dbc; width: 250px; margin: auto;"),

      br(),

      p("Agri-food Data Canada, based at the University of Guelph,
          enhances research data utility through tools and training aligned with FAIR principles
          (i.e., Findable, Accessible, Interoperable, and Reusable).
          Our mission is to support researchers in maximizing data impact by
          simplifying access,fostering innovation, and integrating historical research.
          Learn more about the Agric-Food data Canada at the University of Guelph,",
        tags$a(href = "https://agrifooddatacanada.ca/",
               tags$b("here."), target = "_blank", style = "color:#3c8dbc;")
      )),

    br(), br(),

    tags$div(
      style = "
      padding: 20px;
      width: 80%;
      text-align:center;
      margin: auto;
      border-radius: 10px;
      box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
      font-size: 20px;
      line-height: 1.6;
      color: #333333;",
      tags$h4(tags$b("Unique Features of the App"), style = "text-align: center;"),
      tags$hr(style = "border: 2px solid #3c8dbc; width: 250px; margin: auto;"),
      br(),
      tags$div(
        style = "display: flex; justify-content: space-between; align-items: flex-start;",
        tags$div(
          style = "flex: 1; text-align: center; padding: 15px; max-width: 30%;",
          tags$h5(tags$b("Network")),
          tags$img(src = "network.png", height = "80px"),
          p("Visualizes study connections through shared keywords and authors,
            aiding discovery of related research.")
        ),
        tags$div(
          style = "flex: 1; text-align: center; padding: 15px; max-width: 30%;",
          tags$h5(tags$b("Metadata")),
          tags$img(src = "Metadata.png", height = "80px"),
          p("Provides essential details on study variables, methodology,
            and objectives for better data interpretation.")
        ),
        tags$div(
          style = "flex: 1; text-align: center; padding: 15px; max-width: 30%;",
          tags$h5(tags$b("Exploratory Data Analysis")),
          tags$img(src = "analysis.png", height = "80px"),
          p("Allow users to analyze and visualize study data to uncover trends,
            patterns, and insights.")
        )
      )
    ),

    br(), br(),

    tags$div(
      style = " background-color: lightgrey;
    padding: 20px;
    border-radius: 10px;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
    text-align: center;
    max-width: 50%;
    margin: auto;
    font-size: 20px;
    line-height: 1.6;
    color: #333333;",
      tags$h4(tags$b("How the App works"), style = "text-align: center;"),
      tags$hr(style = "border: 2px solid #3c8dbc; width: 100px; margin: auto;"),
      br(),
      p("The ", strong("Explore Borealis tab"),
        "lets users visualize study networks in the OAC repository based on
        keywords and author names. Users can select a keyword or author to explore
        related studies and gain deeper insights through the study overview,
        metadata, and data exploration tabs. Click ",
        tags$a(href = "https://borealisdata.ca/dataverse/oac",
               tags$b("here"),
               target = "_blank", style = "color:#3c8dbc;"),
        " to access the OAC dataverse in the Borealis database.")
    ),

    br(),br(), br(),

    tags$div(
      style = "text-align: center;",
      p(tags$h2(tags$strong("Have fun exploring re-usable historical data!")))
    )
  )
}


# Server Module for Network Tab
homeTab_server <- function(id, study_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace for the module

    # Render ValueBox for 'Total Studies'
    output$total_dataverses <- renderValueBox({

      # Compute the value only once study_data() is available
      req(study_data())  # Ensures that the data is available

      valueBox(value = tags$span(tags$b(class = "animated-box", style = "font-size: 60px; color: white;", "6")),
               subtitle = tags$span(style = "font-size: 20px; color: white;",
                                    "Dataverses in the OAC repository to explore!"),
               icon = icon("database"),
               color = "maroon",
               elevation = 2,
               gradient = TRUE)
    })

    # Render ValueBox for 'Total Studies'
    output$total_studies <- renderValueBox({

      # Compute the value only once study_data() is available
      req(study_data())  # Ensures that the data is available

      valueBox(value = tags$span(tags$b(class = "animated-box",
                                        style = "font-size: 60px;",
                                        nrow(study_data()))),
               subtitle = tags$span(style = "font-size: 20px;",
                                    "Research papers available for exploration!"),
               icon = icon("book"),
               color = "info",
               elevation = 2,
               gradient = TRUE)
    })

    # Render ValueBox for 'Unique Authors'
    output$unique_authors <- renderValueBox({
      # Compute the value only once study_data() is available
      req(study_data())  # Ensures that the data is available

      # Calculate the number of unique authors once the data is ready
      author_count <- length(unique(unlist(strsplit(study_data()$Authors, ";"))))

      valueBox(value = tags$span(tags$b(class = "animated-box",
                                        style = "font-size: 60px;",
                                        author_count)),
               subtitle = tags$span(style = "font-size: 20px;", "Researchers whose work you can discover!"),
               icon = icon("user"),
               color = "purple",
               elevation = 2,
               gradient = TRUE)
    })
    # Render ValueBox for 'Unique Keywords'
    output$unique_keywords <- renderValueBox({

      # Compute the value only once study_data() is available
      req(study_data())  # Ensures that the data is available

      valueBox(value = tags$span(tags$b(class = "animated-box",
                                        style = "font-size: 60px;",
                                        length(unique(unlist(strsplit(study_data()$Keywords, ";")))))),
               subtitle = tags$span(style = "font-size: 20px;",
                                    "Topics and themes to guide your research!"),
               icon = icon("tags"),
               color = "success",
               elevation = 2,
               gradient = TRUE)
    })

    # Render ValueBox for 'Total Files'
    output$total_file <- renderValueBox({

      # Compute the value only once study_data() is available
      req(study_data())  # Ensures that the data is available

      valueBox(value = tags$span(tags$b(class = "animated-box",
                                        style = "font-size: 60px;",
                                        length(unique(unlist(strsplit(study_data()$FileList, ";")))))),
               subtitle = tags$span(style = "font-size: 20px;",
                                    "Datafiles ready for exploration and visualization!"),
               icon = icon("file"),
               color = "danger",
               elevation = 2,
               gradient = TRUE)
    })

  })
}
