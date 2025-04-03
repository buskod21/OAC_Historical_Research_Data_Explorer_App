# Define the home tab

explorer_list$homeTab_ui  <- tabItem(
  tabName = "home_tab", 
  tags$head(tags$style(HTML("
  /* Fade-in Animation */
  @keyframes fadeIn {
    from { opacity: 0; }
    to { opacity: 1; }
  }
  .animated-box {
    animation: fadeIn 5s ease-in-out;
  }

  /* SlickR Dots Customization */
  .slick-slider button:before {
    font-size: 20px !important;  /* Increase dot size */
    color: lightblue !important;  /* Change dot color */
  }

  .slick-dots li.slick-active button:before {
    color: #3c8dbc !important;  /* Change active dot color */
  }
  "))),
  
  fluidRow(
    slickR::slickR(
      width = "78%",
      slick_list(
        # Value Boxes Slide
        tags$div(
          id = "valueBoxes",
          class = "row justify-content-center",
          style = "gap: 20px; padding: 20px; border-radius: 10px; height: 400px; width: 100%; align-items: center;",
          
          valueBoxOutput("total_dataverses", width = "4 col-lg-2"),
          valueBoxOutput("total_studies", width = "4 col-lg-2"),
          valueBoxOutput("unique_authors", width = "4 col-lg-2"),
          valueBoxOutput("unique_keywords", width = "4 col-lg-2"),
          valueBoxOutput("total_file", width = "4 col-lg-2")
        ),
        
        # Second Slide: Explore Network
        tags$div(
          id = "exploreNetwork", 
          style = "position: relative; width: 100%; height: 400px; display: flex; justify-content: center; align-items: center;",
          tags$div(
            # Image container with responsive settings
            tags$img(src = "view_network.jpg", class = "img-fluid",
                     style = "width: 100%; height: 100%; object-fit: cover; border-radius: 50px;"),  
            
            # Text overlay
            tags$div(
              style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);
                       text-align: center; background: rgba(0, 0, 0, 0.5); padding: 20px;
                       border-radius: 10px; color: white",
              tags$h1("Explore Networks within the App", style = "font-size: 40px; font-weight: bold;"),
              tags$p("Discover relationships and connections between authors and keywords",
                     style = "font-size: 15px; font-weight: bold;"),
              actionButton("go_to_network", "Go to Network Explorer", class = "btn btn-primary")
            )
          )
        ),
        
        # Third Slide: Explore Data
        tags$div(
          id = "exploreData", 
          style = "position: relative; width: 100%; height: 400px; display: flex; justify-content: center; align-items: center;", 
          tags$div(
            # Ensure the image fills the container
            tags$img(src = "Explore_data.jpg", class = "img-fluid", 
                     style = "width: 100%; height: 100%; object-fit: cover; border-radius: 50px;"), 
            
            tags$div(
              style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);
                       text-align: center; background: rgba(0, 0, 0, 0.5); padding: 20px; 
                       border-radius: 10px; color: white",
              tags$h1("Explore and Visualize Data", style = "font-size: 40px; font-weight: bold;"),  
              tags$p("Discover various metadata and datasets for diverse studies within OAC", 
                     style = "font-size: 15px; font-weight: bold;"),
              actionButton("go_to_borealis", "Go to Data Explorer", class = "btn btn-primary")
            )
          )
        )
      )
    ) + slickR::settings(autoplay = TRUE, dots = TRUE)
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
    tags$h4(tags$b("Re-usable data explorer App (RED-X)")),
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
    tags$h4(tags$b("Agri-Food data Canada")),
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




# Server Module for Network Tab
explorer_list$homeTab_server <- function(input, output, session, study_data) {
  
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
    study_data = study_data()
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
                                  "Metadata and Data files to explore!"),
             icon = icon("file"),
             color = "danger",
             elevation = 2,
             gradient = TRUE)
  })
  
  # When the 'go_to_network' image or button is clicked, navigate to the 'network_tab'
  observeEvent(input$go_to_network, {
    updateNavbarTabs(session= session, 
                     inputId = "navmenu", 
                     selected = "network_tab")
  })
  
  # When the 'go_to_borealis' image or button is clicked, navigate to the 'borealis_tab'
  observeEvent(input$go_to_borealis, {
    updateNavbarTabs(session= session,  
                     inputId = "navmenu", 
                     selected = "borealis_tab")
  })
}