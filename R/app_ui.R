#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import tidyverse
#' @import bs4Dash
#' @import skimr
#' @import shinyWidgets
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    # UI interface for the explorer App
    # Create a Shiny dashboard UI
    ui <- dashboardPage(
      skin = "lightblue",  # Set the dashboard's skin color
      scrollToTop = TRUE,  # Enable scroll to top button
      fullscreen = TRUE,  # Enable fullscreen mode

      # Define the dashboard header
      dashboardHeader(
        title = "Menu",  # Set the title of the header
        status = "white",  # Set the header's status color
        h3("Reusable data Explorer")  # Add a subheading
      ),

      # Define the dashboard sidebar
      dashboardSidebar(
        collapsed = F,  # Sidebar is expanded by default
        minified = F,  # Sidebar is not minified
        skin = "light",
        status = "lightblue",
        elevation = 1,

        # Define the sidebar menu items
        sidebarMenu(
          menuItem("Home",  # Sidebar menu item for the home tab
                   icon = icon("house-user"),
                   tabName = "home"),

          menuItem("Data Explorer",  # Sidebar menu item for the data tab
                   icon = icon("table"),
                   tabName = "data"),

          menuItem("Explore borealis",  # Sidebar menu item for the owndata tab
                   icon = icon("server"),
                   tabName = "owndata")
        )
      ),

      # Define the dashboard body
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "home",  # Content for the "home" tab ----
            h5("Welcome to the reusable data explorer App."),  # Subheading
            br(),

            img(src = 'www/workshop1.jpeg', height=400, width=700),  # Display an image

            br(),

            p("This shiny web application was developed as a part of the Reusable
        research data made shiny workshop that was
        held at the University of Guelph, Guelph Ontario Canada.
          You can find more information about the workshop and access the workshop materials",
              tags$a(href = "https://github.com/agrifooddatacanada/RRDMS_Workshop",
                     "here.", target = "_blank")),  # Create a hyperlink

            h5("About the App"),

            p(" The ", strong("Data explorer tab "), "demonstrates how the app works.
                          It displays the Metadata, raw data, some summary statistics
                          and plots of data gotten from an already published article.",
              tags$a(href = "https://doi.org/10.4141/A95-125",
                     "Click here for the publication.",
                     target = "_blank")),  # Create a hyperlink

            p("Finally, the ",
              strong("Explore borealis "),
              "allows you to explore data gotten from the Borealis database.
           After downloading the data into your local computer,
           the data explorer app allows you to upload the data into the app
           where you can view the Metadata, raw data,
           get some summary statistics and visualize some of the results in boxplot or scatter plot.",
              tags$a(href = "https://doi.org/10.5683/SP3/WVC09T",
                     "Click here for the Borealis database.",
                     target = "_blank")),  # Create a hyperlink
            p(h5("Have fun exploring reusable data!")
            )
          ),
          tabItem(
            tabName = "data",  # Content for the "data" tab ----
            mod_example_ui("example_1")  # Include a UI element for the "Example" tab
          ),

          tabItem(
            tabName = "owndata",  # Content for the "owndata" tab ----
            mod_UI_ui("Borealis")  # Include a UI element for the "Borealis" tab
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "DataExplorer"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
