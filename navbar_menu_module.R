# Module: navbar_menu_module.R
# This module defines the user interface for a custom navbar menu in a Shiny application.

navbarMenuUI <- function(id) {
  ns <- NS(id) # Create a namespace for the module to avoid ID conflicts

  tagList(
    # Add custom CSS styles for the navbar
    tags$head(
      tags$style(HTML("
      .navbar-nav .nav-link.active {
        color: #fff;                    /* Active tab text color */
        background-color: #3c8dbc;     /* Active tab background color */
      }
      .navbar-nav > li > a {
        display: grid;                 /* Use grid layout for alignment */
        grid-template-columns: auto 1fr; /* Align icon and text side by side */
        align-items: baseline;         /* Align text and icons vertically */
        justify-content: space-evenly; /* Space out items evenly */
      }
      .navbar-nav > li > a > i {
        margin-right: 5px;             /* Add margin between the icon and text */
      }
    "))
    ),

    # Define the main structure of the navbar menu
    navbarMenu(
      id = ns("navmenu"), # Assign a namespace to the navbar menu

      # Define the Home tab
      navbarTab(
        tabName = "home_tab",                 # Unique identifier for the Home tab
        text = tags$b("Home"),               # Display "Home" in bold
        icon = shiny::icon("home")           # Add a home icon to the tab
      ),

      # Define the About tab
      navbarTab(
        tabName = "about_tab",                # Unique identifier for the About tab
        text = tags$b("About"),              # Display "About" in bold
        icon = shiny::icon("book")           # Add a book icon to the tab
      ),

      # Define the Explore Borealis menu with sub-tabs
      navbarTab(
        text = tags$b("Explore Borealis"),   # Display "Explore Borealis" in bold

        # Study Network sub-tab
        navbarTab(
          tabName = "network_tab",           # Unique identifier for the Study Network sub-tab
          text = tags$span(
            style = "color: black; display: inline-flex; align-items: center; white-space: nowrap;",
            shiny::icon("sitemap", style = "color: black; margin-right: 5px;"), # Add a sitemap icon
            tags$b("Study Network")          # Display "Study Network" in bold
          )
        ),

        # Data Review sub-tab
        navbarTab(
          tabName = "borealis_tab",          # Unique identifier for the Data Review sub-tab
          text = tags$span(
            style = "color: black; display: inline-flex; align-items: center; white-space: nowrap;",
            shiny::icon("chart-bar", style = "color: black; margin-right: 5px;"), # Add a bar chart icon
            tags$b("Data review")            # Display "Data review" in bold
          )
        )
      )
    )
  )
}

