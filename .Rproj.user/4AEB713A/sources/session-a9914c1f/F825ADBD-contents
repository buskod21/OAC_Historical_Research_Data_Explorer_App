# Define UI for the app ----

ui <- dashboardPage(
  skin = "lightblue",       # Set the skin color of the dashboard to light blue
  scrollToTop = TRUE,       # Enable scrolling to the top when navigating tabs
  fullscreen = TRUE,        # Allow the dashboard to open in fullscreen mode
  help = NULL,              # Placeholder for future help functionality (currently unused)
  dark = NULL,              # Placeholder for future dark mode functionality (currently unused)

  # Define the dashboard header with a fixed position and a navbar menu module
  dashboardHeader(
    status = "lightblue",   # Set the header's color to light blue
    fixed = TRUE,           # Fix the header at the top during scrolling
    rightUi = tagList(
      dropdownMenu(
        badgeStatus = "success",
        type = "notifications",
        uiOutput("update_notification")
      )),

    # Include the navigation bar menu defined in the `navbarMenuUI` module
    navbarMenuUI("navbarMenu")
  ),

  # Disable the sidebar for this dashboard layout
  dashboardSidebar(disable = TRUE),

  # Define the main body content of the dashboard
  dashboardBody(
    # Add custom styles for active and hover states of navigation tabs
    tags$head(tags$style(".nav-pills .nav-link.active {color: #fff; background-color: #3c8dbc;}")), # Active tab styling
    tags$head(tags$style(".nav-pills .nav-link:not(.active):hover {color: #3c8dbc !important;}")),  # Hover styling for inactive tabs


    # Define the content of individual tabs
    tabItems(
      tabItem(
        tabName = "home_tab",        # Unique identifier for the Home tab
        homeTab_UI("home")          # Call the `homeTab_UI` module to define the Home tab's UI
      ),
      tabItem(
        tabName = "about_tab",       # Unique identifier for the About tab
        aboutTab_UI("about")        # Call the `aboutTab_UI` module to define the About tab's UI
      ),
      tabItem(
        tabName = "network_tab",     # Unique identifier for the Network tab
        networkTab_UI("network")    # Call the `networkTab_UI` module to define the Network tab's UI
      )
      ,
      tabItem(
        tabName = "borealis_tab",    # Unique identifier for the Data Review tab
        datareviewTab_UI("data")    # Call the `datareviewTab_UI` module to define the Data Review tab's UI
      )
    )
  )
)





