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
    
    
    # Render the navigation menu from the explorer_list 
    explorer_list$navMenu
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
      # Render the UI elements for the home, about, and network tabs from explorer_list
      explorer_list$homeTab_ui,        
      explorer_list$aboutTab_ui,
      explorer_list$networkTab_ui,
      
      # Define the Data Review tab with a unique identifier and content
      tabItem(
        tabName = "borealis_tab",    # Unique identifier for the Data Review tab
        datareviewTab_UI("data")    # Call the `datareviewTab_UI` module to define the Data Review tab's UI
      )
    ),
    
    br(),br(),
    
    # Add Footer
    tags$footer(
      tags$p(
        HTML(paste0("&copy; Agrifood Data Canada ", format(Sys.Date(), "%Y"), ". All rights reserved.")),
        style = "font-size: 16px; font-weight: bold; color: #333;"
      ),
      style = "text-align: center; padding: 10px; background-color: #f8f8f8;"
    )
  )
)







