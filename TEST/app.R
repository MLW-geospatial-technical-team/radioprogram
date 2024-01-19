# Load the required libraries
library(shiny)
library(leaflet)
library(shinydashboard)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Moyo N'kukambilana"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Summaries", tabName = "summaries"),
      menuItem("Feedback", tabName = "feedback"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    tags$style(HTML("#map {height: calc(100vh - 80px) !important;}")),
    leafletOutput("map")
  ),
  tabItems(
    # Home tab
    tabItem(tabName = "home",
            fluidPage(
              titlePanel("Home"),
              leafletOutput("map")
            )
    ),
    
    # Summaries tab
    tabItem(tabName = "summaries",
            h2("Summaries Content")
    ),
    
    # Feedback tab
    tabItem(tabName = "feedback",
            h2("Feedback Content")
    ),
    
    # About tab
    tabItem(tabName = "about",
            h2("About Content")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Create a default leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 35, lat = -13, zoom = 6)
  })
}

# Run the application
shinyApp(ui, server)
