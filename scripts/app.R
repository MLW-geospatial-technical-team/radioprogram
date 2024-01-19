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
    tabItems(
      # Home tab
      tabItem(tabName = "home",
              fluidPage(
                titlePanel("Phone calls from districts across Malawi"),
                tags$style(HTML("#map {height: calc(100vh - 80px) !important;}")),
                leafletOutput("map")
              )
      ),
      
      # Summaries tab
      tabItem(tabName = "summaries",
              titlePanel("Phone calls from districts across Malawi")
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
)

# Define server logic
server <- function(input, output) {
  
  shapefile_path <- "../basedata/malawi_level2/mwi_admbnda_adm2_nso_20181016.shp"
  radio_path <- "../radiodata/summary.csv"
  # Notes for Clinton, Modify the two lines above. On my macbook relative path was not working so I resorted to the old way of using absolute paths 
  malawi_shapefile <- sf::st_read(shapefile_path)
  radio_summary <- read.csv(radio_path)
  
  # Merge data based on common column "ADMN2_EN" and "Districts"
  merged_data <- merge(malawi_shapefile, radio_summary, by.x = "ADM2_EN", by.y = "District", all.x = TRUE)
  
  merged_data$Calls[is.na(merged_data$Calls)] <- 0
  
  # Final map 
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 35, lat = -13, zoom = 7) %>%
      addPolygons(
        data = merged_data,
        color = "black",       
        fillColor = ~colorNumeric("YlOrRd", domain = NULL)(Calls),  
        fillOpacity = 0.7,      
        weight = 1    
      )
  })
  
  #Add summaries
  radio_program = read.csv('../radiodata/rawdata.csv')
}

# Run the application
shinyApp(ui, server)