# Load the required libraries
library(shiny)
library(leaflet)
library(shinydashboard)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Umoyo n'kukambirana"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Summaries", tabName = "summaries",
               menuSubItem("Average Number of Calls", tabName = "avg_calls"),
               menuSubItem("Frequent Callers", tabName = "freq_callers"),
               menuSubItem("Gender", tabName = "gender"),
               menuSubItem("Township with Most Calls", tabName = "township_calls")
      ),
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
      tabItem(tabName = "avg_calls",
              titlePanel("Average Number of Calls Content")
      ),
      tabItem(tabName = "freq_callers",
              titlePanel("Frequent Callers Content")
      ),
      tabItem(tabName = "gender",
              titlePanel("Gender Content")
      ),
      tabItem(tabName = "township_calls",
              titlePanel("Township with Most Calls Content")
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
    pal <- colorNumeric("YlOrRd", domain = merged_data$Calls)
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = 35, lat = -13, zoom = 7) %>%
      addPolygons(
        data = merged_data,
        color = "black",       
        fillColor = ~pal(Calls),  
        fillOpacity = 0.7,      
        weight = 1    
      ) 
  })
  
  # Add summaries
  radio_program = read.csv('../radiodata/rawdata.csv')
  
  # Add tab panels for summaries
  output$avg_calls <- renderText("Average Number of Calls Content")
  output$freq_callers <- renderText("Frequent Callers Content")
  output$gender <- renderText("Gender Content")
  output$township_calls <- renderText("Township with Most Calls Content")
}

# Run the application
shinyApp(ui, server)