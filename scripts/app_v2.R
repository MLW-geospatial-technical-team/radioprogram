
# Load the required libraries
library(shiny)
library(leaflet)
library(shinydashboard)
library(plotly)

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
              titlePanel("Average Number of Calls"),
              fluidRow(
                column(6, dataTableOutput("avg_calls_table")),
                column(6, plotlyOutput("donut_chart"))
              )
      ),
      tabItem(tabName = "freq_callers",
              titlePanel("Frequent Callers Content")
      ),
      tabItem(tabName = "gender",
              titlePanel("Gender Summary"),
              fluidRow(
                column(6, dataTableOutput("gender_table")),
                column(6, plotlyOutput("gender_pie"))
              )
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
  
  shapefile_path <- "C:/R/Umoyo N'kukambirana/radioprogram/basedata/malawi_level2/mwi_admbnda_adm2_nso_20181016.shp"
  radio_path <- "../radiodata/summary.csv"
  rawdata_path <-  "../radiodata/rawdata.csv"
  # Notes for Clinton, Modify the two lines above. On my macbook relative path was not working so I resorted to the old way of using absolute paths 
  malawi_shapefile <- sf::st_read(shapefile_path)
  radio_summary <- read.csv(radio_path)
  rawdata <- read.csv(rawdata_path)
  
  # Merge data based on common column "ADMN2_EN" and "Districts"
  merged_data <- merge(malawi_shapefile, radio_summary, by.x = "ADM2_EN", by.y = "District", all.x = TRUE)
  
  merged_data$Calls[is.na(merged_data$Calls)] <- 0
  
  # Final map 
  output$map <- renderLeaflet({
    pal <- colorNumeric("YlOrRd", domain = merged_data$Calls)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 35, lat = -13, zoom = 6) %>%
      addPolygons(
        data = merged_data,
        color = "black",       
        fillColor = ~pal(Calls),  
        fillOpacity = 0.7,      
        weight = 1    
      ) 
  })
  
  # Add summaries
  radio_program <- read.csv('../radiodata/rawdata.csv')
  
  # Calculate average number of calls per district
  avg_calls <- tapply(radio_summary$Calls, radio_summary$District, mean, na.rm = TRUE)
  
  # Create a dataframe with the average number of calls
  avg_calls_df <- data.frame(District = names(avg_calls), Average_Calls = avg_calls)
  
  # Rename column to "Average Calls"
  names(avg_calls_df)[2] <- "Average Calls"
  
  # Render the average calls table
  output$avg_calls_table <- renderDataTable({
    avg_calls_df
  })
  
  # Create a donut chart
  output$donut_chart <- renderPlotly({
    plot_ly(avg_calls_df, labels = ~District, values = ~`Average Calls`, type = 'pie', hole = 0.6) %>%
      layout(title =  "Calls per District",
             legend = list(orientation = "h", x = 0, y = -0.2), 
             height = 500)
  })
  
  # Summarize gender by district
  gender_summary <- aggregate(Gender ~ District, data = rawdata, FUN = function(x) {
    gender_counts <- table(x)
    return(gender_counts)
  })
  
  # Render the table summarizing gender by district
  output$gender_table <- renderDataTable({
    gender_summary
  })
  
  # Create a pie chart summarizing gender by district
  output$gender_pie <- renderPlotly({
    plot_ly(gender_summary, labels = ~District, values = ~Gender$Male, type = 'pie') %>%
      layout(title = "Gender Distribution by District")
  })
  
  output$freq_callers <- renderText("Frequent Callers Content")
  output$township_calls <- renderText("Township with Most Calls Content")
}

# Run the application
shinyApp(ui, server)
