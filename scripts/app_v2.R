
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
                plotlyOutput("gender_bar")
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
  #radio_path <- "C:/R/Umoyo N'kukambirana/radioprogram/radiodata/summary.csv"
  rawdata_path <-  "../radiodata/rawdata.csv"
  # Notes for Clinton, Modify the two lines above. On my macbook relative path was not working so I resorted to the old way of using absolute paths 
  malawi_shapefile <- sf::st_read(shapefile_path)
  radio_summary <- read.csv(radio_path)
  rawdata <- read.csv(rawdata_path)
  
  # Merge data based on common column "ADMN2_EN" and "Districts"
  merged_data <- merge(malawi_shapefile, radio_summary, by.x = "ADM2_EN", by.y = "District", all.x = TRUE)
  
  merged_data$Calls[is.na(merged_data$Calls)] <- 0
  
  # Calculate the total number of calls
  total_calls <- sum(merged_data$Calls)
  
  # Calculate the percentage of calls for each district
  merged_data$Percentage_calls <- round((merged_data$Calls / total_calls) * 100)
  
  # View the result
  # head(merged_data[, c("ADM2_EN", "Calls", "Percentage_calls")])
  
  # Define categories
  merged_data$Percentage_category <- cut(merged_data$Percentage_calls,
                                         breaks = c(-Inf, 0, 1, 5, 10, 15, 20, Inf),
                                         labels = c("0%", "0%", "1-5%", "5-10%", "10-15%","15-20%", ">20%"))
  
  # Rename the category label for NA values to "0%"
  merged_data$Percentage_category[is.na(merged_data$Percentage_category)] <- "0%"
  
  # Final map 
  output$map <- renderLeaflet({
    # Define color palette
    #pal <- colorNumeric("YlOrRd", domain = merged_data$Calls)
    pal <- colorNumeric("YlOrRd", domain = merged_data$Percentage_calls)
    
   #pal <- colorFactor(palette = "YlOrRd", domain = merged_data$Percentage_category)
    
   # A qualitative color palette
   # qual_palette <- c("#1f78b4", "#a6cee3", "#b2df8a","#33a02c", "#fb9a99",
   #                   "#cab2d6", "#ffff99", "#fdbf6f","#e31a1c", "#ff7f00",
   #                   "#6a3d9a", "#b15928")
   
   # Create a color palette factor
   #qual_pal <- colorFactor(palette = qual_palette, domain = merged_data$Percentage_category)

    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 35, lat = -13, zoom = 6) %>%
      addPolygons(
        data = merged_data,
        color = "black",
        #fillColor = ~pal(Calls),
        fillColor = ~pal(Percentage_calls),
        #fillColor = ~pal(Percentage_category),
        fillOpacity = 0.7,
        weight = 1
      ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = merged_data$Percentage_calls,
      #values = merged_data$Percentage_category,
      title = "Percentage of Calls"
    )
      # addLegend(
      #   position = "bottomright",
      #   pal = pal,
      #   values = merged_data$Calls,
      #   title = "Number of Calls"#,
      #   #labFormat = labelFormat(suffix = " calls")
      # )
    
   #  # Create the Leaflet map with qualitative color palette
   # leaflet() %>%
   #   addProviderTiles("CartoDB.Positron") %>%
   #   setView(lng = 35, lat = -13, zoom = 6) %>%
   #   addPolygons(
   #     data = merged_data,
   #     color = "black",       
   #     fillColor = ~qual_pal(Percentage_category),
   #     fillOpacity = 0.7,      
   #     weight = 1,
   #     popup = paste("<b>District:</b>", merged_data$ADM2_EN, "<br>",
   #                   "<b>Number of Calls:</b>", merged_data$Calls) 
   #    ) %>%
   #    addLegend(
   #      position = "bottomright",
   #      pal = qual_pal,
   #      values = merged_data$Percentage_category,
   #      title = "Percentage of Calls"
   #    )
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
             legend = list(orientation = "h", x = 0, y = -0.2))
  })
  
  # Summarize gender by district
  gender_summary <- table(rawdata$District, rawdata$Gender)
  
  # Create a bar graph summarizing gender by district
   output$gender_bar <- renderPlotly({
    gender_df <- as.data.frame(gender_summary)
    names(gender_df) <- c("District", "Gender", "Count")
    
    plot_ly(data = gender_df, x = ~District, y = ~Count, color = ~Gender, type = 'bar',
            marker = list(line = list(width = 6))) %>%
      layout(title = "Gender Distribution by District") %>%
      layout(margin = list(l = 200)) %>%
      layout(barmode = "group") %>%
      layout(xaxis = list(tickvals = seq(1, nrow(gender_df), by = 5), 
                          ticktext = gender_df$District, 
                          tickangle = -45))
  })
  
  
  
  
  
  output$freq_callers <- renderText("Frequent Callers Content")
  output$township_calls <- renderText("Township with Most Calls Content")
}

# Run the application
shinyApp(ui, server)
