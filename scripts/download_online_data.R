
url <- "https://script.google.com/macros/s/AKfycbyrEyulGB9e-abcA0SBNEjsALddiSNGadurXlwX0f9Jo4c-SHS7MAjqoIpmCyhiFqJZ/exec" #API to Google Sheet

destination_path <- "radiodata/rawdata.csv"
download.file(url, destfile = destination_path, method = "auto", quiet = FALSE)
rawdata = read.csv("radiodata/rawdata.csv")
View(rawdata)

# Summary for Map (Home page)
mapdata <- data.frame(District = rawdata$District)
# Restructure the Data, Have only the First letter to be caps  
if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}
library(stringr)
mapdata$District <- str_to_title(mapdata$District)
mapdata$District <- trimws(mapdata$District)
summary_data <- data.frame(
  District = names(table(mapdata$District)),
  Calls = as.numeric(table(mapdata$District))
)
write.csv(summary_data, file = "radiodata/summary.csv", row.names = FALSE)
summary_data
# Calculate percentage of calls per district
# Calculate total calls
total_calls <- sum(summary_data$Calls)
total_calls

# Remove the first row as it represents the total calls
summary_data <- summary_data[-1, ]

# Calculate percentage of calls for each district
summary_data$Percentage <- round((summary_data$Calls / total_calls) * 100, )

summary_data
