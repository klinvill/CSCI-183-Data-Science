# Visualization of the people biking to work in each county in the US

library(choroplethr)

data <- get_acs_data("B08101", "county",column_idx = 41)

data[[1]]$value <- data[[1]]$value / max(data[[1]]$value)
  
county_choropleth(data[[1]])
write.table(data[[1]], "biking_to_work.tsv", sep = "\t", row.names = FALSE)

