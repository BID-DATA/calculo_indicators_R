
setwd("C:/Users/linaa/OneDrive - Inter-American Development Bank Group/Documents/GitHub/calculo_indicators_R")
# Read your csv file

library(tidyverse)
library(haven)
library(srvyr)
library(readxl)
library(parallel)
options(scipen = 999)

available_years <- read.csv("Inputs/running_survey.csv") %>% 
  filter(availability==1) %>%
  filter(year %in% c("2021")) %>%
  filter(Pais %in% c( "GUY"))

# if needed you can run by chunks of countries here
#"ARG",   "CHL", "BLZ", "BRA", "COL", "CRI", "ECU", "GUY", "HTI",  "PAN", "PRY", "SUR", "URY"

# Get unique combinations of country and year
unique_combinations <- unique(available_years[c("Pais", "year")])


# Loop over each unique row in unique_combinations
for (i in 1:nrow(unique_combinations)) {
  
  skip_to_next <- FALSE
  
  # Get country and year from the current row
  pais <- unique_combinations[[i, "Pais"]]
  anio <- unique_combinations[[i, "year"]]
  tipo <- "encuestas"
  
  tryCatch({
    source("scl_indicators.R")
    
    if (tipo == "encuestas") {
      write.csv(data_total, paste("Outputs/indicadores_encuestas_hogares_", pais, "_", anio, ".csv", sep = ""), row.names = FALSE)
    }
    
    # Add more conditions for other types if needed
    #if (tipo == "censos") {
      # do something
    #}
    
    # Add more code here if needed
    
  }, error = function(e) {
    skip_to_next <<- TRUE
    print(paste(pais,anio,"Error al correr base"))
  })
  
  if (skip_to_next) {
    next
  }
  
}
