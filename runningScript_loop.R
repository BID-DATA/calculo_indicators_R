
# Read your csv file

library(tidyverse)
library(haven)
library(srvyr)
library(readxl)
library(matrixStats)
library(parallel)
library(multidplyr)
library(reldist)
options(scipen = 999)
# select between "censos"/"encuestas"
tipo <- "encuestas"
# select between country or ine01 for surveys and country or geolev1 for census
geoLevel <- "country"
if (tipo=="encuestas"){
  
  available_years <- read.csv("Inputs/running_survey.csv") %>% 
    filter(availability==1 & person=="David") 
}

if (tipo=="censos"){
  available_years <- read.csv("Inputs/running_census.csv") %>% 
    filter(availability==1&person=="jillie")
}
# Get files from data_arm folder
files <- list.files("data_arm", pattern = "\\.dta$", full.names = TRUE)

unique_combinations <- data.frame(
  Pais = substr(basename(files), 1, 3),
  year = as.numeric(substr(basename(files), 5, 8)),
  file = files,
  stringsAsFactors = FALSE
)

# Loop over each unique row in unique_combinations
for (i in 1:nrow(unique_combinations)) {
  
  skip_to_next <- FALSE
  
  # Get country and year from the current row
  pais <- unique_combinations[[i, "Pais"]]
  anio <- unique_combinations[[i, "year"]]
  base_in_data_arm <- unique_combinations[[i, "file"]]
  
  
  output_file <- if (tipo == "encuestas") {
    paste0("Outputs/indicadores_encuestas_hogares_", pais, "_", anio, ".csv")
  } else {
    paste0("Outputs/indicadores_censos_hogares_", pais, "_", anio, ".csv")
  }
  
  if (file.exists(output_file)) {
    message(paste("Skipping (already exists):", pais, anio))
    next
  }
  
  tryCatch({
    source("scl_indicators.R")
    
    if (tipo == "encuestas") {
      write.csv(data_total, output_file, row.names = FALSE)
      rm("data_scl","data_total","data_aux")
      gc()      
    }
    
    # Add more conditions for other types if needed
    if (tipo == "censos") {
      write.csv(data_total, output_file, row.names = FALSE)
      rm("data","data_total","data_aux","data_scl")
      gc()
    }
    
    # Add more code here if needed
    
  }, error = function(e) {
    skip_to_next <<- TRUE
    message(paste("ERROR in:", pais, anio))
    message("Error message: ", conditionMessage(e))
    message("Error call: ", deparse(conditionCall(e)))
    message("Full traceback:")
    traceback()
  })
  
  if (skip_to_next) {
    next
  }
  
}

