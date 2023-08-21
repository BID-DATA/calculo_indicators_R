
# Read your csv file

library(tidyverse)
library(haven)
library(srvyr)
library(readxl)
library(parallel)
options(scipen = 999)
# select between "censos"/"encuestas"
tipo <- "censos"
# select between country or ine01 for surveys and country or geolev1 for census
geoLevel <- "geolev1"
if (tipo=="encuestas"){

available_years <- read.csv("Inputs/running_survey.csv") %>% 
  filter(availability==1) 
}

if (tipo=="censos"){
  available_years <- read.csv("Inputs/running_census.csv") %>% 
    filter(availability==1&person=="David")
}
# if needed you can run by chunks of countries here
#"ARG",   "CHL", "BLZ", "BRA", "COL", "CRI", "ECU", "GUY", "HTI",  "PAN", "PRY", "SUR", "URY"

# Get unique combinations of country and year
unique_combinations <- unique(available_years[c("Pais", "year")])

# LMK
# Loop over each unique row in unique_combinations
for (i in 1:nrow(unique_combinations)) {
  
  skip_to_next <- FALSE
  
  # Get country and year from the current row
  pais <- unique_combinations[[i, "Pais"]]
  anio <- unique_combinations[[i, "year"]]
  
  
  tryCatch({
    source("scl_indicatorsLMK.R")
    
    if (tipo == "encuestas") {
      write.csv(data_total, paste("Outputs/indicadores_encuestas_hogares_", pais, "_", anio, ".csv", sep = ""), row.names = FALSE)
    }
    
    # Add more conditions for other types if needed
    if (tipo == "censos") {
      write.csv(data_total, paste("Outputs/indicadores_censos_hogares_demographic_", pais, "_", anio, ".csv", sep = ""), row.names = FALSE)
      rm(data_total)
      rm("data_filt", "data_total","results")
      gc()
      gc()
      gc()
      gc()
    }
    
    # Add more code here if needed
    
  }, error = function(e) {
    skip_to_next <<- TRUE
    print(paste(pais,anio,"Error al correr base"))
  })
  
  if (skip_to_next) {
    next
  }
  
}

# demographic
# Loop over each unique row in unique_combinations
for (i in 1:nrow(unique_combinations)) {
  
  skip_to_next <- FALSE
  
  # Get country and year from the current row
  pais <- unique_combinations[[i, "Pais"]]
  anio <- unique_combinations[[i, "year"]]

  
  tryCatch({
    source("scl_indicatorsDemographic.R")
    
    if (tipo == "encuestas") {
      write.csv(data_total, paste("Outputs/indicadores_encuestas_hogares_", pais, "_", anio, ".csv", sep = ""), row.names = FALSE)
    }
    
    # Add more conditions for other types if needed
    if (tipo == "censos") {
      write.csv(data_total, paste("Outputs/indicadores_censos_hogares_demographic_", pais, "_", anio, ".csv", sep = ""), row.names = FALSE)
      rm(data_total)
      rm("data_filt", "data_total","results")
      gc()
      gc()
      gc()
      gc()
    }
    
    # Add more code here if needed
    
  }, error = function(e) {
    skip_to_next <<- TRUE
    print(paste(pais,anio,"Error al correr base"))
  })
  
  if (skip_to_next) {
    next
  }
  
}

# vivienda
# Loop over each unique row in unique_combinations
for (i in 1:nrow(unique_combinations)) {
  
  skip_to_next <- FALSE
  
  # Get country and year from the current row
  pais <- unique_combinations[[i, "Pais"]]
  anio <- unique_combinations[[i, "year"]]
  
  
  tryCatch({
    source("scl_indicatorsVivienda.R")
    
    if (tipo == "encuestas") {
      write.csv(data_total, paste("Outputs/indicadores_encuestas_hogares_", pais, "_", anio, ".csv", sep = ""), row.names = FALSE)
    }
    
    # Add more conditions for other types if needed
    if (tipo == "censos") {
      write.csv(data_total, paste("Outputs/indicadores_censos_hogares_vivienda_", pais, "_", anio, ".csv", sep = ""), row.names = FALSE)
      rm(data_total)
      gc()
      gc()
      gc()
      gc()
    }
    
    # Add more code here if needed
    
  }, error = function(e) {
    skip_to_next <<- TRUE
    print(paste(pais,anio,"Error al correr base"))
  })
  
  if (skip_to_next) {
    next
  }
  
}

#EDU
# Loop over each unique row in unique_combinations
for (i in 1:nrow(unique_combinations)) {
  
  skip_to_next <- FALSE
  
  # Get country and year from the current row
  pais <- unique_combinations[[i, "Pais"]]
  anio <- unique_combinations[[i, "year"]]
  
  
  tryCatch({
    source("scl_indicatorsEDU.R")
    
    if (tipo == "encuestas") {
      write.csv(data_total, paste("Outputs/indicadores_encuestas_hogares_", pais, "_", anio, ".csv", sep = ""), row.names = FALSE)
    }
    
    # Add more conditions for other types if needed
    if (tipo == "censos") {
      write.csv(data_total, paste("Outputs/indicadores_censos_hogares_edu_", pais, "_", anio, ".csv", sep = ""), row.names = FALSE)
      rm(data_total)
      rm("data_filt", "data_total","results")
      gc()
      gc()
      gc()
      gc()
    }
    
    # Add more code here if needed
    
  }, error = function(e) {
    skip_to_next <<- TRUE
    print(paste(pais,anio,"Error al correr base"))
  })
  
  if (skip_to_next) {
    next
  }
  
}