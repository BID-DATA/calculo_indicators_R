# to do make it generalized so that you only have to specity country, type and year.
# Path of the data
library(tidyverse)
library(haven)
library(srvyr)
library(readxl)
library(parallel)
library(multidplyr)


pais<-"COL"
anio<-"2005"

# select between "censos"/"encuestas"
tipo<-"censos"

# select between country or ine01 for surveys and country or geolevel1 for census
geoLevel <- "geolev1"


source("scl_indicators.R")
if (tipo == "encuestas") {
  write.csv(data_total, paste("Outputs/indicadores_encuestas_hogares_", pais,"_",anio,".csv",sep = ""), row.names=FALSE)
  rm("data_scl", "data_total","results")
  gc()
}

if (tipo=="censos"){
  
  write.csv(data_total, paste("Outputs/indicadores_censos_hogares_", pais,"_",anio,".csv",sep = ""), row.names=FALSE)
  rm("data_scl", "data_total","results")
  gc()
}

