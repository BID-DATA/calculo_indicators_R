
# to do make it generalized so that you only have to specity country, type and year.

# Path of the data
library(tidyverse)
library(haven)
library(srvyr)
library(readxl)
library(parallel)
library(multidplyr)
library(reldist)

pais<-"COL"
anio<-"2020"

# select between "censos"/"encuestas"
tipo<-"encuestas"

# select between country or ine01 for surveys and country or geolevel1 for census
geoLevel <- "country"
#deleting dataset
rm("data_scl","data_total","data_aux")
gc()


source("scl_indicators.R")
if (tipo == "encuestas") {
  write.csv(data_total, paste("Outputs/indicadores_encuestas_hogares_", pais,"_",anio,".csv",sep = "",na = ""), row.names=FALSE)
  rm("data_scl","data_total","data_aux")
  gc()
}

if (tipo=="censos"){
  
  write.csv(data_total, paste("Outputs/indicadores_censos_hogares_", pais,"_",anio,".csv",sep = "",na = ""), row.names=FALSE)
  rm("data_scl","data_total","data_aux")
  gc()

}
library(RSocrata)
token <- "m3ORpWwI87oPebpA0MSmB8GmE"
earthquakesDataFrame <- read.socrata("https://mydata.iadb.org/resource/rvn3-znbm.json", app_token = token)
nrow(earthquakesDataFrame)

datasetToAddToUrl <- "https://mydata.iadb.org/resource/s8uc-zq8r.json"
# Store user email and password
#socrataEmail <- Sys.getenv("SOCRATA_EMAIL", "dcor@iadb.org")
#socrataPassword <- Sys.getenv("SOCRATA_PASSWORD", "XXXXXXXXXX")
#Elt9d7zx29hqeefy2fv4me9nv
write.socrata(data_total,"https://mydata.iadb.org/resource/rvn3-znbm.json","REPLACE","dwduksb93gxvefcjze0n27v2c","227z2jcta8grx4g9m9ru96lzp6mf90xatw2ysjtivyyk7kdk8w")



# With Mauro we set the identifier even though not unique to something like
# indicator, year,isoalpha3, idgeo, indicator
#
# Api Key ID
# KEY SECRET
#dwduksb93gxvefcjze0n27v2c
#227z2jcta8grx4g9m9ru96lzp6mf90xatw2ysjtivyyk7kdk8w

require(httr)
library(jsonlite)
#basePrueba <- basePrueba %>% select() 

# basePrueba <- basePrueba %>% 
#  rename(
#    `:deleted`= `X..deleted`
#  )
basePruebaJson <- toJSON(data_total)
#dwduksb93gxvefcjze0n27v2c
#227z2jcta8grx4g9m9ru96lzp6mf90xatw2ysjtivyyk7kdk8w
#api key: key id
#dwduksb93gxvefcjze0n27v2c:227z2jcta8grx4g9m9ru96lzp6mf90xatw2ysjtivyyk7kdk8w
#ZHdkdWtzYjkzZ3h2ZWZjanplMG4yN3YyYzoyMjd6MmpjdGE4Z3J4NGc5bTlydTk2bHpwNm1mOTB4YXR3MnlzanRpdnl5azdrZGs4dw


headers = c(
  `Authorization` = "Basic ZHdkdWtzYjkzZ3h2ZWZjanplMG4yN3YyYzoyMjd6MmpjdGE4Z3J4NGc5bTlydTk2bHpwNm1mOTB4YXR3MnlzanRpdnl5azdrZGs4dw==",
  `Content-Type` = "application/json",
  `Accept` = "application/json"
)


httr::POST(url = "https://mydata.iadb.org/resource/rvn3-znbm.json?DELETE WHERE ISOALPHA3=COL", httr::add_headers(.headers=headers))

# Note: delete function does not work, we have to try something different
