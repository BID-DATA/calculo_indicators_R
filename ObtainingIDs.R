### trying to obtain a certain ID column from a query
## let's just create a correlative id
# Capas geoespaciales Amazonias
# DataLake y la otra es Atlas
# 
message("Obtaining the IDs required")

#---------------------------------------------------------------- - # 1. Searching indicators -----   #---------------------------------------------------------------- - scl_dict <- idbsocialdataR:::query_dictionary() |>    filter(theme_en=='Education') list_ind_scl<-scl_dict |> pull(indicator) #---------------------------------------------------------------- - # 2. Loading indicators -----   #---------------------------------------------------------------- - # Creaing Query Request # Model #

concatenate_values_scl <- function(values, separator = ",", prefix = "%27", suffix = "%27"){
  stringr::str_c(values, suffix, collapse = str_c(separator, prefix)) 
  } 

urlIndicatorsToDelete <- "https://mydata.iadb.org/resource/rvn3-znbm.json?$$app_token=app_token&$limit=11000000&$select=identifier&$where=indicator%20IN%20(%27"

urlIndicatorsToDelete<- str_c(urlIndicatorsToDelete,concatenate_values_scl(str_to_lower(unique(data_total$indicator))),")")

# now that I have those indicators that I want I need to send an upsert deleting them
dfIndicatorsToDelete <- read.socrata(urlIndicatorsToDelete) %>% mutate(
  `:deleted` = TRUE
)


jsonIndicatorsToDelete <- toJSON(dfIndicatorsToDelete)

#### Deleting the parts of the code not required
headers = c(
  `Authorization` = "Basic authorizationToken==",
  `Content-Type` = "application/json",
  `Accept` = "application/json"
)


httr::POST(url = "https://mydata.iadb.org/resource/rvn3-znbm.json", body=jsonIndicatorsToDelete, httr::add_headers(.headers=headers))

#### Adding the new variables
message("Adding the new variables")

jsonIndicatorsToAdd <- toJSON(data_total)

httr::POST(url = "https://mydata.iadb.org/resource/rvn3-znbm.json", body=jsonIndicatorsToAdd, httr::add_headers(.headers=headers))
