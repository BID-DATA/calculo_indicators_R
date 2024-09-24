### trying to obtain a certain ID column from a query
## let's just create a correlative id
# Capas geoespaciales Amazonias
# DataLake y la otra es Atlas
# 
message("Obtaining the IDs required")

#---------------------------------------------------------------- - # 1. Searching indicators -----   #---------------------------------------------------------------- - scl_dict <- idbsocialdataR:::query_dictionary() |>    filter(theme_en=='Education') list_ind_scl<-scl_dict |> pull(indicator) #---------------------------------------------------------------- - # 2. Loading indicators -----   #---------------------------------------------------------------- - # Creaing Query Request # Model #
#https://mydata.iadb.org/resource/q8e9-eb82.json?$$app_token=IjHG1z0fQXsM9vxlkB8vPq3S2&$limit=11000000&$where=indicator%20IN%20(%27tasa_terminacion_c_primar%27,%27
# Custom function to concatenate values into a long string 
concatenate_values_scl <- function(values, separator = ",", prefix = "%27", suffix = "%27"){
  stringr::str_c(values, suffix, collapse = str_c(separator, prefix)) 
  } 

urlIndicatorsToDelete <- "https://mydata.iadb.org/resource/rvn3-znbm.json?$$app_token=IjHG1z0fQXsM9vxlkB8vPq3S2&$limit=11000000&$select=identifier&$where=indicator%20IN%20(%27"

urlIndicatorsToDelete<- str_c(urlIndicatorsToDelete,concatenate_values_scl(str_to_lower(unique(data_total$indicator))),")")
#https://mydata.iadb.org/resource/q8e9-eb82.json?$$app_token=IjHG1z0fQXsM9vxlkB8vPq3S2&$limit=11000000&$where=indicator%20IN%20(%27tasa_terminacion_c_primar%27,%27tasa_terminacion_c_secund%27,%27tasa_neta_asis_prim%27,%27tasa_neta_asis_seco%27,%27tasa_neta_asis_tert%27,%27ninis_2_15_24%27,%27tasa_aban_18_24%27,%27anos_esc_25_mas_0%27,%27anos_esc_25_mas_1_5%27,%27anos_esc_25_mas_6%27,%27anos_esc_25_mas_7_11%27,%27anos_esc_25_mas_12%27,%27anos_esc_25_mas_13_mas%27,%27pobreza31%27,%27pobreza%27,%27ginihh%27,%27tasa_desocupacion%27,%27tasa_ocupacion%27,%27subempleo%27,%27formalidad_2%27,%27tasa_independientes%27,%27tasa_participacion%27,%27poblacion_total%27,%27prangoedad_00_15%27,%27prangoedad_16_30%27,%27prangoedad_31_45%27,%27prangoedad_46_60%27,%27prangoedad_61_75%27,%27prangoedad_76_90%27,%27prangoedad_91mas%27,%27migrante_ci%27,%27miglac_ci%27,%27migrantiguo5_ci%27,%27ptmc_coverage2%27,%27ptmc_dist2%27,%27whs6_102%27,%27hwf_0001%27,%27lexp%27,%27mdg_0000000007%27,%27che_gdp%27,%27gghed_gdp%27,%27hf2_gdp%27,%27hf3_gdp%27,%27haq%27,%27ncd_bmi_25a%27,%27lbw_prevalence%27,%27uhc_index_reported%27,%27jefa_ch%27,%27depen_ch%27,%27aguared_ch%27,%27luz_ch%27,%27internet_ch%27,%27cel_ch%27,%27sanred_ch%27,%27pafro_ci%27,%27pindi_ci%27,%27pdis_ci%27,%27hf3_gdp%27,%27finprotection_cata_tot_10_pop%27,%27whs6_102%27,%27hwf_0001%27,%27gghed_usd%27,%27hf2_usd%27,%27hf3_usd%27,%27lexp%27,%27mdg_0000000007%27,%27HAQ%27,%27whs4_543%27,%27uhc_index_reported%27,%27lbw_prevalence%27,%27wsh_sanitation_basic%27)


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
