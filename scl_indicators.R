# script general para crear indicadores de SCL
  
  ### Data ----

  start_time <- Sys.time()

  # code that returns the address of the survey
  source("directory_periods.R")
  message(paste("Loading database ",pais,": ", anio))
  base <- functionRoundAndSurvey(pais,tipo,anio)
 
  
  if (tipo == "censos") {
    #Keep only needed variables
    variables_censos <- readxl::read_xlsx("Inputs/D.7.1.3 Diccionario variables censos.xlsx")
    
    varlist_censos <- variables_censos %>% 
      filter(!is.na(Variable))
    
    # Get the names of the variables that need to be in the data
    required_vars <- unique(varlist_censos$Variable)
    # Read data
    data_filt <- read_dta(base,col_select=any_of(required_vars))
    # Remove data we do not need and free memory
    # Check which of the required variables are not in the data
    missing_vars <- setdiff(required_vars, colnames(data_filt))
    
    # Add the missing variables to the data with NA values
    for (var in missing_vars) {
      data_filt[[var]] <- NA
    }
    if (all(is.na(data_filt$factor_ci))){
      data_filt[["factor_ci"]] <- 1
    }
  }
  
if (tipo == "encuestas") {
  # to do si no encuentra las variables ponlas en missing
  
  #Keep only needed variables
  variables_encuestas <- readxl::read_xlsx("Inputs/D.1.1.4 Diccionario microdatos encuestas de hogares.xlsx") %>% 
    filter(!(Variable %in% c("region_ci", "afroind_ano_ci", "atención_ci")))
  
  variables_encuestas <- variables_encuestas %>% 
    filter(!is.na(Variable))
  
  # Get the names of the variables that need to be in the data
  required_vars <- unique(variables_encuestas$Variable)
  # Read data
  data_filt <- read_dta(base,col_select=any_of(required_vars))  
  # Check which of the required variables are not in the data
  missing_vars <- setdiff(required_vars, colnames(data_filt))
  
  # Add the missing variables to the data with NA values
  for (var in missing_vars) {
    data_filt[[var]] <- NA
  }
  
  
}

#### Compute intermediate variables  ####
message(paste("Loading intermediate variables ",pais,": ", anio))
source("var_LMK.R")

source("var_EDU.R")

source("var_GDI.R")

source("var_SOC.R")


#### Join final data with intermediate variables #####

if (tipo == "censos") {
  
  # Make sure the joining columns form a unique identifier in the right datasets
  data_filt <- data_filt %>% 
    distinct(across(c("region_BID_c", "isoalpha3","estrato_ci", "zona_c","geolev1",
                      "relacion_ci", "idh_ch", "idp_ci", "factor_ci", "factor_ch")), .keep_all = TRUE)
  
}

if (tipo == "encuestas") {
  
  # Make sure the joining columns form a unique identifier in the right datasets
  data_filt <- data_filt %>% 
    distinct(across(c("region_BID_c", "isoalpha3","estrato_ci", "zona_c","ine01",
                      "relacion_ci", "idh_ch", "idp_ci", "factor_ci", "factor_ch")), .keep_all = TRUE)

  
}

# Remove data we do not need and free memory
  rm("variables_encuestas", "varlist_censos", "variables_censos", "required_vars","missing_vars")
  rm("data", "data_aux", "data_scl", "data_total")
  gc()

# Creating survey design object
intoSurveyDesignObject <- function(.data) {
    if (!all(is.na(.data$upm_ci)) & !all(is.na(.data$estrato_ci)) ) {
      .data<- .data %>%
        as_survey_design(upm_ci, strata = estrato_ci, weight = factor_ch,
                         nest = TRUE)
    } else if( !all(is.na(.data$upm_ci)) & all(is.na(.data$estrato_ci)) ){
      .data<- .data %>%
        as_survey_design(upm_ci, weight = factor_ch)
    }
    else{ 
      .data<- .data %>%
        as_survey_design(weight = factor_ch)
    }
    return(.data)
  }

  
# Read all functions needed for computation 
message(paste("Preparing calculations for parallelization ",pais,": ", anio))
source("functions.R")

##### Use parallel programming -----

# read the indicators definitions in the csv
if (tipo=="encuestas"){
indicator_definitions <- read.csv("Inputs/idef.csv")
}
if (tipo=="censos"){
  indicator_definitions <- read.csv("Inputs/idefCensos.csv")
}
# if needed you can filter here by theme
num_cores <- detectCores() - 1

if (tipo=="censos"){
  indicator_definitions <- indicator_definitions %>% filter(includedInCensus==1)
  
  ### adding disagregation
  if (geoLevel == "geolev1"){
    ### adding to disagregation column, geolevel1
    indicator_definitions$disaggregation <- sub(",isoalpha3", ",geolev1,isoalpha3", indicator_definitions$disaggregation)
  }
  num_cores <- 3
}
num_cores <- 3

# Call the function in parallel
# read the indicators definitions in the csv
if (tipo=="encuestas"){
  data_filtSVD<-   intoSurveyDesignObject(data_filt)
  # number of cores to use, often set to one less than the total available
  cl <- makeCluster(num_cores)

  ### for some reason there's error in serealization
  
  # Export data, indicator definitions and the necessary functions to the cluster
  # it{s definetely in the new data object that we created maybe I should convert it laters}
  clusterExport(cl, c("data_filt","data_filtSVD", "indicator_definitions", "scl_pct","scl_pctv2","scl_ratio_decil", "scl_mean","scl_gini","calculate_indicators", "evaluatingFilter", "drop_na"))
  
  # Load necessary packages on each node of the cluster
  clusterEvalQ(cl, {
    library(magrittr)
    library(dplyr)
    library(rlang)
  })
  
  is_haven_labelled <- function(x) {
    inherits(x, "haven_labelled")
  }
  
  # Convert all haven_labelled columns to numeric
  data_filt <- data_filt %>%
    mutate(across(where(is_haven_labelled), as.numeric))
  message(paste("Calculating indicators ",pais,": ", anio))
  results <- parLapply(cl, 1:nrow(indicator_definitions), calculate_indicators, data_filt,data_filtSVD, indicator_definitions)
}
if (tipo=="censos"){
    # number of cores to use, often set to one less than the total available
  cl <- makeCluster(num_cores)
  
  # Export data, indicator definitions and the necessary functions to the cluster
  clusterExport(cl, c("data_filt", "indicator_definitions", "scl_pct","scl_pctv2","scl_ratio_decil", "scl_mean","scl_gini","calculate_indicators", "evaluatingFilter", "drop_na"))
  
  # Load necessary packages on each node of the cluster
  clusterEvalQ(cl, {
    library(magrittr)
    library(dplyr)
    library(rlang)
  })
  
  is_haven_labelled <- function(x) {
    inherits(x, "haven_labelled")
  }
  
  # Convert all haven_labelled columns to numeric
  data_filt <- data_filt %>%
    mutate(across(where(is_haven_labelled), as.numeric))
  message(paste("Calculating indicators ",pais,": ", anio))
  results <- parLapply(cl, 1:nrow(indicator_definitions), calculate_indicators, data_filt,NaN, indicator_definitions)
}
#### adding unnecesary code:
# .condicion1 <- rlang::parse_expr("sexo_ci==1")
# .condicion2 <- rlang::parse_expr("npers==1")
# 
# data_aux <- data_filtSVD %>%
# dplyr::summarize(proportion = survey_ratio(eval(.condicion1), eval(.condicion2),vartype = c("cv","se"),na.rm=TRUE),
#                  indicator = ".nombre",
#                  n =sum(eval(.condicion2))) %>%
#   rename(
#     value = proportion,
#     cv = proportion_cv,
#     se = proportion_se,
#     sample = n
#   )
# .condicion1 <- rlang::parse_expr("sexo_ci==1")
# .condicion2 <- rlang::parse_expr("npers==1")
# .group_vars <- "isoalpha3,year"
# disaggregation <- strsplit(.group_vars, ",")[[1]]
# data_aux <- data_filtSVD %>%
#   dplyr::group_by_at(disaggregation) %>%
#   dplyr::summarize(proportion = survey_ratio(eval(.condicion1), eval(.condicion2),vartype = c("cv","se"),na.rm=TRUE),
#                    indicator = ".nombre",
#                    n =sum(eval(.condicion2))) %>%
#   rename(
#     value = proportion,
#     cv = proportion_cv,
#     se = proportion_se,
#     sample = n
#   )%>%
#   dplyr::ungroup()



rm("data_filt")
gc()
# Combine results
data_total <- do.call(rbind, results)

# Stop the cluster
stopCluster(cl)

# remove NA 
message(paste("Quality analysis ",pais,": ", anio))
#disaggregations to remove NA
#to do add this to the code so that they are removed
vars_to_check <- c("sex", "disability", "ethnicity", "migration", "area", "quintile", "age", "value")

data_total <- data_total %>%
  purrr::reduce(vars_to_check, function(data, var) {
    data %>%
      dplyr::filter(!is.na(.data[[var]])) %>%
      dplyr::filter(!is.infinite(.data[[var]]))

  }, .init = .)

# showing NA as NA instead of zeros

 data_total <- data_total %>%
   filter(!(is.na(cv) & value==0 & level==0 & se==0))


 data_total <- data_total %>% mutate(# adding iddate
                       iddate = "year",
                       #substituing in geolev1 Total by country and name to idgeo
                       geolev1 = ifelse(geolev1=="Total","country",geolev1),
                       # adding fuente
                       fuente = ifelse(tipo=="censos",paste(pais,"-IPUMS",sep = ""),
                                       paste(pais,"-",str_extract(base, paste("(?<=",pais,"//).+(?=//data_arm)",sep = "")),sep = "")),
                       quality_check = ""
                       ) %>% rename("idgeo"="geolev1")
 

# left join of dictionary and data_total to know the function 
indicator_definitionsv2 <- indicator_definitions  %>% select(indicator_name,aggregation_function)
data_total <- left_join(data_total, indicator_definitionsv2 ,join_by("indicator"=="indicator_name"))

data_total <- data_total %>% mutate(
  muestra_baja = ifelse(!is.na(sample),as.numeric(sample<30),NA_real_),
  se_fuera05 = ifelse(!is.na(se),as.numeric(aggregation_function=="pct"&(value<=0.5 & se>(value^(2/3))/9)),NA_real_),
  se_fuera = ifelse(!is.na(se),as.numeric(aggregation_function=="pct"&value>0.5 & (se>((1-value)^(2/3))/9)),NA_real_),
  quality_check = NA_real_,
  quality_check = case_when((muestra_baja==0 & (se_fuera05==0|se_fuera==0))~1,
                            (muestra_baja==0 & (se_fuera05==1|se_fuera==1))~2,
                            muestra_baja==1~3,
                            TRUE~NA_real_)
)

# reorder by column name
data_total <- data_total[, c("iddate", "year", "idgeo","isoalpha3","fuente","indicator","area",
                             "quintile","sex","education_level","age","ethnicity","disability","migration",
                             "value","level","se","cv","sample","quality_check")]

# if census then add to the name of the results
if (tipo=="censos"){
  data_total$indicator <- lapply(data_total$indicator, function(x) paste(x,'_PHC',sep = ""))
  data_total <- as.data.frame(apply(data_total,2,as.character))
}

end_time <- Sys.time() 

# Now calculate the difference
time_difference <- difftime(end_time, start_time, units = "mins")

message(paste(pais,anio, time_difference, "minutes"))

