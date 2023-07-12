# script general para crear indicadores de SCL
  
  ### Data ----

  start_time <- Sys.time()

  # code that returns the address of the survey
  source("directory_periods.R")
  
  base <- functionRoundAndSurvey(pais,tipo,anio)
  
  # Read data
  data <- read_dta(base)
 
  
  if (tipo == "censos") {
    #Keep only needed variables
    variables_censos <- readxl::read_xlsx("Inputs/D.7.1.3 Diccionario variables censos.xlsx")
    
    varlist_censos <- variables_censos %>% 
      filter(!is.na(Variable))
    
    data_filt <- data[,varlist_censos$Variable]
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
  
  # Check which of the required variables are not in the data
  missing_vars <- setdiff(required_vars, colnames(data))
  
  # Add the missing variables to the data with NA values
  for (var in missing_vars) {
    data[[var]] <- NA
  }
  
  # creating empty column for each missing variable in R
  data_filt <- data[,unique(variables_encuestas$Variable)]
  
}

#### Compute intermediate variables  ####

source("var_LMK.R")

source("var_EDU.R")

source("var_GDI.R")

source("var_SOC.R")


#### Join final data with intermediate variables #####

if (tipo == "censos") {
  
  data_scl <- data_filt %>%  
    select(-c(afroind_ci)) %>% 
    left_join(data_lmk, by = c("region_BID_c", "pais_c","geolev1",  "estrato_ci", "zona_c",
                               "relacion_ci", "idh_ch", "factor_ch", "idp_ci", "factor_ci")) %>% 
    left_join(data_edu, by = c("region_BID_c", "pais_c", "geolev1", "estrato_ci", "zona_c",
                               "relacion_ci", "idh_ch", "factor_ch", "idp_ci", "factor_ci")) %>%
    left_join(data_soc, by = c("region_BID_c", "pais_c", "geolev1", "estrato_ci", "zona_c",
                               "relacion_ci", "idh_ch", "factor_ch", "idp_ci", "factor_ci")) %>% 
    left_join(data_gdi, by = c("region_BID_c", "pais_c", "geolev1","estrato_ci", "zona_c",
                               "relacion_ci", "idh_ch", "factor_ch", "idp_ci", "factor_ci")) %>% 
    rename(year = anio_c, isoalpha3 = pais_c)
  
}

if (tipo == "encuestas") {
  
data_scl <- data_filt %>%  
  select(-c(afroind_ci)) %>% 
  left_join(data_lmk, by = c("region_BID_c", "pais_c","estrato_ci", "zona_c","ine01",
                             "relacion_ci", "idh_ch", "idp_ci", "factor_ci", "factor_ch")) %>% 
  left_join(data_edu, by = c("region_BID_c", "pais_c","estrato_ci", "zona_c", "factor_ch",
                             "relacion_ci", "idh_ch", "idp_ci", "factor_ci", "ine01")) %>%
  left_join(data_soc, by = c("region_BID_c", "pais_c", "estrato_ci", "zona_c", "factor_ch",
                             "relacion_ci", "idh_ch","idp_ci", "factor_ci", "ine01")) %>% 
  left_join(data_gdi, by = c("region_BID_c", "pais_c","estrato_ci", "zona_c", "factor_ch",
                             "relacion_ci", "idh_ch", "idp_ci", "factor_ci", "ine01")) %>% 
# left_join(base_geo, by = c("ine01" = "ine01", "pais_c" = "pais")) %>% 
    rename(year = anio_c, isoalpha3 = pais_c)
  
}

# Remove data we do not need and free memory
rm("data_lmk", "data_edu", "data_soc", "data_gdi", "data", "data_filt")
gc()

# Read all functions needed for computation 

source("functions.R")

##### Use parallel programming -----

# read the indicators definitions in the csv
indicator_definitions <- read.csv("Inputs/idef.csv") %>% 
  filter(theme == "wash")
# if needed you can filter here by theme
# 

num_cores <- detectCores() - 1  # number of cores to use, often set to one less than the total available
cl <- makeCluster(num_cores)

# Export data, indicator definitions and the necessary functions to the cluster
clusterExport(cl, c("data_scl", "indicator_definitions", "scl_pct", "scl_mean","scl_gini","calculate_indicators", "evaluatingFilter"))

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
data_scl <- data_scl %>%
  mutate(across(where(is_haven_labelled), as.numeric))

# Call the function in parallel
results <- parLapply(cl, 1:nrow(indicator_definitions), calculate_indicators, data_scl, indicator_definitions)

# Combine results
data_total <- do.call(rbind, results)

# Stop the cluster
stopCluster(cl)

# remove NA 

# disaggregations to remove NA
# to do add this to the code so that they are removed
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

# Now calculate the difference
time_difference <- difftime(end_time, start_time, units = "mins")

print(paste(pais,anio, time_difference, "minutes"))

