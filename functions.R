##### This code creates the functions needed to run the code of indicators
# we define the different types of computations: percentage, mean and level\

# function to restrict combinations
evaluatingFilter <- function(x, variable) {
  # Initialize result as FALSE. It will be set to TRUE if the conditions are met.
  result<-FALSE
  
  # Iterate over each element in variable
  for(condicionExcluyente in variable) {
    
    # If more than one element of 'condicionExcluyente' is found in 'x'
    # The 'unlist' function is used to flatten 'condicionExcluyente' into a vector
    # The '%in%' operator is used to find matching elements in 'x'
    # The 'sum' function counts the number of TRUEs (matches found)
    
    # If condition is met, set result to TRUE
    
    if(sum(unlist(condicionExcluyente) %in% x)==length(condicionExcluyente)){
      result<-(TRUE)}
    else{
      # If condition is not met, continue with the next iteration of the loop
      next
    }
  }
  return(result)
}

# Percentage function 

scl_pct <- function(.data, .nombre, .condicion1, .condicion2, .group_vars) {
  
  # Convert conditions to expressions
  .condicion1 <- rlang::parse_expr(.condicion1)
  .condicion2 <- rlang::parse_expr(.condicion2)
  
  if (!is.null(.group_vars)) {
    data_aux <- .data %>%
      dplyr::group_by_at(.group_vars) %>%
      dplyr::summarise(
        value = sum(factor_ci[!!.condicion1], na.rm=TRUE) / sum(factor_ci[!!.condicion2], na.rm=TRUE),
        indicator = .nombre,
        se = sqrt(value * (1 - value) / sum(!!.condicion2, na.rm=TRUE)),
        cv = se / value * 100,
        level =  sum(factor_ci * !!.condicion1, na.rm=TRUE),
        sample = sum(!!.condicion2, na.rm=TRUE)
      ) %>% 
      dplyr::ungroup()
  } else {
    data_aux <- .data %>%
      dplyr::summarise(
        value = sum(factor_ci[!!.condicion1], na.rm=TRUE) / sum(!!.condicion2, na.rm=TRUE),
        indicator = .nombre,
        se = sqrt(value * (1 - value) / sum(!!.condicion2, na.rm=TRUE)),
        cv = se / value * 100,
        level =  sum(factor_ci * !!.condicion1, na.rm=TRUE),
        sample = sum(!!.condicion2, na.rm=TRUE)
      )
  }
  
  
  # Renaming 'age_lmk' or 'age_scl' to 'age' if they are present in .group_vars
  if('age_lmk' %in% .group_vars){
    data_aux <- data_aux %>% rename(age = age_lmk)
  } else if('age_scl' %in% .group_vars){
    data_aux <- data_aux %>% rename(age = age_scl)
  } else if('age_15_64_lmk' %in% .group_vars){
    data_aux <- data_aux %>% rename(age = age_15_64_lmk)
  } else if('age_15_29_lmk' %in% .group_vars){
    data_aux <- data_aux %>% rename(age = age_15_29_lmk)
  }
  
  # Add disaggregation columns if not already present
  for (disaggregation_col in c("sex", "education_level", "disability", "quintile", "ethnicity", "migration", "age", "area", "year", "isoalpha3", "geolev1")) {
    if (!(disaggregation_col %in% colnames(data_aux))) {
      data_aux[[disaggregation_col]] <- "Total"
    }
  }
  
  # Rearrange columns
  data_aux <- data_aux %>% 
    dplyr::select(isoalpha3, year, geolev1, indicator, sex, education_level, disability, quintile, ethnicity, migration, age, area,
                  value, level, se, cv, sample)
  
  return(data_aux)
}



# Mean function 
scl_mean <- function(.data, .nombre, .mean_var, .condicion, .group_vars) {
  
  # Convert conditions to expressions
  .condicion <- rlang::parse_expr(.condicion)
  .mean_var <- rlang::sym(.mean_var)  # convert to symbol
  
  if (!is.null(.group_vars)) {
    data_aux <- .data %>%
      dplyr::filter(!!.condicion) %>%
      dplyr::group_by_at(.group_vars) %>%
      dplyr::summarise(
        value = weighted.mean(!!.mean_var,w=factor_ci, na.rm=TRUE),
        indicator = .nombre,
        se = sqrt(stats::var(!!.mean_var * factor_ci, na.rm = TRUE) / sum(!!.mean_var,na.rm=TRUE)),
        cv = sqrt(stats::var(!!.mean_var * factor_ci, na.rm = TRUE)) / value * 100,
        level = sum(!!.condicion, na.rm=TRUE),
        sample = sum(!!.mean_var, na.rm=TRUE)
      ) %>% 
      dplyr::ungroup()
  } else {
    data_aux <- .data %>%
      dplyr::filter(!!.condicion) %>%
      dplyr::summarise(
        value = weighted.mean(!!.mean_var,w=factor_ci, na.rm=TRUE),
        indicator = .nombre,
        se = sqrt(stats::var(!!.mean_var * factor_ci, na.rm = TRUE) / sum(!!.mean_var,na.rm=TRUE)),
        cv = sqrt(stats::var(!!.mean_var * factor_ci, na.rm = TRUE)) / value * 100,
        level = sum(!!.condicion, na.rm=TRUE),
        sample = sum(!!.mean_var, na.rm=TRUE)
      )
  }
  
  # Renaming 'age_lmk' or 'age_scl' to 'age' if they are present in .group_vars
  if('age_lmk' %in% .group_vars){
    data_aux <- data_aux %>% rename(age = age_lmk)
  }
  
  if('age_scl' %in% .group_vars){
    data_aux <- data_aux %>% rename(age = age_scl)
  }
  
  if('age_15_64_lmk' %in% .group_vars){
    data_aux <- data_aux %>% rename(age = age_15_64_lmk)
  }
  
  if('age_15_29_lmk' %in% .group_vars){
    data_aux <- data_aux %>% rename(age = age_15_29_lmk)
  }
  
  # Add disaggregation columns if not already present
  for (disaggregation_col in c("sex", "education_level", "disability", "quintile", "ethnicity", "migration", "age", "area", "year", "isoalpha3", "geolev1")) {
    if (!(disaggregation_col %in% colnames(data_aux))) {
      data_aux[[disaggregation_col]] <- "Total"
    }
  }
  
  # Rearrange columns
  data_aux <- data_aux %>% 
    dplyr::select(isoalpha3, year, geolev1, indicator, sex, education_level, disability, quintile, ethnicity, migration, age, area,
                  value, level, se, cv, sample)
  
  return(data_aux)
}

# Gini function 

scl_gini <- function(.data, .nombre, .condicion1, .condicion2, .group_vars) {
  
  # Convert conditions to expressions
  .condicion1 <- rlang::parse_expr(.condicion1)
  .condicion2 <- rlang::parse_expr(.condicion2)
  
  if (!is.null(.group_vars)) {
    data_aux <- .data %>%
      filter(eval(!!.condicion2)) %>% 
      filter(!is.na(!!.condicion1)) %>% 
      filter(!is.infinite(!!.condicion1)) %>% 
      dplyr::group_by_at(.group_vars) %>%
      dplyr::summarise(
        value = reldist::gini(!!.condicion1, weights = factor_ci),
        indicator = .nombre,
        level =  NA_real_,
        se = NA_real_, 
        cv = NA_real_,
        sample = sum(eval(!!.condicion2), na.rm=TRUE)
      ) %>% 
      dplyr::ungroup()
  } else {
    data_aux <- .data %>%
      filter(eval(!!.condicion2)) %>% 
      filter(!is.na(!!.condicion1)) %>% 
      filter(!is.infinite(!!.condicion1)) %>% 
      dplyr::summarise(
        value = reldist::gini(!!.condicion1, weights = factor_ci),
        indicator = .nombre,
        level =  NA_real_,
        se = NA_real_, 
        cv = NA_real_,
        sample = sum(eval(!!.condicion2), na.rm=TRUE)
      )
  }
  
  # Add disaggregation columns if not already present
  for (disaggregation_col in c("sex", "education_level", "disability", "quintile", "ethnicity", "migration", "age", "area", "year", "isoalpha3", "geolev1")) {
    if (!(disaggregation_col %in% colnames(data_aux))) {
      data_aux[[disaggregation_col]] <- "Total"
    }
  }
  
  # Rearrange columns
  data_aux <- data_aux %>% 
    dplyr::select(isoalpha3, year, geolev1, indicator, sex, education_level, disability, quintile, ethnicity, migration, age, area,
                  value, level, se, cv, sample)
  
  return(data_aux)
}

calculate_indicators <- function(i, data, indicator_definitions) {
  
  # Extract each component of the current indicator definition
  ind <- indicator_definitions[i, ]
  aggregation_function <- ind$aggregation_function
  numerator_condition <- ind$numerator_condition
  denominator_condition <- ind$denominator_condition
  disaggregation <- strsplit(ind$disaggregation, ",")[[1]]
  excludeDisaggregation <- strsplit(strsplit(ind$excludeDisaggregation," ")[[1]],",")
  
  # Initialize a list to store results
  res_list <- list()
  
  # Generate all possible combinations of disaggregations
  disaggregation_combinations <- expand.grid(lapply(disaggregation, function(x) {
    if (x %in% c("year", "isoalpha3")) {
      return(x)
    } else {
      return(c(x, "Total"))
    }
  }))
  disaggregation_combinations <- unique(disaggregation_combinations) # Remove duplicates
  
  # Iterate over each disaggregation combination
  for (j in 1:nrow(disaggregation_combinations)) {
    
    current_disaggregation <- as.vector(unlist(disaggregation_combinations[j, ]))
    current_disaggregation <- current_disaggregation[current_disaggregation != "Total"]
    
    # Evaluate exclusion condition
    conditionDesaggregation <- evaluatingFilter(as.vector(t(current_disaggregation)),excludeDisaggregation)
    
    # If the condition for exclusion is not met, calculate the indicator
    if(!conditionDesaggregation) {
      if(aggregation_function == "pct") {
        res <- scl_pct(data, ind$indicator_name, numerator_condition, denominator_condition, current_disaggregation)
        res_list[[j]] <- res
      } else if(aggregation_function == "mean") {
        res <- scl_mean(data, ind$indicator_name, numerator_condition, denominator_condition, current_disaggregation)
        res_list[[j]] <- res
      }
          else if(aggregation_function == "gini") {
          res <- scl_gini(data, ind$indicator_name, numerator_condition, denominator_condition, current_disaggregation)
          res_list[[j]] <- res
      }
      
    }
  }
  
  # Combine all disaggregated and total results
  res <- do.call(rbind, res_list)
  
  return(res)
}


