##### Script para generar variables intermedias - SCL/SCL

# 1. Censos

if (tipo == "censos") {
  
  # creating a vector with initial column names
  initial_column_names <- names(data_filt)
  is_haven_labelled <- function(x) {
    inherits(x, "haven_labelled")
  }
  
  # Convert all haven_labelled columns to numeric
  data_filt <- data_filt %>%
    mutate(across(where(is_haven_labelled), as.numeric))
  num_cores <- as.integer((detectCores() - 1)/2)  # number of cores to use, often set to one less than the total available
  
  cluster <- new_cluster(num_cores)
  cluster_library(cluster, "dplyr")  
  initial_column_names <- names(data_filt)
  
  
  data_soc <- data_filt %>% group_by(geolev1) %>%partition(cluster) %>% 
    mutate(jefa_ci = dplyr::if_else(jefe_ci == 1, as.numeric(sexo_ci == 2), NA_real_),
           ylm_ci=as.double(ylm_ci), ynlm_ci=as.double(ynlm_ci),
           urbano_ci = case_when(zona_c == 1 ~ 1, 
                                 is.na(zona_c) ~NA_real_, 
                                 TRUE ~ 0), 
           pob_sfd = dplyr::if_else(sexo_ci == 2 | afroind_ci == 1 | afroind_ci == 2 | dis_ci == 1, 1, 0),  # variable requested for SFD - GDI
           pob18_ci = as.numeric(edad_ci <= 18),
           pob65_ci = as.numeric(edad_ci >= 65),
           single_member = miembros_ci == 1,
           age_scl = case_when(edad_ci>=0 & edad_ci<5 ~"00_04",
                               edad_ci>=5 & edad_ci<15 ~"05_14",
                               edad_ci>=15 & edad_ci<25 ~"15_24",
                               edad_ci>=25 & edad_ci<65 ~"25_64",
                               edad_ci>=65 & edad_ci<99 ~"65+", 
                               TRUE ~NA_character_)) %>%
    mutate(ytot_ci = pmax(0, rowSums(cbind(ylm_ci, ylnm_ci, ynlm_ci, ynlnm_ci), na.rm=TRUE)),
           ytot_ci = dplyr::if_else(is.na(ylm_ci) & is.na(ylnm_ci) & is.na(ynlm_ci) & is.na(ynlnm_ci),NA_real_, ytot_ci),
           yallsr18 = dplyr::if_else(edad_ci>=18, ytot_ci, NA)) %>%
    group_by(anio_c, pais_c, idh_ch) %>%
    mutate(ytot_ch = dplyr::if_else(single_member, sum(ytot_ci,na.rm=TRUE), NA),
           ytot_ch = pmax(0, ytot_ch),
           hhyallsr = dplyr::if_else(single_member, sum(yallsr18,na.rm=TRUE), NA),
           hhyallsr = pmax(0, hhyallsr),
           ywomen = sum(yallsr18[sexo_ci == 2], na.rm = TRUE),
           hhywomen = max(ywomen, na.rm = TRUE),
           jefa_ch = dplyr::if_else(jefe_ci == 1, sum(jefa_ci, na.rm = TRUE), 0),
           miembro6_ch = as.numeric(sum(edad_ci < 6 & relacion_ci > 0 & relacion_ci <= 5) > 0),
           miembro65_ch = as.numeric(sum(edad_ci >= 65 & relacion_ci > 0 & relacion_ci <= 5) > 0),
           miembro6y16_ch = as.numeric(sum(edad_ci >=6 & edad_ci <=16  & relacion_ci > 0 & relacion_ci <= 5) > 0),
           shareylmfem_ch = hhywomen / hhyallsr,
           perceptor_ci = dplyr::if_else(ytot_ci > 0, sum(miembros_ci, na.rm = TRUE), NA_real_),
           perceptor_ch = suppressWarnings(max(perceptor_ci, na.rm = TRUE))) %>%
    ungroup() %>%
    # Mutate to compute additional variables
    mutate(
      # Income per capita definition 
      pc_ytot_ch = dplyr::if_else(nmiembros_ch > 0, ytot_ch / nmiembros_ch, NA),
      pc_ytot_ch = dplyr::if_else(pc_ytot_ch <= 0, NA, pc_ytot_ch),
      # Define area and sex based on zona_c and sexo_ci respectively, 
      income_category = case_when(
        (pc_ytot_ch < lp31_ci ~ "extreme"),  # extreme poverty
        (pc_ytot_ch >= lp31_ci) & (pc_ytot_ch < lp5_ci) ~ "poverty",  # poverty
        (pc_ytot_ch >= lp5_ci) & (pc_ytot_ch < lp31_ci*4) ~ "vulnerable",  # vulnerable
        (pc_ytot_ch >= lp31_ci*4) & (pc_ytot_ch < lp31_ci*20) ~ "middle",  # middle class
        (pc_ytot_ch >= lp31_ci*20) ~ "rich", 
        TRUE ~ NA_character_),  # rich,
      area = case_when(
        zona_c == 1 ~ "urban", 
        zona_c == 0 ~ "rural", 
        TRUE ~ NA_character_
      ),
      sex = case_when(
        sexo_ci == 2 ~ "women",
        sexo_ci == 1 ~ "men", 
        TRUE ~ NA_character_
      ),
      # Calculate hhfem_ch
      hhfem_ch = dplyr::if_else(hhywomen >= .5, 1, dplyr::if_else(is.na(yallsr18), NA, 0)),
      # remesas
      #indexrem = dplyr::if_else(jefe_ci == 1 & !is.na(remesas_ch) & remesas_ch > 0, 1, NA),
      #ylmprixh = ylmpri_ci / (horaspri_ci * 4.34),
      #vivienda 
      hacinamiento_ch = nmiembros_ch / cuartos_ch,
      #demografia dependencia 
      depen_ch = nmiembros_ch / perceptor_ch
    )%>% 
    collect()

  # creating an if to see if pc_ytot_ch has a value%>% 
  if (length(unique(data_soc$pc_ytot_ch))>5){ 
    data_soc <- data_soc %>%
      arrange(pc_ytot_ch) %>%
      mutate(
        quintile = cut(pc_ytot_ch, 
                       breaks = quantile(pc_ytot_ch, 
                                         probs = seq(0, 1, by = 0.2), 
                                         na.rm = TRUE, 
                                         names = FALSE),
                       labels = c("quintile_1", "quintile_2", "quintile_3", "quintile_4", "quintile_5"))
      )
  } else{
    data_soc <- data_soc %>% mutate(quintile = NA_character_)
  }    
  # then select only added variables and specific columns
  new_column_names <- setdiff(names(data_soc), initial_column_names)
  
  select_column_names <- c(new_column_names, 
                           "region_BID_c", "pais_c", "geolev1","estrato_ci","area", "zona_c", "relacion_ci", 
                           "idh_ch", "factor_ch", "factor_ci", "idp_ci")
  
  data_soc <- select(data_soc, all_of(select_column_names))
}

# 2. Encuestas

start_time <- Sys.time()

if (tipo == "encuestas") {
  
  # creating a vector with initial column names
  initial_column_names <- names(data_filt)
  
  
  data_soc <- data_filt %>%  
    mutate(jefa_ci = dplyr::if_else(jefe_ci == 1, as.numeric(sexo_ci == 2), NA_real_),
           ylm_ci = as.double(ylm_ci), 
           ynlm_ci = as.double(ynlm_ci),
           pob_sfd = dplyr::if_else(sexo_ci == 2 | afroind_ci == 1 | afroind_ci == 2 | dis_ci == 1, 1, 0),
           pob18_ci = as.numeric(edad_ci <= 18),
           pob65_ci = as.numeric(edad_ci >= 65),
           miembros_ci = as.numeric(miembros_ci == 1),
           ytot_ci = pmax(0, rowSums(cbind(ylm_ci, ylnm_ci, ynlm_ci, ynlnm_ci), na.rm = TRUE)),
           ytot_ci = dplyr::if_else(is.na(ylm_ci) & is.na(ylnm_ci) & is.na(ynlm_ci) & is.na(ynlnm_ci),NA_real_, ytot_ci),
           yallsr18 = dplyr::if_else(edad_ci >= 18, ytot_ci, NA_real_),
           age_scl = case_when(edad_ci>=0 & edad_ci<5 ~"00_04",
                               edad_ci>=5 & edad_ci<15 ~"05_14",
                               edad_ci>=15 & edad_ci<25 ~"15_24",
                               edad_ci>=25 & edad_ci<65 ~"25_64",
                               edad_ci>=65 & edad_ci<99 ~"65+", 
                               TRUE ~NA_character_)) %>%
    group_by(idh_ch) %>%
    mutate(ytot_ch = dplyr::if_else(miembros_ci == 1, sum(ytot_ci, na.rm = TRUE), NA_real_),
           ytot_ch = pmax(0, ytot_ch),
           hhyallsr = dplyr::if_else(miembros_ci == 1, sum(yallsr18, na.rm = TRUE), NA_real_),
           hhyallsr = pmax(0, hhyallsr),
           ywomen = sum(yallsr18[sexo_ci == 2], na.rm = TRUE),
           hhywomen = max(ywomen, na.rm = TRUE),
           shareylmfem_ch = hhywomen / hhyallsr,
           jefa_ch = dplyr::if_else(jefe_ci == 1, sum(jefa_ci, na.rm = TRUE), 0),
           miembro6_ch = as.numeric(sum(edad_ci < 6 & relacion_ci > 0 & relacion_ci <= 5) > 0),
           miembro65_ch = as.numeric(sum(edad_ci >= 65 & relacion_ci > 0 & relacion_ci <= 5) > 0),
           miembro6y16_ch = as.numeric(sum(edad_ci >=6 & edad_ci <=16  & relacion_ci > 0 & relacion_ci <= 5) > 0),
           perceptor_ci = dplyr::if_else(ytot_ci > 0, sum(miembros_ci, na.rm = TRUE), NA_real_),
           perceptor_ch = suppressWarnings(max(perceptor_ci, na.rm = TRUE))) %>%
    ungroup() %>% 
    # Mutate to compute additional variables
    mutate(
      # Income per capita definition 
      pc_ytot_ch = dplyr::if_else(nmiembros_ch > 0, ytot_ch / nmiembros_ch, NA),
      pc_ytot_ch = dplyr::if_else(pc_ytot_ch <= 0, NA, pc_ytot_ch),
      # Define area and sex based on zona_c and sexo_ci respectively, 
      income_category = case_when(
        (pc_ytot_ch < lp31_ci ~ "extreme"),  # extreme poverty
        (pc_ytot_ch >= lp31_ci) & (pc_ytot_ch < lp5_ci) ~ "poverty",  # poverty
        (pc_ytot_ch >= lp5_ci) & (pc_ytot_ch < lp31_ci*4) ~ "vulnerable",  # vulnerable
        (pc_ytot_ch >= lp31_ci*4) & (pc_ytot_ch < lp31_ci*20) ~ "middle",  # middle class
        (pc_ytot_ch >= lp31_ci*20) ~ "rich", 
        TRUE ~ NA_character_),  # rich,
      area = case_when(
        zona_c == 1 ~ "urban", 
        zona_c == 0 ~ "rural", 
        TRUE ~ NA_character_
      ),
      sex = case_when(
        sexo_ci == 2 ~ "women",
        sexo_ci == 1 ~ "men", 
        TRUE ~ NA_character_
      ),
      # Calculate hhfem_ch
      hhfem_ch = dplyr::if_else(hhywomen >= .5, 1, dplyr::if_else(is.na(yallsr18), NA, 0)),
      # remesas
      indexrem = dplyr::if_else(jefe_ci == 1 & !is.na(remesas_ch) & remesas_ch > 0, 1, NA),
      ylmprixh = ylmpri_ci / (horaspri_ci * 4.34),
      #vivienda 
      hacinamiento_ch = nmiembros_ch / cuartos_ch,
      #demografia dependencia 
      depen_ch = nmiembros_ch / perceptor_ch
    ) 
  
  # Calculate quintiles
  data_soc <- data_soc %>%
    arrange(pc_ytot_ch) %>%
    mutate(
      quintile = cut(pc_ytot_ch, 
                     breaks = quantile(pc_ytot_ch, 
                                       probs = seq(0, 1, by = 0.2), 
                                       na.rm = TRUE),
                     include.lowest = TRUE,
                     labels = c("quintile_1", "quintile_2", "quintile_3", "quintile_4", "quintile_5"))
    )
  
  # then select only added variables and specific columns
  new_column_names <- setdiff(names(data_soc), initial_column_names)
  
  select_column_names <- c(new_column_names, 
                           "region_BID_c", "pais_c", "ine01","estrato_ci","area", "zona_c", "relacion_ci", 
                           "idh_ch", "factor_ch", "factor_ci", "idp_ci")
  
  data_soc <- select(data_soc, all_of(select_column_names)) 
  
}


end_time <- Sys.time()


end_time - start_time