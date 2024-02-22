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
  
  
  data_filt <- data_filt %>% group_by(geolev1) %>%partition(cluster) %>% 
    mutate(jefa_ci = if_else(jefe_ci == 1, as.numeric(sexo_ci == 2), NA_real_),
           ylm_ci=as.double(ylm_ci), ynlm_ci=as.double(ynlm_ci),
           urbano_ci = case_when(zona_c == 1 ~ 1, 
                                 is.na(zona_c) ~NA_real_, 
                                 TRUE ~ 0), 
           pob_sfd = if_else(sexo_ci == 2 | afroind_ci == 1 | afroind_ci == 2 | dis_ci == 1, 1, 0),  # variable requested for SFD - GDI
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
           ytot_ci = ifelse(is.na(ylm_ci) & is.na(ylnm_ci) & is.na(ynlm_ci) & is.na(ynlnm_ci),NA_real_, ytot_ci),
           yallsr18 = ifelse(edad_ci>=18, ytot_ci, NA)) %>%
    group_by(anio_c, pais_c, idh_ch) %>%
    mutate(ytot_ch = ifelse(single_member, sum(ytot_ci,na.rm=TRUE), NA),
           ytot_ch = pmax(0, ytot_ch),
           hhyallsr = ifelse(single_member, sum(yallsr18,na.rm=TRUE), NA),
           hhyallsr = pmax(0, hhyallsr),
           ywomen = sum(yallsr18[sexo_ci == 2], na.rm = TRUE),
           hhywomen = max(ywomen, na.rm = TRUE),
           jefa_ch = if_else(jefe_ci == 1, sum(jefa_ci, na.rm = TRUE), 0),
           miembro6_ch = as.numeric(sum(edad_ci < 6 & relacion_ci > 0 & relacion_ci <= 5) > 0),
           miembro65_ch = as.numeric(sum(edad_ci >= 65 & relacion_ci > 0 & relacion_ci <= 5) > 0),
           miembro6y16_ch = as.numeric(sum(edad_ci >=6 & edad_ci <=16  & relacion_ci > 0 & relacion_ci <= 5) > 0),
           shareylmfem_ch = hhywomen / hhyallsr,
           perceptor_ci = if_else(ytot_ci > 0, sum(miembros_ci, na.rm = TRUE), NA_real_),
           dis_ch = as.numeric(sum(dis_ci) > 0),
           perceptor_ch = suppressWarnings(max(perceptor_ci, na.rm = TRUE))) %>%
    ungroup() %>%
    # Mutate to compute additional variables
    mutate(
      # Income per capita definition 
      pc_ytot_ch = ifelse(nmiembros_ch > 0, ytot_ch / nmiembros_ch, NA),
      pc_ytot_ch = ifelse(pc_ytot_ch <= 0, NA, pc_ytot_ch),
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
      disability_ch =  dplyr::case_when(dis_ch == 1 ~ "household_with_disability",
                                        dis_ch == 0 ~"household_with_no_disability",  
                                        TRUE ~ NA_character_),      
      # Calculate hhfem_ch
      hhfem_ch = ifelse(hhywomen >= .5, 1, ifelse(is.na(yallsr18), NA, 0)),
      # remesas
      #indexrem = ifelse(jefe_ci == 1 & !is.na(remesas_ch) & remesas_ch > 0, 1, NA),
      #ylmprixh = ylmpri_ci / (horaspri_ci * 4.34),
      #vivienda 
      hacinamiento_ch = nmiembros_ch / cuartos_ch,
      #demografia dependencia 
      depen_ch = nmiembros_ch / perceptor_ch
    )%>% 
    collect()

  # creating an if to see if pc_ytot_ch has a value%>% 
  if (length(unique(data_filt$pc_ytot_ch))>5){ 
    data_filt <- data_filt %>%
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
    data_filt <- data_filt %>% mutate(quintile = NA_character_)
  }    
  data_filt <- data_filt %>% rename(isoalpha3 = pais_c,
                                    year = anio_c)
}

# 2. Encuestas

start_time <- Sys.time()

if (tipo == "encuestas") {
  
  # creating a vector with initial column names

  
  data_filt <- data_filt %>%  
    mutate(jefa_ci = if_else(jefe_ci == 1, as.numeric(sexo_ci == 2), NA_real_),
           ylm_ci = as.double(ylm_ci), 
           ynlm_ci = as.double(ynlm_ci),
           pob_sfd = if_else(sexo_ci == 2 | afroind_ci == 1 | afroind_ci == 2 | dis_ci == 1, 1, 0),
           pob18_ci = as.numeric(edad_ci <= 18),
           pob65_ci = as.numeric(edad_ci >= 65),
           miembros_ci = as.numeric(miembros_ci == 1),
           ytot_ci = pmax(0, rowSums(cbind(ylm_ci, ylnm_ci, ynlm_ci, ynlnm_ci), na.rm = TRUE)),
           ytot_ci = ifelse(is.na(ylm_ci) & is.na(ylnm_ci) & is.na(ynlm_ci) & is.na(ynlnm_ci),NA_real_, ytot_ci),
           yallsr18 = if_else(edad_ci >= 18, ytot_ci, NA_real_),
           age_scl = case_when(edad_ci>=0 & edad_ci<5 ~"00_04",
                               edad_ci>=5 & edad_ci<15 ~"05_14",
                               edad_ci>=15 & edad_ci<25 ~"15_24",
                               edad_ci>=25 & edad_ci<65 ~"25_64",
                               edad_ci>=65 & edad_ci<99 ~"65+", 
                               TRUE ~NA_character_)) %>%
    group_by(idh_ch) %>%
    mutate(ytot_ch = if_else(miembros_ci == 1, sum(ytot_ci, na.rm = TRUE), NA_real_),
           ytot_ch = pmax(0, ytot_ch),
           hhyallsr = if_else(miembros_ci == 1, sum(yallsr18, na.rm = TRUE), NA_real_),
           hhyallsr = pmax(0, hhyallsr),
           ywomen = sum(yallsr18[sexo_ci == 2], na.rm = TRUE),
           hhywomen = max(ywomen, na.rm = TRUE),
           shareylmfem_ch = hhywomen / hhyallsr,
           jefa_ch = if_else(jefe_ci == 1, sum(jefa_ci, na.rm = TRUE), 0),
           miembro6_ch = as.numeric(sum(edad_ci < 6 & relacion_ci > 0 & relacion_ci <= 5) > 0),
           miembro65_ch = as.numeric(sum(edad_ci >= 65 & relacion_ci > 0 & relacion_ci <= 5) > 0),
           miembro6y16_ch = as.numeric(sum(edad_ci >=6 & edad_ci <=16  & relacion_ci > 0 & relacion_ci <= 5) > 0),
           perceptor_ci = if_else(ytot_ci > 0, sum(miembros_ci, na.rm = TRUE), NA_real_),
           perceptor_ch = suppressWarnings(max(perceptor_ci, na.rm = TRUE))) %>%
    ungroup() %>% 
    # Mutate to compute additional variables
    mutate(
      # Income per capita definition 
      pc_ytot_ch = ifelse(nmiembros_ch > 0, ytot_ch / nmiembros_ch, NA),
      pc_ytot_ch = ifelse(pc_ytot_ch <= 0, NA, pc_ytot_ch),
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
      hhfem_ch = ifelse(hhywomen >= .5, 1, ifelse(is.na(yallsr18), NA, 0)),
      # remesas
      indexrem = ifelse(jefe_ci == 1 & !is.na(remesas_ch) & remesas_ch > 0, 1, ifelse(is.na(remesas_ch),NA_real_,0)),
      ylmprixh = ylmpri_ci / (horaspri_ci * 4.34),
      #vivienda 
      hacinamiento_ch = nmiembros_ch / cuartos_ch,
      #demografia dependencia 
      depen_ch = nmiembros_ch / perceptor_ch
    ) 
  
  # Calculate quintiles
  # sum all the values of factor ci where ytot"
  data_filt <- data_filt %>%
    arrange(pc_ytot_ch,idh_ch) %>%
    mutate(suma1 = ifelse(!is.na(ytot_ch),factor_ci,0),
           suma2 = cumsum(suma1),
           cumulative_weight_prop = ifelse(sum(suma1)!=0, suma2/ sum(suma1),0)) 
  
  countQuintile <- data_filt %>% 
    filter(cumulative_weight_prop!=0) %>% 
    count() %>% pull()
  
  if (countQuintile != 0) {
  data_filt <- data_filt %>%
    mutate(
      quintile = case_when(
        cumulative_weight_prop < 0.20 ~ "quintile_1",
        cumulative_weight_prop < 0.40 ~ "quintile_2",
        cumulative_weight_prop < 0.60 ~ "quintile_3",
        cumulative_weight_prop < 0.80 ~ "quintile_4",
        cumulative_weight_prop >= 0.80 ~ "quintile_5",
        TRUE ~ NA_character_
      )
    )
  countQuintile <- data_filt %>% 
    filter(cumulative_weight_prop!=0) %>% 
    count() %>% pull()
  
  # Quintile_ci urban
  data_filt <- data_filt %>%
    arrange(pc_ytot_ch,idh_ch) %>%
    mutate(suma1 = ifelse(zona_c==1 & !is.na(ytot_ch),factor_ci,0),
           suma2 = cumsum(suma1),
           cumulative_weight_prop = suma2 / sum(suma1)) %>%
    mutate(
      quintile_ci_urban = case_when(
        cumulative_weight_prop < 0.20 & zona_c==1 ~ "quintile_1_urban",
        cumulative_weight_prop < 0.40 & zona_c==1 ~ "quintile_2_urban",
        cumulative_weight_prop < 0.60 & zona_c==1 ~ "quintile_3_urban",
        cumulative_weight_prop < 0.80 & zona_c==1 ~ "quintile_4_urban",
        cumulative_weight_prop >= 0.80 & zona_c==1 ~ "quintile_5_urban",
        TRUE ~ NA_character_
      )
    )
  
  # Quintile rural
  data_filt <- data_filt %>%
    arrange(pc_ytot_ch,idh_ch) %>%
    mutate(suma1 = ifelse(zona_c==0 & !is.na(ytot_ch),factor_ci,0),
           suma2 = cumsum(suma1),
           cumulative_weight_prop_rural = suma2 / sum(suma1)) %>%
    mutate(
      quintile_ci_rural = case_when(
        cumulative_weight_prop_rural < 0.20 & zona_c==0 ~ "quintile_1_rural",
        cumulative_weight_prop_rural < 0.40 & zona_c==0 ~ "quintile_2_rural",
        cumulative_weight_prop_rural < 0.60 & zona_c==0 ~ "quintile_3_rural",
        cumulative_weight_prop_rural < 0.80 & zona_c==0 ~ "quintile_4_rural",
        cumulative_weight_prop_rural >= 0.80 & zona_c==0  ~ "quintile_5_rural",
        TRUE ~ NA_character_
      )
    )
  
  # quintile_ch
  data_filt <- data_filt %>%
    arrange(pc_ytot_ch,idh_ch) %>%
    mutate(suma1 = ifelse(jefe_ci==1 & !is.na(ytot_ch),factor_ci,0),
           suma2 = cumsum(suma1),
           cumulative_weight_prop2 = (suma2 / sum(suma1))) %>%
    mutate(
      quintile_ch = case_when(
        cumulative_weight_prop2 < 0.20 & jefe_ci==1 ~ "quintile_1_ch",
        cumulative_weight_prop2 < 0.40 & jefe_ci==1 ~ "quintile_2_ch",
        cumulative_weight_prop2 < 0.60 & jefe_ci==1 ~ "quintile_3_ch",
        cumulative_weight_prop2 < 0.80 & jefe_ci==1 ~ "quintile_4_ch",
        cumulative_weight_prop2 >= 0.80 & jefe_ci==1 ~ "quintile_5_ch",
        TRUE ~ NA_character_
      )
    )
  
  # quintile_ch_urban
  data_filt <- data_filt %>%
    arrange(pc_ytot_ch,idh_ch) %>%
    mutate(suma1 = ifelse(jefe_ci==1 & !is.na(ytot_ch) & zona_c==1,factor_ci,0),
           suma2 = cumsum(suma1),
           cumulative_weight_prop2 = (suma2 / sum(suma1))) %>%
    mutate(
      quintile_ch_urban = case_when(
        cumulative_weight_prop2 < 0.20 & jefe_ci==1 & zona_c==1 ~ "quintile_1_ch_urban",
        cumulative_weight_prop2 < 0.40 & jefe_ci==1 & zona_c==1 ~ "quintile_2_ch_urban",
        cumulative_weight_prop2 < 0.60 & jefe_ci==1 & zona_c==1 ~ "quintile_3_ch_urban",
        cumulative_weight_prop2 < 0.80 & jefe_ci==1 & zona_c==1 ~ "quintile_4_ch_urban",
        cumulative_weight_prop2 >= 0.80 & jefe_ci==1  & zona_c==1~ "quintile_5_ch_urban",
        TRUE ~ NA_character_
      )
    )
  
  # quintile_ch_rural
  data_filt <- data_filt %>%
    arrange(pc_ytot_ch,idh_ch) %>%
    mutate(suma1 = ifelse(jefe_ci==1 & !is.na(ytot_ch) & zona_c==0,factor_ci,0),
           suma2 = cumsum(suma1),
           cumulative_weight_prop2 = (suma2 / sum(suma1))) %>%
    mutate(
      quintile_ch_rural = case_when(
        cumulative_weight_prop2 < 0.20 & jefe_ci==1 & zona_c==0 ~ "quintile_1_ch_rural",
        cumulative_weight_prop2 < 0.40 & jefe_ci==1 & zona_c==0 ~ "quintile_2_ch_rural",
        cumulative_weight_prop2 < 0.60 & jefe_ci==1 & zona_c==0 ~ "quintile_3_ch_rural",
        cumulative_weight_prop2 < 0.80 & jefe_ci==1 & zona_c==0 ~ "quintile_4_ch_rural",
        cumulative_weight_prop2 >= 0.80 & jefe_ci==1  & zona_c==0~ "quintile_5_ch_rural",
        TRUE ~ NA_character_
      )
    )
  
  # Calculate decile points
  # sum all the values of factor ci where ytot"
  # decile
  data_filt <- data_filt %>%
    arrange(pc_ytot_ch,idh_ch) %>%
    mutate(suma1 = ifelse(!is.na(pc_ytot_ch),factor_ci,0),
           suma2 = cumsum(suma1),
           cumulative_weight_prop = suma2 / sum(factor_ci)) %>%
    mutate(
      decile_ci = case_when(
        cumulative_weight_prop < 0.10 ~ 1,
        cumulative_weight_prop < 0.20 ~ 2,
        cumulative_weight_prop < 0.30 ~ 3,
        cumulative_weight_prop < 0.40 ~ 4,
        cumulative_weight_prop < 0.50 ~ 5,
        cumulative_weight_prop < 0.60 ~ 6,
        cumulative_weight_prop < 0.70 ~ 7,
        cumulative_weight_prop < 0.80 ~ 8,
        cumulative_weight_prop >=0.90 ~ 9,
        TRUE ~ NA_real_
      )
    )
  
  
  data_filt <- data_filt %>% group_by(decile_ci) %>%
    mutate(
      isminv2 = ifelse(!is.na(pc_ytot_ch) & !is.infinite(pc_ytot_ch) ,min(pc_ytot_ch, na.rm = T),NA_real_)
    ) %>%
    ungroup() %>%
    mutate(percentil_points = case_when(
      decile_ci == 2 & (isminv2 == pc_ytot_ch) ~ 10,
      decile_ci == 6 & (isminv2 == pc_ytot_ch) ~ 50,
      decile_ci == 9 & (isminv2 == pc_ytot_ch) ~ 90,
      TRUE ~ NA_real_
    ))

  }
  
  if (countQuintile == 0) {
    
    data_filt <- data_filt %>%
      mutate(quintile=NA_real_,
             quintile_ci_urban = NA_real_,
             quintile_ci_rural = NA_real_,
             quintile_ch = NA_real_,
             quintile_ch_urban = NA_real_,
             quintile_ch_rural = NA_real_,
             percentil_points = 0
      )
    
  }
  
  data_filt <- data_filt %>% rename(isoalpha3 = pais_c,
                                    year = anio_c)
}




