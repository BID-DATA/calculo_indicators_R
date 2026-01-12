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
           perceptor_ch = sum(as.numeric(ytot_ci > 0 & miembros_ci>0))) %>%
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
  povertyLinesUpdated <- read_dta("Inputs/masterdata.dta")  
  
  data_filt <- left_join(data_filt, povertyLinesUpdated, by = c("pais_c" = "isoalpha3","anio_c" = "year"))
  
  data_filt <- data_filt %>%  
    mutate(npers=1,
           jefa_ci = if_else(jefe_ci == 1, as.numeric(sexo_ci == 2), NA_real_),
           ylm_ci = as.double(ylm_ci),
           ylnm_ci = as.double(ylnm_ci),
           ynlm_ci = as.double(ynlm_ci),
           miembros_ci = as.numeric(miembros_ci == 1),
           pob65_ci = as.numeric(edad_ci >= 65),
           ytot_ci = pmax(0, rowSums(cbind(ylm_ci, ylnm_ci, ynlm_ci, ynlnm_ci), na.rm = TRUE)),
           ytot_ci = ifelse(is.na(ylm_ci) & is.na(ylnm_ci) & is.na(ynlm_ci) & is.na(ynlnm_ci),NA_real_, ytot_ci),
           perceptor_ci = if_else(ytot_ci > 0, sum(miembros_ci, na.rm = TRUE), NA_real_),
           dis_ch = as.numeric(sum(dis_ci) > 0)
           ) %>%
    group_by(idh_ch) %>%
    mutate(ytot_ch = sum(ytot_ci*(miembros_ci == 1), na.rm = TRUE),
           jefa_ch = if_else(jefe_ci == 1, sum(jefa_ci, na.rm = TRUE), 0),
           perceptor_ch = sum(as.numeric(ytot_ci > 0 & miembros_ci>0))
) %>%
    ungroup() %>% 
    # Mutate to compute additional variables
    mutate(
      # Income per capita definition
      pc_ytot_ch = ifelse(nmiembros_ch > 0, ytot_ch / nmiembros_ch, NA),
      pc_ytot_ch = ifelse(pc_ytot_ch <= 0, NA, pc_ytot_ch),
      pc_ytot_ch_ppp2021 = pc_ytot_ch/ppp_2021/cpi2021_imf,
      # Define area and sex based on zona_c and sexo_ci respectively,
      income_category = case_when(
        (pc_ytot_ch < lp31_2011_old ~ "extreme_2011_old"),  # extreme poverty
        (pc_ytot_ch >= lp31_2011_old) & (pc_ytot_ch < lp5_2011_old) ~ "poverty_2011_old",  # poverty
        (pc_ytot_ch >= lp5_2011_old) & (pc_ytot_ch < lp31_2011_old*4) ~ "vulnerable_2011_old",  # vulnerable
        (pc_ytot_ch >= lp31_2011_old*4) & (pc_ytot_ch < lp31_2011_old*20) ~ "middle_2011_old",  # middle class
        (pc_ytot_ch >= lp31_2011_old*20) ~ "rich_2011_old", 
        TRUE ~ NA_character_),  # rich,
      income_category_2011_CPI = case_when(
        (pc_ytot_ch < lp31_2011.y ~ "extreme_2011_CPI"),  # extreme poverty
        (pc_ytot_ch >= lp31_2011.y) & (pc_ytot_ch < lp5_2011.y) ~ "poverty_2011_CPI",  # poverty
        (pc_ytot_ch >= lp5_2011.y) & (pc_ytot_ch < lp31_2011.y*4) ~ "vulnerable_2011_CPI",  # vulnerable
        (pc_ytot_ch >= lp31_2011.y*4) & (pc_ytot_ch < lp31_2011.y*20) ~ "middle_2011_CPI",  # middle class
        (pc_ytot_ch >= lp31_2011.y*20) ~ "rich_2011_CPI", 
        TRUE ~ NA_character_),  # rich,
      income_category_lp2017 = case_when(
        (pc_ytot_ch < lp365_2017_old ~ "extreme_2017_old"),  # extreme poverty
        (pc_ytot_ch >= lp365_2017_old) & (pc_ytot_ch < lp685_2017_old) ~ "poverty_2017_old",  # poverty
        (pc_ytot_ch >= lp685_2017_old) & (pc_ytot_ch < lp14_2017_old) ~ "vulnerable_2017_old",  # vulnerable
        (pc_ytot_ch >= lp14_2017_old) & (pc_ytot_ch < lp685_2017_old) ~ "middle_2017_old",  # middle class
        (pc_ytot_ch >= lp685_2017_old) ~ "rich_2017_old", 
        TRUE ~ NA_character_),  # rich,
      income_category_lp2017_CPI = case_when(
        (pc_ytot_ch < lp365_2017.y ~ "extreme_lp2017_CPI"),  # extreme poverty
        (pc_ytot_ch >= lp365_2017.y) & (pc_ytot_ch < lp685_2017.y) ~ "poverty_lp2017_CPI",  # poverty
        (pc_ytot_ch >= lp685_2017.y) & (pc_ytot_ch < lp14_2017.y) ~ "vulnerable_lp2017_CPI",  # vulnerable
        (pc_ytot_ch >= lp14_2017.y) & (pc_ytot_ch < lp81_2017.y) ~ "middle_lp2017_CPI",  # middle class
        (pc_ytot_ch >= lp81_2017.y) ~ "rich_lp2017_CPI", 
        TRUE ~ NA_character_),  # rich,      
      income_category_lp2021Jillie = case_when(
        (pc_ytot_ch < lp420_2021_old ~ "extreme_2021_old"),  # extreme poverty
        (pc_ytot_ch >= lp420_2021_old) & (pc_ytot_ch < lp830_2021_old) ~ "poverty_2021_old",  # poverty
        (pc_ytot_ch >= lp830_2021_old) & (pc_ytot_ch < lp420_2021_old*4) ~ "vulnerable_2021_old",  # vulnerable
        (pc_ytot_ch >= lp420_2021_old*4) & (pc_ytot_ch < lp420_2021_old*20) ~ "middle_2021_old",  # middle class
        (pc_ytot_ch >= lp420_2021_old*20) ~ "rich_2021_old", 
        TRUE ~ NA_character_), # rich,
      income_category_lp2021IMF_ICP = case_when(
        (pc_ytot_ch < lp420_2021 ~ "extreme_lp2021_CPI"),  # extreme poverty
        (pc_ytot_ch >= lp420_2021) & (pc_ytot_ch < lp830_2021) ~ "poverty_lp2021_CPI",  # poverty
        (pc_ytot_ch >= lp830_2021) & (pc_ytot_ch < lp420_2021*4) ~ "vulnerable_lp2021_CPI",  # vulnerable
        (pc_ytot_ch >= lp420_2021*4) & (pc_ytot_ch < lp420_2021*20) ~ "middle_lp2021_CPI",  # middle class
        (pc_ytot_ch >= lp420_2021*20) ~ "rich_lp2021_CPI", 
        TRUE ~ NA_character_),
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
      # remesas
      #indexrem = ifelse(jefe_ci == 1 & !is.na(remesas_ch) & remesas_ch > 0, 1, NA),
      #ylmprixh = ylmpri_ci / (horaspri_ci * 4.34),
      #vivienda
      depen_ch = nmiembros_ch / perceptor_ch,
      hacinamiento_ch = nmiembros_ch / cuartos_ch
      )
    
  #weighted_table <- wtd.table(x = data_filt$income_category_lp2021, weights = data_filt$factor_ch)
  #print(weighted_table)
  # Calculate percentages
  #percentages <- (weighted_table / sum(weighted_table)) * 100
  #print(percentages)  
  
  # Calculate quintiles
  # sum all the values of factor ci where ytot"
  # Calculate quintiles


  data_filt <- data_filt %>% rename(isoalpha3 = pais_c,
                                    year = anio_c)
  
  
}




