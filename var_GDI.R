# Script para generar variables intermedias - GDI

# 1. Censos

if (tipo == "censos") {
  
  data_gdi <- data_filt %>% 
    mutate(disability =  dplyr::case_when(dis_ci == 1 ~ "person_with_disability",
                                   dis_ci == 0 ~"person_with_no_disability",  
                                   TRUE ~ NA_character_), 
           afroind_ci = ifelse(afroind_ci==9 & relacion_ci ==3, afroind_ch, afroind_ci),
           ethnicity = dplyr::case_when(afroind_ci == 1 ~ "Indi", 
                                 afroind_ci == 2 ~ "Afro",
                                 afroind_ci == 3 ~ "Otro", 
                                 TRUE ~NA_character_),
           recortes_poblacion = dplyr::case_when(edad_ci >= 0 & edad_ci <=15 ~ "rango_0_15", 
                                          edad_ci >=16 & edad_ci <= 30 ~ "rango_16_30", 
                                          edad_ci >=31 & edad_ci <= 45 ~ "rango_31_45", 
                                          edad_ci >=46 & edad_ci <= 60 ~ "rango_46_60", 
                                          edad_ci >=61 & edad_ci <= 75 ~ "rango_61_75", 
                                          edad_ci >=76 & edad_ci <= 90 ~ "rango_76_90",
                                          edad_ci>=91 ~ "rango_91_mas"), 
           migration = dplyr::if_else(migrante_ci == 1, "Migrant", 
                              dplyr::if_else(migrante_ci == 0, "Non-migrant", NA_character_))) 
  
  # then select only added variables and specific columns
  new_column_names <- setdiff(names(data_gdi), initial_column_names)
  
  select_column_names <- c(new_column_names, 
                           "region_BID_c", "pais_c", "geolev1","estrato_ci", "zona_c", "relacion_ci", 
                           "idh_ch", "factor_ch", "factor_ci", "idp_ci")
  
  data_gdi <- select(data_gdi, all_of(select_column_names))
  
}

# 2. Encuestas

if (tipo == "encuestas") {
  
  # creating a vector with initial column names
  initial_column_names <- names(data_filt)

data_gdi <- data_filt %>% 
  mutate(disability =  dplyr::case_when(dis_ci == 1 ~ "person_with_disability",
                                 dis_ci == 0 ~"person_with_no_disability", 
                                 TRUE ~ NA_character_), 
         afroind_ci = ifelse(afroind_ci==9 & relacion_ci ==3, afroind_ch, afroind_ci),
         ethnicity = dplyr::case_when(afroind_ci == 1 ~ "Indi", 
                               afroind_ci == 2 ~ "Afro",
                               afroind_ci == 3 ~ "Otro", 
                               TRUE ~NA_character_),
         recortes_poblacion = dplyr::case_when(edad_ci >= 0 & edad_ci <=15 ~ "rango_0_15", 
                                        edad_ci >=16 & edad_ci <= 30 ~ "rango_16_30", 
                                        edad_ci >=31 & edad_ci <= 45 ~ "rango_31_45", 
                                        edad_ci >=46 & edad_ci <= 60 ~ "rango_46_60", 
                                        edad_ci >=61 & edad_ci <= 75 ~ "rango_61_75", 
                                        edad_ci >=76 & edad_ci <= 90 ~ "rango_76_90",
                                        edad_ci>=91 ~ "rango_91_mas"),
         migration = dplyr::if_else(migrante_ci == 1, "Migrant", 
                            dplyr::if_else(migrante_ci == 0, "Non-migrant", NA_character_))) 

# then select only added variables and specific columns
new_column_names <- setdiff(names(data_gdi), initial_column_names)

select_column_names <- c(new_column_names, 
                         "region_BID_c", "pais_c", "ine01","estrato_ci", "zona_c", "relacion_ci", 
                         "idh_ch", "factor_ch", "factor_ci", "idp_ci")

data_gdi <- select(data_gdi, all_of(select_column_names))

}