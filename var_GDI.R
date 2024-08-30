# Script para generar variables intermedias - GDI

# 1. Censos

if (tipo == "censos") {
  
  data_filt <- data_filt %>% 
    mutate(disability =  case_when(dis_ci == 1 ~ "person_with_disability",
                                   dis_ci == 0 ~"person_with_no_disability",  
                                   TRUE ~ NA_character_), 
           afroind_ci = ifelse(afroind_ci==9 & relacion_ci ==3, afroind_ch, afroind_ci),
           ethnicity = case_when(afroind_ci == 1 ~ "Indi", 
                                 afroind_ci == 2 ~ "Afro",
                                 afroind_ci == 3 ~ "Otro", 
                                 TRUE ~NA_character_),
           recortes_poblacion = case_when(edad_ci >= 0 & edad_ci <=15 ~ "rango_0_15", 
                                          edad_ci >=16 & edad_ci <= 30 ~ "rango_16_30", 
                                          edad_ci >=31 & edad_ci <= 45 ~ "rango_31_45", 
                                          edad_ci >=46 & edad_ci <= 60 ~ "rango_46_60", 
                                          edad_ci >=61 & edad_ci <= 75 ~ "rango_61_75", 
                                          edad_ci >=76 ~ "rango_76_mas"), 
           migration = ifelse(migrante_ci == 1, "Migrant", 
                              ifelse(migrante_ci == 0, "Non-migrant", NA_character_))) 
  
  # then select only added variables and specific columns
  
}

# 2. Encuestas

if (tipo == "encuestas") {
  
  # creating a vector with initial column names
  initial_column_names <- names(data_filt)

  data_filt <- data_filt %>% 
  mutate(disability =  case_when(dis_ci == 1 ~ "person_with_disability",
                                 dis_ci == 0 ~"person_with_no_disability", 
                                 TRUE ~ NA_character_), 
         afroind_ci = ifelse(afroind_ci==9 & relacion_ci ==3, afroind_ch, afroind_ci),
         ethnicity = case_when(afroind_ci == 1 ~ "Indi", 
                               afroind_ci == 2 ~ "Afro",
                               afroind_ci == 3 ~ "Otro", 
                               TRUE ~NA_character_),
         recortes_poblacion = case_when(edad_ci >= 0 & edad_ci <=15 ~ "rango_0_15", 
                                        edad_ci >=16 & edad_ci <= 30 ~ "rango_16_30", 
                                        edad_ci >=31 & edad_ci <= 45 ~ "rango_31_45", 
                                        edad_ci >=46 & edad_ci <= 60 ~ "rango_46_60", 
                                        edad_ci >=61 & edad_ci <= 75 ~ "rango_61_75", 
                                        edad_ci >=76 ~ "rango_76_mas"),
         migration = ifelse(migrante_ci == 1, "Migrant", 
                            ifelse(migrante_ci == 0, "Non-migrant", NA_character_))) 



}