# Script para generar variables intermedias - GDI

# 1. Censos

if (tipo == "censos") {
  
  message(paste("Preparing variables group by",pais,": ", anio))  
  # creating a vector with initial column names
  is_haven_labelled <- function(x) {
    inherits(x, "haven_labelled")
  }
  
  # Convert all haven_labelled columns to numeric
  data_filt <- data_filt %>%
    mutate(across(where(is_haven_labelled), as.numeric))
  num_cores <- as.integer((detectCores() - 1)/2)  # number of cores to use, often set to one less than the total available
  
  cluster <- new_cluster(num_cores)
  cluster_library(cluster, "dplyr")  
  

  
  message(paste("Creating variables group by",pais,": ", anio))
  data_filt <- data_filt %>% group_by(geolev1) %>%partition(cluster) %>% 
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
                                          edad_ci >=76 ~ "rango_76_mas"), 
          #migration = dplyr::if_else(migrante_ci == 1, "Migrant", 
          #                    dplyr::if_else(migrante_ci == 0, "Non-migrant", NA_character_)),
           sex = dplyr::case_when(
             sexo_ci == 2 ~ "women",
             sexo_ci == 1 ~ "men", 
             TRUE ~ NA_character_
           ),
          isoalpha3 = pais_c,
          year = anio_c,
          #pea = dplyr::case_when((condocup_ci == 1 | condocup_ci == 2 ) & pet == 1 ~ 1,
          #                       condocup_ci==3 & pet==1 ~ 0,
          #                       TRUE ~NA_real_),
          #pet = dplyr::if_else(edad_ci>=15 & edad_ci<=64,1,0),
          #3.1 Primaria por grupo de países
          #nini = dplyr::if_else(asiste_ci==0 & condocup_ci==3,1,0),
          #nini = dplyr::if_else(is.na(asiste_ci) | is.na(condocup_ci),NA,nini),
          #age_15_24_edu = dplyr::if_else(edad_ci>=15 & edad_ci<=24 & !is.na(asiste_ci), 1, 0),
          #age_18_24_edu = dplyr::if_else(edad_ci>=18 & edad_ci<=24 & !is.na(asiste_ci), 1, 0),
          #age_prim_c = dplyr::case_when((pais_c=="COL"|pais_c=="BRA") & (edad_ci>=6 & edad_ci<=10) & !is.na(aedu_ci) ~ 1,
          #                              (pais_c=="COL"|pais_c=="BRA") & !is.na(aedu_ci) ~ 0,
          #                              (pais_c=="BRB") & (edad_ci>=5&edad_ci<=10) ~ 1,
          #                              (pais_c=="BRB") & !is.na(aedu_ci) ~ 0,
          #                              (pais_c=="TTO") & (edad_ci>=6&edad_ci<=12) ~ 1,
          #                              (pais_c=="TTO") & !is.na(aedu_ci) ~ 0,
          #                              (edad_ci>=6 & edad_ci<=11) & !is.na(aedu_ci) ~ 1,
          #                              !(edad_ci>=6 & edad_ci<=11) & !is.na(aedu_ci) ~ 0,
          #                              TRUE ~ NA_real_),
          #asis_net_prim_c = dplyr::case_when((pais_c=="COL"|pais_c=="BRA") & (aedu_ci>=0 & aedu_ci<5) & (asiste_ci==1 & age_prim_c==1)~ 1,
          #                                   (pais_c=="COL"|pais_c=="BRA") & age_prim_c==1 & !(aedu_ci>=0 & aedu_ci<5 & asiste_ci==1)~ 0,
          ##                                   (pais_c=="BRB") & (aedu_ci>=0 & aedu_ci<6) & (asiste_ci==1 & age_prim_c==1)~ 1,
          #                                   (pais_c=="BRB") & age_prim_c==1 & !(aedu_ci>=0 & aedu_ci<6 & asiste_ci==1)~ 0,
          #                                   (pais_c=="TTO") & (aedu_ci>=0 & aedu_ci<7) & (asiste_ci==1 & age_prim_c==1)~ 1,
          #                                   (pais_c=="TTO") & age_prim_c==1 & !(aedu_ci>=0 & aedu_ci<7 & asiste_ci==1)~ 0,
          #                                   (aedu_ci>=0 & aedu_ci<6)   & asiste_ci==1 & age_prim_c==1 ~ 1,
          #                                   age_prim_c==1 & (!(aedu_ci>=0 & aedu_ci<6)   | !asiste_ci==1)~ 0,
          #                                   TRUE ~ NA_real_),
          #age_seco_c = dplyr::case_when((pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & (edad_ci>=12&edad_ci<=16) ~ 1,
          #                              (pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & !(edad_ci>=12&edad_ci<=16) ~ 0,
          #                              (pais_c=="HTI"|pais_c=="SUR") & (edad_ci>=12&edad_ci<=18) ~ 1,
          #                              (pais_c=="HTI"|pais_c=="SUR")& !(edad_ci>=12&edad_ci<=18) ~ 0,
          #                              (pais_c=="BRB") & (edad_ci>=11&edad_ci<=15) ~ 1,
          #                              (pais_c=="BRB")& !(edad_ci>=11&edad_ci<=15) ~ 0,
          #                              (pais_c=="COL") & (edad_ci>=11&edad_ci<=16) ~ 1,
          #                              (pais_c=="COL") & !(edad_ci>=11&edad_ci<=16) ~ 0,
          #                              (pais_c=="BRA") & (edad_ci>=11&edad_ci<=17) ~ 1,
          #                              (pais_c=="BRA")& !(edad_ci>=11&edad_ci<=17) ~ 0,                              
          #                              (pais_c=="TTO") & (edad_ci>=13&edad_ci<=18) ~ 1,
          #                              (pais_c=="TTO") & !(edad_ci>=13&edad_ci<=18) ~ 0,                                
          #                              (edad_ci>=12&edad_ci<=17) ~ 1,
          #                              !(edad_ci>=12&edad_ci<=17) ~ 0,
          #                              TRUE ~ NA_real_),
          #asis_net_seco_c = dplyr::case_when((pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & (aedu_ci>=6 & aedu_ci<11) & asiste_ci==1 & age_seco_c==1~ 1,
          #                                   (pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & age_seco_c==1 & !((aedu_ci>=6 & aedu_ci<11) & asiste_ci==1)~ 0,
          #                                   (pais_c=="HTI"|pais_c=="SUR") & (aedu_ci>=6 & aedu_ci<13) & age_seco_c==1  & asiste_ci==1 ~ 1,
          #                                   (pais_c=="HTI"|pais_c=="SUR") & age_seco_c==1  & !((aedu_ci>=6 & aedu_ci<13) & asiste_ci==1) ~ 0,
          #                                   (pais_c=="BRB") & (aedu_ci>=6 & aedu_ci<11) & age_seco_c==1  & asiste_ci==1 ~ 1,
          #                                   (pais_c=="BRB") & age_seco_c==1 & !((aedu_ci>=6 & aedu_ci<11)   & asiste_ci==1) ~ 0,
          #                                   (pais_c=="COL") & (aedu_ci>=5 & aedu_ci<11) & age_seco_c==1  & asiste_ci==1 ~ 1,
          #                                   (pais_c=="COL") & age_seco_c==1 & !((aedu_ci>=5 & aedu_ci<11)   & asiste_ci==1) ~ 0,
          #                                   (pais_c=="BRA") & (aedu_ci>=5 & aedu_ci<12) & age_seco_c==1  & asiste_ci==1 ~ 1,
          #                                   (pais_c=="BRA") & age_seco_c==1 & !((aedu_ci>=5 & aedu_ci<12)   & asiste_ci==1) ~ 0,
          #                                   (pais_c=="TTO") & (aedu_ci>=7 & aedu_ci<12) & age_seco_c==1  & asiste_ci==1 ~ 1,
          #                                   (pais_c=="TTO") & age_seco_c==1 & !((aedu_ci>=7 & aedu_ci<12)   & asiste_ci==1) ~ 0,
          #                                   (aedu_ci>=6 & aedu_ci<12)   & asiste_ci==1 & age_seco_c==1 ~ 1,
          #                                   !((aedu_ci>=6 & aedu_ci<12)   & asiste_ci==1) & age_seco_c==1 ~ 0,
          #                                   TRUE ~ NA_real_),
          #age_tert_c = dplyr::case_when((pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & (edad_ci>=17&edad_ci<=23) ~ 1,
          #                              (pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & !(edad_ci>=17&edad_ci<=23) ~ 0,
          #                              (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO")&(edad_ci>=19&edad_ci<=23) ~ 1,
          #                              (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO")&!(edad_ci>=19&edad_ci<=23) ~ 0,
          #                              (pais_c=="BRB") &(edad_ci>=16&edad_ci<=23)~ 1,
          #                              (pais_c=="BRB") &!(edad_ci>=16&edad_ci<=23)~ 0,
          #                              (edad_ci>=18&edad_ci<=23)~ 1,
          #                              !(edad_ci>=18&edad_ci<=23)~ 0,
          #                              TRUE ~NA_real_),
        #  
        #  
          #asis_net_tert_c = dplyr::case_when((pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & aedu_ci>11 & asiste_ci==1 & age_tert_c==1 ~ 1,
          #                                   (pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & age_tert_c==1 & !(aedu_ci>11 & asiste_ci==1)  ~ 0,
          #                                   (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO") & aedu_ci>13 & asiste_ci==1 & age_tert_c==1 ~ 1,
          #                                   (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO") & age_tert_c==1 & !(aedu_ci>13 & asiste_ci==1) ~ 0,
          #                                   (pais_c=="BRB") & aedu_ci>11 & asiste_ci==1 & age_tert_c==1~ 1,
          #                                   (pais_c=="BRB") & age_tert_c==1 & !(aedu_ci>11 & asiste_ci==1) ~ 0,
          #                                   aedu_ci>12 & asiste_ci==1 & age_tert_c==1 ~ 1,
          #                                   age_tert_c==1 & !(aedu_ci>12 & asiste_ci==1) ~ 0,
          #                                   TRUE ~NA_real_),          
          #age_term_p_c = dplyr::case_when( is.na(edupc_ci)~NA_real_,
          #                                 (pais_c=="COL"|pais_c=="BRA"|pais_c=="BRB")&(edad_ci>=13&edad_ci<=15)~ 1,
          #                                 (pais_c=="COL"|pais_c=="BRA"|pais_c=="BRB")&!(edad_ci>=13&edad_ci<=15)~ 0,
          #                                 (pais_c=="COL"|pais_c=="BRA"|pais_c=="BRB") & is.na(edupc_ci)~NA_real_,
          #                                 pais_c=="TTO" & (edad_ci>=15&edad_ci<=17)~ 1,
          #                                 pais_c=="TTO" & !(edad_ci>=15&edad_ci<=17)~ 0,
          #                                 pais_c=="TTO" &is.na(edupc_ci)~NA_real_,
          #                                 (edad_ci>=14&edad_ci<=16)~ 1,
          #                                 !(edad_ci>=14&edad_ci<=16)~ 0
          #),          
          #age_term_s_c = dplyr::case_when((pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN")&(edad_ci>=19&edad_ci<=21)~ 1,
          #                                (pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN")&!(edad_ci>=19&edad_ci<=21)~ 0,
          #                                (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO")&(edad_ci>=21&edad_ci<=23) ~ 1,
          #                                (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO") &!(edad_ci>=21&edad_ci<=23) ~ 0,
          #                                (pais_c=="BRB") &(edad_ci>=18&edad_ci<=20)~ 1,
          #                                (pais_c=="BRB") & !(edad_ci>=18&edad_ci<=20) ~ 0,                                  
          #                                (edad_ci>=20&edad_ci<=22)~ 1,
          #                                !(edad_ci>=20&edad_ci<=22)~ 0,
          #                                TRUE ~NA_real_),
          #eduscm_ci = dplyr::case_when((pais_c=="BRB"|pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="HND"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="SLV"|pais_c=="VEN") & (aedu_ci>11) ~ 1,
          #                             (pais_c=="BRB"|pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="HND"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="SLV"|pais_c=="VEN") & !(aedu_ci>11) ~ 0,
          #                             (pais_c=="HTI"|pais_c=="SUR") & (aedu_ci>13) ~ 1,
          #                             (pais_c=="HTI"|pais_c=="SUR") & !(aedu_ci>13) ~ 0,
          #                             (aedu_ci>12) ~ 1,
          #                             !(aedu_ci>12) ~ 0),
          #leavers = dplyr::if_else((edupi_ci==1 | edupc_ci==1 | edus1i_ci==1 | edus1c_ci==1) & (asiste_ci == 0),1,0),
          #leavers = dplyr::if_else(is.na(edupc_ci),NA,leavers),          
          #tprimaria = dplyr::if_else((edupc_ci ==1 | edusi_ci==1 | edusc_ci==1 | eduscm_ci==1),1,0),
          #tprimaria = dplyr::if_else(is.na(edupc_ci),NA,tprimaria),
          #tsecundaria  = dplyr::if_else((edusc_ci ==1 | eduscm_ci==1),1,0),
          #tsecundaria = dplyr::if_else(is.na(edupc_ci),NA,tsecundaria),
          #t_cond_primaria   = dplyr::if_else((tprimaria==1   & age_term_p_c==1),1,0),          
          #t_cond_secundaria = dplyr::if_else((tsecundaria==1 & age_term_s_c==1),1,0),
          npers = dplyr::if_else(TRUE,1,0)
          #jefa_ci = dplyr::if_else(jefe_ci == 1, as.numeric(sexo_ci == 2), NA_real_),
          #1.6.11 Categorías de grandes grupos de ocupación
          #administrativo = dplyr::case_when(
          #  condocup_ci==1 & ocupa_ci==3~ 1,
          #  condocup_ci==1 & !is.na(ocupa_ci)~ 0,
          #  TRUE ~ NA_real_
          #),
          #director = dplyr::case_when(
          #  condocup_ci==1 & ocupa_ci==2~ 1,
          #  condocup_ci==1 & !is.na(ocupa_ci)~ 0,
          #  TRUE ~ NA_real_
          #),
          #obreros = dplyr::case_when(
          #  condocup_ci==1 & ocupa_ci==7~ 1,
          #  condocup_ci==1 & !is.na(ocupa_ci)~ 0,
          #  TRUE ~ NA_real_
          #),
          #profestecnico = dplyr::case_when(
          #  condocup_ci==1 & ocupa_ci==1~ 1,
          #  condocup_ci==1 & !is.na(ocupa_ci)~ 0,
          #  TRUE ~ NA_real_
          #),
          #trabagricola = dplyr::case_when(
          #  condocup_ci==1 & ocupa_ci==6~ 1,
          #  condocup_ci==1 & !is.na(ocupa_ci)~ 0,
          #  TRUE ~ NA_real_
          #),
          #otrostrab = dplyr::case_when(
          #  condocup_ci==1 & ocupa_ci==9~ 1,
          #  condocup_ci==1 & !is.na(ocupa_ci)~ 0,
          #  TRUE ~ NA_real_
          #), 
          #trabss = dplyr::case_when(
          #  condocup_ci==1 & ocupa_ci==5~ 1,
          #  condocup_ci==1 & !is.na(ocupa_ci)~ 0,
          #  TRUE ~ NA_real_
          #),
          #comerciantes = dplyr::case_when(
          #  condocup_ci==1 & ocupa_ci==4~ 1,
          #  condocup_ci==1 & !is.na(ocupa_ci)~ 0,
          #  TRUE ~ NA_real_
          #),
          #agro = dplyr::case_when(
          #  condocup_ci==1 & rama_ci==1~ 1,
          #  condocup_ci==1 & !is.na(rama_ci)~ 0,
          #  TRUE ~ NA_real_
          #),
          #comercio = dplyr::case_when(
          #  condocup_ci==1 & rama_ci==6~ 1,
          #  condocup_ci==1 & !is.na(rama_ci)~ 0,
          #  TRUE ~ NA_real_
          #),
          #construccion = dplyr::case_when(
          #  condocup_ci==1 & rama_ci==5~ 1,
          #  condocup_ci==1 & !is.na(rama_ci)~ 0,
          #  TRUE ~ NA_real_
          #),
          #sspublicos = dplyr::case_when(
          #  condocup_ci==1 & rama_ci==4~ 1,
          #  condocup_ci==1 & !is.na(rama_ci)~ 0,
          #  TRUE ~ NA_real_
          #),
          #minas = dplyr::case_when(
          #  condocup_ci==1 & rama_ci==2~ 1,
          #  condocup_ci==1 & !is.na(rama_ci)~ 0,
          #  TRUE ~ NA_real_
          #),
          #servicios = dplyr::case_when(
          #  condocup_ci==1 & rama_ci==9~ 1,
          #  condocup_ci==1 & !is.na(rama_ci)~ 0,
          #  TRUE ~ NA_real_
          #), 
          #financiero = dplyr::case_when(
          #  condocup_ci==1 & rama_ci==8~ 1,
          #  condocup_ci==1 & !is.na(rama_ci)~ 0,
          #  TRUE ~ NA_real_
          #),
          #industria = dplyr::case_when(
          #  condocup_ci==1 & rama_ci==3~ 1,
          #  condocup_ci==1 & !is.na(rama_ci)~ 0,
          #  TRUE ~ NA_real_
          #),
          #transporte = dplyr::case_when(
          #  condocup_ci==1 & rama_ci==7~ 1,
          #  condocup_ci==1 & !is.na(rama_ci)~ 0,
          #  TRUE ~ NA_real_
          #),
          #1.7 Categorías
          #patron = dplyr::case_when(condocup_ci==1 & categopri_ci==1 ~ 1, 
          #                          condocup_ci==1 & categopri_ci!=1 ~ 0),
          #ctapropia = dplyr::case_when(condocup_ci==1 & categopri_ci==2 ~ 1, 
          #                             condocup_ci==1 & categopri_ci!=2 ~ 0),
        # 
          )  %>% group_by(anio_c, pais_c, idh_ch) %>%
    mutate(#jefa_ch = dplyr::if_else(jefe_ci == 1, sum(jefa_ci, na.rm = TRUE), 0),
     miembro6_ch = as.numeric(sum(edad_ci < 6 & relacion_ci > 0 & relacion_ci <= 5) > 0),
     miembro65_ch = as.numeric(sum(edad_ci >= 65 & relacion_ci > 0 & relacion_ci <= 5) > 0),
     miembro6y16_ch = as.numeric(sum(edad_ci >=6 & edad_ci <=16  & relacion_ci > 0 & relacion_ci <= 5) > 0),
     dis_ch = as.numeric(sum(dis_ci) > 0)
     )  %>% 
    ungroup() %>%
    mutate(disability_ch =  dplyr::case_when(dis_ch == 1 ~ "household_with_disability",
                                          dis_ch == 0 ~"household_with_no_disability",  
                                   TRUE ~ NA_character_))  %>% 
    collect()
  
  # then select only added variables and specific columns
  #new_column_names <- setdiff(names(data_gdi), initial_column_names)
  
  #select_column_names <- c(new_column_names, 
  #                         "region_BID_c", "pais_c", "geolev1","estrato_ci", "zona_c", "relacion_ci", 
  #                         "idh_ch", "factor_ch", "factor_ci", "idp_ci")
  
  #data_gdi <- select(data_gdi, all_of(select_column_names))
  
}

# 2. Encuestas

if (tipo == "encuestas") {
  
  # creating a vector with initial column names
  #initial_column_names <- names(data_filt)

  data_filt <- data_filt %>% 
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
#new_column_names <- setdiff(names(data_gdi), initial_column_names)

#select_column_names <- c(new_column_names, 
#                         "region_BID_c", "pais_c", "ine01","estrato_ci", "zona_c", "relacion_ci", 
#                         "idh_ch", "factor_ch", "factor_ci", "idp_ci")

#data_gdi <- select(data_gdi, all_of(select_column_names))

}