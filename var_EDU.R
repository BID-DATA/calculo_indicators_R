# Script para generar variables intermedias - EDU

# Censos ----

if (tipo == "censos") {
  
  data_filt <- data_filt %>% 
    mutate (age_25_mas = ifelse(edad_ci >= 25, 1, 0), 
            #2. Ninis
            nini = ifelse(is.na(asiste_ci) & is.na(condocup_ci), NA, 
                          ifelse(asiste_ci == 0 & (condocup_ci == 2 | condocup_ci == 3), 1, 0)))
  
  #3. Edades teóricas costumizadas por país y nivel 
  # 3.1 Primaria por grupo de pasises 
  # 3.1.1 Edad teorica y asistencia  
  if (pais == "COL"|pais == "BRA") {
    data_filt <- data_filt %>% 
      mutate(age_prim_c = case_when((edad_ci >= 6 & edad_ci <= 10) ~ 1, 
                                    !(edad_ci >= 6 & edad_ci <= 10) ~ 0,
                                    TRUE ~ NA_real_),
             asis_net_prim_c = case_when((aedu_ci >= 0 & aedu_ci < 5) & (asiste_ci == 1 & age_prim_c == 1) ~ 1,
                                         TRUE ~ NA_real_),
             asis_prim_c = case_when((aedu_ci >= 0 & aedu_ci < 5) & (asiste_ci == 1 & edad_ci >= 6) ~ 1,
                                     TRUE ~ NA_real_)
      )
    
  } else if(pais=="BRB") {
    data_filt <- data_filt %>% 
      mutate(age_prim_c = case_when((edad_ci >= 5 & edad_ci <= 10) ~ 1, 
                                    !(edad_ci >=5 & edad_ci <= 10) ~ 0,
                                    TRUE ~ NA_real_),
             asis_net_prim_c = case_when((aedu_ci >= 0 & aedu_ci < 6) & asiste_ci == 1 & age_prim_c == 1 ~ 1,
                                         TRUE ~ NA_real_),
             asis_prim_c = case_when((aedu_ci >= 0 & aedu_ci < 6) & (asiste_ci == 1 & edad_ci >= 5)~ 1,
                                     TRUE ~ NA_real_)
      )
    
  } else if(pais=="TTO") {
    data_filt <- data_filt %>% 
      mutate(age_prim_c = case_when((edad_ci >= 6 & edad_ci <= 12) ~ 1, 
                                    !(edad_ci >= 6 & edad_ci <= 12) ~ 0,
                                    TRUE ~ NA_real_),
             asis_net_prim_c = case_when((aedu_ci >= 0 & aedu_ci < 7) & (asiste_ci == 1 & age_prim_c == 1) ~ 1,
                                         TRUE ~ NA_real_),
             asis_prim_c = case_when((aedu_ci >= 0 & aedu_ci < 7) & (asiste_ci == 1 & edad_ci >= 6)~ 1,
                                     TRUE ~ NA_real_)
      )
  }else {
    data_filt <- data_filt %>% 
      mutate(age_prim_c = case_when((edad_ci >= 6 & edad_ci <= 11) ~ 1, 
                                    !(edad_ci >= 6 & edad_ci <= 11) ~ 0,
                                    TRUE ~ NA_real_),
             asis_net_prim_c = case_when((aedu_ci >= 0 & aedu_ci < 6) & asiste_ci == 1 & age_prim_c == 1 ~ 1,
                                         TRUE ~ NA_real_),
             asis_prim_c = case_when((aedu_ci >= 0 & aedu_ci < 6) & (asiste_ci == 1 & edad_ci >= 6) ~ 1,
                                     TRUE ~ NA_real_)
      )
  }
  #*4. Sobreedad
  #*
  if (pais == "COL"|pais == "BRA") {
    data_filt <- data_filt %>% 
      mutate(age_prim_sobre = case_when((asiste_ci == 1 & aedu_ci == 0 & edad_ci >= 8)~ 1,
                                        (asiste_ci == 1 & aedu_ci == 1 & edad_ci >= 9)~ 1,
                                        (asiste_ci == 1 & aedu_ci == 2 & edad_ci >= 10)~ 1,
                                        (asiste_ci == 1 & aedu_ci == 3 & edad_ci >= 11)~ 1,
                                        (asiste_ci == 1 & aedu_ci == 4 & edad_ci >= 12)~ 1,
                                        TRUE ~ NA_real_))
    
  } else if(pais=="BRB") {
    data_filt <- data_filt %>% 
      mutate(age_prim_sobre = case_when(
        (asiste_ci==1 & aedu_ci == 0 & edad_ci >= 7)~ 1,
        (asiste_ci==1 & aedu_ci == 1 & edad_ci >= 8)~ 1,
        (asiste_ci==1 & aedu_ci == 2 & edad_ci >= 9)~ 1,
        (asiste_ci==1 & aedu_ci == 3 & edad_ci >= 10)~ 1,
        (asiste_ci==1 & aedu_ci == 4 & edad_ci >= 11)~ 1,
        (asiste_ci==1 & aedu_ci == 5 & edad_ci >= 12)~ 1,
        TRUE ~ NA_real_)
      )
    
  } else if(pais=="TTO") {
    data_filt <- data_filt %>% 
      mutate(age_prim_sobre = case_when(
        (asiste_ci == 1 & aedu_ci == 0 & edad_ci >= 8)~ 1,
        (asiste_ci == 1 & aedu_ci == 1 & edad_ci >= 9)~ 1,
        (asiste_ci == 1 & aedu_ci == 2 & edad_ci >= 10)~ 1,
        (asiste_ci == 1 & aedu_ci == 3 & edad_ci >= 11)~ 1,
        (asiste_ci == 1 & aedu_ci == 4 & edad_ci >= 12)~ 1,                                 
        (asiste_ci == 1 & aedu_ci == 5 & edad_ci >= 13)~ 1,
        (asiste_ci == 1 & aedu_ci == 6 & edad_ci >= 14)~ 1,
        TRUE ~ NA_real_)
      )
  }else {
    data_filt <- data_filt %>% 
      mutate(age_prim_sobre = case_when(
        (asiste_ci == 1 & aedu_ci == 0 & edad_ci >= 8)~ 1,
        (asiste_ci == 1 & aedu_ci == 1 & edad_ci >= 9)~ 1,
        (asiste_ci == 1 & aedu_ci == 2 & edad_ci >= 10)~ 1,
        (asiste_ci == 1 & aedu_ci == 3 & edad_ci >= 11)~ 1,
        (asiste_ci == 1 & aedu_ci == 4 & edad_ci >= 12)~ 1,
        (asiste_ci == 1 & aedu_ci == 5 & edad_ci >= 13)~ 1,
        TRUE ~ NA_real_)
      )
  }
  #*Terminación primaria
  if (pais == "COL"|pais == "BRA"|pais == "BRB") {
    data_filt <- data_filt %>% 
      mutate(age_term_p_c = case_when((edad_ci >= 13 & edad_ci <= 15) ~ 1, 
                                      !(edad_ci >= 13 & edad_ci <= 15) ~ 0,
                                      TRUE ~ NA_real_)
      )
    
  } else if(pais=="TTO") {
    data_filt <- data_filt %>% 
      mutate(age_term_p_c = case_when((edad_ci >= 15 & edad_ci <= 17) ~ 1, 
                                      !(edad_ci >= 15 & edad_ci <= 17) ~ 0,
                                      TRUE ~ NA_real_)
      )
  }else {
    data_filt <- data_filt %>% 
      mutate(age_term_p_c = case_when((edad_ci >= 14 & edad_ci <= 16) ~ 1, 
                                      !(edad_ci >= 14 & edad_ci <= 16) ~ 0,
                                      TRUE ~ NA_real_)
      )
  }
  #6 Asistencia Secundaria
  if (pais=="CRI"|pais=="GTM"|pais=="GUY"|pais=="JAM"|pais=="NIC"|pais=="PER"|pais=="VEN") {
    data_filt <- data_filt %>% 
      mutate(age_seco_c = case_when((edad_ci >= 12&edad_ci <= 16) ~ 1, 
                                    !(edad_ci >= 12 & edad_ci <= 16) ~ 0,
                                    TRUE ~ NA_real_),
             asis_net_seco_c = case_when((aedu_ci >= 6 & aedu_ci < 11) & asiste_ci == 1 & age_seco_c == 1 ~ 1,
                                         TRUE ~ NA_real_),
             asis_seco_c = case_when((aedu_ci >= 6 & aedu_ci < 11)  & edad_ci >= 6  & asiste_ci == 1 ~ 1,
                                     TRUE ~ NA_real_)
      )
    
  } else if(pais=="HTI"|pais=="SUR") {
    data_filt <- data_filt %>% 
      mutate(age_seco_c = case_when((edad_ci >= 12 & edad_ci <= 18) ~ 1, 
                                    !(edad_ci >= 12 & edad_ci <= 18) ~ 0,
                                    TRUE ~ NA_real_),
             asis_net_seco_c = case_when((aedu_ci >= 6 & aedu_ci < 13) & age_seco_c == 1  & asiste_ci == 1 ~ 1,
                                         TRUE ~ NA_real_),
             asis_seco_c = case_when((aedu_ci >= 6 & aedu_ci < 13) & edad_ci >= 6  & asiste_ci == 1 ~ 1,
                                     TRUE ~ NA_real_)
      )
    
  } else if(pais=="BRB") {
    data_filt <- data_filt %>% 
      mutate(age_seco_c = case_when((edad_ci >= 11 & edad_ci <= 15) ~ 1, 
                                    !(edad_ci >= 11 & edad_ci <= 15) ~ 0,
                                    TRUE ~ NA_real_),
             asis_net_seco_c = case_when((aedu_ci >= 6 & aedu_ci < 11) & age_seco_c == 1  & asiste_ci == 1 ~ 1,
                                         TRUE ~ NA_real_),
             asis_seco_c = case_when((aedu_ci >= 6 & aedu_ci < 11) & edad_ci >= 5  & asiste_ci == 1 ~ 1,
                                     TRUE ~ NA_real_)
      )
    
  } else if(pais=="COL") {
    data_filt <- data_filt %>% 
      mutate(age_seco_c = case_when((edad_ci >= 11 & edad_ci <= 16) ~ 1,
                                    !(edad_ci >= 11 & edad_ci <= 16) ~ 0,
                                    TRUE ~ NA_real_),
             asis_net_seco_c = case_when((aedu_ci >= 5 & aedu_ci < 11) & age_seco_c == 1  & asiste_ci == 1 ~ 1,
                                         TRUE ~ NA_real_),
             asis_seco_c = case_when((aedu_ci >= 5 & aedu_ci < 11) & edad_ci >= 6  & asiste_ci == 1 ~ 1,
                                     TRUE ~ NA_real_)
      )
  } else if(pais=="BRA") {
    data_filt <- data_filt %>% 
      mutate(age_seco_c = case_when((edad_ci >= 11 & edad_ci <= 17) ~ 1,
                                    !(edad_ci >= 11 & edad_ci <= 17) ~ 0,
                                    TRUE ~ NA_real_),
             asis_net_seco_c = case_when((aedu_ci >= 5 & aedu_ci < 12) & age_seco_c == 1  & asiste_ci == 1 ~ 1,
                                         TRUE ~ NA_real_),
             asis_seco_c = case_when((aedu_ci >= 5 & aedu_ci < 12) & edad_ci >= 6  & asiste_ci == 1 ~ 1,
                                     TRUE ~ NA_real_)
      )
  } else if(pais=="TTO") {
    data_filt <- data_filt %>% 
      mutate(age_seco_c = case_when((edad_ci >= 13 & edad_ci <= 18) ~ 1,
                                    !(edad_ci >= 13 & edad_ci <= 18) ~ 0,
                                    TRUE ~ NA_real_),
             asis_net_seco_c = case_when((aedu_ci >= 7 & aedu_ci < 12) & age_seco_c == 1  & asiste_ci == 1 ~ 1,
                                         TRUE ~ NA_real_),
             asis_seco_c = case_when((aedu_ci >= 7 & aedu_ci < 12) & edad_ci >= 6  & asiste_ci == 1 ~ 1,
                                     TRUE ~ NA_real_)
      )  
  }else {
    data_filt <- data_filt %>% 
      mutate(age_seco_c = case_when((edad_ci >= 12 & edad_ci <= 17) ~ 1,
                                    !(edad_ci >= 12 & edad_ci <= 17) ~ 0,
                                    TRUE ~ NA_real_),
             asis_net_seco_c = case_when((aedu_ci >= 6 & aedu_ci < 12)   & asiste_ci == 1 & age_seco_c == 1 ~ 1,
                                         TRUE ~ NA_real_),
             asis_seco_c = case_when((aedu_ci >= 6 & aedu_ci <=11)  & edad_ci >= 6 & asiste_ci == 1 ~ 1,
                                     TRUE ~ NA_real_)
      ) 
  }
  #7. Terminacion secundaria 
  if (pais=="CRI"|pais=="GTM"|pais=="GUY"|pais=="JAM"|pais=="NIC"|pais=="PER"|pais=="VEN") {
    data_filt <- data_filt %>% 
      mutate(age_term_s_c = case_when((edad_ci >= 19 & edad_ci <= 21)~ 1,
                                      !(edad_ci >= 19 & edad_ci <= 21)~ 0,
                                      TRUE ~NA_real_
      )
      )
    
  } else if(pais=="HTI"|pais=="SUR"|pais=="TTO") {
    data_filt <- data_filt %>% 
      mutate(age_term_s_c = case_when((edad_ci >= 21 & edad_ci <= 23) ~ 1,
                                      !(edad_ci >= 21 & edad_ci <= 23) ~ 0,
                                      TRUE ~NA_real_
      )
      )
    
  } else if(pais=="BRB") {
    data_filt <- data_filt %>% 
      mutate(age_term_s_c = case_when((edad_ci >= 18 & edad_ci <= 20)~ 1,
                                      !(edad_ci >= 18 & edad_ci <= 20) ~ 0,
                                      TRUE ~NA_real_
      )
      )
    
  }else {
    data_filt <- data_filt %>% 
      mutate(age_term_s_c = case_when((edad_ci >= 20 & edad_ci <= 22)~ 1,
                                      !(edad_ci >= 20 & edad_ci <= 22) ~ 0,
                                      TRUE ~NA_real_
      )
      )
  }
  #8. Terciaria asistencia
  if(pais=="COL"|pais=="CRI"|pais=="GTM"|pais=="GUY"|pais=="JAM"|pais=="NIC"|pais=="PER"|pais=="VEN") {
    data_filt <- data_filt %>% 
      mutate(age_tert_c = case_when((edad_ci >= 17 & edad_ci <= 24) ~ 1, 
                                    !(edad_ci >= 17 & edad_ci <= 24) ~ 0,
                                    TRUE ~ NA_real_),
             asis_net_tert_c = case_when(aedu_ci > 11 & asiste_ci == 1 & age_tert_c == 1 ~ 1,
                                         TRUE ~ NA_real_),
             asis_tert_c = case_when(aedu_ci > 11 & asiste_ci == 1 & edad_ci >= 6 ~ 1,
                                     TRUE ~ NA_real_)
      )
    
  } else if(pais=="HTI"|pais=="SUR"|pais=="TTO") {
    data_filt <- data_filt %>% 
      mutate(age_tert_c = case_when((edad_ci >= 19 & edad_ci <= 24) ~ 1,
                                    !( edad_ci >= 19 & edad_ci <= 24) ~ 0,
                                    TRUE ~ NA_real_),
             asis_net_tert_c = case_when(aedu_ci > 13 & asiste_ci == 1 & age_tert_c == 1 ~ 1,
                                         TRUE ~ NA_real_),
             asis_tert_c = case_when(aedu_ci > 13 & asiste_ci == 1 & edad_ci >= 6 ~ 1,
                                     TRUE ~ NA_real_)
      )
    
  } else if(pais=="BRB") {
    data_filt <- data_filt %>% 
      mutate(age_tert_c = case_when((edad_ci >= 16 & edad_ci <= 24)~ 1, 
                                    !(edad_ci >= 16 & edad_ci <= 24)~ 0,
                                    TRUE ~ NA_real_),
             asis_net_tert_c = case_when(aedu_ci > 11 & asiste_ci == 1 & age_tert_c == 1 ~ 1,
                                         TRUE ~ NA_real_),
             asis_tert_c = case_when(aedu_ci > 11 & asiste_ci == 1 & edad_ci >= 6 ~ 1,
                                     TRUE ~ NA_real_)
      )
    
  }else {
    data_filt <- data_filt %>% 
      mutate(age_tert_c = case_when((edad_ci >= 18 & edad_ci <= 24)~ 1,
                                    !(edad_ci >= 18 & edad_ci <= 24)~ 0,
                                    TRUE ~ NA_real_),
             asis_net_tert_c = case_when(aedu_ci > 12 & asiste_ci == 1 & age_tert_c == 1 ~ 1,
                                         TRUE ~ NA_real_),
             asis_tert_c = case_when(aedu_ci > 12 & asiste_ci == 1 & edad_ci >= 6 ~ 1,
                                     TRUE ~ NA_real_)
      )
  }
  #9. Edusm
  if((pais=="BRB"|pais=="COL"|pais=="CRI"|pais=="GTM"|pais=="GUY"|pais=="HND"|pais=="JAM"|pais=="NIC"|pais=="PER"|pais=="SLV"|pais=="VEN")) {
    data_filt <- data_filt %>% 
      mutate(eduscm_ci = case_when((aedu_ci>11) ~ 1,
                                   !(aedu_ci>11) ~ 0,
                                   TRUE ~ NA_real_)
      )
    
  } else if(pais=="HTI"|pais=="SUR") {
    data_filt <- data_filt %>% 
      mutate(eduscm_ci = case_when((aedu_ci>13) ~ 1,
                                   !(aedu_ci>13) ~ 0,
                                   TRUE ~ NA_real_)
      )
    
  }else {
    data_filt <- data_filt %>% 
      mutate(eduscm_ci = case_when((aedu_ci>12) ~ 1,
                                   !(aedu_ci>12) ~ 0,
                                   TRUE ~ NA_real_)
      )
  }
  
  data_filt <- data_filt %>% 
    mutate(leavers = case_when(
      (edupi_ci == 1 | edupc_ci == 1 | edus1i_ci == 1 | edus1c_ci == 1) & (asiste_ci == 0) ~ 1,
      TRUE ~ NA_real_
    ),
    
    tprimaria = case_when(
      (edupc_ci == 1 | edusi_ci == 1 | edusc_ci == 1 | eduscm_ci == 1) ~ 1,
      TRUE ~ NA_real_
    ),
    
    
    tsecundaria = case_when(
      (edusc_ci == 1 | eduscm_ci == 1) ~ 1,
      TRUE ~ NA_real_
    ),
    
    t_cond_primaria   = case_when(
      (tprimaria == 1   & age_term_p_c == 1) ~ 1,
      TRUE ~ NA_real_
    ),
    
    t_cond_secundaria = case_when(
      (tsecundaria == 1 & age_term_s_c == 1) ~ 1,
      TRUE ~ NA_real_
    ),
    
    grupo_etario = case_when(edad_ci >= 4 & edad_ci <= 5 ~ "age_4_5",
                             edad_ci >= 6 & edad_ci <= 11 ~ "age_6_11",
                             edad_ci >= 12 & edad_ci <= 14 ~ "age_12_14",
                             edad_ci >= 15 & edad_ci <= 17 ~ "age_15_17",
                             edad_ci >= 18 & edad_ci <= 23 ~ "age_18_23",
                             TRUE ~NA_character_),
    
    anos_edu = case_when(aedu_ci == 0 ~ "anos_0",
                         aedu_ci >= 1 & aedu_ci <= 5 ~ "anos_1_5", 
                         aedu_ci == 6 ~ "anos_6", 
                         aedu_ci >= 7 & aedu_ci <= 11 ~ "anos_7_11", 
                         aedu_ci == 12 ~ "anos_12",
                         aedu_ci >= 13  ~ "anos_13_mas",
                         TRUE ~NA_character_),
    
    age_15_24_edu = ifelse(edad_ci >= 15 & edad_ci <= 24, 1, 0),
    age_18_24_edu = ifelse(edad_ci >= 18 & edad_ci <= 24, 1, 0),
    
    )
  
}


# Encuestas ----
if (tipo == "encuestas") {
  
  data_filt <- data_filt %>%
    
    # 1. Edad 25+
    mutate(age_25_mas = ifelse(edad_ci >= 25, 1, 0),
           
           # 2. Ninis
           nini = ifelse(is.na(asiste_ci) & is.na(condocup_ci), NA,
                         ifelse(asiste_ci == 0 & condocup_ci %in% c(2,3), 1, 0))
    ) %>%
    
    # 3. Definir edades y duraciones de ciclos por país
    mutate(
      age_start_prim = case_when(
        pais %in% c("BRB", "BHS", "BLZ", "TTO") ~ 5,
        pais %in% c("GTM", "SLV") ~ 7,
        TRUE ~ 6
      ),
      duration_prim = case_when(
        pais %in% c("BRA", "COL") ~ 5,
        pais == "TTO" ~ 7,
        TRUE ~ 6
      ),
      duration_secbaja = case_when(
        pais %in% c("COL", "BRA", "BLZ", "SUR") ~ 4,
        pais %in% c("BOL", "CHL", "DOM") ~ 2,
        TRUE ~ 3
      ),
      duration_secalta = case_when(
        pais %in% c("COL", "BRB", "CRI", "GTM", "GUY",
                    "JAM", "NIC", "PER", "VEN", "SLV",
                    "HND", "BLZ", "TTO") ~ 2,
        pais %in% c("BOL", "CHL", "DOM", "HTI") ~ 4,
        TRUE ~ 3
      ),
      duration_sec_total = duration_prim + duration_secbaja + duration_secalta
    ) %>%
   

  ## Primaria ----
  
  mutate(
    
    # Edad en primaria (desde ingreso a primer año hasta ingreso último año)
    age_prim_c = as.integer(edad_ci >= age_start_prim & edad_ci < age_start_prim + duration_prim),

    # Asistencia bruta: todos los que asisten a primaria sin importar edad
    asis_prim_c = as.integer(aedu_ci < duration_prim & asiste_ci == 1),
    
    # Asistencia neta: niños en edad oficial de primaria que asisten a primaria o más
    asis_net_prim_c = as.integer(asiste_ci == 1 & age_prim_c == 1),
    
    # Edad esperada para el grado que está cursando el individuo
    age_prim_expected = age_start_prim + aedu_ci, 
    # Sobreedad: rezago de 2 o más años respecto a la edad esperada
    age_prim_sobre = as.integer(aedu_ci <= duration_prim & asiste_ci == 1 & edad_ci >= age_prim_expected + 2),
    
    # Cohorte de terminación: personas en la edad esperada para haber concluido primaria 
    # (3–5 años después de edad de ingreso a último grado)
    age_term_p_c = as.integer(edad_ci >= age_start_prim + duration_prim - 1 + 3 &
                                edad_ci <= age_start_prim + duration_prim - 1 + 5),
    t_cond_primaria = as.integer(aedu_ci >= duration_prim & age_term_p_c),
    
    # Terminación oportuna: haber concluido primaria a los 2 años de la edad oficial de ingreso al último grado
    age_term_p_eo = age_start_prim + duration_prim - 1 + 2,
    age_term_p_eo = as.integer(edad_ci == age_term_p_eo),
    term_eo_primaria = as.integer(
      aedu_ci >= duration_prim & age_term_p_eo ==1)
  ) %>%
    
    ## Secundaria baja ----
  
  mutate(
    # Edad oficial de inicio
    age_secbaja_start = age_start_prim + duration_prim,
    # Edad en sec baja (desde ingreso a primer año hasta ingreso último año)
    age_secbaja_c = as.integer(edad_ci >= age_secbaja_start & 
                                 edad_ci < age_secbaja_start + duration_secbaja),
    
    # Asistencia bruta: todos los que asisten a secbaja sin importar edad
    asis_secbaja_c = as.integer(aedu_ci < duration_prim + duration_secbaja &
                                  aedu_ci >= duration_prim &
                                  asiste_ci == 1),
    
    # Asistencia neta ajustada: jóvenes en edad oficial de secbaja que asisten sin importar nivel
    asis_net_secbaja_c = as.integer(asiste_ci == 1 & age_secbaja_c == 1 & aedu_ci>=duration_prim),
    
    # Edad esperada para el grado que está cursando el individuo
    age_secbaja_expected = age_secbaja_start + (aedu_ci - duration_prim),
    # Sobreedad
    age_secbaja_sobre = as.integer(aedu_ci < duration_prim + duration_secbaja &
                                     aedu_ci>=duration_prim &
                                     asiste_ci == 1 & edad_ci >= age_secbaja_expected + 2),
    
    # Cohorte de terminación: personas en la edad esperada para haber concluido secbaja 
    # (3–5 años después de edad de ingreso a último grado)
    age_term_sb_c = as.integer(edad_ci >= age_secbaja_start + duration_secbaja - 1 + 3 &
                                 edad_ci <= age_secbaja_start + duration_secbaja - 1 + 5),
    t_cond_secbaja = as.integer(aedu_ci >= duration_prim + duration_secbaja & age_term_sb_c),
    

    # Terminación oportuna: haber concluido a los 2 años de la edad oficial de ingreso al último grado
    age_term_sb_eo = age_secbaja_start + duration_secbaja - 1 + 2,
    age_term_sb_eo = as.integer(edad_ci ==  age_term_sb_eo),
    term_eo_secbaja = as.integer(aedu_ci >= duration_prim + duration_secbaja & age_term_sb_eo==1)
  ) %>%

  ## Secundaria alta ----
  
  mutate(
    # Edad oficial de inicio
    age_secalta_start = age_secbaja_start + duration_secbaja,
    # Edad en sec alta (desde ingreso a primer año hasta ingreso último año)
    age_secalta_c = as.integer(edad_ci >= age_secalta_start & edad_ci < age_secalta_start + duration_secalta),
    
    # Asistencia bruta: todos los que asisten a secbaja sin importar edad
    asis_secalta_c = as.integer((aedu_ci < duration_prim + duration_secbaja + duration_secalta) &
                                  (aedu_ci >= duration_prim + duration_secbaja) &
                                  asiste_ci == 1),
    
    # Asistencia neta ajustada: jóvenes en edad oficial de secalta que asisten sin importar nivel
    asis_net_secalta_c = as.integer(asiste_ci == 1 & age_secalta_c == 1 & aedu_ci>=duration_prim + duration_secbaja),
    
    # Edad esperada para el grado que está cursando el individuo
    age_secalta_expected = age_secalta_start + (aedu_ci - (duration_prim + duration_secbaja)),
    # Sobreedad
    age_secalta_sobre = as.integer(aedu_ci < duration_prim + duration_secbaja + duration_secalta &
                                     aedu_ci>=duration_prim + duration_secbaja &
                                     asiste_ci == 1 & edad_ci >= age_secalta_expected + 2),
    
    # Cohorte de terminación: personas en la edad esperada para haber concluido secalta 
    # (3–5 años después de edad de ingreso a último grado)
    age_term_s_c = as.integer(edad_ci >= age_secalta_start + duration_secalta - 1 + 3 &
                                 edad_ci <= age_secalta_start + duration_secalta - 1 + 5),
    t_cond_secundaria = as.integer(aedu_ci >= duration_prim + duration_secbaja + duration_secalta & age_term_s_c),
    
    # Terminación oportuna: haber concluido a los 2 años de la edad oficial de ingreso al último grado
    age_term_sa_eo = age_secalta_start + duration_secalta - 1 + 2,
    age_term_sa_eo = as.integer(edad_ci== age_term_sa_eo),
    term_eo_secalta = as.integer(aedu_ci >= duration_prim + duration_secbaja + duration_secalta & age_term_sa_eo==1)
  ) %>%
    
  ## Secundaria total ----
  mutate(
    age_seco_c = as.integer(edad_ci >= age_secbaja_start & edad_ci < age_secalta_start + duration_secalta),
    asis_seco_c = as.integer((aedu_ci < duration_prim + duration_secbaja + duration_secalta) &
                                  (aedu_ci >= duration_prim) &
                                  asiste_ci == 1),
    asis_net_seco_c = as.integer(asiste_ci == 1 & age_seco_c == 1 & aedu_ci>=duration_prim)
  ) %>% 
    
    
    ## Leavers ----
  # Personas que dejaron de estudiar antes de terminar secundaria
  mutate(
    leavers = case_when(
      aedu_ci < duration_sec_total & asiste_ci == 0 ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
    
    ## Terciaria ----
  mutate(
    # Edad oficial de ingreso a terciaria (último grado de secundaria + 1)
    # age_tert_start = age_secalta_start + duration_secalta,
    # Fin del rango (hasta 24 años)
    age_tert_c = as.integer(edad_ci >= 18 & edad_ci <= 24),
    
    # Asistencia bruta: todos los que asisten a terciaria sin importar edad
    asis_tert_c = as.integer(aedu_ci >= duration_sec_total & asiste_ci == 1),
    
    # Asistencia neta ajustada: jóvenes en edad de tercoaria que asisten sin importar nivel
    asis_net_tert_c = as.integer(asiste_ci == 1 & age_tert_c == 1),
    
    # Cohorte para medir finalización: entre 25 y 34 años
    age_tert_fin = as.integer(edad_ci >= 25 & edad_ci <= 34)
  ) %>%
    
    ## Nivel educativo ----
  mutate(
    # Clasificación según años de educación
    nivel_edu = case_when(
      aedu_ci == 0 ~ 0,  # Sin educación formal
      aedu_ci < duration_prim ~ 1,  # Primaria incompleta
      aedu_ci == duration_prim ~ 2, # Primaria completa
      aedu_ci < duration_prim + duration_secbaja ~ 3,  # Secundaria baja incompleta
      aedu_ci == duration_prim + duration_secbaja ~ 4, # Secundaria baja completa
      aedu_ci < duration_prim + duration_secbaja + duration_secalta ~ 5,  # Secundaria alta incompleta
      aedu_ci >= duration_prim + duration_secbaja + duration_secalta ~ 6, # Secundaria alta completa
      TRUE ~ NA_real_
    ),
    
    # Ajuste con educación superior
    nivel_edu = case_when(
      eduui_ci == 1 ~ 7,  # Superior incompleto
      eduuc_ci == 1 ~ 8,  # Superior completo
      TRUE ~ nivel_edu
    ),
    
    # Agrupación de grados en niveles generales (highest_degree)
    highest_degree = case_when(
      nivel_edu %in% 1:2 ~ 1,  # Primaria
      nivel_edu %in% 3:4 ~ 2,  # Secundaria baja
      nivel_edu %in% 5:6 ~ 3,  # Secundaria alta
      nivel_edu %in% 7:8 ~ 4,  # Terciaria
      TRUE ~ NA_real_
    )
  ) %>% 
    
    mutate(grupo_etario = case_when(edad_ci >= 4 & edad_ci <= 5 ~ "age_4_5",
                                    edad_ci >= 6 & edad_ci <= 11 ~ "age_6_11",
                                    edad_ci >= 12 & edad_ci <= 14 ~ "age_12_14",
                                    edad_ci >= 15 & edad_ci <= 17 ~ "age_15_17",
                                    edad_ci >= 18 & edad_ci <= 23 ~ "age_18_23",
                                    TRUE ~NA_character_),
           
           anos_edu = case_when(aedu_ci == 0 ~ "anos_0",
                                aedu_ci >= 1 & aedu_ci <= 5 ~ "anos_1_5", 
                                aedu_ci == 6 ~ "anos_6", 
                                aedu_ci >= 7 & aedu_ci <= 11 ~ "anos_7_11", 
                                aedu_ci == 12 ~ "anos_12",
                                aedu_ci >= 13 & aedu_ci <= 25  ~ "anos_13_mas",
                                TRUE ~NA_character_),
           
           age_15_24_edu = ifelse(edad_ci >= 15 & edad_ci <= 24, 1, 0),
           age_18_24_edu = ifelse(edad_ci >= 18 & edad_ci <= 24, 1, 0),
           
    )
}