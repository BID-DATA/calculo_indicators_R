# Script para generar variables intermedias - EDU

# 1. Censos 

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

if (tipo == "encuestas") {
  
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
  # 10 leavers
  if(pais=="SUR"|pais=="HTI") {
    data_filt <- data_filt %>% 
      mutate(leavers = case_when((aedu_ci<13) & (asiste_ci==0) ~ 1,
                                 TRUE ~ NA_real_)
      )
    
  } else if(pais=="BRB"|pais=="COL"|pais=="CRI"|pais=="GUY"|pais=="SLV"|pais=="GTM"|pais=="HND"|pais=="JAM"|pais=="NIC"|pais=="PER"|pais=="VEN") {
    data_filt <- data_filt %>% 
      mutate(leavers = case_when((aedu_ci<11) & (asiste_ci==0) ~ 1,
                                 TRUE ~ NA_real_)
      )
    
  }else {
    data_filt <- data_filt %>% 
      mutate(leavers = case_when((aedu_ci<12) & (asiste_ci==0) ~ 1,
                                   TRUE ~ NA_real_)
      )
  }
  # 11 t_cond_secundaria
  if(pais=="SUR"|pais=="HTI") {
    data_filt <- data_filt %>% 
      mutate(t_cond_secundaria = case_when((aedu_ci>=13) & (age_term_s_c == 1) ~ 1,
                                 TRUE ~ NA_real_)
      )
    
  } else if(pais=="BRB"|pais=="COL"|pais=="CRI"|pais=="GUY"|pais=="SLV"|pais=="GTM"|pais=="HND"|pais=="JAM"|pais=="NIC"|pais=="PER"|pais=="VEN") {
    data_filt <- data_filt %>% 
      mutate(t_cond_secundaria = case_when((aedu_ci>=11) & (age_term_s_c == 1)  ~ 1,
                                 TRUE ~ NA_real_)
      )
    
  }else {
    data_filt <- data_filt %>% 
      mutate(t_cond_secundaria = case_when((aedu_ci>=12) & (age_term_s_c == 1) ~ 1,
                                 TRUE ~ NA_real_)
      )
  }
  # 11 t_cond_primaria
  if(pais=="BRA"|pais=="COL") {
    data_filt <- data_filt %>% 
      mutate(t_cond_primaria = case_when((aedu_ci>=5) & (age_term_p_c == 1) ~ 1,
                                           TRUE ~ NA_real_)
      )
    
  } else if(pais=="TTO") {
    data_filt <- data_filt %>% 
      mutate(t_cond_primaria = case_when((aedu_ci>=7) & (age_term_p_c == 1)  ~ 1,
                                           TRUE ~ NA_real_)
      )
    
  }else {
    data_filt <- data_filt %>% 
      mutate(t_cond_primaria = case_when((aedu_ci>=6) & (age_term_p_c == 1) ~ 1,
                                           TRUE ~ NA_real_)
      )
  }
  
  # 12 age_tert_c, asis_net_tert_c, asis_tert_c
  data_filt <- data_filt %>% 
    mutate(age_tert_c = case_when((edad_ci >= 18 & edad_ci <= 24) ~ 1, 
                                  !(edad_ci >= 18 & edad_ci <= 24) ~ 0,
                                  TRUE ~ NA_real_))
           
  if(pais=="SUR"|pais=="HTI") {
    data_filt <- data_filt %>% 
      mutate( asis_net_tert_c = case_when(aedu_ci >= 13 & asiste_ci == 1 & age_tert_c == 1 ~ 1,
                                                                       TRUE ~ NA_real_),
                                           asis_tert_c = case_when(aedu_ci >= 13 & asiste_ci == 1 & edad_ci >= 17 ~ 1,
                                                                   TRUE ~ NA_real_)
      )
    
  } else if(pais=="BRB"|pais=="COL"|pais=="CRI"|pais=="GUY"|pais=="SLV"|pais=="GTM"|pais=="HND"|pais=="JAM"|pais=="NIC"|pais=="PER"|pais=="VEN") {
    data_filt <- data_filt %>% 
      mutate( asis_net_tert_c = case_when(aedu_ci >= 11 & asiste_ci == 1 & age_tert_c == 1 ~ 1,
                                          TRUE ~ NA_real_),
              asis_tert_c = case_when(aedu_ci >= 11 & asiste_ci == 1 & edad_ci >= 17 ~ 1,
                                      TRUE ~ NA_real_)
      )
    
  }else {
    data_filt <- data_filt %>% 
      mutate( asis_net_tert_c = case_when(aedu_ci >= 12 & asiste_ci == 1 & age_tert_c == 1 ~ 1,
                                          TRUE ~ NA_real_),
              asis_tert_c = case_when(aedu_ci >= 12 & asiste_ci == 1 & edad_ci >= 17 ~ 1,
                                      TRUE ~ NA_real_)
      )
  }
 # age_tert_fin
  data_filt <- data_filt %>% 
    mutate( age_tert_fin = case_when((edad_ci > 24) & (edad_ci <36) ~ 1,
                                        TRUE ~ NA_real_)
    )
  
  # Creando las variables de secundaria baja y alta
  data_filt <- data_filt %>% 
    mutate(seco_low = case_when(
      pais_c %in% c("COL", "BRA") & aedu_ci %in% 6:9 ~ 1,
      pais_c %in% c("BRB","CRI","GTM","GUY","JAM","NIC","PER","VEN","SLV","HND", 
                    "BHS","ARG","ECU","MEX","URY","PAN","PRY", "HTI") & aedu_ci %in% 7:9 ~ 1,
      pais_c %in% c("BOL","CHL","DOM") & aedu_ci %in% 7:8 ~ 1,
      pais_c %in% c("BHS","ARG","ECU","MEX","URY","PAN","PRY") & aedu_ci %in% 7:9 ~ 1,
      pais_c %in% c("BLZ", "SUR") & aedu_ci %in% 7:10 ~ 1,
      pais_c == "TTO" & aedu_ci %in% 8:10 ~ 1,
      TRUE ~ 0
    ),
  # Define secondary high
  seco_high = case_when(
    pais_c %in% c("COL", "BRB","CRI","GTM","GUY","JAM","NIC","PER","VEN","SLV","HND") & aedu_ci %in% 10:11 ~ 1,
    pais_c %in% c("BOL","CHL","DOM") & aedu_ci %in% 9:12 ~ 1,
    pais_c %in% c("BRA", "BHS","ARG","ECU","MEX","URY","PAN","PRY") & aedu_ci %in% 10:12 ~ 1,
    pais_c == "HTI" & aedu_ci %in% 10:13 ~ 1,
    pais_c == "BLZ" & aedu_ci %in% 11:12 ~ 1,
    pais_c == "SUR" & aedu_ci %in% 11:13 ~ 1,
    pais_c == "TTO" & aedu_ci %in% 11:12 ~ 1,
    TRUE ~ 0
  ),
  asis_net_secbaja_c = if_else(seco_low == 1 & asis_net_seco_c == 1, 1, 0),
  asis_net_secalta_c = if_else(seco_high == 1 & asis_net_seco_c == 1, 1, 0),
  
  asis_secbaja_c = if_else(seco_low == 1 & asis_seco_c == 1, 1, 0),
  asis_secalta_c  = if_else(seco_high == 1 & asis_seco_c == 1, 1, 0),
  
  age_seco_low = if_else(seco_low == 1 & age_seco_c == 1, 1, 0),
  age_seco_high = if_else(seco_high == 1 & age_seco_c == 1, 1, 0),
  
  age_term_sb_c = case_when(
    pais_c %in% c("COL", "BRA", "BRB", "CRI", "GTM", "GUY", "JAM", "NIC", "PER", "VEN", "SLV", "HND", "BHS", 
                  "ARG", "ECU", "MEX", "URY", "PAN", "PRY", "HTI") & edad_ci %in% 15:16 ~ 1,
    pais_c %in% c("BOL", "CHL", "DOM") & edad_ci %in% 14:15 ~ 1,
    pais_c %in% c("BLZ", "SUR", "TTO") & edad_ci %in% 16:17 ~ 1,
    TRUE ~ 0
  ),
  
  age_term_sa_c_ =age_term_s_c, # same as full secondary
    )
  #
  
  data_filt <- data_filt %>% 
    mutate(
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
  # Nivel Edu ----
  # Create `nivel_edu` variable
  data_filt <- data_filt %>%
    mutate(
      # Initialize `nivel_edu` as NA
      nivel_edu = NA_real_,
      
      # Apply conditions for each country based on the values of `pais_c` and `aedu_ci`
      
      # Colombia (COL)
      nivel_edu = case_when(
        pais_c == "COL" & aedu_ci <= 4 ~ 1,   # Primaria incompleta
        pais_c == "COL" & aedu_ci == 5 ~ 2,   # Primaria completa
        pais_c == "COL" & aedu_ci %in% 6:8 ~ 3, # Secundaria ciclo 1 incompleta
        pais_c == "COL" & aedu_ci == 9 ~ 4,   # Secundaria ciclo 1 completa
        pais_c == "COL" & aedu_ci == 10 ~ 5,  # Secundaria incompleta
        pais_c == "COL" & aedu_ci >= 11 ~ 6,  # Secundaria completa
        TRUE ~ nivel_edu
      ),
      
      # Brazil (BRA)
      nivel_edu = case_when(
        pais_c == "BRA" & aedu_ci <= 4 ~ 1,
        pais_c == "BRA" & aedu_ci == 5 ~ 2,
        pais_c == "BRA" & aedu_ci %in% 6:8 ~ 3,
        pais_c == "BRA" & aedu_ci == 9 ~ 4,
        pais_c == "BRA" & aedu_ci %in% 10:11 ~ 5,
        pais_c == "BRA" & aedu_ci >= 12 ~ 6,
        TRUE ~ nivel_edu
      ),
      
      # Bolivia (BOL), Chile (CHL), Dominican Republic (DOM)
      nivel_edu = case_when(
        pais_c %in% c("BOL", "CHL", "DOM") & aedu_ci <= 5 ~ 1,
        pais_c %in% c("BOL", "CHL", "DOM") & aedu_ci == 6 ~ 2,
        pais_c %in% c("BOL", "CHL", "DOM") & aedu_ci == 7 ~ 3,
        pais_c %in% c("BOL", "CHL", "DOM") & aedu_ci == 8 ~ 4,
        pais_c %in% c("BOL", "CHL", "DOM") & aedu_ci %in% 9:11 ~ 5,
        pais_c %in% c("BOL", "CHL", "DOM") & aedu_ci >= 12 ~ 6,
        TRUE ~ nivel_edu
      ),
      
      # Other countries like BHS, ARG, ECU, MEX, URY, PAN, PRY
      nivel_edu = case_when(
        pais_c %in% c("BHS", "ARG", "ECU", "MEX", "URY", "PAN", "PRY") & aedu_ci <= 5 ~ 1,
        pais_c %in% c("BHS", "ARG", "ECU", "MEX", "URY", "PAN", "PRY") & aedu_ci == 6 ~ 2,
        pais_c %in% c("BHS", "ARG", "ECU", "MEX", "URY", "PAN", "PRY") & aedu_ci %in% 7:8 ~ 3,
        pais_c %in% c("BHS", "ARG", "ECU", "MEX", "URY", "PAN", "PRY") & aedu_ci == 9 ~ 4,
        pais_c %in% c("BHS", "ARG", "ECU", "MEX", "URY", "PAN", "PRY") & aedu_ci %in% 10:11 ~ 5,
        pais_c %in% c("BHS", "ARG", "ECU", "MEX", "URY", "PAN", "PRY") & aedu_ci >= 12 ~ 6,
        TRUE ~ nivel_edu
      ),
      
      # Other countries: CRI, BRB, GTM, GUY, JAM, NIC, PER, VEN, SLV, HND
      nivel_edu = case_when(
        pais_c %in% c("CRI", "GTM", "GUY", "JAM", "NIC", "PER", "VEN", "SLV", "HND") & aedu_ci <= 5 ~ 1,
        pais_c %in% c("CRI", "GTM", "GUY", "JAM", "NIC", "PER", "VEN", "SLV", "HND") & aedu_ci == 6 ~ 2,
        pais_c %in% c("CRI", "GTM", "GUY", "JAM", "NIC", "PER", "VEN", "SLV", "HND") & aedu_ci %in% 7:8 ~ 3,
        pais_c %in% c("CRI", "GTM", "GUY", "JAM", "NIC", "PER", "VEN", "SLV", "HND") & aedu_ci == 9 ~ 4,
        pais_c %in% c("CRI", "GTM", "GUY", "JAM", "NIC", "PER", "VEN", "SLV", "HND") & aedu_ci %in% 10:11 ~ 5,
        pais_c %in% c("CRI", "GTM", "GUY", "JAM", "NIC", "PER", "VEN", "SLV", "HND") & aedu_ci >= 12 ~ 6,
        TRUE ~ nivel_edu
      ),
      
      # Suriname (SUR)
      nivel_edu = case_when(
        pais_c == "SUR" & aedu_ci <= 5 ~ 1,
        pais_c == "SUR" & aedu_ci == 6 ~ 2,
        pais_c == "SUR" & aedu_ci %in% 7:9 ~ 3,
        pais_c == "SUR" & aedu_ci == 10 ~ 4,
        pais_c == "SUR" & aedu_ci %in% 11:12 ~ 5,
        pais_c == "SUR" & aedu_ci >= 13 ~ 6,
        TRUE ~ nivel_edu
      ),
      
      # Trinidad and Tobago (TTO)
      nivel_edu = case_when(
        pais_c == "TTO" & aedu_ci <= 6 ~ 1,
        pais_c == "TTO" & aedu_ci == 7 ~ 2,
        pais_c == "TTO" & aedu_ci %in% 8:9 ~ 3,
        pais_c == "TTO" & aedu_ci == 10 ~ 4,
        pais_c == "TTO" & aedu_ci == 11 ~ 5,
        pais_c == "TTO" & aedu_ci >= 12 ~ 6,
        TRUE ~ nivel_edu
      ),
      
      # Barbados (BRB)
      nivel_edu = case_when(
        pais_c == "BRB" & aedu_ci <= 5 ~ 1,
        pais_c == "BRB" & aedu_ci == 6 ~ 2,
        pais_c == "BRB" & aedu_ci %in% 7:8 ~ 3,
        pais_c == "BRB" & aedu_ci == 9 ~ 4,
        pais_c == "BRB" & aedu_ci %in% 10:11 ~ 5,
        pais_c == "BRB" & aedu_ci >= 12 ~ 6,
        TRUE ~ nivel_edu
      ),
      
      # Haiti (HTI)
      nivel_edu = case_when(
        pais_c == "HTI" & aedu_ci <= 5 ~ 1,
        pais_c == "HTI" & aedu_ci == 6 ~ 2,
        pais_c == "HTI" & aedu_ci %in% 7:8 ~ 3,
        pais_c == "HTI" & aedu_ci == 9 ~ 4,
        pais_c == "HTI" & aedu_ci %in% 10:12 ~ 5,
        pais_c == "HTI" & aedu_ci >= 13 ~ 6,
        TRUE ~ nivel_edu
      ),
      
      # Belize (BLZ)
      nivel_edu = case_when(
        pais_c == "BLZ" & aedu_ci <= 5 ~ 1,
        pais_c == "BLZ" & aedu_ci == 6 ~ 2,
        pais_c == "BLZ" & aedu_ci %in% 7:9 ~ 3,
        pais_c == "BLZ" & aedu_ci == 10 ~ 4,
        pais_c == "BLZ" & aedu_ci == 11 ~ 5,
        pais_c == "BLZ" & aedu_ci >= 12 ~ 6,
        TRUE ~ nivel_edu
      ),
      
      # Adding superior education (eduui_ci and eduuc_ci)
      nivel_edu = case_when(
        eduui_ci == 1 ~ 7,  # Superior incomplete
        eduuc_ci == 1 ~ 8,  # Superior complete
        TRUE ~ nivel_edu
      )
    )
  
  # Create `highest_degree` variable
  data_filt <- data_filt %>%
    mutate(
      highest_degree = case_when(
        nivel_edu == 1 ~ 0,  # Ninguno
        nivel_edu == 2 | nivel_edu == 3 ~ 1,  # Primaria
        nivel_edu %in% 4:5 ~ 2,  # Secundaria baja
        nivel_edu %in% 6:7 ~ 3,  # Secundaria alta
        nivel_edu == 8 ~ 4,  # Universitaria
        TRUE ~ NA_real_
      )
    )
  
  
}