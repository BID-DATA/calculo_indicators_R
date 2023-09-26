# Script para generar variables intermedias - EDU

# 1. Censos 

if (tipo == "censos") {
  # creating a vector with initial column names
  
  data_filt <- data_filt %>% 
    mutate (age_25_mas = ifelse(edad_ci >= 25, 1, 0), 
            
           #2. Ninis
           nini = ifelse(is.na(asiste_ci) & is.na(condocup_ci), NA, 
                         ifelse(asiste_ci == 0 & (condocup_ci == 2 | condocup_ci == 3), 1, 0)),
           
           age_prim_c = case_when((pais_c=="COL"| pais_c=="BRA") & (edad_ci >= 6 & edad_ci <= 10) ~ 1, 
                                  (pais_c=="COL"| pais_c=="BRA") & !(edad_ci >= 6 & edad_ci <= 10) ~ 0, 
                                  
                                  (pais_c=="BRB") & (edad_ci >= 5 & edad_ci <= 10) ~ 1, 
                                  (pais_c=="BRB") & !(edad_ci >=5 & edad_ci <= 10) ~ 0,
                                  
                                  (pais_c=="TTO") & (edad_ci >= 6 & edad_ci <= 12) ~ 1, 
                                  (pais_c=="TTO") & !(edad_ci >= 6 & edad_ci <= 12) ~ 0, 
                                  
                                  (edad_ci >= 6 & edad_ci <= 11) ~ 1, 
                                  !(edad_ci >= 6 & edad_ci <= 11) ~ 0, 
                                  TRUE ~ NA_real_),
           
           asis_net_prim_c = case_when((pais_c=="COL"|pais_c=="BRA") & (aedu_ci >= 0 & aedu_ci < 5) & (asiste_ci == 1 & age_prim_c == 1) ~ 1, 
                                       
                                       (pais_c=="TTO") & (aedu_ci >= 0 & aedu_ci < 7) & (asiste_ci == 1 & age_prim_c == 1) ~ 1,
                                       
                                       (aedu_ci >= 0 & aedu_ci < 6) & asiste_ci == 1 & age_prim_c == 1 ~ 1,
                                       TRUE ~ NA_real_),
           
           asis_prim_c = case_when((pais_c=="COL"|pais_c=="BRA") & (aedu_ci >= 0 & aedu_ci < 5) & (asiste_ci == 1 & edad_ci >= 6) ~ 1,
                                   
                                   (pais_c=="BRB") & (aedu_ci >= 0 & aedu_ci < 6) & (asiste_ci == 1 & edad_ci >= 5)~ 1,
                                   
                                   (pais_c=="TTO") & (aedu_ci >= 0 & aedu_ci < 7) & (asiste_ci == 1 & edad_ci >= 6)~ 1,
                                   
                                   (aedu_ci >= 0 & aedu_ci < 6) & (asiste_ci == 1 & edad_ci >= 6) ~ 1,
                                   TRUE ~ NA_real_),
           
           age_prim_sobre = case_when(
                                       (pais_c=="BRB") &(asiste_ci==1 & aedu_ci == 0 & edad_ci >= 7)~ 1,
                                       (pais_c=="BRB") &(asiste_ci==1 & aedu_ci == 1 & edad_ci >= 8)~ 1,
                                       (pais_c=="BRB") &(asiste_ci==1 & aedu_ci == 2 & edad_ci >= 9)~ 1,
                                       (pais_c=="BRB") &(asiste_ci==1 & aedu_ci == 3 & edad_ci >= 10)~ 1,
                                       (pais_c=="BRB") &(asiste_ci==1 & aedu_ci == 4 & edad_ci >= 11)~ 1,
                                       (pais_c=="BRB") &(asiste_ci==1 & aedu_ci == 5 & edad_ci >= 12)~ 1,
                                       
                                       (pais_c=="COL"|pais_c=="BRA") & (asiste_ci == 1 & aedu_ci == 0 & edad_ci >= 8)~ 1,
                                       (pais_c=="COL"|pais_c=="BRA") & (asiste_ci == 1 & aedu_ci == 1 & edad_ci >= 9)~ 1,
                                       (pais_c=="COL"|pais_c=="BRA") & (asiste_ci == 1 & aedu_ci == 2 & edad_ci >= 10)~ 1,
                                       (pais_c=="COL"|pais_c=="BRA") & (asiste_ci == 1 & aedu_ci == 3 & edad_ci >= 11)~ 1,
                                       (pais_c=="COL"|pais_c=="BRA") & (asiste_ci == 1 & aedu_ci == 4 & edad_ci >= 12)~ 1,
                                       
                                       (pais_c=="TTO") & (asiste_ci == 1 & aedu_ci == 0 & edad_ci >= 8)~ 1,
                                       (pais_c=="TTO") & (asiste_ci == 1 & aedu_ci == 1 & edad_ci >= 9)~ 1,
                                       (pais_c=="TTO") & (asiste_ci == 1 & aedu_ci == 2 & edad_ci >= 10)~ 1,
                                       (pais_c=="TTO") & (asiste_ci == 1 & aedu_ci == 3 & edad_ci >= 11)~ 1,
                                       (pais_c=="TTO") & (asiste_ci == 1 & aedu_ci == 4 & edad_ci >= 12)~ 1,                                 
                                       (pais_c=="TTO") & (asiste_ci == 1 & aedu_ci == 5 & edad_ci >= 13)~ 1,
                                       (pais_c=="TTO") & (asiste_ci == 1 & aedu_ci == 6 & edad_ci >= 14)~ 1,
                                       
                                       (asiste_ci == 1 & aedu_ci == 0 & edad_ci >= 8)~ 1,
                                       (asiste_ci == 1 & aedu_ci == 1 & edad_ci >= 9)~ 1,
                                       (asiste_ci == 1 & aedu_ci == 2 & edad_ci >= 10)~ 1,
                                       (asiste_ci == 1 & aedu_ci == 3 & edad_ci >= 11)~ 1,
                                       (asiste_ci == 1 & aedu_ci == 4 & edad_ci >= 12)~ 1,
                                       (asiste_ci == 1 & aedu_ci == 5 & edad_ci >= 13)~ 1,),
           
           age_term_p_c = case_when((pais_c == "COL"| pais_c == "BRA"| pais_c == "BRB") & (edad_ci >= 13 & edad_ci <= 15) ~ 1, 
                                    (pais_c == "COL"| pais_c == "BRA"| pais_c == "BRB") & !(edad_ci >= 13 & edad_ci <= 15) ~ 0,
                                    
                                    pais_c == "TTO" & (edad_ci >= 15 & edad_ci <= 17) ~ 1,
                                    pais_c == "TTO" & !(edad_ci >= 15 & edad_ci <= 17) ~ 0,
                                    
                                    (edad_ci >= 14 & edad_ci <= 16) ~ 1,
                                    !(edad_ci >= 14 & edad_ci <= 16) ~ 0),
           
           age_seco_c = case_when((pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & (edad_ci >= 12&edad_ci <= 16) ~ 1, 
                                  (pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & !(edad_ci >= 12 & edad_ci <= 16) ~ 0,
                                  
                                  (pais_c=="HTI"|pais_c=="SUR") & (edad_ci >= 12 & edad_ci <= 18) ~ 1,
                                  (pais_c=="HTI"|pais_c=="SUR")& !(edad_ci >= 12 & edad_ci <= 18) ~ 0,
                                   
                                  (pais_c=="BRB") & (edad_ci >= 11 & edad_ci <= 15) ~ 1,
                                  (pais_c=="BRB") & !(edad_ci >= 11 & edad_ci <= 15) ~ 0,
                                  
                                  (pais_c=="COL") & (edad_ci >= 11 & edad_ci <= 16) ~ 1,
                                  (pais_c=="COL") & !(edad_ci >= 11 & edad_ci <= 16) ~ 0,
                                  
                                  (pais_c=="BRA") & (edad_ci >= 11 & edad_ci <= 17) ~ 1,
                                  (pais_c=="BRA")& !(edad_ci >= 11 & edad_ci <= 17) ~ 0,
                                  
                                  (pais_c=="TTO") & (edad_ci >= 13 & edad_ci <= 18) ~ 1,
                                  (pais_c=="TTO") & !(edad_ci >= 13 & edad_ci <= 18) ~ 0,  
                                  
                                  (edad_ci >= 12 & edad_ci <= 17) ~ 1,
                                  !(edad_ci >= 12 & edad_ci <= 17) ~ 0,
                                  TRUE ~ NA_real_),
           
           asis_net_seco_c = case_when((pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & (aedu_ci >= 6 & aedu_ci < 11) & asiste_ci == 1 & age_seco_c == 1 ~ 1,
                                       
                                       (pais_c=="HTI"|pais_c=="SUR") & (aedu_ci >= 6 & aedu_ci < 13) & age_seco_c == 1  & asiste_ci == 1 ~ 1,
                                       
                                       (pais_c=="BRB") & (aedu_ci >= 6 & aedu_ci < 11) & age_seco_c == 1  & asiste_ci == 1 ~ 1,
                                       
                                       (pais_c=="COL") & (aedu_ci >= 5 & aedu_ci < 11) & age_seco_c == 1  & asiste_ci == 1 ~ 1,
                                       
                                       (pais_c=="BRA") & (aedu_ci >= 5 & aedu_ci < 12) & age_seco_c == 1  & asiste_ci == 1 ~ 1,
                                       
                                       (pais_c=="TTO") & (aedu_ci >= 7 & aedu_ci < 12) & age_seco_c == 1  & asiste_ci == 1 ~ 1,
                                       
                                       (aedu_ci >= 6 & aedu_ci < 12)   & asiste_ci == 1 & age_seco_c == 1 ~ 1,
                                       TRUE ~ NA_real_),
           
           asis_seco_c = case_when((pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") &  (aedu_ci >= 6 & aedu_ci < 11) & asiste_ci == 1  & edad_ci >= 6 ~ 1,
                                   
                                   (pais_c=="HTI"|pais_c=="SUR") & (aedu_ci >= 6 & aedu_ci < 13) & edad_ci >= 6  & asiste_ci == 1 ~ 1,
                                   
                                   (pais_c=="BRB") & (aedu_ci >= 6 & aedu_ci < 11) & edad_ci >= 5  & asiste_ci == 1 ~ 1,
                                   
                                   (pais_c=="COL") & (aedu_ci >= 5 & aedu_ci < 11) & edad_ci >= 6  & asiste_ci == 1 ~ 1,
                                   
                                   (pais_c=="BRA") & (aedu_ci >= 5 & aedu_ci < 12) & edad_ci >= 6  & asiste_ci == 1 ~ 1,
                                   
                                   (pais_c=="TTO") & (aedu_ci >= 7 & aedu_ci < 12) & edad_ci >= 6  & asiste_ci == 1 ~ 1,
                                   
                                   (aedu_ci >= 6 & aedu_ci < 12)  & edad_ci >= 6 & asiste_ci == 1 ~ 1,
                                   TRUE ~ NA_real_), 
           
           age_term_s_c = case_when((pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & (edad_ci >= 19 & edad_ci <= 21)~ 1, 
                                    (pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & !(edad_ci >= 19 & edad_ci <= 21)~ 0,
                                    
                                    (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO")& (edad_ci >= 21 & edad_ci <= 23) ~ 1,
                                    (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO") & !(edad_ci >= 21 & edad_ci <= 23) ~ 0,
                                    
                                    (pais_c=="BRB") & (edad_ci >= 18 & edad_ci <= 20)~ 1,
                                    (pais_c=="BRB") & !(edad_ci >= 18 & edad_ci <= 20) ~ 0, 
                                    
                                    (edad_ci >= 20 & edad_ci <= 22)~ 1,
                                    !(edad_ci >= 20 & edad_ci <= 22)~ 0,
                                    TRUE ~NA_real_),
           
           age_tert_c = case_when((pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & (edad_ci >= 17 & edad_ci <= 23) ~ 1,
                                  (pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & !(edad_ci >= 17 & edad_ci <= 23) ~ 0,
                                  
                                  (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO") & (edad_ci >= 19 & edad_ci <= 23) ~ 1,
                                  (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO") & !( edad_ci >= 19 & edad_ci <= 23) ~ 0,
                                  
                                  (pais_c=="BRB") & (edad_ci >= 16 & edad_ci <= 23)~ 1,
                                  (pais_c=="BRB") & !(edad_ci >= 16 & edad_ci <= 23)~ 0,
                                  
                                  (edad_ci >= 18 & edad_ci <= 23)~ 1,
                                  !(edad_ci >= 18 & edad_ci <= 23)~ 0,
                                  
                                  TRUE ~NA_real_),
           
           asis_net_tert_c = case_when((pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN" | pais_c=="BRB") & aedu_ci > 11 & asiste_ci == 1 & age_tert_c == 1 ~ 1, 
                                       
                                       (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO") & aedu_ci > 13 & asiste_ci == 1 & age_tert_c == 1 ~ 1,
                                       
                                       aedu_ci > 12 & asiste_ci == 1 & age_tert_c == 1 ~ 1,
                                       TRUE ~NA_real_),
           
           asis_tert_c = case_when((pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN" | pais_c=="BRB") & aedu_ci > 11 & asiste_ci == 1 & edad_ci >= 6 ~ 1,
                                   
                                   (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO") & aedu_ci > 13 & asiste_ci == 1 & edad_ci >= 6 ~ 1,
                                   
                                   aedu_ci > 12 & asiste_ci == 1 & edad_ci >= 6 ~ 1,
                                   TRUE ~NA_real_),
           
           eduscm_ci = case_when((pais_c=="BRB"|pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="HND"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="SLV"|pais_c=="VEN") & (aedu_ci>11) ~ 1,
                                 (pais_c=="BRB"|pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="HND"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="SLV"|pais_c=="VEN") & !(aedu_ci>11) ~ 0,
                                 (pais_c=="HTI"|pais_c=="SUR") & (aedu_ci>13) ~ 1,
                                 (pais_c=="HTI"|pais_c=="SUR") & !(aedu_ci>13) ~ 0,
                                 (aedu_ci>12) ~ 1,
                                 !(aedu_ci>12) ~ 0),
           
           leavers = case_when(
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

# 2. Encuestas

if (tipo == "encuestas") {
  

  data_filt <- data_filt %>% 
    mutate (age_25_mas = ifelse(edad_ci >= 25, 1, 0), 
            
            #2. Ninis
            nini = ifelse(is.na(asiste_ci) & is.na(condocup_ci), NA, 
                          ifelse(asiste_ci == 0 & (condocup_ci == 2 | condocup_ci == 3), 1, 0)),
            
            age_prim_c = case_when((pais_c=="COL"| pais_c=="BRA") & (edad_ci >= 6 & edad_ci <= 10) ~ 1, 
                                   (pais_c=="COL"| pais_c=="BRA") & !(edad_ci >= 6 & edad_ci <= 10) ~ 0, 
                                   
                                   (pais_c=="BRB") & (edad_ci >= 5 & edad_ci <= 10) ~ 1, 
                                   (pais_c=="BRB") & !(edad_ci >=5 & edad_ci <= 10) ~ 0,
                                   
                                   (pais_c=="TTO") & (edad_ci >= 6 & edad_ci <= 12) ~ 1, 
                                   (pais_c=="TTO") & !(edad_ci >= 6 & edad_ci <= 12) ~ 0, 
                                   
                                   (edad_ci >= 6 & edad_ci <= 11) ~ 1, 
                                   !(edad_ci >= 6 & edad_ci <= 11) ~ 0, 
                                   TRUE ~ NA_real_),
            
            asis_net_prim_c = case_when((pais_c=="COL"|pais_c=="BRA") & (aedu_ci >= 0 & aedu_ci < 5) & (asiste_ci == 1 & age_prim_c == 1) ~ 1, 
                                        
                                        (pais_c=="TTO") & (aedu_ci >= 0 & aedu_ci < 7) & (asiste_ci == 1 & age_prim_c == 1) ~ 1,
                                        
                                        (aedu_ci >= 0 & aedu_ci < 6) & asiste_ci == 1 & age_prim_c == 1 ~ 1,
                                        TRUE ~ NA_real_),
            
            asis_prim_c = case_when((pais_c=="COL"|pais_c=="BRA") & (aedu_ci >= 0 & aedu_ci < 5) & (asiste_ci == 1 & edad_ci >= 6) ~ 1,
                                    
                                    (pais_c=="BRB") & (aedu_ci >= 0 & aedu_ci < 6) & (asiste_ci == 1 & edad_ci >= 5)~ 1,
                                    
                                    (pais_c=="TTO") & (aedu_ci >= 0 & aedu_ci < 7) & (asiste_ci == 1 & edad_ci >= 6)~ 1,
                                    
                                    (aedu_ci >= 0 & aedu_ci < 6) & (asiste_ci == 1 & edad_ci >= 6) ~ 1,
                                    TRUE ~ NA_real_),
            
            age_prim_sobre = case_when(
              (pais_c=="BRB") &(asiste_ci==1 & aedu_ci == 0 & edad_ci >= 7)~ 1,
              (pais_c=="BRB") &(asiste_ci==1 & aedu_ci == 1 & edad_ci >= 8)~ 1,
              (pais_c=="BRB") &(asiste_ci==1 & aedu_ci == 2 & edad_ci >= 9)~ 1,
              (pais_c=="BRB") &(asiste_ci==1 & aedu_ci == 3 & edad_ci >= 10)~ 1,
              (pais_c=="BRB") &(asiste_ci==1 & aedu_ci == 4 & edad_ci >= 11)~ 1,
              (pais_c=="BRB") &(asiste_ci==1 & aedu_ci == 5 & edad_ci >= 12)~ 1,
              
              (pais_c=="COL"|pais_c=="BRA") & (asiste_ci == 1 & aedu_ci == 0 & edad_ci >= 8)~ 1,
              (pais_c=="COL"|pais_c=="BRA") & (asiste_ci == 1 & aedu_ci == 1 & edad_ci >= 9)~ 1,
              (pais_c=="COL"|pais_c=="BRA") & (asiste_ci == 1 & aedu_ci == 2 & edad_ci >= 10)~ 1,
              (pais_c=="COL"|pais_c=="BRA") & (asiste_ci == 1 & aedu_ci == 3 & edad_ci >= 11)~ 1,
              (pais_c=="COL"|pais_c=="BRA") & (asiste_ci == 1 & aedu_ci == 4 & edad_ci >= 12)~ 1,
              
              (pais_c=="TTO") & (asiste_ci == 1 & aedu_ci == 0 & edad_ci >= 8)~ 1,
              (pais_c=="TTO") & (asiste_ci == 1 & aedu_ci == 1 & edad_ci >= 9)~ 1,
              (pais_c=="TTO") & (asiste_ci == 1 & aedu_ci == 2 & edad_ci >= 10)~ 1,
              (pais_c=="TTO") & (asiste_ci == 1 & aedu_ci == 3 & edad_ci >= 11)~ 1,
              (pais_c=="TTO") & (asiste_ci == 1 & aedu_ci == 4 & edad_ci >= 12)~ 1,                                 
              (pais_c=="TTO") & (asiste_ci == 1 & aedu_ci == 5 & edad_ci >= 13)~ 1,
              (pais_c=="TTO") & (asiste_ci == 1 & aedu_ci == 6 & edad_ci >= 14)~ 1,
              
              (asiste_ci == 1 & aedu_ci == 0 & edad_ci >= 8)~ 1,
              (asiste_ci == 1 & aedu_ci == 1 & edad_ci >= 9)~ 1,
              (asiste_ci == 1 & aedu_ci == 2 & edad_ci >= 10)~ 1,
              (asiste_ci == 1 & aedu_ci == 3 & edad_ci >= 11)~ 1,
              (asiste_ci == 1 & aedu_ci == 4 & edad_ci >= 12)~ 1,
              (asiste_ci == 1 & aedu_ci == 5 & edad_ci >= 13)~ 1,),
            
            age_term_p_c = case_when((pais_c == "COL"| pais_c == "BRA"| pais_c == "BRB") & (edad_ci >= 13 & edad_ci <= 15) ~ 1, 
                                     (pais_c == "COL"| pais_c == "BRA"| pais_c == "BRB") & !(edad_ci >= 13 & edad_ci <= 15) ~ 0,
                                     
                                     pais_c == "TTO" & (edad_ci >= 15 & edad_ci <= 17) ~ 1,
                                     pais_c == "TTO" & !(edad_ci >= 15 & edad_ci <= 17) ~ 0,
                                     
                                     (edad_ci >= 14 & edad_ci <= 16) ~ 1,
                                     !(edad_ci >= 14 & edad_ci <= 16) ~ 0),
            
            age_seco_c = case_when((pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & (edad_ci >= 12&edad_ci <= 16) ~ 1, 
                                   (pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & !(edad_ci >= 12 & edad_ci <= 16) ~ 0,
                                   
                                   (pais_c=="HTI"|pais_c=="SUR") & (edad_ci >= 12 & edad_ci <= 18) ~ 1,
                                   (pais_c=="HTI"|pais_c=="SUR")& !(edad_ci >= 12 & edad_ci <= 18) ~ 0,
                                   
                                   (pais_c=="BRB") & (edad_ci >= 11 & edad_ci <= 15) ~ 1,
                                   (pais_c=="BRB") & !(edad_ci >= 11 & edad_ci <= 15) ~ 0,
                                   
                                   (pais_c=="COL") & (edad_ci >= 11 & edad_ci <= 16) ~ 1,
                                   (pais_c=="COL") & !(edad_ci >= 11 & edad_ci <= 16) ~ 0,
                                   
                                   (pais_c=="BRA") & (edad_ci >= 11 & edad_ci <= 17) ~ 1,
                                   (pais_c=="BRA")& !(edad_ci >= 11 & edad_ci <= 17) ~ 0,
                                   
                                   (pais_c=="TTO") & (edad_ci >= 13 & edad_ci <= 18) ~ 1,
                                   (pais_c=="TTO") & !(edad_ci >= 13 & edad_ci <= 18) ~ 0,  
                                   
                                   (edad_ci >= 12 & edad_ci <= 17) ~ 1,
                                   !(edad_ci >= 12 & edad_ci <= 17) ~ 0,
                                   TRUE ~ NA_real_),
            
            asis_net_seco_c = case_when((pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & (aedu_ci >= 6 & aedu_ci < 11) & asiste_ci == 1 & age_seco_c == 1 ~ 1,
                                        
                                        (pais_c=="HTI"|pais_c=="SUR") & (aedu_ci >= 6 & aedu_ci < 13) & age_seco_c == 1  & asiste_ci == 1 ~ 1,
                                        
                                        (pais_c=="BRB") & (aedu_ci >= 6 & aedu_ci < 11) & age_seco_c == 1  & asiste_ci == 1 ~ 1,
                                        
                                        (pais_c=="COL") & (aedu_ci >= 5 & aedu_ci < 11) & age_seco_c == 1  & asiste_ci == 1 ~ 1,
                                        
                                        (pais_c=="BRA") & (aedu_ci >= 5 & aedu_ci < 12) & age_seco_c == 1  & asiste_ci == 1 ~ 1,
                                        
                                        (pais_c=="TTO") & (aedu_ci >= 7 & aedu_ci < 12) & age_seco_c == 1  & asiste_ci == 1 ~ 1,
                                        
                                        (aedu_ci >= 6 & aedu_ci < 12)   & asiste_ci == 1 & age_seco_c == 1 ~ 1,
                                        TRUE ~ NA_real_),
            
            asis_seco_c = case_when((pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") &  (aedu_ci >= 6 & aedu_ci < 11) & asiste_ci == 1  & edad_ci >= 6 ~ 1,
                                    
                                    (pais_c=="HTI"|pais_c=="SUR") & (aedu_ci >= 6 & aedu_ci < 13) & edad_ci >= 6  & asiste_ci == 1 ~ 1,
                                    
                                    (pais_c=="BRB") & (aedu_ci >= 6 & aedu_ci < 11) & edad_ci >= 5  & asiste_ci == 1 ~ 1,
                                    
                                    (pais_c=="COL") & (aedu_ci >= 5 & aedu_ci < 11) & edad_ci >= 6  & asiste_ci == 1 ~ 1,
                                    
                                    (pais_c=="BRA") & (aedu_ci >= 5 & aedu_ci < 12) & edad_ci >= 6  & asiste_ci == 1 ~ 1,
                                    
                                    (pais_c=="TTO") & (aedu_ci >= 7 & aedu_ci < 12) & edad_ci >= 6  & asiste_ci == 1 ~ 1,
                                    
                                    (aedu_ci >= 6 & aedu_ci < 12)  & edad_ci >= 6 & asiste_ci == 1 ~ 1,
                                    TRUE ~ NA_real_), 
            
            age_term_s_c = case_when((pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & (edad_ci >= 19 & edad_ci <= 21)~ 1, 
                                     (pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & !(edad_ci >= 19 & edad_ci <= 21)~ 0,
                                     
                                     (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO")& (edad_ci >= 21 & edad_ci <= 23) ~ 1,
                                     (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO") & !(edad_ci >= 21 & edad_ci <= 23) ~ 0,
                                     
                                     (pais_c=="BRB") & (edad_ci >= 18 & edad_ci <= 20)~ 1,
                                     (pais_c=="BRB") & !(edad_ci >= 18 & edad_ci <= 20) ~ 0, 
                                     
                                     (edad_ci >= 20 & edad_ci <= 22)~ 1,
                                     !(edad_ci >= 20 & edad_ci <= 22)~ 0,
                                     TRUE ~NA_real_),
            
            age_tert_c = case_when((pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & (edad_ci >= 17 & edad_ci <= 23) ~ 1,
                                   (pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN") & !(edad_ci >= 17 & edad_ci <= 23) ~ 0,
                                   
                                   (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO") & (edad_ci >= 19 & edad_ci <= 23) ~ 1,
                                   (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO") & !( edad_ci >= 19 & edad_ci <= 23) ~ 0,
                                   
                                   (pais_c=="BRB") & (edad_ci >= 16 & edad_ci <= 23)~ 1,
                                   (pais_c=="BRB") & !(edad_ci >= 16 & edad_ci <= 23)~ 0,
                                   
                                   (edad_ci >= 18 & edad_ci <= 23)~ 1,
                                   !(edad_ci >= 18 & edad_ci <= 23)~ 0,
                                   
                                   TRUE ~NA_real_),
            
            asis_net_tert_c = case_when((pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN" | pais_c=="BRB") & aedu_ci > 11 & asiste_ci == 1 & age_tert_c == 1 ~ 1, 
                                        
                                        (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO") & aedu_ci > 13 & asiste_ci == 1 & age_tert_c == 1 ~ 1,
                                        
                                        aedu_ci > 12 & asiste_ci == 1 & age_tert_c == 1 ~ 1,
                                        TRUE ~NA_real_),
            
            asis_tert_c = case_when((pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="VEN" | pais_c=="BRB") & aedu_ci > 11 & asiste_ci == 1 & edad_ci >= 6 ~ 1,
                                    
                                    (pais_c=="HTI"|pais_c=="SUR"|pais_c=="TTO") & aedu_ci > 13 & asiste_ci == 1 & edad_ci >= 6 ~ 1,
                                    
                                    aedu_ci > 12 & asiste_ci == 1 & edad_ci >= 6 ~ 1,
                                    TRUE ~NA_real_),
            
            eduscm_ci = case_when((pais_c=="BRB"|pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="HND"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="SLV"|pais_c=="VEN") & (aedu_ci>11) ~ 1,
                                  (pais_c=="BRB"|pais_c=="COL"|pais_c=="CRI"|pais_c=="GTM"|pais_c=="GUY"|pais_c=="HND"|pais_c=="JAM"|pais_c=="NIC"|pais_c=="PER"|pais_c=="SLV"|pais_c=="VEN") & !(aedu_ci>11) ~ 0,
                                  (pais_c=="HTI"|pais_c=="SUR") & (aedu_ci>13) ~ 1,
                                  (pais_c=="HTI"|pais_c=="SUR") & !(aedu_ci>13) ~ 0,
                                  (aedu_ci>12) ~ 1,
                                  !(aedu_ci>12) ~ 0),
            
            leavers = case_when(
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