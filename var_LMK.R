# Script para generar variables intermedias - LMK

# 1. Censos

if (tipo == "censos") {
  
  # creating a vector with initial column names
  initial_column_names <- names(data_filt)
  
  data_filt <- data_filt %>%  
    mutate(npers = ifelse(TRUE,1,0),
           pet = ifelse(edad_ci>=15 & edad_ci<=64,1,0),
           pea = case_when((condocup_ci == 1 | condocup_ci == 2 ) & pet == 1 ~ 1,
                           condocup_ci==3 & pet==1 ~ 0,
                           TRUE ~NA_real_),
           # age exclusive
           age_lmk = case_when(edad_ci>=15 & edad_ci<25 ~"15_24",
                               edad_ci>=25 & edad_ci<65 ~"25_64",
                               edad_ci>=65 & edad_ci<99 ~"65+", 
                               TRUE ~NA_character_),
           age_15_64_lmk = ifelse(edad_ci>=15 & edad_ci<65, "15_64", NA_real_), 
           age_15_29_lmk = ifelse(edad_ci>=15 & edad_ci<30, "15_29", NA_real_), 
           patron = case_when(condocup_ci==1 & categopri_ci==1 ~ 1, 
                              condocup_ci==1 & categopri_ci!=1 ~ 0),
           asalariado = case_when(condocup_ci==1 & categopri_ci==3 ~ 1, 
                                  condocup_ci==1 & categopri_ci!=3 ~ 0),
           ctapropia = case_when(condocup_ci==1 & categopri_ci==2 ~ 1, 
                                 condocup_ci==1 & categopri_ci!=2 ~ 0),
           sinremuner = case_when(condocup_ci==1 & categopri_ci==4 ~ 1, 
                                  condocup_ci==1 & categopri_ci!=4 ~ 0),
           #1.2 PPP
           ppp = case_when(
             pais_c=="ARG"~ 2.768382,
             pais_c=="BHS"~ 1.150889,
             pais_c=="BLZ"~ 1.182611,
             pais_c=="BOL"~ 2.906106,
             pais_c=="BRA"~ 1.658783,
             pais_c=="BRB"~ 2.412881,
             pais_c=="CHL"~ 370.1987,
             pais_c=="COL"~ 1196.955,
             pais_c=="CRI"~ 343.7857,
             pais_c=="DOM"~ 20.74103,
             pais_c=="ECU"~ 0.5472345,
             pais_c=="GTM"~ 3.873239,
             pais_c=="HND"~ 10.08031,
             pais_c=="MEX"~ 8.940212,
             pais_c=="NIC"~ 9.160075,
             pais_c=="PAN"~ 0.553408,
             pais_c=="PER"~ 1.568639,
             pais_c=="PRY"~ 2309.43,
             pais_c=="SLV"~ 0.5307735,
             pais_c=="URY"~ 16.42385,
             pais_c=="VEN"~ 2.915005,
             pais_c=="JAM"~ 63.35445,
             pais_c=="TTO"~ 4.619226
           ),
           #1.3 Salarios
           #ylm_ci = ifelse(is.na(ylmpri_ci),NA_real_,ylm_ci),
           ylab_ci = ifelse(pea==1 & emp_ci==1,ylm_ci,NA_real_),
           #1.4 Categorías de rama de actividad
           agro = case_when(
             condocup_ci==1 & rama_ci==1~ 1,
             condocup_ci==1 & !is.na(rama_ci)~ 0,
             TRUE ~ NA_real_
           ),
           minas = case_when(
             condocup_ci==1 & rama_ci==2~ 1,
             condocup_ci==1 & !is.na(rama_ci)~ 0,
             TRUE ~ NA_real_
           ),           
           industria = case_when(
             condocup_ci==1 & rama_ci==3~ 1,
             condocup_ci==1 & !is.na(rama_ci)~ 0,
             TRUE ~ NA_real_
           ),
           sspublicos = case_when(
             condocup_ci==1 & rama_ci==4~ 1,
             condocup_ci==1 & !is.na(rama_ci)~ 0,
             TRUE ~ NA_real_
           ),
           construccion = case_when(
             condocup_ci==1 & rama_ci==5~ 1,
             condocup_ci==1 & !is.na(rama_ci)~ 0,
             TRUE ~ NA_real_
           ),
           comercio = case_when(
             condocup_ci==1 & rama_ci==6~ 1,
             condocup_ci==1 & !is.na(rama_ci)~ 0,
             TRUE ~ NA_real_
           ),
           transporte = case_when(
             condocup_ci==1 & rama_ci==7~ 1,
             condocup_ci==1 & !is.na(rama_ci)~ 0,
             TRUE ~ NA_real_
           ),
           financiero = case_when(
             condocup_ci==1 & rama_ci==8~ 1,
             condocup_ci==1 & !is.na(rama_ci)~ 0,
             TRUE ~ NA_real_
           ),
           servicios = case_when(
             condocup_ci==1 & rama_ci==9~ 1,
             condocup_ci==1 & !is.na(rama_ci)~ 0,
             TRUE ~ NA_real_
           ), 
           #1.7 Categorías
           patron = case_when(condocup_ci==1 & categopri_ci==1 ~ 1, 
                              condocup_ci==1 & categopri_ci!=1 ~ 0),
           asalariado = case_when(condocup_ci==1 & categopri_ci==3 ~ 1, 
                                  condocup_ci==1 & categopri_ci!=3 ~ 0),
           ctapropia = case_when(condocup_ci==1 & categopri_ci==2 ~ 1, 
                                 condocup_ci==1 & categopri_ci!=2 ~ 0),
           sinremuner = case_when(condocup_ci==1 & categopri_ci==4 ~ 1, 
                                  condocup_ci==1 & categopri_ci!=4 ~ 0)
           )
  
  
}

# 2. Encuestas

if (tipo == "encuestas") {
  
  # creating a vector with initial column names
 # povertyLinesUpdated <- read_dta("Inputs/masterdata.dta")  
  
#  data_filt <- left_join(data_filt, povertyLinesUpdated, by = c("pais_c" = "isoalpha3","anio_c" = "year"))
  povertyLinesUpdated <- read_dta("Inputs/masterdata.dta")  
  
  data_filt <- left_join(data_filt, povertyLinesUpdated, by = c("pais_c" = "isoalpha3","anio_c" = "year"))
  
  
  data_filt <- data_filt %>% 
    mutate(#1.1 Poblacion Total, en Edad de Trabajar - PET y economicamente activa PEA:
      npers = ifelse(TRUE,1,0),
      pet = ifelse(edad_ci>=15 & edad_ci<=64,1,0),
      pea = case_when((condocup_ci == 1 | condocup_ci == 2 ) & pet == 1 ~ 1,
                      condocup_ci==3 & pet==1 ~ 0,
                      TRUE ~NA_real_),
      # age exclusive
      age_lmk = case_when(edad_ci>=15 & edad_ci<25 ~"15_24",
                          edad_ci>=25 & edad_ci<65 ~"25_64",
                          edad_ci>=65 & edad_ci<120 ~"65+", 
                          TRUE ~NA_character_),
      #1.2 Diferente analisis de PET
      age_15_64_lmk = ifelse(edad_ci>=15 & edad_ci<65, "15_64", NA_real_), 
      age_15_29_lmk = ifelse(edad_ci>=15 & edad_ci<30, "15_29", NA_real_), 
      #1.3 Personas con más de un empleo
      otraocup_ci = case_when(nempleos_ci >= 2 ~1,
                              nempleos_ci==1~0,
                              TRUE ~ NA_real_),
      #1.4 sal min
      rsalmin_ci = case_when(
        (ylmpri_ci<(0.5*salmm_ci) & condocup_ci==1)~ 1,
        (ylmpri_ci>=(0.5*salmm_ci) & ylmpri_ci<(0.95*salmm_ci) & condocup_ci==1)~ 2,
        (ylmpri_ci>=(0.95*salmm_ci) & ylmpri_ci<salmm_ci & condocup_ci==1)~ 3,
        (ylmpri_ci==salmm_ci & condocup_ci==1)~ 4,
        (ylmpri_ci>salmm_ci & ylmpri_ci<(1.05*salmm_ci) & condocup_ci==1)~ 5,
        (ylmpri_ci>=(1.05*salmm_ci) & ylmpri_ci<(2*salmm_ci) & condocup_ci==1)~ 6,
        (ylmpri_ci>=(2*salmm_ci) & ylmpri_ci<(3*salmm_ci) & condocup_ci==1)~ 7,
        (ylmpri_ci>=(3*salmm_ci) & condocup_ci==1)~ 8,
        (is.na(salmm_ci)& condocup_ci==1)~NA_real_
      ),
      #1.5 PPP
      #1.6 Ingresos
      #1.6.1 Población ocupada por encima del umbral del salario horario suficiente (1.95 US ppp) 
      ylmpri_ppp = ylmpri_ci/ppp_2021/cpi_2021,
      #ylmpri_ppp_2017 = ylmpri_ci/ppp_2017/ratio_cpi2017,
      hsal_ci= ifelse(condocup_ci==1, ylmpri_ppp/(horaspri_ci*4.3), NA_real_), 
      #hsal_ci_2017= ifelse(condocup_ci==1, ylmpri_ppp_2017/(horaspri_ci*4.3), NA_real_), 
      #liv_wage   = ifelse(is.na(hsal_ci),NA_real_,hsal_ci>1.95),
      #1.6.2 Ingreso laboral monetario
      #ylm_ci = ifelse(is.na(ylmpri_ci),NA_real_,ylm_ci),

      ylab_ci = ifelse(pea==1 & emp_ci==1,ylm_ci,NA_real_),
      #ylab_ppp=ylab_ci/ppp_2011/ratio_cpi2011,
      #ylab_ppp_2017=ylab_ci/ppp_2017/ratio_cpi2017,
      ylab_ppp_2021=ylab_ci/ppp_2021/cpi_2021,
      #1.6.3 Ingreso horario en la actividad principal USD
      hwage_ci = ifelse(condocup_ci==1,ylmpri_ci/(horaspri_ci*4.3),NA_real_),
      #hwage_ppp=hwage_ci/ppp_2011/ratio_cpi2011,
      #hwage_ppp_2017=hwage_ci/ppp_2017/ratio_cpi2017,
      hwage_ppp_2021=hwage_ci/ppp_2021/cpi_2021,
      
      #1.6.4 Ingreso por pensión contributiva USD
      #ypen_ppp=ypen_ci/ppp_2011/ratio_cpi2011,
      #ypen_ppp_2017=ypen_ci/ppp_2017/ratio_cpi2017,
      ypen_ppp_2021=ypen_ci/ppp_2021/cpi_2021,
      #1.6.5 Salario mínimo mensual y horario - USD PPP
      #salmm_ppp=salmm_ci/ppp_2011/ratio_cpi2011,
      #salmm_ppp_2017=salmm_ci/ppp_2017/ratio_cpi2017,
      hsmin_ci=salmm_ci/(5*8*4.3),
      #hsmin_ppp=salmm_ppp/(5*8*4.3),
      #hsmin_ppp_2017=salmm_ppp_2017/(5*8*4.3),
      #1.6.6 Salario por actividad principal y total menor al mínimo legal (por mes)
      yltotal_ci = pmax(0, rowSums(cbind(ylm_ci, ylnm_ci), na.rm = TRUE)),
      menorwmin = ifelse((condocup_ci==1 & !is.na(salmm_ci) & !is.na(ylmpri_ci)),(ylmpri_ci<=salmm_ci),NA_real_),
      menorwmin1 = ifelse((condocup_ci==1 & !is.na(salmm_ci) & !is.na(ylmpri_ci)),(yltotal_ci<=salmm_ci),NA_real_),
      #1.6.7 Valor de todas las pensiones
      ypent_ci = pmax(0, rowSums(cbind(ypen_ci, ypensub_ci), na.rm = TRUE)),
      ypent_ci = ifelse(edad_ci<65 | pension_ci==0,NA_real_,ypent_ci),
      pensiont_ci = case_when(pension_ci==1 | pensionsub_ci==1 ~ 1, 
                              edad_ci >=65 ~ 0, 
                              TRUE ~ NA_real_),
      aux_pensiont_ci=mean(pensiont_ci),
      pensionwmin_ci = case_when(
        (pensiont_ci==1 & ypent_ci>0 & ypent_ci<salmm_ci & !is.na(salmm_ci))~ 1,
        (pensiont_ci==1 & ypent_ci>=salmm_ci & !is.na(ypent_ci) & !is.na(salmm_ci))~ 0,
        TRUE ~ NA_real_),
      #1.6.8 Cuociente salario mínimo mensual/ingreso ocupación principal mensual
      sm_smeanm_ci= salmm_ci/ylmpri_ci,
      #1.6.9 Cuociente salario mínimo por hora/ingreso ocupación principal por hora
      sm_smeanh_ci= hsmin_ci/hwage_ci,
      #1.6.10 Categorías de rama de actividad
      agro = case_when(
        condocup_ci==1 & rama_ci==1~ 1,
        condocup_ci==1 & !is.na(rama_ci)~ 0,
        TRUE ~ NA_real_
      ),
      minas = case_when(
        condocup_ci==1 & rama_ci==2~ 1,
        condocup_ci==1 & !is.na(rama_ci)~ 0,
        TRUE ~ NA_real_
      ),           
      industria = case_when(
        condocup_ci==1 & rama_ci==3~ 1,
        condocup_ci==1 & !is.na(rama_ci)~ 0,
        TRUE ~ NA_real_
      ),
      sspublicos = case_when(
        condocup_ci==1 & rama_ci==4~ 1,
        condocup_ci==1 & !is.na(rama_ci)~ 0,
        TRUE ~ NA_real_
      ),
      construccion = case_when(
        condocup_ci==1 & rama_ci==5~ 1,
        condocup_ci==1 & !is.na(rama_ci)~ 0,
        TRUE ~ NA_real_
      ),
      comercio = case_when(
        condocup_ci==1 & rama_ci==6~ 1,
        condocup_ci==1 & !is.na(rama_ci)~ 0,
        TRUE ~ NA_real_
      ),
      transporte = case_when(
        condocup_ci==1 & rama_ci==7~ 1,
        condocup_ci==1 & !is.na(rama_ci)~ 0,
        TRUE ~ NA_real_
      ),
      financiero = case_when(
        condocup_ci==1 & rama_ci==8~ 1,
        condocup_ci==1 & !is.na(rama_ci)~ 0,
        TRUE ~ NA_real_
      ),
      servicios = case_when(
        condocup_ci==1 & rama_ci==9~ 1,
        condocup_ci==1 & !is.na(rama_ci)~ 0,
        TRUE ~ NA_real_
      ), 
      #1.6.11 Categorías de grandes grupos de ocupación
      profestecnico = case_when(
        condocup_ci==1 & ocupa_ci==1~ 1,
        condocup_ci==1 & !is.na(ocupa_ci)~ 0,
        TRUE ~ NA_real_
      ),
      director = case_when(
        condocup_ci==1 & ocupa_ci==2~ 1,
        condocup_ci==1 & !is.na(ocupa_ci)~ 0,
        TRUE ~ NA_real_
      ),
      administrativo = case_when(
        condocup_ci==1 & ocupa_ci==3~ 1,
        condocup_ci==1 & !is.na(ocupa_ci)~ 0,
        TRUE ~ NA_real_
      ), 
      comerciantes = case_when(
        condocup_ci==1 & ocupa_ci==4~ 1,
        condocup_ci==1 & !is.na(ocupa_ci)~ 0,
        TRUE ~ NA_real_
      ),
      trabss = case_when(
        condocup_ci==1 & ocupa_ci==5~ 1,
        condocup_ci==1 & !is.na(ocupa_ci)~ 0,
        TRUE ~ NA_real_
      ),
      trabagricola = case_when(
        condocup_ci==1 & ocupa_ci==6~ 1,
        condocup_ci==1 & !is.na(ocupa_ci)~ 0,
        TRUE ~ NA_real_
      ),
      obreros = case_when(
        condocup_ci==1 & ocupa_ci==7~ 1,
        condocup_ci==1 & !is.na(ocupa_ci)~ 0,
        TRUE ~ NA_real_
      ), 
      ffaa = case_when(
        condocup_ci==1 & ocupa_ci==8~ 1,
        condocup_ci==1 & !is.na(ocupa_ci)~ 0,
        TRUE ~ NA_real_
      ), 
      otrostrab = case_when(
        condocup_ci==1 & ocupa_ci==9~ 1,
        condocup_ci==1 & !is.na(ocupa_ci)~ 0,
        TRUE ~ NA_real_
      ), 
      #1.7 Categorías
      patron = case_when(condocup_ci==1 & categopri_ci==1 ~ 1, 
                         condocup_ci==1 & categopri_ci!=1 ~ 0),
      asalariado = case_when(condocup_ci==1 & categopri_ci==3 ~ 1, 
                             condocup_ci==1 & categopri_ci!=3 ~ 0),
      ctapropia = case_when(condocup_ci==1 & categopri_ci==2 ~ 1, 
                            condocup_ci==1 & categopri_ci!=2 ~ 0),
      sinremuner = case_when(condocup_ci==1 & categopri_ci==4 ~ 1, 
                             condocup_ci==1 & categopri_ci!=4 ~ 0),
      #1.8 Categoría por tipo de contrato
      contratoindef = case_when((condocup_ci==1 & tipocontrato_ci==1 & categopri_ci==3)~ 1,
                                (condocup_ci==1 & !is.na(tipocontrato_ci) & categopri_ci==3)~0),
      contratofijo = case_when((condocup_ci==1 & tipocontrato_ci==2 & categopri_ci==3)~ 1,
                               (condocup_ci==1 & !is.na(tipocontrato_ci) & categopri_ci==3)~0),
      sincontrato = case_when((condocup_ci==1 & tipocontrato_ci==3 & categopri_ci==3)~ 1,
                              (condocup_ci==1 & !is.na(tipocontrato_ci) & categopri_ci==3)~0),
      #1.9 Tasa de desempleo de larga duración
      durades1_ci = ifelse("durades1_ci" %in% names(.), durades1_ci,NA),
      
      desemplp_ci = case_when(
        condocup_ci !=2 ~ NA_real_,
        durades_ci>=12~ 1,
        durades_ci<12~ 0,
        durades1_ci==5 & pais_c=="ARG" & anio_c>=2003~ 1,
        condocup_ci==2 & pais_c=="ARG" & anio_c>=2003~ 0,
        TRUE ~ NA_real_
      ),
      aux_n=mean(desemplp_ci),
      desemplp_ci = ifelse(aux_n==0,NA_real_,desemplp_ci),
      aux_n=mean(durades_ci),
      durades_ci = ifelse(aux_n==0,NA_real_,durades_ci),
      #1.10 Desempleados aspirantes
      aspirante = case_when(
        condocup_ci==2 & cesante_ci==0 ~ 1,
        condocup_ci==2 & cesante_ci==1 ~ 0,
        TRUE ~ NA_real_
      ),
      #1.11 Anios de educacion de la poblacion activa e inactiva
      aedupea_ci = ifelse(pea==1,aedu_ci,NA_real_),
      aedupei_ci = ifelse(condocup_ci==3,aedu_ci,NA_real_),
      #1.12 Formalidad laboral
      formal_aux = case_when(
        afiliado_ci==1 & is.na(cotizando_ci) & condocup_ci==1 & pais_c=="URY" & anio_c<=2000 ~ 1, 
        afiliado_ci==1 & is.na(cotizando_ci) & condocup_ci==1 & pais_c=="BOL" ~ 1, 
        afiliado_ci==1 & is.na(cotizando_ci) & condocup_ci==1 & pais_c=="CRI" & anio_c<2000 ~1,
        afiliado_ci==1 & is.na(cotizando_ci) & condocup_ci==1 & pais_c=="GTM" & anio_c>1998 ~ 1, 
        afiliado_ci==1 & is.na(cotizando_ci) & condocup_ci==1 & pais_c=="PAN" ~ 1, 
        afiliado_ci==1 & is.na(cotizando_ci) & condocup_ci==1 & pais_c=="PRY" & anio_c<=2006 ~ 1,
        afiliado_ci==1 & is.na(cotizando_ci) & condocup_ci==1 & pais_c=="DOM" ~ 1, 
        afiliado_ci==1 & is.na(cotizando_ci) & condocup_ci==1 & pais_c=="MEX" & anio_c>=2008 ~ 1, 
        afiliado_ci==1 & is.na(cotizando_ci) & condocup_ci==1 & pais_c=="COL" & anio_c<=1999~ 1, 
        afiliado_ci==1 & is.na(cotizando_ci) & condocup_ci==1 & pais_c=="ECU"  ~ 1, 
        afiliado_ci==1 & is.na(cotizando_ci) & condocup_ci==1 & pais_c=="BHS" ~ 1,
        cotizando_ci ==1~1, 
        TRUE ~ 0),
      formal_ci = case_when(formal_aux==1 & (condocup_ci==1 | condocup_ci==2) ~ 1, 
                            (condocup_ci==1 | condocup_ci==2) ~ 0,
                            TRUE ~ NA_real_),
      #1.13 Antiguedad laboral
      t1yr = case_when(antiguedad_ci<=1 & condocup_ci==1 & !is.na(antiguedad_ci) ~ 1,
                       antiguedad_ci>1 & condocup_ci==1 & !is.na(antiguedad_ci) ~ 0,
                       TRUE ~ NA_real_),
      t1yrasal = case_when(antiguedad_ci<=1 & condocup_ci==1 & categopri_ci==3 & !is.na(antiguedad_ci) ~ 1,
                           antiguedad_ci>1 & condocup_ci==1 & categopri_ci==3 & !is.na(antiguedad_ci) ~ 0,
                           TRUE ~ NA_real_),
      t1yrctapr = case_when(antiguedad_ci<=1 & condocup_ci==1 & categopri_ci==2 & !is.na(antiguedad_ci) ~ 1,
                            antiguedad_ci>1 & condocup_ci==1 & categopri_ci==2 & !is.na(antiguedad_ci) ~ 0,
                            TRUE ~ NA_real_),
      t1a5yr = case_when(antiguedad_ci>1 & antiguedad_ci<5 & condocup_ci==1 & !is.na(antiguedad_ci) ~ 1,
                         (antiguedad_ci<=1 | antiguedad_ci>=5) & condocup_ci==1 & !is.na(antiguedad_ci) ~ 0,
                         TRUE ~ NA_real_),
      t1a5yrasal = case_when(antiguedad_ci>1 & antiguedad_ci<5 & condocup_ci==1 & categopri_ci==3 & !is.na(antiguedad_ci) ~ 1,
                             (antiguedad_ci<=1 | antiguedad_ci>=5) & condocup_ci==1 & categopri_ci==3 & !is.na(antiguedad_ci) ~ 0,
                             TRUE ~ NA_real_),
      t1a5yrctapr = case_when(antiguedad_ci>1 & antiguedad_ci<5 & condocup_ci==1 & categopri_ci==2 & !is.na(antiguedad_ci) ~ 1,
                              (antiguedad_ci<=1 | antiguedad_ci>=5) & condocup_ci==1 & categopri_ci==2 & !is.na(antiguedad_ci) ~ 0,
                              TRUE ~ NA_real_),
      t5yr = case_when(antiguedad_ci>=5 & condocup_ci==1 & !is.na(antiguedad_ci) ~ 1,
                       antiguedad_ci<5 & condocup_ci==1 & !is.na(antiguedad_ci) ~ 0,
                       TRUE ~ NA_real_),
      t5yrasal = case_when(antiguedad_ci>=5 & condocup_ci==1 & categopri_ci==3 & !is.na(antiguedad_ci) ~ 1,
                           antiguedad_ci<5 & condocup_ci==1 & categopri_ci==3 & !is.na(antiguedad_ci) ~ 0,
                           TRUE ~ NA_real_),
      t5yrctapr = case_when(antiguedad_ci>=5 & condocup_ci==1 & categopri_ci==2 & !is.na(antiguedad_ci) ~ 1,
                            antiguedad_ci<5 & condocup_ci==1 & categopri_ci==2 & !is.na(antiguedad_ci) ~ 0,
                            TRUE ~ NA_real_),
      asal1yrtenure = case_when(condocup_ci==1 & categopri_ci==3 & antiguedad_ci<=1 & !is.na(antiguedad_ci) ~ 1,
                                condocup_ci==1 & antiguedad_ci<=1 & !is.na(antiguedad_ci) ~ 0,
                                TRUE ~ NA_real_),
      ctapr1yrtenure = case_when(condocup_ci==1 & categopri_ci==2 & antiguedad_ci<=1 & !is.na(antiguedad_ci) ~ 1,
                                 condocup_ci==1 & antiguedad_ci<=1 & !is.na(antiguedad_ci) ~ 0,
                                 TRUE ~ NA_real_),
      patron1yrtenure = case_when(condocup_ci==1 & categopri_ci==1 & antiguedad_ci<=1 & !is.na(antiguedad_ci) ~ 1,
                                  condocup_ci==1 & antiguedad_ci<=1 & !is.na(antiguedad_ci) ~ 0,
                                  TRUE ~ NA_real_),
      sinrem1yrtenure = case_when(condocup_ci==1 & categopri_ci==4 & antiguedad_ci<=1 & !is.na(antiguedad_ci) ~ 1,
                                  condocup_ci==1 & antiguedad_ci<=1 & !is.na(antiguedad_ci) ~ 0,
                                  TRUE ~ NA_real_)
    ) 
  
  
}