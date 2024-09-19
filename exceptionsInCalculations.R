#### Leaving only individual variables from JAM

variablesAtIndividuoLevel<- c('pob18_ci','pob65_ci','pobedad_ci','pobfem_ci','urbano_ci','rural_ci','union_ci','miglac_ci','migrantiguo5_ci','migantiguo5_ci',
  'migrante_ci','migrantelac_ci','dura_desempleo','empleo_publico','formalidad_1','formalidad_2','formalidad_3','formalidad_4',
  'horas_trabajadas','inglaboral_formales','inglaboral_informales','inglaboral_ppp_formales','inglaboral_ppp_informales','ingreso_hor_prom',
  'ingreso_hor_prom_ppp','ingreso_mens_prom','ingreso_mens_prom_ppp','ingreso_mens_prom_pri','ingreso_mens_prom_pri_ppp',
  'ocup_suf_salario','pensionista_65_mas','pensionista_cont_65_mas','pensionista_nocont_65_mas','pensionista_ocup_65_mas',
  'sal_menor_salmin','salmin_hora','salmin_mes','salminhora_ppp','salminmes_ppp','saltotal_menor_salmin','subempleo',
  'tasa_administrativo','tasa_agro','tasa_antiguedad_1_asal','tasa_antiguedad_1_ctapropia','tasa_antiguedad_1_ocup',
  'tasa_antiguedad_1a5_asal','tasa_antiguedad_1a5_ctapropia','tasa_antiguedad_1a5_ocup','tasa_antiguedad_5mas_asal','tasa_antiguedad_5mas_ctapropia',
  'tasa_antiguedad_5mas_ocup','tasa_asal1yrtenure','tasa_asalariados','tasa_comerciantes','tasa_comercio',
  'tasa_construccion','tasa_contrato_fijo','tasa_contrato_indefinido','tasa_sincontrato','tasa_ctapr1yrtenure',
  'tasa_desemp_aspi','tasa_desemp_cesa','tasa_desemp_ldur','tasa_desocupacion','tasa_director','tasa_ffaa','tasa_financiero',
  'tasa_inactivos','tasa_independientes','tasa_industria','tasa_minas','tasa_obreros','tasa_ocupacion','tasa_otra_ocupacion',
  'tasa_otrostrab','tasa_participacion','tasa_patron1yrtenure','tasa_patrones','tasa_pea','tasa_pension_menor_salmin',
  'tasa_pet','tasa_profestecnico','tasa_servicios','tasa_sinrem1yrtenure','tasa_sinremuneracion','tasa_sspublicos',
  'tasa_trabagricola','tasa_trabss','tasa_transporte','tenure_prom','y_pen_cont','y_pen_nocont','y_pen_total','anos_esc_25_mas_0',
  'anos_esc_25_mas_1_5','anos_esc_25_mas_12','anos_esc_25_mas_13_mas','anos_esc_25_mas_6','anos_esc_25_mas_7_11',
  'anos_promedio_educ','anos_promedio_educ_activos','anos_promedio_educ_inactivos','anos_promedio_educ_sims',
  'ninis_2_15_24','tasa_aban_18_24','tasa_asis_12_14','tasa_asis_15_17','tasa_asis_18_23','tasa_asis_4_5',
  'tasa_asis_6_11','tasa_bruta_asis_prim','tasa_bruta_asis_seco','tasa_bruta_asis_tert','tasa_neta_asis_prim',
  'tasa_neta_asis_seco','tasa_neta_asis_tert','tasa_no_asis_12_14','tasa_no_asis_15_17','tasa_no_asis_18_23',
  'tasa_no_asis_4_5','tasa_no_asis_6_11','tasa_sobre_edad_prim','tasa_terminacion_c_primar','tasa_terminacion_c_secund',
  'tasa_terminacion_c_terc','tasa_terminacion_c_terc_univ','prangoedad_00_15','prangoedad_16_30','prangoedad_31_45',
  'prangoedad_46_60','prangoedad_61_75','prangoedad_76_90','pnc_ci','pafro_ci','pindi_ci','pnoafronoindi_ci','pdis_ci',
  'cobsalud_ci','men_ci','women_ci','pob_total')

data_total <- data_total %>% mutate(individualLevel = as.integer(indicator %in% variablesAtIndividuoLevel))

# stating conditions
data_total <- data_total %>% 
  mutate(filesToDrop = case_when(
    (isoalpha3=='JAM') & (year>=2000 & year<=2009) & (individualLevel!=1) ~ 1,
    # household identifier not working properly
    (isoalpha3=='PAN') & (year<2015) & (indicator=="pafro_ci") ~ 1,
    (isoalpha3=='PAN') & (year<2015) & (indicator=="pindi_ci") ~ 1,
    # indicator of afrodescendenci not defined properly for 2015 and below
    (isoalpha3=='BHS') & (indicator=="dependency_ratio") ~ 1,
    (isoalpha3=='BHS') & (indicator=="prangoedad_00_15") ~ 1,
    # survey does not include all people between 0 and 15
    (isoalpha3=='BRB') & (indicator=="prangoedad_00_15") ~ 1,
    (isoalpha3=='BRB') & (indicator=="dependency_ratio") ~ 1,
    TRUE~ 0
    # survey does not include all people between 0 and 15
    ))

# dropping those observations
data_total <- data_total %>% filter(filesToDrop!=1) %>% subset(select=-c(filesToDrop))

