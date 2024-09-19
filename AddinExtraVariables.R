
# transforming variables
# homologating categories
# dictionary
encode_dict <- tibble(
  
  col_nm = c(
    rep("sex", 10),
    rep("quintile",21)
  ),
  
  value = c(
    c("men","man","Man","Male",
      "women","woman","Woman","Female",
      "Total","Both"),
    c("quintile_1","quintile_1_ch","quintile_1_ch_rural","quintile_1_ch_urban",
      "quintile_2","quintile_2_ch","quintile_2_ch_rural","quintile_2_ch_urban",
      "quintile_3","quintile_3_ch","quintile_3_ch_rural","quintile_3_ch_urban",
      "quintile_4","quintile_4_ch","quintile_4_ch_rural","quintile_4_ch_urban",
      "quintile_5","quintile_5_ch","quintile_5_ch_rural","quintile_5_ch_urban",
      "Total"
      )
  ),
  
  label = c(
    rep("man", 4),
    rep("women", 4),'Total','Total',
    rep('quintile_1',4),
    rep('quintile_2',4),
    rep('quintile_3',4),
    rep('quintile_4',4),
    rep('quintile_5',4),
    'Total'
  )
)

#
# recode function to be used within `dplyr::mutate(across(...))`
recode_col <- function(x) {
  
  recode_vec <- encode_dict |>
    filter(col_nm == cur_column()) |>
    pull(label, name = value)
  
  dplyr::recode(x, !!! recode_vec)
}

# vector of columns to recode
cols_vec <- unique(encode_dict$col_nm)
cols_vec

data_total |> 
  mutate(across(all_of(cols_vec),
                recode_col)
  )

# subsittuing for named variables NAN with total


replacingValues <- function(x) {
  x <- as.character(x)
  x <- replace(x,is.na(x),rep("Total",length(x)))
  x <- replace(x,x=="nan",rep("Total",length(x)))
  x <- replace(x,x=="NaN",rep("Total",length(x)))
  
}

# creating list of variables
groups <- c('area','quintile', 'sex', 'education_level', 'age', 'ethnicity', 'disability', 'migration', 'management', 'funding', 'language')
# creating empty columns if they do not exist
data_total[groups[!(groups %in% colnames(data_total))]] = ""
# replacing by missing those who don't have values
data_total <- data_total %>% mutate_at(groups, replacingValues)
# creating new groups
newGroup <- c('admin1_ipums')
data_total[newGroup[!(newGroup %in% colnames(data_total))]] = ""
# renaming variables
data_total <- data_total <- rename(data_total, source = fuente)
# selecting only required columns

data_total <- data_total[,c('iddate', 'year', 'idgeo', 'isoalpha3', 'admin1_ipums', 'source', 'indicator', 'area',
              'quintile', 'sex', 'education_level', 'age', 'ethnicity', 'disability', 'migration', 'management', 'funding', 'language', 'value', 'se',
              'cv', 'sample','level','quality_check',"identifier")]

# Things to do:
# recheck the type of data is correct, I think it's fine
str(data_total)

# duplicate analysis
totalDuplicated<- sum(duplicated(data_total))
if (totalDuplicated>0){
  message(paste('Duplicated rows: '),totalDuplicated)
  data_total<- data_total %>% distinct()
}
### Adding dates
currentDate<-Sys.Date()
month_num <- format(Sys.Date(),"%m")
day_num <- format(Sys.Date(),"%d")
date <- format(Sys.Date(),"%m-%d-%Y")
#### adding month, day and year
data_total <- data_total %>% mutate(month = as.integer(month_num),
                                    day = as.integer(month_num),
                                    dt = date)


### reading the dictionary
dictionaryDF<- read.csv(dictionary)
# keeping only those indicators in the dictionary
# Validating indicators
valid_indicators<- Reduce(intersect,list(dictionaryDF$indicator,data_total$indicator))

# merge between output indicators and idctionary
data_total<- data_total %>% filter(indicator %in% valid_indicators)
dictionaryDF<- dictionaryDF %>% filter(indicator %in% valid_indicators) %>% select("indicator", "theme_es", "theme_en", "collection")

collectionsDf<- read_excel(collections_dictionary, sheet = "collections") %>% rename("collection_es"="label_es", "collection_en"="label_en")

data_total<- merge(data_total, dictionaryDF, by="indicator",how="left")
data_total<- merge(data_total,collectionsDf,by="collection",how="left") %>% subset(select=-c(collection))

# select
data_total <- data_total %>% select('dt', 'iddate', 'year', 'month', 'idgeo', 'isoalpha3', 'admin1_ipums', 'indicator', 'area',
                      'quintile', 'sex', 'level', 'education_level', 'age', 'ethnicity', 'disability', 'migration','management', 'funding', 'language', 'value', 'se',
                      'cv', 'sample','quality_check', 'source','theme_es', 'theme_en', 'collection_en', 'collection_es',"identifier")


# Filtering variables that cann't be calcualted such as JAM
source("exceptionsInCalculations.R")
#

#
data_total <- data_total %>% mutate(
  # add flag for totals_dummy
  totals_dummy = case_when(
    (area=="Total")&(quintile=="Total")&(sex=="Total")&(education_level=="Total")&(age=="Total")&(ethnicity=="Total")&(disability=="Total")&(migration=="Total")&((management=="Total")|(management==""))&((funding=="Total")|(funding==""))&((language=="Total")|(language==""))~ 1,
    (area!="Total")&(quintile=="Total")&(sex=="Total")&(education_level=="Total")&(age=="Total")&(ethnicity=="Total")&(disability=="Total")&(migration=="Total")&((management=="Total")|(management==""))&((funding=="Total")|(funding==""))&((language=="Total")|(language==""))~ 2,
    (area=="Total")&(quintile!="Total")&(sex=="Total")&(education_level=="Total")&(age=="Total")&(ethnicity=="Total")&(disability=="Total")&(migration=="Total")&((management=="Total")|(management==""))&((funding=="Total")|(funding==""))&((language=="Total")|(language==""))~ 3,
    (area=="Total")&(quintile=="Total")&(sex!="Total")&(education_level=="Total")&(age=="Total")&(ethnicity=="Total")&(disability=="Total")&(migration=="Total")&((management=="Total")|(management==""))&((funding=="Total")|(funding==""))&((language=="Total")|(language==""))~ 4,
    (area=="Total")&(quintile=="Total")&(sex=="Total")&(education_level!="Total")&(age=="Total")&(ethnicity=="Total")&(disability=="Total")&(migration=="Total")&((management=="Total")|(management==""))&((funding=="Total")|(funding==""))&((language=="Total")|(language==""))~ 5,
    (area=="Total")&(quintile=="Total")&(sex=="Total")&(education_level=="Total")&(age!="Total")&(ethnicity=="Total")&(disability=="Total")&(migration=="Total")&((management=="Total")|(management==""))&((funding=="Total")|(funding==""))&((language=="Total")|(language==""))~ 6,
    (area=="Total")&(quintile=="Total")&(sex=="Total")&(education_level=="Total")&(age=="Total")&(ethnicity!="Total")&(disability=="Total")&(migration=="Total")&((management=="Total")|(management==""))&((funding=="Total")|(funding==""))&((language=="Total")|(language==""))~ 7,
    (area=="Total")&(quintile=="Total")&(sex=="Total")&(education_level=="Total")&(age=="Total")&(ethnicity=="Total")&(disability!="Total")&(migration=="Total")&((management=="Total")|(management==""))&((funding=="Total")|(funding==""))&((language=="Total")|(language==""))~ 8,
    (area=="Total")&(quintile=="Total")&(sex=="Total")&(education_level=="Total")&(age=="Total")&(ethnicity=="Total")&(disability=="Total")&(migration!="Total")&((management=="Total")|(management==""))&((funding=="Total")|(funding==""))&((language=="Total")|(language==""))~ 9
    ),
  # add flag for GDI
  dummy_GDI = case_when(
    (area=="Total")&(quintile=="Total")&((sex=='man')|(sex=='woman'))&(education_level=="Total")&(age=="Total")&(ethnicity=="Total")&(disability=="Total")&(migration=="Total")~ 1,
    (area=="Total")&(quintile=="Total")&((sex=='man')|(sex=='woman'))&(education_level=="Total")&(age=="Total")&(ethnicity=="Afro")&(disability=="Total")&(migration=="Total")~ 2,
    (area=="Total")&(quintile=="Total")&((sex=='man')|(sex=='woman'))&(education_level=="Total")&(age=="Total")&(ethnicity=="Indi")&(disability=="Total")&(migration=="Total")~ 3,
    (area=="Total")&(quintile=="Total")&((sex=='man')|(sex=='woman'))&(education_level=="Total")&(age=="Total")&(ethnicity=="Total")&(disability=="person_with_disability")&(migration=="Total")~ 4,
  )
  )

# list of indicators for query builder and social data
listOfIndicators <- c("tasa_terminacion_c_primar","tasa_terminacion_c_secund","tasa_neta_asis_prim","tasa_neta_asis_seco","tasa_neta_asis_tert","tasa_terminacion_c_terc","tasa_terminacion_c_terc_univ","ninis_2_15_24","tasa_aban_18_24","anos_esc_25_mas_0","anos_esc_25_mas_1_5",
                      "anos_esc_25_mas_6","anos_esc_25_mas_7_11","anos_esc_25_mas_12","anos_esc_25_mas_13_mas","pobreza31","pobreza","ginihh","middle","vulnerable","rich","tasa_desocupacion","tasa_ocupacion",
                      "subempleo","formalidad_2","tasa_independientes","tasa_participacion","poblacion_total","ingreso_mens_prom","inglaboral_informales","inglaboral_formales","migrante_ci","miglac_ci",
                      "migrantiguo5_ci","ptmc_coverage2","ptmc_dist2","ptmc_ingneto2","ptmc_ch","whs6_102","hwf_0001","lexp","mdg_0000000007","gghed_usd","hf2_usd","hf3_usd","che_usd","che_gdp","gghed_gdp","hf2_gdp","hf3_gdp","HAQ","ncd_bmi_25a",
                      "lbw_prevalence","whs4_543","wsh_sanitation_basic","uhc_index_reported","uhc_sci_rmnch","finprotection_cata_tot_10_pop","jefa_ch","depen_ch","aguared_ch","sinsan_ch","luz_ch","internet_ch","cel_ch",
                      "sanred_ch","sanmejorado_ch","aguafmejorada_ch","prangoedad_00_15","prangoedad_16_30","prangoedad_31_45",
                      "prangoedad_46_60","prangoedad_61_75","prangoedad_76mas",
                      "pafro_ci","pindi_ci","pnoafronoindi_ci","pdis_ci","vulnerable","middle","rich","tamh_ch",
                      "p50_10","p90_50","p90_10","dependency_ratio","tamh_ch","urbano_ci","rural_ci","men_ci",
                      "women_ci","pob_total",'pobreza_under5')

listOfIndicatorsCensus <- c("prangoedad_00_15_PHC","prangoedad_16_30_PHC","prangoedad_31_45_PHC","prangoedad_46_60_PHC","prangoedad_61_75_PHC","prangoedad_76mas_PHC",
                            "pobfem_ci_PHC","pafro_ci_PHC","pindi_ci_PHC","pnoafronoindi_ci_PHC","pdis_ci_PHC","internet_ch_PHC","des1_ch_PHC","auto_ch_PHC","luz_ch_PHC","estable_ch_PHC",
                            "parednp_ch_PHC","dirtf_ch_PHC","refrig_ch_PHC","aguared_ch_PHC","techonp_ch_PHC","cel_ch_PHC","tasa_desocupacion_PHC","tasa_ocupacion_PHC","tasa_participacion_PHC",
                            "tasa_terminacion_c_secund_PHC","union_ci_PHC","miembro6_ch_PHC","miembro6y16_ch_PHC","miembro65_ch_PHC",
                            "tasa_administrativo_PHC","tasa_director_PHC","tasa_obreros_PHC","tasa_profestecnico_PHC","tasa_trabagricola_PHC",
                            "tasa_otrostrab_PHC","tasa_trabss_PHC","empleo_publico_PHC","tasa_comerciantes_PHC","tasa_agro_PHC",
                            "tasa_comercio_PHC","tasa_construccion_PHC","tasa_sspublicos_PHC","tasa_minas_PHC","tasa_servicios_PHC",
                            "tasa_financiero_PHC","tasa_industria_PHC","tasa_transporte_PHC","tasa_patrones_PHC","tasa_independientes_PHC",
                            "cobsalud_ci_PHC","tasa_neta_asis_prim_PHC","tasa_neta_asis_seco_PHC","tasa_neta_asis_tert_PHC","ninis_2_15_24_PHC",
                            "tasa_aban_18_24_PHC","anos_promedio_educ_PHC","tasa_terminacion_c_secund_PHC","tasa_terminacion_c_primar_PHC")

data_total <- data_total %>% mutate(
  scldata3_highlight_profile = as.integer(indicator %in% listOfIndicators),
  scldata3_highlight_census = as.integer(indicator %in% listOfIndicatorsCensus)
)

listOfCountries <- c('BOL','COL','ECU','PER','VEN','BHS','BRB','GUY','JAM','SUR','TTO','BLZ',
                     'CRI','SLV','GTM','HTI','HND','MEX','NIC','PAN','DOM','ARG','BRA','CHL','PRY','URY')

data_total <- data_total %>% mutate(
  scldata3_countryList = as.integer(indicator %in% listOfCountries)
)

## filtering data
data_total <- data_total %>% mutate(
  scldata3_highlight_profile = case_when(
    (scldata3_countryList==1)&(scldata3_highlight_profile==1)&(year>=2006)~ 1,
    (scldata3_countryList!=1)|(scldata3_highlight_profile!=1)|(year<2006)~ 0
  ),
  scldata3_highlight_census = case_when(
    (scldata3_countryList==1)&(scldata3_highlight_census==1)&(year>=2001)~ 1,
    (scldata3_countryList!=1)|(scldata3_highlight_census!=1)|(year<2001)~ 0,
  )
)  %>% subset(select=-c(scldata3_countryList))

data_total <- data_total[,c('dt','iddate', 'year','month', 'idgeo', 'isoalpha3', 'admin1_ipums', 'indicator', 'area',
                            'quintile', 'sex', 'education_level', 'age', 'ethnicity', 'disability', 'migration', 'management', 'funding', 'language', 'value', 'se',
                            'cv', 'sample','level','quality_check', 'source','theme_es','theme_en','collection_en','collection_es',"identifier","totals_dummy","dummy_GDI","scldata3_highlight_profile","scldata3_highlight_census")]