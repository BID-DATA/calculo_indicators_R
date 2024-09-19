
# Function that returns the address of the survey based on country, type and period


functionRoundAndSurvey <- function(pais, tipo, anio) {


if (tipo == "encuestas") {

# 1. reading dataset with surveys, round and year
planificacionSurveys <- readxl::read_xlsx("Inputs/Planeación - Armonización de Encuestas de Hogares.xlsx", sheet = "HH surveys")

# 2. Pivoting to transform excel
# 2.1 Getting list of years availables
yearsAvailable <- planificacionSurveys %>% 
                  dplyr::select(where(is.numeric)) %>% 
                  select(-"Total") %>% 
                  colnames()
# choosing survey depending on year and country and round depending on that information
planificacionSurveysPivot <- planificacionSurveys %>%
                    pivot_longer(cols=yearsAvailable,
                                 names_to='year',
                                 values_to='availability')

# choosing round depending on that information
round <- planificacionSurveysPivot %>% 
         filter(`País`== pais & `year`== anio & availability ==1) %>% 
         pull(`Ronda armonizada BID`)

survey <- planificacionSurveysPivot %>% 
          filter(`País`== pais & `year`== anio & availability ==1) %>% 
          pull(Encuesta)

base <- paste("Z://harmonized//",pais,"//",survey,"//data_arm//",pais,"_",anio,round,"_BID.dta",sep = "")
# return database address
return(base)


}

if (tipo == "censos"){

base <- paste("Z://census//clean//",pais,"//",pais,"_",anio,"_censusBID.dta",sep = "")  

}
}
# dictionary address
if (tipo == "encuestas") {
dictionary <- "C:/Users/DCOR/Inter-American Development Bank Group/Data Governance - SCL - General/W. Other/W.5 - SCLData2.0/2. Página web/bases de datos/dictionariesMyData/D.1.2.1 Diccionario - indicadores encuestas de hogares.csv"
}

collections_dictionary <- "C:/Users/DCOR/Inter-American Development Bank Group/Data Governance - SCL - General/W. Other/W.5 - SCLData2.0/2. Página web/bases de datos/data projects/M.232 Processed-Semantic - Schema Standard.xlsx"

if (tipo == "censos") {
  dictionary <- "C:/Users/DCOR/Inter-American Development Bank Group/Data Governance - SCL - General/W. Other/W.5 - SCLData2.0/2. Página web/bases de datos/dictionariesMyData/D.7.2.1 Diccionario - indicadores de censos de población.csv"
}