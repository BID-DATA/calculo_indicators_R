
# Function that returns the address of the survey based on country, type and period


functionRoundAndSurvey <- function(pais, tipo, anio) {
  
  
  if (tipo == "encuestas") {
    
    # 1. reading dataset with surveys, round and year
#    planificacionSurveys <- readxl::read_xlsx("Inputs/Planeación - Armonización de Encuestas de Hogares.xlsx", sheet = "HH surveys")
    planificacionSurveys <- read.csv("Inputs/running_survey.csv") 
    # 2. Pivoting to transform excel
    # 2.1 Getting list of years availables
    
    # choosing round depending on that information
    round <- planificacionSurveys %>% 
      filter(`Pais`== pais & `year`== anio & availability ==1) %>% 
      pull(`Ronda.armonizada.BID`)
    
    survey <- planificacionSurveys %>% 
      filter(`Pais`== pais & `year`== anio & availability ==1) %>% 
      pull(Encuesta)
    
    restriction <- planificacionSurveys %>% 
      filter(`Pais`== pais & `year`== anio & availability ==1) %>% 
      pull(access_right)
    print(restriction)
    if (isTRUE(restriction == "restricted")) {
      
      base <- paste("//sapidbshares.file.core.windows.net//idbrestrictedshares//SCL_DATAFILES_RESTRICTED//harmonized//",pais,"//",survey,"//data_arm//",pais,"_",anio,round,"_BID.dta",sep = "")
    }
    else {
      #base <- paste("//sapidbshares.file.core.windows.net/idbshares/SURVEYS/",pais,"//",survey,"//data_arm//",pais,"_",anio,round,"_BID.dta",sep = "")
      #
      base_dir <- "//sapidbshares.file.core.windows.net/idbshares/SURVEYS/harmonized"
      base <- file.path(base_dir, pais, survey, "data_arm",
                        sprintf("%s_%s%s_BID.dta", pais, anio, round),
                        fsep = "/")
      
      print(base)
    }
    #\\sapidbshares.file.core.windows.net\idbshares\SURVEYS
    # return database address
    return(base)
    
    
  }
  
  if (tipo == "censos"){
    
    base <- paste("Z://census//clean//",pais,"//",pais,"_",anio,"_censusBID.dta",sep = "")  
    
  }
}