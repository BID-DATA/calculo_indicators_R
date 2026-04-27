
# Function that returns the address of the survey based on country, type and period


functionRoundAndSurvey <- function(pais, tipo, anio) {
  
  
  if (tipo == "encuestas") {
    
    # 1. reading dataset with surveys, round and year
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
    
    if (isTRUE(restriction == "restricted")) {
      
      base <- paste("//sapidbshares.file.core.windows.net//idbrestrictedshares//SCL_DATAFILES_RESTRICTED//harmonized//",pais,"//",survey,"//data_arm//",pais,"_",anio,round,"_BID.dta",sep = "")
    }
    else {
      base <- paste("Z://harmonized//",pais,"//",survey,"//data_arm//",pais,"_",anio,round,"_BID.dta",sep = "")  
    }
    # return database address
    return(base)
    
    
  }
  
  if (tipo == "censos"){
    
    base <- paste("Z://census//clean//",pais,"//",pais,"_",anio,"_censusBID.dta",sep = "")  
    
  }
}