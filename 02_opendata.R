library(readxl)
readxl::excel_sheets("data/peerj-55829-Raw_data.xls")
dir("data")
#dir("C:\Users\lsare\Desktop\Plant Health\Estadistica\Biologicalcontrol\data\")
library(readxl)
#AntagonisticActivity <- read_excel("data/peerj-55829-Raw_data.xls")

#Open sheets with ranges 
AntagonisticActivity <-
  read_excel("data/peerj-55829-Raw_data.xls",
             sheet = "Antagonistic activity",
             range = "A3:E38")
DiseaseIncidenceAndSeverity <- read_excel("data/peerj-55829-Raw_data.xls",
                                          sheet = "disease incidence and severity",
                                          range = "A4:N40")
MorphometricCharactOfPotato <- read_excel("data/peerj-55829-Raw_data.xls",
                                          sheet = "morphometric charact of potato",)
BiomassDistribution <- read_excel("data/peerj-55829-Raw_data.xls",sheet = "biomass distribution")
YieldOfPotato <- read_excel("data/peerj-55829-Raw_data.xls",
                            sheet = "yield of potato",
                            range = "B3:I15")
#Fill 
library(tidyr)
library(dplyr)
AntagonisticActivity <- AntagonisticActivity %>% fill(everything(), .direction = "down")
BiomassDistribution <- BiomassDistribution %>% fill(everything(), .direction = "down")
DiseaseIncidenceAndSeverity <- DiseaseIncidenceAndSeverity %>% fill(everything(), .direction = "down")
MorphometricCharactOfPotato <- MorphometricCharactOfPotato %>% fill(everything(), .direction = "down")
YieldOfPotato <- YieldOfPotato %>% fill(everything(), .direction = "down")

#Rename 
library(dplyr)
names(AntagonisticActivity)[names(AntagonisticActivity) == 'strain'] <- "Strain"
names(AntagonisticActivity)[names(AntagonisticActivity) == 'CFU/ml'] <- "Concentration"

#Pivot longer 
AntagonisticActivity <- pivot_longer(AntagonisticActivity,cols= c("3d", "5d", "7d"), names_to="Days", values_to="Colony.Diameter")
