library(readxl)
readxl::excel_sheets("data/peerj-55829-Raw_data.xls")
dir("data")
#dir("C:\Users\lsare\Desktop\Plant Health\Estadistica\Biologicalcontrol\data\")
library(readxl)
#AntagonisticActivity <- read_excel("data/peerj-55829-Raw_data.xls")

AntagonisticActivity <-
  read_excel("data/peerj-55829-Raw_data.xls",
             sheet = "Antagonistic activity",
             range = "A3:E38")
DiseaseIncidenceAndSeverity <- read_excel("data/peerj-55829-Raw_data.xls",sheet = "disease incidence and severity")
MorphometricCharactOfPotato <- read_excel("data/peerj-55829-Raw_data.xls",sheet = "morphometric charact of potato")
BiomassDistribution <- read_excel("data/peerj-55829-Raw_data.xls",sheet = "biomass distribution")
YieldOfPotato <- read_excel("data/peerj-55829-Raw_data.xls",sheet = "yield of potato")

