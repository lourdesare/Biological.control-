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

#change potencia
AntagonisticActivity$Concentration
a <- as.numeric(substr(AntagonisticActivity$Concentration, 1, 2))
b <- as.numeric(substr(AntagonisticActivity$Concentration, 3, 3))
a^b
AntagonisticActivity$Concentration <- a^b
rm(a,b)

#Create new column for treatments
AntagonisticActivity$Trat <- paste(AntagonisticActivity$Strain,AntagonisticActivity$Concentration)

#Join data frame 
library(dplyr)
Data.Cultivars <- full_join(MorphometricCharactOfPotato, DiseaseIncidenceAndSeverity, by = c("Cultivar","Year", "Treatment","Weeks after planting"))

#Rename 
library(dplyr)
names(Data.Cultivars)[names(Data.Cultivars) == 'Plant height, cm'] <- "1"
names(Data.Cultivars)[names(Data.Cultivars) == '...6.x'] <- "2"
names(Data.Cultivars)[names(Data.Cultivars) == '...7.x'] <- "3"
names(Data.Cultivars)[names(Data.Cultivars) == '...8.x'] <- "4"
names(Data.Cultivars)[names(Data.Cultivars) == '...9.x'] <- "5"
#Pivot longer 
Data.Cultivars <- pivot_longer(Data.Cultivars,cols= c("1", "2", "3","4","5"), names_to="Rep", values_to="Plant.Height")

#Create empty new columns 
Data.Cultivars$Disease.Incidence <- NA
Data.Cultivars$Disease.Severity <- NA
Data.Cultivars$Stems <- NA

#Add data to the columns 
variables <- c("Disease incidence, %","...6.y","...7.y","...8.y","...9.y")
for (i in 1:5) {
  A <- Data.Cultivars$Rep==i
  Data.Cultivars$Disease.Incidence[A] <- Data.Cultivars[[variables[i]]][A]
}

Data.Cultivars$Stems <- NA
variables1 <- c("The number of stems per plant","...11.x","...12.x","...13.x","...14.x")
for (i in 1:5) {
  A1 <- Data.Cultivars$Rep==i
  Data.Cultivars$Stems[A1] <- Data.Cultivars[[variables1[i]]][A1]
}

Data.Cultivars$Disease.Severity <- NA
variables1 <- c("Disease severity, %","...11.y","...12.y","...13.y","...14.y")
for (i in 1:5) {
  A2 <- Data.Cultivars$Rep==i
  Data.Cultivars$Disease.Severity[A2] <- Data.Cultivars[[variables1[i]]][A2]
}


#dataframe with variables that I need 
namesdatacultivar <- c("Cultivar","Treatment","Year","Weeks after planting", "Rep","Plant.Height","Disease.Incidence","Disease.Severity","Stems")           
Data.Cultivars.Final <- Data.Cultivars [which(names(Data.Cultivars) %in% namesdatacultivar)]

#Yieldofpotato 
#Rename 
library(dplyr)
names(YieldOfPotato)[names(YieldOfPotato) == 'Yield, t ha-1'] <- "1"
names(YieldOfPotato)[names(YieldOfPotato) == '...5'] <- "2"
names(YieldOfPotato)[names(YieldOfPotato) == '...6'] <- "3"
names(YieldOfPotato)[names(YieldOfPotato) == '...7'] <- "4"
names(YieldOfPotato)[names(YieldOfPotato) == '...8'] <- "5"
#Pivot longer 
YieldOfPotato <- pivot_longer(YieldOfPotato,cols= c("1", "2", "3","4","5"), names_to="Rep", values_to="Yield.Potato")

#BiomassDistribution
#Rename 
library(dplyr)
names(BiomassDistribution)[names(BiomassDistribution) == 'Biomass distribution by fractions, %'] <- "1"
names(BiomassDistribution)[names(BiomassDistribution) == '...6'] <- "2"
names(BiomassDistribution)[names(BiomassDistribution) == '...7'] <- "3"
names(BiomassDistribution)[names(BiomassDistribution) == '...8'] <- "4"
names(BiomassDistribution)[names(BiomassDistribution) == '...9'] <- "5"
#Pivot longer 
BiomassDistribution <- pivot_longer(BiomassDistribution,cols= c("1", "2", "3","4","5"), names_to="Rep", values_to="Percentage.Biomass")

#create a sheet for medium fraction
medium.fraction <- BiomassDistribution[BiomassDistribution$Fractions=="medium",]

#join sheets of yield and medium fraction
names(YieldOfPotato)[names(YieldOfPotato) == 'Bacillus strain'] <- "Treatment"
YieldOfPotato <- full_join(YieldOfPotato,medium.fraction)
YieldOfPotato <- YieldOfPotato[,-which(names(YieldOfPotato)=="Fractions")]

#remove sheets
ls()
rm("Data.Cultivars","DiseaseIncidenceAndSeverity","medium.fraction","MorphometricCharactOfPotato")

