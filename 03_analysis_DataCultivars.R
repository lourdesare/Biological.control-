# Analysis Data.Cultivars.Final

library(agricolae)

########### GRAFICS

# Plant Height
model<-aov(Plant.Height~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final)
summary(model)

Data.Cultivars.Final$`Weeks after planting` <- factor(Data.Cultivars.Final$`Weeks after planting`)

library(ggplot2)
ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Plant.Height, fill=Treatment))+
  geom_boxplot()+
  facet_grid(.~Year)+
  theme_light()

# Disease Incidence
model<-aov(Disease.Incidence~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final)
summary(model)

Data.Cultivars.Final$`Weeks after planting` <- factor(Data.Cultivars.Final$`Weeks after planting`)

library(ggplot2)
ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Disease.Incidence, fill=Treatment))+
  geom_boxplot()+
  facet_grid(.~Year)+
  theme_light()

# Disease Severity
model<-aov(Disease.Severity~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final)
summary(model)

Data.Cultivars.Final$`Weeks after planting` <- factor(Data.Cultivars.Final$`Weeks after planting`)

library(ggplot2)
ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Disease.Severity, fill=Treatment))+
  geom_boxplot()+
  facet_grid(.~Year)+
  theme_light()

# Stems
model<-aov(Stems~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final)
summary(model)

Data.Cultivars.Final$`Weeks after planting` <- factor(Data.Cultivars.Final$`Weeks after planting`)

library(ggplot2)
ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Stems, fill=Treatment))+
  geom_boxplot()+
  facet_grid(.~Year)+
  theme_light()

########### DUNCAN TEST

## Plant Height

for (i in 2015:2016) { 
  for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
    year <- i
  week <- j
  source("duncan.R")}
}


## Disease Incidence

for (i in 2015:2016) { 
  for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
    year <- i
    week <- j
    source("duncan.R")}
}


