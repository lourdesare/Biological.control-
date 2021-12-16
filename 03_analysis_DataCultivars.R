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

d1 <- Data.Cultivars.Final[Data.Cultivars.Final$Year==year & Data.Cultivars.Final$`Weeks after planting`==week, ]

## Plant Height
Y <- W <- Tr <- L <- numeric()

#i <- 2015
#j <- levels(Data.Cultivars.Final$`Weeks after planting`)[1]

for (i in 2015:2016) { 
  for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
    year <- i
  week <- j
  model <- aov(Plant.Height~Treatment, data=d1)
  summary(model)
  DT <- duncan.test(model, "Treatment", console = TRUE)
  Y <- c(Y, i,i,i)
  W <- c(W, j,j,j)
  Tr <- c(Tr, row.names(DT$groups))
  L <- c(L, DT$groups$groups)
  } 
}

LettersPH <- data.frame(year = Y, `Weeks after planting` = factor(W), Treatment = factor(Tr), letra = L )

## Disease Incidence

Y <- W <- Tr <- L <- numeric()

for (i in 2015:2016) { 
  for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
    year <- i
    week <- j
    model <- aov(Disease.Incidence~Treatment, data=d1)
    summary(model)
    DT <- duncan.test(model, "Treatment", console = TRUE)
    Y <- c(Y, i,i,i)
    W <- c(W, j,j,j)
    Tr <- c(Tr, row.names(DT$groups))
    L <- c(L, DT$groups$groups)
  } 
}

LettersDI <- data.frame(year = Y, `Weeks after planting` = factor(W), Treatment = factor(Tr), letra = L )

## Disease Severity

Y <- W <- Tr <- L <- numeric()

for (i in 2015:2016) { 
  for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
    year <- i
    week <- j
    model <- aov(Disease.Severity~Treatment, data=d1)
    summary(model)
    DT <- duncan.test(model, "Treatment", console = TRUE)
    Y <- c(Y, i,i,i)
    W <- c(W, j,j,j)
    Tr <- c(Tr, row.names(DT$groups))
    L <- c(L, DT$groups$groups)
  } 
}

LettersDS <- data.frame(year = Y, `Weeks after planting` = factor(W), Treatment = factor(Tr), letra = L )


## Stems

Y <- W <- Tr <- L <- numeric()

for (i in 2015:2016) { 
  for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
    year <- i
    week <- j
    model <- aov(Stems~Treatment, data=d1)
    summary(model)
    DT <- duncan.test(model, "Treatment", console = TRUE)
    Y <- c(Y, i,i,i)
    W <- c(W, j,j,j)
    Tr <- c(Tr, row.names(DT$groups))
    L <- c(L, DT$groups$groups)
  } 
}

LettersST <- data.frame(year = Y, `Weeks after planting` = factor(W), Treatment = factor(Tr), letra = L )



#### Juntar letras en la grafica 

# Plant Height

ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Plant.Height, fill=Treatment))+
  geom_boxplot()+
  facet_grid(.~Year)+
  theme_light()+
  geom_text(data = LettersPH,
            mapping = aes(x = Weeks.after.planting,
                          y = 45,
                          label = letra),
            position = position_dodge(0.9))
            

# Disease Incidence

ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Disease.Incidence, fill=Treatment))+
  geom_boxplot()+
  facet_grid(.~Year)+
  theme_light()+
  geom_text(data = LettersDI,
            mapping = aes(x = Weeks.after.planting,
                          y = 112,
                          label = letra),
            position = position_dodge(0.9))

# Disease Severity

ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Disease.Severity, fill=Treatment))+
  geom_boxplot()+
  facet_grid(.~Year)+
  theme_light()+
  geom_text(data = LettersDS,
            mapping = aes(x = Weeks.after.planting,
                          y = 60,
                          label = letra),
            position = position_dodge(0.9))

# Stems

ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Stems, fill=Treatment))+
  geom_boxplot()+
  facet_grid(.~Year)+
  theme_light()+
  geom_text(data = LettersST,
            mapping = aes(x = Weeks.after.planting,
                          y = 10,
                          label = letra),
            position = position_dodge(0.9))
