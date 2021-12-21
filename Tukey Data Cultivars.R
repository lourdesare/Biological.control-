#Tukey Data.Cultivars.Final

library(agricolae)

########### GRAFICS

# Plant Height
model<-aov(Plant.Height~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final)
summary(model)

Data.Cultivars.Final$`Weeks after planting` <- factor(Data.Cultivars.Final$`Weeks after planting`)


# Disease Incidence
model<-aov(Disease.Incidence~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final)
summary(model)

Data.Cultivars.Final$`Weeks after planting` <- factor(Data.Cultivars.Final$`Weeks after planting`)


# Disease Severity
model<-aov(Disease.Severity~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final)
summary(model)

Data.Cultivars.Final$`Weeks after planting` <- factor(Data.Cultivars.Final$`Weeks after planting`)


# Stems
model<-aov(Stems~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final)
summary(model)

Data.Cultivars.Final$`Weeks after planting` <- factor(Data.Cultivars.Final$`Weeks after planting`)



#Tukey

## Plant Height

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1z <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                   Data.Cultivars.Final$`Weeks after planting`==j &
                                   Data.Cultivars.Final$Cultivar==k, ]
      model <- aov(Plant.Height~Treatment, data=d1z)
      summary(model)
      DT <- HSD.test(model, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
      M <- c(M, DT$groups$Plant.Height)
    } 
  }
}


LettersPHz <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                        Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

## Disease Incidence

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1z <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                   Data.Cultivars.Final$`Weeks after planting`==j &
                                   Data.Cultivars.Final$Cultivar==k, ]
      model <- aov(Disease.Incidence~Treatment, data=d1z)
      summary(model)
      DT <- duncan.test(model, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
      M <- c(M, DT$groups$Disease.Incidence)
    } 
  }
}


LettersDIz <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                        Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

## Disease Severity

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1z <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                   Data.Cultivars.Final$`Weeks after planting`==j &
                                   Data.Cultivars.Final$Cultivar==k, ]
      model <- aov(Disease.Severity~Treatment, data=d1z)
      summary(model)
      DT <- duncan.test(model, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
      M <- c(M, DT$groups$Disease.Severity)
    } 
  }
}


LettersDSz <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                        Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

## Stems

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1z <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                   Data.Cultivars.Final$`Weeks after planting`==j &
                                   Data.Cultivars.Final$Cultivar==k, ]
      model <- aov(Stems~Treatment, data=d1z)
      summary(model)
      DT <- duncan.test(model, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
      M <- c(M, DT$groups$Stems)
    } 
  }
}


LettersSTz <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                        Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)


#### Juntar letras en la grafica 

# Plant Height

ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Plant.Height, fill=Treatment))+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersPHz,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 5,
                          label = letra),
            position = position_dodge(0.9))


## Disease Incidence

ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Disease.Incidence, fill=Treatment))+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersDIz,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 5,
                          label = letra),
            position = position_dodge(0.9))

## Disease Severity

ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Disease.Severity, fill=Treatment))+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersDSz,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 5,
                          label = letra),
            position = position_dodge(0.9))


## Stems

ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Stems, fill=Treatment))+
  geom_boxplot()+ 
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersSTz,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 1,
                          label = letra),
            position = position_dodge(0.9))