#Prueba Biomass
model<-aov(Percentage.Biomass~Treatment+`Fractions`+Year, data=BiomassDistribution)
summary(model)
BiomassDistribution$`Fractions` <- factor(BiomassDistribution$`Fractions`)


library(ggplot2)
ggplot(BiomassDistribution, aes(x=`Fractions`, y=Percentage.Biomass, fill=Treatment))+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()

##Biomass

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(BiomassDistribution$Cultivar))) {
    for(j in levels(BiomassDistribution$`Fractions`)) {
      d200 <- BiomassDistribution[BiomassDistribution$Year==i & 
                                   BiomassDistribution$`Fractions`==j &
                                   BiomassDistribution$Cultivar==k, ]
      model <- aov(Percentage.Biomass~Treatment, data=d200)
      summary(model)
      DT <- duncan.test(model, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
      M <- c(M, DT$groups$Percentage.Biomass)
    } 
  }
}

#Biomass
LettersPH1 <- data.frame(Year = Y, `Fractions` = factor(W), 
                        Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

ggplot(BiomassDistribution, aes(x=`Fractions`, y=Percentage.Biomass, fill=Treatment))+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersPH1,
            mapping = aes(x = Fractions,
                          y = mea + 5,
                          label = letra),
            position = position_dodge(0.9))+
  labs(caption = "Figure No. 1 Result of Duncan Percentage of Biomass of potato") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

#Biomass duncan test  
model<-aov(Percentage.Biomass~Treatment+`Fractions`+Year, data=BiomassDistribution)
summary(model)
BiomassDistribution$`Fractions` <- factor(BiomassDistribution$`Fractions`)


library(ggplot2)
ggplot(BiomassDistribution, aes(x=`Fractions`, y=Percentage.Biomass, fill=Treatment))+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()

##Biomass

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(BiomassDistribution$Cultivar))) {
    for(j in levels(BiomassDistribution$`Fractions`)) {
      d2001 <- BiomassDistribution[BiomassDistribution$Year==i & 
                                     BiomassDistribution$`Fractions`==j &
                                     BiomassDistribution$Cultivar==k, ]
      model <- aov(Percentage.Biomass~Treatment, data=d2001)
      summary(model)
      DT <- HSD.test(model, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
      M <- c(M, DT$groups$Percentage.Biomass)
    } 
  }
}

#Biomass
LettersPH11 <- data.frame(Year = Y, `Fractions` = factor(W), 
                          Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

ggplot(BiomassDistribution, aes(x=`Fractions`, y=Percentage.Biomass, fill=Treatment))+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersPH11,
            mapping = aes(x = Fractions,
                          y = mea + 5,
                          label = letra),
            position = position_dodge(0.9))+
  labs(caption = "Results Tukey Figure No. 1 Percentage of Biomass of potato") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))
  



