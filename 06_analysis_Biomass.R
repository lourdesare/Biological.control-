# Analysis Biomass.Distribution duncan

Y <- W <- Tr <- L <- C <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(BiomassDistribution$Cultivar))) {
    for(j in levels(BiomassDistribution$`Fractions`)) {
      d1 <- BiomassDistribution[BiomassDistribution$Year==i & 
                                   BiomassDistribution$`Fractions`==j &
                                   BiomassDistribution$Cultivar==k, ]
      model <- aov(Percentage.Biomass~Treatment, data=d1)
      summary(model)
      DT <- duncan.test(model, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
    } 
  }
}

#graph
LettersBio <- data.frame(Year = Y, `Fractions` = factor(W), 
                        Cultivar = C, Treatment = factor(Tr), letra = L )
#
model<-aov(Percentage.Biomass~Treatment+`Fractions`+Year, data=BiomassDistribution)
summary(model)

BiomassDistribution$`Fractions` <- factor(BiomassDistribution$`Fractions`)

library(ggplot2)
ggplot(BiomassDistribution, aes(x=`Fractions`, y=Percentage.Biomass, fill=Treatment))+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersBio,
            mapping = aes(x = Fractions,
                          y = 40,
                          label = letra),
            position = position_dodge(0.9))

coord_cartesian(xlim = c(0.3, 3.5), ylim = c(0.5, 3.5))