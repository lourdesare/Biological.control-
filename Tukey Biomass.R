#Prueba Biomass tukey 
model<-aov(Percentage.Biomass~Treatment+`Fractions`+Year, data=BiomassDistribution)
summary(model)
BiomassDistribution$`Fractions` <- factor(BiomassDistribution$`Fractions`)

##Biomass
library(agricolae)
Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(BiomassDistribution$Cultivar))) {
    for(j in levels(BiomassDistribution$`Fractions`)) {
      t200 <- BiomassDistribution[BiomassDistribution$Year==i & 
                                    BiomassDistribution$`Fractions`==j &
                                    BiomassDistribution$Cultivar==k, ]
      model <- aov(Percentage.Biomass~Treatment, data=t200)
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
LetterstPH11 <- data.frame(Year = Y, `Fractions` = factor(W), 
                         Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

ggplot(BiomassDistribution, aes(x=`Fractions`, y=Percentage.Biomass, fill=Treatment))+ ylab("Biomass (%)")+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LetterstPH11,
            mapping = aes(x = Fractions,
                          y = mea + 8,
                          label = letra),
            position = position_dodge(0.9))+
  labs(caption = "Results Tukey Figure No.  Percentage of Biomass of potato") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))


