#Prueba Biomass
model<-aov(Percentage.Biomass~Treatment+`Fractions`+Year, data=BiomassDistribution)
summary(model)
BiomassDistribution$`Fractions` <- factor(BiomassDistribution$`Fractions`)

#Biomass Duncant Test
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

LettersPH1 <- data.frame(Year = Y, `Fractions` = factor(W), 
                         Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)



#Prueba Biomass tukey 
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
LetterstPH11 <- data.frame(Year = Y, `Fractions` = factor(W), 
                           Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)


#Biomass Graph Duncan 
ggplot(BiomassDistribution, aes(x=`Fractions`, y=Percentage.Biomass, fill=Treatment))+ylab("Biomass (%)")+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersPH1,
            mapping = aes(x = Fractions,
                          y = mea + 8,
                          label = letra),
            position = position_dodge(0.9))+
  labs(caption = "Figure No. 1 Result of Duncan Percentage of Biomass of potato") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

#Biomass Graph Tukey 
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

#Residuals Biomass
anova.biomass<-aov(Percentage.Biomass~Treatment+`Fractions`+Year, data=BiomassDistribution) #anova 
anova.biomass.sqrt <- aov(sqrt(Percentage.Biomass)~Treatment+`Fractions`+Year, data=BiomassDistribution) #square root 
shapiro.test(anova.biomass$residuals) #0.06424
shapiro.test(anova.biomass.sqrt$residuals) #0.3364
plot(anova.biomass, main = "ANOVA Percentage of Biomass", which = 2, ask = F,sub.caption = "")

#AIC and BIC 
AIC(anova.biomass, anova.biomass.sqrt)
BIC(anova.biomass, anova.biomass.sqrt)


#Biomass Kruskal  

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(BiomassDistribution$Cultivar))) {
    for(j in levels(BiomassDistribution$`Fractions`)) {
      d1k <- BiomassDistribution[BiomassDistribution$Year==i & 
                                   BiomassDistribution$`Fractions`==j &
                                   BiomassDistribution$Cultivar==k, ]
      d1k$Treatment <- factor(d1k$Treatment)
      kw <- with(d1k,kruskal(Percentage.Biomass,Treatment))
      gr <- kw$groups
      lt <- gr$groups[order(row.names(gr))]
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, levels(d1k$Treatment))
      L <- c(L, lt)
      M <- c(M, mean(d1k$Percentage.Biomass[d1k$Treatment == levels(d1k$Treatment)[1]]),
             mean(d1k$Percentage.Biomass[d1k$Treatment == levels(d1k$Treatment)[2]]),
             mean(d1k$Percentage.Biomass[d1k$Treatment == levels(d1k$Treatment)[3]]))
    }
  }
}

LettersPH12 <- data.frame(Year = Y, `Fractions` = factor(W), 
                         Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

#Biomass Kruskal Graph

ggplot(BiomassDistribution, aes(x=`Fractions`, y=Percentage.Biomass, fill=Treatment))+ylab("Biomass (%)")+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersPH12,
            mapping = aes(x = Fractions,
                          y = mea + 5,
                          label = letra),
            position = position_dodge(0.9))+
  labs(caption = "Results Kruskal Figure No.  Percentage of Biomass of potato") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))
