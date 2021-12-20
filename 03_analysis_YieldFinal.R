#Duncan Yield 2015 Svitanok kievskiy
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con <- YieldOfPotato$Cultivar == "Svitanok kievskiy" & YieldOfPotato$Year==2015
d1 <- YieldOfPotato[Con,]

model<-aov(Yield.Potato~Treatment,data=d1)
out <- duncan.test(model,"Treatment", 
                   main="Yield of potato")
plot(out,variation="IQR")
out

#Duncan Yield 2015 Yuna
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con1 <- YieldOfPotato$Cultivar == "Yuna" & YieldOfPotato$Year==2015
d2 <- YieldOfPotato[Con1,]

model<-aov(Yield.Potato~Treatment,data=d2)
out <- duncan.test(model,"Treatment", 
                   main="Yield of potato")
plot(out,variation="IQR")
out

#Duncan Yield 2016 Svitanok kievskiy
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con1234 <- YieldOfPotato$Cultivar == "Svitanok kievskiy" & YieldOfPotato$Year==2016
d1234 <- YieldOfPotato[Con1234,]

model<-aov(Yield.Potato~Treatment,data=d1234)
out <- duncan.test(model,"Treatment", 
                   main="Yield of potato")
plot(out,variation="IQR")
out

#Duncan Yield 2016 Yuna
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con12345 <- YieldOfPotato$Cultivar == "Yuna" & YieldOfPotato$Year==2016
d2345 <- YieldOfPotato[Con12345,]

model<-aov(Yield.Potato~Treatment,data=d2345)
out <- duncan.test(model,"Treatment", 
                   main="Yield of potato")
plot(out,variation="IQR")

########### DUNCAN TEST
yield123 <- ggerrorplot(YieldOfPotato, x = "Cultivar", y = "Yield.Potato",
                     color = "Treatment", palette = "Paired", 
                     error.plot = "pointrange",
                     position = position_dodge(0.5),xlab = ("Cultivars"), ylab = ("Yield of potato (t/ha)"))+
  facet_grid(.~Year)+
  theme_light()
yield123
#graph yield of potato
# Analysis Biomass.Distribution duncan

Y <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(YieldOfPotato$Cultivar))) {
    {
      hola <- YieldOfPotato[YieldOfPotato$Year==i & 
                               YieldOfPotato$Cultivar==k, ]
      
      model1 <- aov(Yield.Potato~Treatment, data=hola)
      summary(model1)
      DT <- duncan.test(model1, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
      M <- c(M,DT$groups$Percentage.Biomass)
    } 
  }
}
LettersBio <- data.frame(Year = Y, `Fractions` = factor(W), 
                         Cultivar = C, Treatment = factor(Tr), MeanBiomass = M, letra = L )

LetterYield <- data.frame(Year = Y, Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)







