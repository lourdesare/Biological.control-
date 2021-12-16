

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
out
#graph yield
graphyield <- ggerrorplot(YieldOfPotato, x = "Cultivar", y = "Yield.Potato",
                     color = "Treatment", palette = "Paired", 
                     error.plot = "pointrange",
                     position = position_dodge(0.5),xlab = ("Cultivars"), ylab = ("Yield of potato (t/ha)"))+
  facet_grid(.~Year)+
  theme_light()
graphyield


#join two graphs 
graphs <- plot_grid(
  dp + theme(legend.position="none"), dens + theme(legend.position="bottom"),
  labels = "AUTO", ncol = 1
)
graphs
