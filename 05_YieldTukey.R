#Tukey Yield 2015 Svitanok kievskiy
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con0 <- YieldOfPotato$Cultivar == "Svitanok kievskiy" & YieldOfPotato$Year==2015
t1 <- YieldOfPotato[Con0,]

model<-aov(Yield.Potato~Treatment,data=t1)
out1 <- HSD.test(model,"Treatment", console =TRUE)
out1$groups$groups

lettert <- character()
lettert <- c(lettert,out1$groups$groups)


#Tukey Yield 2015 Yuna
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con10 <- YieldOfPotato$Cultivar == "Yuna" & YieldOfPotato$Year==2015
t2 <- YieldOfPotato[Con10,]

model<-aov(Yield.Potato~Treatment,data=t2)
out1 <- HSD.test(model,"Treatment", console =TRUE)
lettert <- c(lettert,out1$groups$groups)


#Tukey Yield 2016 Svitanok kievskiy
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Cont <- YieldOfPotato$Cultivar == "Svitanok kievskiy" & YieldOfPotato$Year==2016
t3 <- YieldOfPotato[Cont,]

model<-aov(Yield.Potato~Treatment,data=t3)
out1 <- HSD.test(model,"Treatment", console =TRUE)
lettert <- c(lettert,out1$groups$groups)

#Tukey Yield 2016 Yuna
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con12345 <- YieldOfPotato$Cultivar == "Yuna" & YieldOfPotato$Year==2016
t4 <- YieldOfPotato[Con12345,]

model<-aov(Yield.Potato~Treatment,data=t4)
out1 <- HSD.test(model,"Treatment", console =TRUE)
lettert <- c(lettert,out1$groups$groups)

#Graph Tukey
library(ggplot2)
library(ggpubr)
yieldtuk <- ggerrorplot(YieldOfPotato, x = "Cultivar", y = "Yield.Potato",
                        color = "Treatment", palette = "Paired", 
                        error.plot = "pointrange",
                        position = position_dodge(0.5),xlab = ("Cultivars"), ylab = ("Yield of potato (t/ha)"))+
  facet_grid(.~Year)+
  theme_light()
yieldtuk

#add letterss
#c("a", "b", "c","a","a","b","a", "b", "c","a","a","b")
dat_text1 <- data.frame(
  label = lettert,
  Year   = c(rep(2015,6),rep(2016,6)),
  x     = c(0.85, 1,1.2,1.83,2,2.17,0.85, 1,1.2,1.83,2,2.17),
  y     = c(29.3, 23.3,21.6, 27,26,21.8,19.8,17.3,12.3,20.55,20.6,16.1)
)

yieldtukey1 <- yieldtuk + geom_text(
  data    = dat_text1,
  mapping = aes(x = x, y = y, label = label)
)
yieldtukey1
