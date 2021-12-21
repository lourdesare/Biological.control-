#Tukey Yield 2015 Svitanok kievskiy
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con0 <- YieldOfPotato$Cultivar == "Svitanok kievskiy" & YieldOfPotato$Year==2015
d111 <- YieldOfPotato[Co0,]

model<-aov(Yield.Potato~Treatment,data=d111)
out1 <- HSD.test(model,"Treatment", console =TRUE)
out1$groups$groups

letter142 <- character()
letter142 <- c(letter142,out$groups$groups)



#Duncan Yield 2015 Yuna
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con10 <- YieldOfPotato$Cultivar == "Yuna" & YieldOfPotato$Year==2015
d20 <- YieldOfPotato[Con10,]

model<-aov(Yield.Potato~Treatment,data=d20)
out01 <- HSD.test(model,"Treatment", 
                   main="Yield of potato")
plot(out,variation="IQR")
out
letter1 <- c(letter1,out$groups$groups)

#BZR 336g         25.7      a
#BZR 517          24.6      a
#Control          21.0      b
#Duncan Yield 2016 Svitanok kievskiy
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con1234 <- YieldOfPotato$Cultivar == "Svitanok kievskiy" & YieldOfPotato$Year==2016
d1234 <- YieldOfPotato[Con1234,]

model<-aov(Yield.Potato~Treatment,data=d1234)
out <- duncan.test(model,"Treatment", 
                   main="Yield of potato")
plot(out,variation="IQR")
out
letter1 <- c(letter1,out$groups$groups)
#BZR 336g         18.9      a
#BZR 517          16.5      b
#Control          11.6      c
#Duncan Yield 2016 Yuna
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con12345 <- YieldOfPotato$Cultivar == "Yuna" & YieldOfPotato$Year==2016
d2345 <- YieldOfPotato[Con12345,]

model<-aov(Yield.Potato~Treatment,data=d2345)
out <- duncan.test(model,"Treatment", 
                   main="Yield of potato")
plot(out,variation="IQR")
out
letter1 <- c(letter1,out$groups$groups)
#BZR 336g        19.70      a
#BZR 517         19.62      a
#Control         15.46      b
########### graph
library(ggplot2)
library(ggpubr)
yield123 <- ggerrorplot(YieldOfPotato, x = "Cultivar", y = "Yield.Potato",
                        color = "Treatment", palette = "Paired", 
                        error.plot = "pointrange",
                        position = position_dodge(0.5),xlab = ("Cultivars"), ylab = ("Yield of potato (t/ha)"))+
  facet_grid(.~Year)+
  theme_light()
yield123

#add letterss
#c("a", "b", "c","a","a","b","a", "b", "c","a","a","b")
dat_text <- data.frame(
  label = letter1,
  Year   = c(rep(2015,6),rep(2016,6)),
  x     = c(0.85, 1,1.2,1.83,2,2.17,0.85, 1,1.2,1.83,2,2.17),
  y     = c(29.3, 23.3,21.6, 27,26,21.8,19.8,17.3,12.3,20.55,20.6,16.1)
)

yield1234 <- yield123 + geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label)
)
yield1234