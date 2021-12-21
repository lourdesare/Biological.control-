#Residuals 

#Residuals Yield 2015 Svitanok kievskiy
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con <- YieldOfPotato$Cultivar == "Svitanok kievskiy" & YieldOfPotato$Year==2015
d1 <- YieldOfPotato[Con,]
anova.yieldsk<-aov(Yield.Potato~Treatment,data=d1) #anova 
anova.yieldsk.sqrt <- aov(sqrt(Yield.Potato)~Treatment,data=d1) #square root 
shapiro.test(anova.yieldsk$residuals) #0.0553
shapiro.test(anova.yieldsk.sqrt$residuals) #0.02795

#Residuals Yield 2015 Yuna
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con1 <- YieldOfPotato$Cultivar == "Yuna" & YieldOfPotato$Year==2015
d2 <- YieldOfPotato[Con1,]
anova.yieldy<-aov(Yield.Potato~Treatment,data=d2)#anova 
anova.yieldy.sqrt <- aov(sqrt(Yield.Potato)~Treatment,data=d2) #square root
shapiro.test(anova.yieldy$residuals) #0.4242
shapiro.test(anova.yieldy.sqrt$residuals) #0.3887

#Residuals Yield 2016 Svitanok kievskiy
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con1234 <- YieldOfPotato$Cultivar == "Svitanok kievskiy" & YieldOfPotato$Year==2016
d1234 <- YieldOfPotato[Con1234,]
anova.yieldsk6<-aov(Yield.Potato~Treatment,data=d1234)#anova
anova.yieldsk6.sqrt <- aov(sqrt(Yield.Potato)~Treatment,data=d1234)#square root
shapiro.test(anova.yieldsk6$residuals) #0.1674
shapiro.test(anova.yieldsk6.sqrt$residuals) #0.1481

#Residuals Yield 2016 Yuna
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con12345 <- YieldOfPotato$Cultivar == "Yuna" & YieldOfPotato$Year==2016
d2345 <- YieldOfPotato[Con12345,]
anova.yieldy6<-aov(Yield.Potato~Treatment,data=d2345) #anova
anova.yieldy6.sqrt <- aov(sqrt(Yield.Potato)~Treatment,data=d2345) #square root
shapiro.test(anova.yieldy6$residuals) #0.6108
shapiro.test(anova.yieldy6.sqrt$residuals) #0.6299

#Graph 
par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))
p1 <- plot(anova.yieldsk, main = "ANOVA 2015 SK", which = 2, ask = F,sub.caption = "") 
p2 <- plot(anova.yieldy, main = "ANOVA 2015 Y", which = 2, ask = F,sub.caption = "") 
p3 <- plot(anova.yieldsk6, main = "ANOVA 2016 SK", which = 2, ask = F,sub.caption = "") 
p4 <- plot(anova.yieldy6, main = "ANOVA 2016 y", which = 2, ask = F,sub.caption = "")

