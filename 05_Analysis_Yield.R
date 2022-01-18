#Duncan Yield 2015 Svitanok kievskiy
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con <- YieldOfPotato$Cultivar == "Svitanok kievskiy" & YieldOfPotato$Year==2015
d1 <- YieldOfPotato[Con,]

model<-aov(Yield.Potato~Treatment,data=d1)
out <- duncan.test(model,"Treatment", 
                   main="Yield of potato")
plot(out,variation="IQR")
out$groups$groups

letter1 <- character()
letter1 <- c(letter1,out$groups$groups)


#Duncan Yield 2015 Yuna
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con1 <- YieldOfPotato$Cultivar == "Yuna" & YieldOfPotato$Year==2015
d2 <- YieldOfPotato[Con1,]

model<-aov(Yield.Potato~Treatment,data=d2)
out <- duncan.test(model,"Treatment", 
                   main="Yield of potato")
plot(out,variation="IQR")
out
letter1 <- c(letter1,out$groups$groups)

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

#Duncan Yield Graph 
#Create data frame 
dat_text <- data.frame(
  label = letter1,
  Year   = c(rep(2015,6),rep(2016,6)),
  Treatment = rep(levels(as.factor(YieldOfPotato$Treatment)),4),
  Cultivar= c("Svitanok kievskiy","Svitanok kievskiy","Svitanok kievskiy","Yuna","Yuna","Yuna","Svitanok kievskiy","Svitanok kievskiy","Svitanok kievskiy","Yuna","Yuna","Yuna"))

duncanyieldgraph <- ggplot(YieldOfPotato, aes(x=`Cultivar`, y=Yield.Potato, fill=Treatment))+ylab("Yield of potato(t/ha)")+
  geom_boxplot()+
  facet_grid(.~Year)+
  theme_light()+
  geom_text(data = dat_text,
            mapping = aes(x = Cultivar,
                          y = c(29.7, 23.8,21.8, 28,27,22,20.5,17.9,13.1,21,21.6,16.8),
                          label = letter1),
            position = position_dodge(0.9))+
  labs(caption = "Figure No. 16 Results of Duncan test Yield of potato") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))
duncanyieldgraph

#Tukey Yield Graph 
#Create data frame 
dat_text1 <- data.frame(
  label = lettert,
  Year   = c(rep(2015,6),rep(2016,6)),
  Treatment = rep(levels(as.factor(YieldOfPotato$Treatment)),4),
  Cultivar= c("Svitanok kievskiy","Svitanok kievskiy","Svitanok kievskiy","Yuna","Yuna","Yuna","Svitanok kievskiy","Svitanok kievskiy","Svitanok kievskiy","Yuna","Yuna","Yuna"))

tukeyyieldgraph <- ggplot(YieldOfPotato, aes(x=`Cultivar`, y=Yield.Potato, fill=Treatment))+ylab("Yield of potato(t/ha)")+
  geom_boxplot()+
  facet_grid(.~Year)+
  theme_light()+
  geom_text(data = dat_text1,
            mapping = aes(x = Cultivar,
                          y = c(29.7, 23.8,21.8, 28,27,22.5,20.5,17.9,13.1,21.3,21.6,16.8),
                          label = lettert),
            position = position_dodge(0.9))+
  labs(caption = "Figure No. 17 Results of Tukey test Yield of potato") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))
tukeyyieldgraph



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




#Kruskal Yield 2015 Svitanok kievskiy
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con0 <- YieldOfPotato$Cultivar == "Svitanok kievskiy" & YieldOfPotato$Year==2015
k1 <- YieldOfPotato[Con0,]

kw <- kruskal(k1$Yield.Potato, k1$Treatment)
kw$groups

letterk <- character()
letterk <- c(letterk,out1$groups$groups)


#Kruskal Yield 2015 Yuna
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con10 <- YieldOfPotato$Cultivar == "Yuna" & YieldOfPotato$Year==2015
k2 <- YieldOfPotato[Con10,]

kw <- kruskal(k2$Yield.Potato, k2$Treatment)
kw$groups

letterk <- c(letterk,out1$groups$groups)


#Kruskal Yield 2016 Svitanok kievskiy
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Cont <- YieldOfPotato$Cultivar == "Svitanok kievskiy" & YieldOfPotato$Year==2016
k3 <- YieldOfPotato[Cont,]

kw <- kruskal(k3$Yield.Potato, k3$Treatment)
kw$groups

letterk <- c(letterk,out1$groups$groups)

#Kruskal Yield 2016 Yuna
YieldOfPotato$Cultivar <- factor(YieldOfPotato$Cultivar)
Con12345 <- YieldOfPotato$Cultivar == "Yuna" & YieldOfPotato$Year==2016
k4 <- YieldOfPotato[Con12345,]

kw <- kruskal(k4$Yield.Potato, k4$Treatment)
kw$groups

letterk <- c(letterk,out1$groups$groups)

#Kruskal Yield Graph 
#Create data frame 
dat_textk <- data.frame(
  label = letterk,
  Year   = c(rep(2015,6),rep(2016,6)),
  Treatment = rep(levels(as.factor(YieldOfPotato$Treatment)),4),
  Cultivar= c("Svitanok kievskiy","Svitanok kievskiy","Svitanok kievskiy","Yuna","Yuna","Yuna","Svitanok kievskiy","Svitanok kievskiy","Svitanok kievskiy","Yuna","Yuna","Yuna"))

kruskalyieldgraph <- ggplot(YieldOfPotato, aes(x=`Cultivar`, y=Yield.Potato, fill=Treatment))+ylab("Yield of potato(t/ha)")+
  geom_boxplot()+
  facet_grid(.~Year)+
  theme_light()+
  geom_text(data = dat_textk,
            mapping = aes(x = Cultivar,
                          y = c(29.7, 23.8,21.8, 28,27,22,20.5,17.9,13.1,21,21.6,16.8),
                          label = letter1),
            position = position_dodge(0.9))+
  labs(caption = "Figure. Results of Kruskal test Yield of potato") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))
kruskalyieldgraph




