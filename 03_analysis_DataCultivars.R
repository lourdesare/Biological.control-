#Duncan Analysis Data.Cultivars.Final
library(agricolae)
library(ggplot2)
library(ggpubr)

# Plant Height

#Anova Plant Height
model<-aov(Plant.Height~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final)
summary(model)
Data.Cultivars.Final$`Weeks after planting` <- factor(Data.Cultivars.Final$`Weeks after planting`)

## Plant Height Duncan 

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1 <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                   Data.Cultivars.Final$`Weeks after planting`==j &
                                   Data.Cultivars.Final$Cultivar==k, ]
      model <- aov(Plant.Height~Treatment, data=d1)
      summary(model)
      DT <- duncan.test(model, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
      M <- c(M, DT$groups$Plant.Height)
    } 
  }
}


LettersPH <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                        Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

#Plant Height Duncan Graph

ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Plant.Height, fill=Treatment))+ylab("Plant Height (cm)")+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersPH,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 5,
                          label = letra),
            position = position_dodge(0.9))+
  labs(caption = "Figure 1. Results of Duncan Test in the variable of Plant Height on potato (cm)") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

#Plant Height Tukey 

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1z <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                    Data.Cultivars.Final$`Weeks after planting`==j &
                                    Data.Cultivars.Final$Cultivar==k, ]
      model <- aov(Plant.Height~Treatment, data=d1z)
      summary(model)
      DT <- HSD.test(model, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
      M <- c(M, DT$groups$Plant.Height)
    } 
  }
}

LettersPHz <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                         Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

#Plant Height Tukey Graph

ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Plant.Height, fill=Treatment))+ylab("Plant Height (cm)")+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersPHz,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 5,
                          label = letra),
            position = position_dodge(0.9))+
  labs(caption = "Figure No 2. Results of Tukey Test in the variable of Plant Height on potato (cm)") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

#Residuals Plant Height 
anova.ph<-aov(Plant.Height~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final) #anova 
anova.ph.sqrt <- aov(sqrt(Plant.Height)~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final) #square root 
shapiro.test(anova.ph$residuals) #0.0329 anormal 
shapiro.test(anova.ph.sqrt$residuals) #0.02933
par(mfrow = c(1, 2), oma = c(0, 0, 1.1, 0))
plot(anova.ph, main = "ANOVA", which = 2, ask = F,sub.caption = "Plant Height Residuals")
plot(anova.ph.sqrt, main = "Square Root ANOVA", which = 2, ask = F,sub.caption = "")


#Plant Height Kruskal  

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1k <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                    Data.Cultivars.Final$`Weeks after planting`==j &
                                    Data.Cultivars.Final$Cultivar==k, ]
      d1k$Treatment <- factor(d1k$Treatment)
      kw <- with(d1k,kruskal(Plant.Height,Treatment))
      gr <- kw$groups
      lt <- gr$groups[order(row.names(gr))]
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, levels(d1k$Treatment))
      L <- c(L, lt)
      M <- c(M, mean(d1k$Plant.Height[d1k$Treatment == levels(d1k$Treatment)[1]]),
             mean(d1k$Plant.Height[d1k$Treatment == levels(d1k$Treatment)[2]]),
             mean(d1k$Plant.Height[d1k$Treatment == levels(d1k$Treatment)[3]]))
    }
  }
}

LettersPHk <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                         Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

#Plant Height Kruskal Graph

ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Plant.Height, fill=Treatment))+ylab("Plant Height (cm)")+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersPHk,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 5,
                          label = letra),
            position = position_dodge(0.9))+
  labs(caption = "Figure No X. Results of Kruskal Test in the variable of Plant Height on potato (cm)") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))



#Disease Incidence
#Anova Disease Incidence 
model<-aov(Disease.Incidence~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final)
summary(model)
Data.Cultivars.Final$`Weeks after planting` <- factor(Data.Cultivars.Final$`Weeks after planting`)


#Duncan Test Disease Incidence

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1 <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                   Data.Cultivars.Final$`Weeks after planting`==j &
                                   Data.Cultivars.Final$Cultivar==k, ]
      model <- aov(Disease.Incidence~Treatment, data=d1)
      summary(model)
      DT <- duncan.test(model, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
      M <- c(M, DT$groups$Disease.Incidence)
    } 
  }
}


LettersDI <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                        Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

#Tukey Disease Incidence

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1z <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                    Data.Cultivars.Final$`Weeks after planting`==j &
                                    Data.Cultivars.Final$Cultivar==k, ]
      model <- aov(Disease.Incidence~Treatment, data=d1z)
      summary(model)
      DT <- HSD.test(model, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
      M <- c(M, DT$groups$Disease.Incidence)
    } 
  }
}


LettersDIz <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                         Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

#Duncan Graph Disease Incidence
ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Disease.Incidence, fill=Treatment))+ylab("Disease Incidence(%)")+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersDI,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 8,
                          label = letra),
            position = position_dodge(0.9))

#Tukey Graph Disease Incidence
ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Disease.Incidence, fill=Treatment))+ylab("Disease Incidence(%)")+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersDIz,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 8,
                          label = letra),
            position = position_dodge(0.9))
#Residuals Disease Incidence
anova.di<-aov(Disease.Incidence~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final) #anova 
anova.di.sqrt <- aov(sqrt(Disease.Incidence)~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final) #square root 
shapiro.test(anova.di$residuals) #0.03132 anormal 
shapiro.test(anova.di.sqrt$residuals) #0.001165
par(mfrow = c(1, 2), oma = c(0, 0, 1.1, 0))
plot(anova.di, main = "ANOVA", which = 2, ask = F,sub.caption = "Disease Insidence")
plot(anova.di.sqrt, main = "Square Root ANOVA", which = 2, ask = F,sub.caption = "")

#Disease Incidence Kruskal  

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1k <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                    Data.Cultivars.Final$`Weeks after planting`==j &
                                    Data.Cultivars.Final$Cultivar==k, ]
      d1k$Treatment <- factor(d1k$Treatment)
      kw <- with(d1k,kruskal(Disease.Incidence,Treatment))
      gr <- kw$groups
      lt <- gr$groups[order(row.names(gr))]
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, levels(d1k$Treatment))
      L <- c(L, lt)
      M <- c(M, mean(d1k$Disease.Incidence[d1k$Treatment == levels(d1k$Treatment)[1]]),
             mean(d1k$Disease.Incidence[d1k$Treatment == levels(d1k$Treatment)[2]]),
             mean(d1k$Disease.Incidence[d1k$Treatment == levels(d1k$Treatment)[3]]))
    }
  }
}

LettersDIk <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                         Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

#Disease Incidence Kruskal Graph

ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Disease.Incidence, fill=Treatment))+ylab("Disease Incidence(%)")+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersDIk,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 5,
                          label = letra),
            position = position_dodge(0.9))+
  labs(caption = "Figure No X. Results of Kruskal Test in the variable of Disease Incidence on potato (cm)") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))






#Disease Severity

#Anova Disease Severity
model<-aov(Disease.Severity~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final)
summary(model)
Data.Cultivars.Final$`Weeks after planting` <- factor(Data.Cultivars.Final$`Weeks after planting`)
#Disease Severity Duncan Test

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1 <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                   Data.Cultivars.Final$`Weeks after planting`==j &
                                   Data.Cultivars.Final$Cultivar==k, ]
      model <- aov(Disease.Severity~Treatment, data=d1)
      summary(model)
      DT <- duncan.test(model, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
      M <- c(M, DT$groups$Disease.Severity)
    } 
  }
}


LettersDS <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                        Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

#Tukey Test Disease Severity

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1z <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                    Data.Cultivars.Final$`Weeks after planting`==j &
                                    Data.Cultivars.Final$Cultivar==k, ]
      model <- aov(Disease.Severity~Treatment, data=d1z)
      summary(model)
      DT <- HSD.test(model, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
      M <- c(M, DT$groups$Disease.Severity)
    } 
  }
}


LettersDSz <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                         Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

#Duncan Graph Disease Severity
ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Disease.Severity, fill=Treatment))+ylab("Disease Severity (%)")+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersDS,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 5,
                          label = letra),
            position = position_dodge(0.9))+
  labs(caption = "Figure No 7. Results of Duncan Test in the variable of Disease Severity (%)") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

#Tukey Graph Disease Severity

ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Disease.Severity, fill=Treatment))+ylab("Disease Severity(%)")+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+ 
  geom_text(data = LettersDSz,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 7,
                          label = letra),
            position = position_dodge(0.9))+
  labs(caption = "Figure No 8. Results of Tukey Test in the variable of Disease Severity (%)") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

#Residuals Disease Severity
anova.ds<-aov(Disease.Severity~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final) #anova 
anova.ds.sqrt <- aov(sqrt(Disease.Severity)~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final) #square root 
shapiro.test(anova.ds$residuals) #0.002356 anormal  
shapiro.test(anova.ds.sqrt$residuals) #0.0009698
par(mfrow = c(1, 2), oma = c(0, 0, 1.1, 0))
plot(anova.ds, main = "ANOVA", which = 2, ask = F,sub.caption = "Disease Severity")
plot(anova.ds.sqrt, main = "Square Root ANOVA", which = 2, ask = F,sub.caption = "")

#Disease Severity Kruskal  

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1k <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                    Data.Cultivars.Final$`Weeks after planting`==j &
                                    Data.Cultivars.Final$Cultivar==k, ]
      d1k$Treatment <- factor(d1k$Treatment)
      kw <- with(d1k,kruskal(Disease.Severity,Treatment))
      gr <- kw$groups
      lt <- gr$groups[order(row.names(gr))]
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, levels(d1k$Treatment))
      L <- c(L, lt)
      M <- c(M, mean(d1k$Disease.Severity[d1k$Treatment == levels(d1k$Treatment)[1]]),
             mean(d1k$Disease.Severity[d1k$Treatment == levels(d1k$Treatment)[2]]),
             mean(d1k$Disease.Severity[d1k$Treatment == levels(d1k$Treatment)[3]]))
    }
  }
}

LettersDSk <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                         Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

#Disease Severity Kruskal Graph

ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Disease.Severity, fill=Treatment))+ylab("Disease Severity(%)")+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersDSk,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 5,
                          label = letra),
            position = position_dodge(0.9))+
  labs(caption = "Figure No X. Results of Kruskal Test in the variable of Disease Severity on potato (cm)") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))




#Stems
#Anova Stems
model<-aov(Stems~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final)
summary(model)
Data.Cultivars.Final$`Weeks after planting` <- factor(Data.Cultivars.Final$`Weeks after planting`)

#Ducant Test Stems
Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1 <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                   Data.Cultivars.Final$`Weeks after planting`==j &
                                   Data.Cultivars.Final$Cultivar==k, ]
      model <- aov(Stems~Treatment, data=d1)
      summary(model)
      DT <- duncan.test(model, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
      M <- c(M, DT$groups$Stems)
    } 
  }
}


LettersST <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                        Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

#Tukey Graph Stems

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1z <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                    Data.Cultivars.Final$`Weeks after planting`==j &
                                    Data.Cultivars.Final$Cultivar==k, ]
      model <- aov(Stems~Treatment, data=d1z)
      summary(model)
      DT <- HSD.test(model, "Treatment", console = TRUE)
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, row.names(DT$groups))
      L <- c(L, DT$groups$groups)
      M <- c(M, DT$groups$Stems)
    } 
  }
}


LettersSTz <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                         Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)




#Duncan Graph Stems 
ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Stems, fill=Treatment))+ylab("Number of Stems")+
  geom_boxplot()+ 
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersST,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 1,
                          label = letra),
            position = position_dodge(0.9))+
  labs(caption = "Figure No 10. Results of Duncan Test in the variable Number of Stems") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

#Tukey Graph Stems
ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Stems, fill=Treatment))+ylab("Number of Stems")+
  geom_boxplot()+ 
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersSTz,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 1,
                          label = letra),
            position = position_dodge(0.9))+
  labs(caption = "Figure No 11. Results of Tukey Test in the variable Number of Stems") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

#Residuals Stems
anova.st<-aov(Stems~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final) #anova 
anova.st.sqrt <- aov(sqrt(Stems)~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final) #square root 
shapiro.test(anova.st$residuals) #0.007876 anormal  
shapiro.test(anova.st.sqrt$residuals) #0.002167
par(mfrow = c(1, 2), oma = c(0, 0, 1.1, 0))
plot(anova.ds, main = "ANOVA", which = 2, ask = F,sub.caption = "")
plot(anova.ds.sqrt, main = "Square Root ANOVA", which = 2, ask = F,sub.caption = "")

#Stems Kruskal  

Y <- W <- Tr <- L <- C <- M <- numeric()

for (i in 2015:2016) { 
  for (k in levels(as.factor(Data.Cultivars.Final$Cultivar))) {
    for(j in levels(Data.Cultivars.Final$`Weeks after planting`)) {
      d1k <- Data.Cultivars.Final[Data.Cultivars.Final$Year==i & 
                                    Data.Cultivars.Final$`Weeks after planting`==j &
                                    Data.Cultivars.Final$Cultivar==k, ]
      d1k$Treatment <- factor(d1k$Treatment)
      kw <- with(d1k,kruskal(Stems,Treatment))
      gr <- kw$groups
      lt <- gr$groups[order(row.names(gr))]
      Y <- c(Y, i,i,i)
      W <- c(W, j,j,j)
      C <- c(C, k,k,k)
      Tr <- c(Tr, levels(d1k$Treatment))
      L <- c(L, lt)
      M <- c(M, mean(d1k$Stems[d1k$Treatment == levels(d1k$Treatment)[1]]),
             mean(d1k$Stems[d1k$Treatment == levels(d1k$Treatment)[2]]),
             mean(d1k$Stems[d1k$Treatment == levels(d1k$Treatment)[3]]))
    }
  }
}

LettersSTk <- data.frame(Year = Y, `Weeks after planting` = factor(W), 
                         Cultivar = C, Treatment = factor(Tr), letra = L, mea = M)

#Stems Kruskal Graph

ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Stems, fill=Treatment))+ylab("Stems (cm)")+
  geom_boxplot()+
  facet_grid(Cultivar~Year)+
  theme_light()+
  geom_text(data = LettersSTk,
            mapping = aes(x = Weeks.after.planting,
                          y = mea + 1,
                          label = letra),
            position = position_dodge(0.9))+
  labs(caption = "Figure No X. Results of Kruskal Test in the variable of Stems on potato (cm)") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))
