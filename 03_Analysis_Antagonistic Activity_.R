##### 1. ANTAGONISTIC ACTIVITY

#### Antagonistic activity was evaluated by measuring the diameter of the fungal 
#### colonies after 3, 5, and 7 days. The fungal inhibition (%) was determined 
#### based on the data obtained. 

#### All results were expressed and analyzed as the mean.

#### 1.1 Mean and Standard error for Fungal diameter for each treatment.

## Creating a new plot with means and standard errors

#install.packages("dplyr")

library("dplyr")

plot.means <- AntagonisticActivity %>%
  group_by(Days,Trat) %>%
  summarize(n = n(),
            mean = mean(Colony.Diameter),
            sd = sd(Colony.Diameter),
            se = sd / sqrt(n))

```


```{r}
##Graph the influence of treatments on the fungal diameter growth after 3, 5 and 7 days.

#install.packages("ggplot2")

library(ggplot2)

ggplot(plot.means, aes(x = Days,
                       y = mean,
                       group=Trat,
                       color=Trat)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin=mean-se,
                    ymax=mean+se),
                width = .1)+
  labs(caption = "Figure No. 19. The influence of B. velezensis strains on the growth of R. solani in vitro (colony growth)",
       x = "Days",
       y = "Colony diameter",
       color = "Treatment") +theme_light()+ guides(col=guide_legend("Treatments")) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

```

**3.2 Growth of the diameter of the colonies of *R. solani* under the influence of *B. velenzis* after 3, 5 and 7 days**
  
  Little differences were observed between Duncan and Tukey analyzes for *R. solani* colony growth at 3 and 5 days. However, there were no differences between both analyzes at 7 days (Figures 20 and 21). The treatments with the best performance were both *B. velezensis* strains: BZR 517 and BZR 336g, in their medium and high concentrations: 10^5 CFU/mL and 10^6 CFU/mL, generating the lower colony growth after 7 days.

The Shapiro-Wilk normality test was performed for ANOVA, the p values at 3, 5 and 7 days were less than alpha=0.05, wich means that the data do not follow a normal distribution (Figure 22).


```{r include = FALSE}
#### 1.2 Statistical Analysis

### 1.2.1 Duncan and ANOVA test

### Multiple comparisons of the means were performed by the authors using 
### Duncan test with a significance level of P = 0.05.

## Duncan analysis for Fungal colony diameter at 3 d
library ("agricolae")
Dia <- 3

AntagonisticActivity$Days <- factor(AntagonisticActivity$Days)
Y1 <- AntagonisticActivity$Days==Dia
AA <- AntagonisticActivity[Y1,]

model <- aov(Colony.Diameter~Trat,data=AA)
out <- duncan.test(model,"Trat",main = "Colony diameter")
out

#plot(out,variation="IQR")

## Tukey analysis for Fungal colony diameter at 3 d

out.hsd <- HSD.test(model,"Trat",console = TRUE)

## Duncan analysis for Fungal colony diameter at 5 d
Dia <- 5

AntagonisticActivity$Days <- factor(AntagonisticActivity$Days)
Y1 <- AntagonisticActivity$Days==Dia
AA <- AntagonisticActivity[Y1,]

model <- aov(Colony.Diameter~Trat,data=AA)
out <- duncan.test(model,"Trat",main = "Colony diameter")
out

#plot(out,variation="IQR")

## Tukey analysis for Fungal colony diameter at 5 d

out.hsd <- HSD.test(model,"Trat",console = TRUE)

## Duncan analysis for Fungal colony diameter at 7 d
Dia <- 7

AntagonisticActivity$Days <- factor(AntagonisticActivity$Days)
Y1 <- AntagonisticActivity$Days==Dia
AA <- AntagonisticActivity[Y1,]

model <- aov(Colony.Diameter~Trat,data=AA)
out <- duncan.test(model,"Trat",main = "Colony diameter")
out

#plot(out,variation="IQR")


## Tukey analysis for Fungal colony diameter at 7 d

out.hsd <- HSD.test(model,"Trat",console = TRUE)
```

```{r}
## Combine Duncan graphs for 3, 5 and 7 d

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

DunCD <- ggerrorplot(AntagonisticActivity, x = "Trat", y = "Colony.Diameter",
                     color = "Trat", palette = "Paired", 
                     error.plot = "pointrange",
                     position = position_dodge(0.5),xlab = ("Days"),ylab = ("Fungal colony diameter(cm)"))+
  facet_grid(.~Days)+
  labs(caption = "Figure No. 20. Duncan Test for Antagonistic activity")+
  theme_light()+ guides(col=guide_legend("Treatments"))
#DunCD

dl <- data.frame(
  l=c("bc","bcd","d","b","cd","bc","a","c","c","c","b","c","c","a","b","c","c","b","c","c","a"),
  Days=c(3,3,3,3,3,3,3,5,5,5,5,5,5,5,7,7,7,7,7,7,7),
  xd=c(1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7),
  yd=c(2.1,1.9,1.6,2.1,1.7,1.9,4.5,2.5,1.9,1.8,2.9,1.9,2,8.1,2.8,2,1.9,2.9,2,1.8,9.2)
)

DunCD1 <- DunCD+geom_text(data = dl,mapping = aes(x = xd, y = yd, label = l)
)
#DunCD1

DunCD2 = DunCD1+scale_x_discrete(("Treatments"),limitedLabels("Treatments"))
DunCD2

## Combine Tukey graphs for 3, 5 and 7 d

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

TukCD <- ggerrorplot(AntagonisticActivity, x = "Trat", y = "Colony.Diameter",
                     color = "Trat", palette = "Paired", 
                     error.plot = "pointrange",
                     position = position_dodge(0.5),xlab = ("Days"),ylab = ("Fungal colony diameter (cm)"))+
  facet_grid(.~Days)+
  labs(caption = "Figure No. 21. Tukey Test for Antagonistic activity")+
  theme_light()+ guides(col=guide_legend("Treatments"))
#TukCD

dlt <- data.frame(
  lt=c("b","bc","c","b","bc","bc","a","bc","c","c","b","c","c","a","b","c","c","b","c","c","a"),
  Days=c(3,3,3,3,3,3,3,5,5,5,5,5,5,5,7,7,7,7,7,7,7),
  xt=c(1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7),
  yt=c(2.1,1.9,1.6,2.1,1.7,1.9,4.5,2.5,1.9,1.8,2.9,1.9,2,8.1,2.8,2,1.9,2.9,2,1.8,9.2)
)

TukCD1 <- TukCD+geom_text(data = dlt,mapping = aes(x = xt, y = yt, label = lt)
)
#TukCD1

TukCD2 = TukCD1+scale_x_discrete(("Treatments"),limitedLabels("Treatments"))
TukCD2

## Residuals for Antagonistic activity

##Residuals for day 3
library ("agricolae")
Dia <- 3

AntagonisticActivity$Days <- factor(AntagonisticActivity$Days)
Y1 <- AntagonisticActivity$Days==Dia
AA <- AntagonisticActivity[Y1,]

anova.3d <- aov(Colony.Diameter~Trat,data=AA) #anova 
anova.3d.sqrt <- aov(sqrt(Colony.Diameter)~Trat,data=AA) #square root 
shapiro.test(anova.3d$residuals) #0.01891
shapiro.test(anova.3d.sqrt$residuals) #0.3884

##Residuals for day 5
library ("agricolae")
Dia <- 5

AntagonisticActivity$Days <- factor(AntagonisticActivity$Days)
Y1 <- AntagonisticActivity$Days==Dia
AA <- AntagonisticActivity[Y1,]

anova.5d <- aov(Colony.Diameter~Trat,data=AA) #anova 
anova.5d.sqrt <- aov(sqrt(Colony.Diameter)~Trat,data=AA) #square root 
shapiro.test(anova.5d$residuals) #1.646 e-06
shapiro.test(anova.5d.sqrt$residuals) #1.099 e-05

##Residuals for day 7
library ("agricolae")
Dia <- 7

AntagonisticActivity$Days <- factor(AntagonisticActivity$Days)
Y1 <- AntagonisticActivity$Days==Dia
AA <- AntagonisticActivity[Y1,]

anova.7d <- aov(Colony.Diameter~Trat,data=AA) #anova 
anova.7d.sqrt <- aov(sqrt(Colony.Diameter)~Trat,data=AA) #square root 
shapiro.test(anova.7d$residuals) #3.084 e-06
shapiro.test(anova.7d.sqrt$residuals) #2.288 e-05

#Graph 
par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))
p1 <- plot(anova.3d, main = "ANOVA at 3 days", which = 2, ask = F,sub.caption = "") 
p2 <- plot(anova.5d, main = "ANOVA at 5 days", which = 2, ask = F,sub.caption = "") 
p3 <- plot(anova.7d, main = "ANOVA at 7 days", which = 2, ask = F,sub.caption = "") 
```
