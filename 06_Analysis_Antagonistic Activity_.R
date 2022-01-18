**3) *In vitro* antifungal activity of *Bacillus strains* against the phytopathogenic fungus *R. solani*, isolated from infected potato tubers**
  
  **3.1 Growth of the diameter of the colonies of *R. solani* under the influence of *B. velezensis* after 3, 5 and 7 days**
  
  Clear differences were observed over time between the colonial growth of *R. solani* under the effect of different treatments with *B. velezensis* and the control (Figure 25). All treatments showed lower initial colonial growth (day 3) compared to the control, also they showed a progressive growth in colony diameter at day 5 and 7. Treatment BZR 517 10e+06 is the only one that shows a decrease in colonial growth at day 7 compared to day 5.

```{r include = FALSE}

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


```{r Figure 25. Progression of colonial growth of R. solani under the antagonistic effect of two B. velezensis strains in vitro}
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
  labs(caption = "Figure 25. Progression of colonial growth of R. solani under the antagonistic effect of two B. velezensis strains in vitro",
       x = "Days",
       y = "Colony diameter (cm)",
       color = "Treatment") +theme_light()+ guides(col=guide_legend("Treatments")) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

```

**3.2 Inhibition of the growth of *R. solani* under the influence of different *B. velezensis* treatments after 3, 5 and 7 days *in vitro* **
  
  The studied *B. velezensis* strains BZR 336 g and BZR 517 inhibited the growth of *R. solani* under laboratory conditions. The inhibition ratio depended on the concentration of *B. velezensis* in the nutrient medium, and it showed a decrease over time.

For determining the treatments with the best performance at day 3, 5 and 7, multiple comparison of means was performed by using Duncan's non-parametric test, according with authors' method. In addition, Tukey's parametric test and Kruskal-Wallis non-parametric test were performed to know the different groupings among analyzes. 

The Shapiro-Wilk normality test was performed for ANOVA, the p values at 3, 5 and 7 days were less than alpha=0.05, which means that the null hypothesis that the data are normally distributed is rejected (Figure 29). Since the data does not follow a normal distribution, the most appropriate is to perform non-parametric tests, which does not assume normality. This is consistent with the authors' decision to analyze the data with Duncan's test, and also strengthens our decision to perform another non-parametric test, Kruskal-Wallis, which is considered more robust. Even if data is not normal, ANOVA test should be still performed.

In an overall perspective, only few differences were found between Duncan, Tukey and Kruskal-Wallis groupings for days 3, 5 and 7. On day 3, data grouping showed differences between each test, however all tests agreed that treatment BZR 336 g at its higher concentration 10^6 CFU/mL had the best performance with FCD of 1.36 cm. 

On day 5 the data clustered very similarly across tests, showing only very few differences. All tests agreed that the treatment with the high concentration (10^6 CFU/mL) of BZR 336 g and medium concentrations (10^5 CFU/mL) of both strains (BZR 336 g and BZR 517) had the best performance with FCD of 1.62 and 1.72 cm, respectively. Also Duncan and Tukey agreed that the 

and by day 7, the clusters were the same across trials.

At day 3, the control showed a fungal colony diameter (FCD) of 4.24 cm, and the treatment with the best performance, according with the tests groupings was BZR 336 g at its higher concentration 10^6 CFU/mL with FCD of 1.36 cm.    

Little differences were observed between Duncan, Tukey and Kruskal-Wallis analyzes groupings for *R. solani* colony growth at 3 and 5 days. However, there were no differences between both analyzes at 7 days (Figures 20 and 21). The treatments with the best performance were both *B. velezensis* strains: BZR 517 and BZR 336g, in their medium and high concentrations: 10^5 CFU/mL and 10^6 CFU/mL, generating the lower colony growth after 7 days.

```{r include = FALSE}
#### 1.2 Statistical Analysis

### 1.2.1 Duncan, ANOVA and Kruskal-Wallis test

### Multiple comparisons of the means were performed by using Duncan, ANOVA and
### Kruskal-Wallis test with a significance level of p = 0.05.

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

## Kruskal-Wallis analysis for Fungal colony diameter at 3 d

library(agricolae)

kw.3d<-with(AA,kruskal(Colony.Diameter,Trat,group=TRUE, main=NULL, console = TRUE))

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

## Kruskal-Wallis analysis for Fungal colony diameter at 5 d

library(agricolae)

kw.5d<-with(AA,kruskal(Colony.Diameter,Trat,group=TRUE, main=NULL, console = TRUE))

## Duncan analysis for Fungal colony diameter at 7 d
Dia <- 7

AntagonisticActivity$Days <- factor(AntagonisticActivity$Days)
Y1 <- AntagonisticActivity$Days==Dia
AA <- AntagonisticActivity[Y1,]

model <- aov(Colony.Diameter~Trat,data=AA)
out <- duncan.test(model,"Trat",main = "Colony diameter")
out

## Tukey analysis for Fungal colony diameter at 7 d

out.hsd <- HSD.test(model,"Trat",console = TRUE)

## Kruskal-Wallis analysis for Fungal colony diameter at 7 d

library(agricolae)

kw.7d<-with(AA,kruskal(Colony.Diameter,Trat,group=TRUE, main=NULL, console = TRUE))
```

```{r Figure 26. Duncan Test for Antagonistic activity of two strains of B. velezensis against R. solani in vitro}
## Combine Duncan graphs for 3, 5 and 7 d

#library(ggplot2)
#library(ggpubr)
#theme_set(theme_pubr())

DunLetters <- data.frame(Days=c(rep(3,7),rep(5,7),rep(7,7)),
                         Trat=c("Bacillus velezensis strain BZR 336 g 10000","Bacillus velezensis strain BZR 336 g 1e+05",
                                "Bacillus velezensis strain BZR 336 g 1e+06","Bacillus velezensis strain BZR 517 10000",
                                "Bacillus velezensis strain BZR 517 1e+05","Bacillus velezensis strain BZR 517 1e+06",
                                "Control 1e+06","Bacillus velezensis strain BZR 336 g 10000","Bacillus velezensis strain BZR 336 g 1e+05",
                                "Bacillus velezensis strain BZR 336 g 1e+06","Bacillus velezensis strain BZR 517 10000",
                                "Bacillus velezensis strain BZR 517 1e+05","Bacillus velezensis strain BZR 517 1e+06",
                                "Control 1e+06","Bacillus velezensis strain BZR 336 g 10000","Bacillus velezensis strain BZR 336 g 1e+05",
                                "Bacillus velezensis strain BZR 336 g 1e+06","Bacillus velezensis strain BZR 517 10000",
                                "Bacillus velezensis strain BZR 517 1e+05","Bacillus velezensis strain BZR 517 1e+06",
                                "Control 1e+06"),
                         Mean=plot.means$mean,
                         SE=plot.means$se,
                         label=c("bc","bcd","d","b","cd","bc","a","c","c","c","b","c","c","a","b","c","c","b","c","c","a"))

DunBox <- ggplot(AntagonisticActivity, aes(x=`Trat`, y=Colony.Diameter, fill=Trat))+xlab("Treatments")+ylab("Fungal colony diameter (cm)")+
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot()+
  theme_light()+
  geom_text(data = DunLetters,
            mapping = aes(x = Trat,
                          y = Mean+1,
                          label=label),
            position = position_dodge(0.9))+
  facet_grid(.~Days)+
  labs(caption = "Figure 26. Duncan Test for Antagonistic activity of two strains of B. velezensis against R. solani in vitro") +
  scale_x_discrete(("Treatments"),limitedLabels("Treatments"))+
  scale_fill_discrete(("Treatments"))+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

DunBox
```


```{r Figure 27. Tukey Test for Antagonistic activity of two strains of B. velezensis against R. solani in vitro}
## Tukey graphs for 3, 5 and 7 d

#library(ggplot2)
#library(ggpubr)
#theme_set(theme_pubr())

TukLetters <- data.frame(Days=c(rep(3,7),rep(5,7),rep(7,7)),
                         Trat=c("Bacillus velezensis strain BZR 336 g 10000","Bacillus velezensis strain BZR 336 g 1e+05",
                                "Bacillus velezensis strain BZR 336 g 1e+06","Bacillus velezensis strain BZR 517 10000",
                                "Bacillus velezensis strain BZR 517 1e+05","Bacillus velezensis strain BZR 517 1e+06",
                                "Control 1e+06","Bacillus velezensis strain BZR 336 g 10000","Bacillus velezensis strain BZR 336 g 1e+05",
                                "Bacillus velezensis strain BZR 336 g 1e+06","Bacillus velezensis strain BZR 517 10000",
                                "Bacillus velezensis strain BZR 517 1e+05","Bacillus velezensis strain BZR 517 1e+06",
                                "Control 1e+06","Bacillus velezensis strain BZR 336 g 10000","Bacillus velezensis strain BZR 336 g 1e+05",
                                "Bacillus velezensis strain BZR 336 g 1e+06","Bacillus velezensis strain BZR 517 10000",
                                "Bacillus velezensis strain BZR 517 1e+05","Bacillus velezensis strain BZR 517 1e+06",
                                "Control 1e+06"),
                         Mean=plot.means$mean,
                         SE=plot.means$se,
                         label=c("b","bc","c","b","bc","bc","a","bc","c","c","b","c","c","a","b","c","c","b","c","c","a"))

TukBox <- ggplot(AntagonisticActivity, aes(x=`Trat`, y=Colony.Diameter, fill=Trat))+xlab("Treatments")+ylab("Fungal colony diameter (cm)")+
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot()+
  theme_light()+
  geom_text(data = TukLetters,
            mapping = aes(x = Trat,
                          y = Mean+1,
                          label=label),
            position = position_dodge(0.9))+
  facet_grid(.~Days)+
  labs(caption = "Figure 27. Tukey Test for Antagonistic activity of two strains of B. velezensis against R. solani in vitro") +
  scale_x_discrete(("Treatments"),limitedLabels("Treatments"))+
  scale_fill_discrete(("Treatments"))+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

TukBox
```


```{r Figure 28. Kruskal Test for Antagonistic activity of two strains of B. velezensis against R. solani in vitro}
## Kruskal-Wallis graphs for 3, 5 and 7 d

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

KWLetters <- data.frame(Days=c(rep(3,7),rep(5,7),rep(7,7)),
                         Trat=c("Bacillus velezensis strain BZR 336 g 10000","Bacillus velezensis strain BZR 336 g 1e+05",
                                "Bacillus velezensis strain BZR 336 g 1e+06","Bacillus velezensis strain BZR 517 10000",
                                "Bacillus velezensis strain BZR 517 1e+05","Bacillus velezensis strain BZR 517 1e+06",
                                "Control 1e+06","Bacillus velezensis strain BZR 336 g 10000","Bacillus velezensis strain BZR 336 g 1e+05",
                                "Bacillus velezensis strain BZR 336 g 1e+06","Bacillus velezensis strain BZR 517 10000",
                                "Bacillus velezensis strain BZR 517 1e+05","Bacillus velezensis strain BZR 517 1e+06",
                                "Control 1e+06","Bacillus velezensis strain BZR 336 g 10000","Bacillus velezensis strain BZR 336 g 1e+05",
                                "Bacillus velezensis strain BZR 336 g 1e+06","Bacillus velezensis strain BZR 517 10000",
                                "Bacillus velezensis strain BZR 517 1e+05","Bacillus velezensis strain BZR 517 1e+06",
                                "Control 1e+06"),
                         Mean=plot.means$mean,
                         SE=plot.means$se,
                         label=c("b","c","d","b","c","bc","a","b","c","c","b","c","b","a","b","c","c","b","c","c","a"))

KWBox <- ggplot(AntagonisticActivity, aes(x=`Trat`, y=Colony.Diameter, fill=Trat))+xlab("Treatments")+ylab("Fungal colony diameter (cm)")+
  stat_boxplot(geom = "errorbar",
               width = 0.2) +
  geom_boxplot()+
  theme_light()+
  geom_text(data = KWLetters,
            mapping = aes(x = Trat,
                          y = Mean+1,
                          label=label),
            position = position_dodge(0.9))+
  facet_grid(.~Days)+
  labs(caption = "Figure 28. Kruskal Test for Antagonistic activity of two strains of B. velezensis against R. solani in vitro") +
  scale_x_discrete(("Treatments"),limitedLabels("Treatments"))+
  scale_fill_discrete(("Treatments"))+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

KWBox
```


```{r}
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
```


```{r Figure 29. Normality of Residuals for Antagonistic activity}
#Graph 
par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))
p1 <- plot(anova.3d, main = "ANOVA at 3 days", which = 2, ask = F,sub.caption = "") 
p2 <- plot(anova.5d, main = "ANOVA at 5 days", which = 2, ask = F,sub.caption = "") 
p3 <- plot(anova.7d, main = "ANOVA at 7 days", which = 2, ask = F,sub.caption = "") 
```