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

## Graph the influence of treatments on the fungal diameter growth after 3, 5 
## and 7 days.

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
  labs(title = "Graphic 1. The influence of B. velezensis strains on the growth of R. solani in vitro",
       subtitle = "(Colony diameter)",
       x = "Days",
       y = "Colony diameter",
       color = "Trat") +
  theme_light()

#### 1.2 Statistical Analysis

### 1.2.1 Duncan and ANOVA test

###  Multiple comparisons of the means were performed by the authors using 
### Duncan test with a significance level of P = 0.05.

# Duncan analysis for Fungal colony diameter at 3 d
library ("agricolae")
Dia <- 3

AntagonisticActivity$Days <- factor(AntagonisticActivity$Days)
Y1 <- AntagonisticActivity$Days==Dia
AA <- AntagonisticActivity[Y1,]

model <- aov(Colony.Diameter~Trat,data=AA)
out <- duncan.test(model,"Trat",main = "Colony diameter")
out

plot(out,variation="IQR")

# Duncan analysis for Fungal colony diameter at 3 d

out.hsd <- HSD.test(model,"Trat",console = TRUE)

# Duncan analysis for Fungal colony diameter at 5 d
Dia <- 5

AntagonisticActivity$Days <- factor(AntagonisticActivity$Days)
Y1 <- AntagonisticActivity$Days==Dia
AA <- AntagonisticActivity[Y1,]

model <- aov(Colony.Diameter~Trat,data=AA)
out <- duncan.test(model,"Trat",main = "Colony diameter")
out

plot(out,variation="IQR")

# Duncan analysis for Fungal colony diameter at 5 d

out.hsd <- HSD.test(model,"Trat",console = TRUE)

# Duncan analysis for Fungal colony diameter at 7 d
Dia <- 7

AntagonisticActivity$Days <- factor(AntagonisticActivity$Days)
Y1 <- AntagonisticActivity$Days==Dia
AA <- AntagonisticActivity[Y1,]

model <- aov(Colony.Diameter~Trat,data=AA)
out <- duncan.test(model,"Trat",main = "Colony diameter")
out

plot(out,variation="IQR")

# Duncan analysis for Fungal colony diameter at 7 d

out.hsd <- HSD.test(model,"Trat",console = TRUE)

############################################################################
