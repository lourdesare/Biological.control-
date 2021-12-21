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
