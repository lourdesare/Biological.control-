# Analysis Data.Cultivars.Final

library(agricolae)

### DUCAN TEST

duncan(y, trt, DFerror, SSerror, alpha = 0.05, group = TRUE, main = NULL)

# Plant Height
model<-aov(Plant.Height~Treatment+`Weeks after planting`+Year, data=Data.Cultivars.Final)
summary(model)

Data.Cultivars.Final$`Weeks after planting` <- factor(Data.Cultivars.Final$`Weeks after planting`)

library(ggplot2)
ggplot(Data.Cultivars.Final, aes(x=`Weeks after planting`, y=Plant.Height, fill=Treatment))+
  geom_boxplot()+
  facet_grid(.~Year)+
  theme_light()






## ANOVA TEST



## NORMALITY OF RESIDUALS
