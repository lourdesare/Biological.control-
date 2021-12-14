#Yield duncan
library(agricolae)
model<-aov(Yield.Potato~Treatment,data=YieldOfPotato)
out <- duncan.test(model,"Treatment", 
                   main="Yield of potato")
plot(out,variation="IQR")
out
#graph yield
dp <- ggerrorplot(YieldOfPotato, x = "Cultivar", y = "Yield.Potato",
                    color = "Treatment", palette = "Paired", 
                    error.plot = "pointrange",
                    position = position_dodge(0.5),xlab = (""), ylab = ("Yield of potato (ton/ha)"))+
  facet_grid(.~Year)+
  theme_light()
dp

#Biomass duncan
library(agricolae)
model<-aov(Percentage.Biomass~Treatment,data=YieldOfPotato)
out <- duncan.test(model,"Treatment", 
                   main="Percentage of Biomass")
plot(out,variation="IQR")
out
#graph yield
dens <- ggerrorplot(YieldOfPotato, x = "Cultivar", y = "Percentage.Biomass",
            color = "Treatment", palette = "Paired", 
            error.plot = "pointrange",
            position = position_dodge(0.5),xlab = ("Cultivars"), ylab = ("Biomass (%)"))+
  facet_grid(.~Year)+
  theme_light()
dens
#join two graphs 
graphs <- plot_grid(
  dp + theme(legend.position="none"), dens + theme(legend.position="bottom"),
  labels = "AUTO", ncol = 1
)
graphs


