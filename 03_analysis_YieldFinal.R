#Yield duncan
library(agricolae)
model<-aov(Yield.Potato~Treatment,data=YieldOfPotato)
out <- duncan.test(model,"Treatment", 
                   main="Yield of potato")
plot(out,variation="IQR")
out
#Biomass duncan
library(agricolae)
model<-aov(Percentage.Biomass~Treatment,data=YieldOfPotato)
out <- duncan.test(model,"Treatment", 
                   main="Percentage of Biomass")
plot(out,variation="IQR")
out