# Analysis Data.Cultivars.Final

library(agricolae)

### DUCAN TEST

# Plant Height
model<-aov(Plant.Height~Treatment,data=Data.Cultivars.Final)
out <- duncan.test(model,"Treatment", 
                   main="Plant Height")
plot(out,variation="IQR")
out

# Disease Incidence
model<-aov(Disease.Incidence~Treatment,data=Data.Cultivars.Final)
out <- duncan.test(model,"Treatment", 
                   main="Disease Incidence")
plot(out,variation="IQR")
out

# Disease Severity
model<-aov(Disease.Severity~Treatment,data=Data.Cultivars.Final)
out <- duncan.test(model,"Treatment", 
                   main="Disease Severity")
plot(out,variation="IQR")
out

# Stems
model<-aov(Stems~Treatment,data=Data.Cultivars.Final)
out <- duncan.test(model,"Treatment", 
                   main="Stems")
plot(out,variation="IQR")
out
