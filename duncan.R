d1 <- Data.Cultivars.Final[Data.Cultivars.Final$Year==year & Data.Cultivars.Final$`Weeks after planting`==week, ]

model <- aov(Plant.Height~Treatment, data=d1)
summary(model)
DT <- duncan.test(model, "Treatment", console = TRUE)
