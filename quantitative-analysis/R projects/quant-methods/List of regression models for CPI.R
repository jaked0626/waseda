df <- read.csv("data/Corruption_practice.csv", na = ".")
names(df)
head(df)

with(df, plot(Term.Limit, CPI))

model_1 <- lm(CPI ~ Term.Limit, data=df)
summary(model_1)
model_2 <- lm(CPI ~ Term.Limit + History.Democracy + LogGDPpercap + British + Protestant + Federation, data = df)
summary(model_2)

model_3 <- lm(CPI ~ Term.Limit*Polity + LogGDPpercap + Protestant + History.Democracy, data = df)

summary(model_3)

model_4 <- lm(CPI ~ Term.Limit + Presidential, data = df)
summary(model_4)

model_4 <- lm(CPI ~ Term.Limit*Democracy + LogGDPpercap + Protestant + History.Democracy, data = df)
summary(model_4)

model_5 <- lm(CPI ~ Term.Limit*Polity, data = df)
summary(model_5)

model_6 <- lm(CPI ~ Term.Limit*Polity + History.Democracy + LogGDPpercap + British + Protestant + Federation, data = df)
summary(model_6)

model_7 <- lm(CPI ~ Term.Limit*Polity + History.Democracy + LogGDPpercap + Protestant, data = df)
summary(model_7)

model_8 <- lm(CPI ~ Term.Limit*Democracy + History.Democracy + LogGDPpercap + Protestant, data = df)
summary(model_8)

model_9 <- lm(CPI ~ Term.Limit*Presidential + History.Democracy + LogGDPpercap + Protestant, data = df)
summary(model_9)

df2 <- read.csv("data/Corruption_practice2.csv")
model_10 <- lm(CPI ~ Stability, data = df2)
summary(model_10)***

model_11 <- lm(CPI ~ Stability*Polity, data = df2)
summary(model_11)*

model_12 <- lm(CPI ~ Stability*Polity + History.Democracy + LogGDPpercap + Protestant, data = df2 )
summary(model_12)


