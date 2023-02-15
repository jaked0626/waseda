read.csv(data/Mac_and_Mos.csv)
df1 <- Mac_and_Mos
library(broom)
library(DT)
library(ggsignif)
library(rcompanion)
library(tidyverse)
df2 <- df1 %>%
  gather(key = "restaurant", value = "score", "MOS", "MAC") 

df3 <- df1 %>%
  gather(key = "restaurant", value = "score", "MAC", "MOS") 
df3

summary(df2)
df2
theme_set(theme_bw(base_size = 14,base_family = "HiraKakuPro-W3"))
df2 %>% 
  ggplot(aes(x = restaurant, y = score, fill = restaurant)) +
  geom_boxplot() +     
  scale_x_discrete(labels=c("MOS", "MAC")) +
  scale_fill_manual(values = c("orangered", "red4")) +
  labs(x = "Restaurant name", y = "Score") +
  geom_signif(comparisons = list(c("MOS", "MAC")),
              test = "t.test", na.rm = FALSE, 
              map_signif_level = TRUE, col = "blue",
              size = .75, textsize = 3.5) +
  theme(legend.position = "none")

install.packages("ggsignif")



df2 %>% 
  ggplot(aes(x = restaurant, y = score, fill = restaurant)) +
  geom_boxplot() +     
  scale_x_discrete(labels=c("MAC", "MOS")) +
  scale_fill_manual(values = c("orangered", "red4")) +
  labs(x = "Restaurant name", y = "Score") +
  geom_signif(comparisons = list(c("MAC", "MOS")),
              test = "t.test", na.rm = FALSE, 
              map_signif_level = TRUE, col = "blue",
              size = .75, textsize = 3.5) +
  theme(legend.position = "none")


df1_ttest <- t.test(df1$MOS, df1$MAC, paired = FALSE)
df1_ttest

summary(df1)






