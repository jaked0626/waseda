3 + 4
6*7
mean(height) #calculating mean of height

## practice-ch04.R
##
## Purpose: Rによる計量政治学，第四章
##          Rスクリプトの書き方を説明する
##
## Created: 2020-06-02
## Modified: 2020-06-02

library("reader")
library("ggplot2")

download.file(url = "https://git.io/fAnIr", destfile = "data/height.csv")
myd <- readr::read_csv("data/height.csv")
names(myd)
head(myd)
library("dplyr")
glimpse(myd)
view(myd)
HR <- readr::read_csv("data/hr96-17.csv", na = ".")


glimpse(HR)

HR <- mutate(HR, expm = exp / 10^6)
glimpse(HR)

