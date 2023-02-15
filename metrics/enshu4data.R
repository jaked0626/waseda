# 演習４：データセット構築

library(tidyverse) 
library(dplyr)
sink("enshu4data_result.txt", split=T)

# WDIデータを取り込む

## 2000-2020年のGDPなどのデータ
library(readxl)
data1 <- read_excel("data_worldbank2.xlsx")

## 元の変数名が長いので短くする
## 補完機能を使えば、元の変数名もすぐ出てくる
data12 <- data1 %>%
  rename(year = Time) %>%
  rename(cty = `Country Name`) %>%
  rename(ctycode = `Country Code`) %>%
  rename(GDPpc = `GDP per capita, PPP (constant 2017 international $) [NY.GDP.PCAP.PP.KD]`) %>%
  rename(invrate = `Gross capital formation (% of GDP) [NE.GDI.TOTL.ZS]`) %>%
  rename(pop = `Population, total [SP.POP.TOTL]`) %>%
  rename(educ = `School enrollment, tertiary (% gross) [SE.TER.ENRR]`) %>%
  rename(export = `Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS]`) %>%
  rename(FDI = `Foreign direct investment, net inflows (% of GDP) [BX.KLT.DINV.WD.GD.ZS]`) %>%
  select(-`Time Code`) %>% # Time Codeは不要なので削除
  filter(year <= 2019) # 2020年のデータはほとんどmissingなので削除

## 2000-19年の各年のデータから、その期間の平均値や成長率などの
## データに変換

## まず、最初に国名で、その後各国ごとに年でソート
arrange(data12, cty, year) 
  
data13 <- data12 %>%  
  group_by(cty) %>% # 国ごとにグループ化
  summarise(avg_invrate = mean(invrate, na.rm = TRUE), 
            ## na.rm = TRUEにしないと１年でも欠損値があると平均も欠損してしまう
            avg_educ = mean(educ, na.rm = TRUE), 
            avg_export = mean(export, na.rm = TRUE), 
            avg_FDI = mean(FDI, na.rm = TRUE), 
            first_GDPpc = first(GDPpc), ## 各グループの最初の観測
            first_pop = first(pop), 
            last_GDPpc = last(GDPpc), ## 各グループの最後の観測
            last_pop = last(pop), 
            ctycode = first(ctycode)) %>% ## これがないとctycodeが消えてしまう
  mutate(growth_GDPpc = ((last_GDPpc/first_GDPpc)^(1/20)-1)*100) %>%
  mutate(growth_pop = ((last_pop/first_pop)^(1/20)-1)*100) 

summary(data13)

## avg_invrate: 期間平均投資率
## avg_educ: 期間平均高等教育就学率
## avg_export: 期間平均輸出額対GDP比（％）
## avg_FDI: 期間平均対内直接投資対GDP比（％）
## growth_GDPpc: 期間平均年率1人あたりGDP成長率（％）
## growth_pop: 期間平均年率人口成長率（％）
## first_GDPpc: 期初の1人当たりGDP

# Polity Vのデータを取り込む

## polity2: 民主的・独裁的の度合いの指標

data2 <- read_excel("polity5.xlsx")

data21 <- data2 %>%
  select(year, country, polity2) %>%
  filter(year >= 2000 & year <= 2019) 

## 後でWDIのデータと結合する時に国名を利用する。
## しかし、WDIとPolityでは国名が違う国があるので、
## それを確認して統一。
## このようにデータセットによって国名表記、国コードが違うことも
## 往々にしてあるので注意すること。

table(data21$country)

data22 <- data21 %>%
  mutate(country2 = case_when( 
    country == "Bosnia" ~ "Bosnia and Herzegovina", 
    country == "Congo-Brazzaville" ~ "Congo, Rep.",
    country == "Congo Brazzaville" ~ "Congo, Rep.",
    country == "Congo Kinshasa" ~ "Congo, Dem. Rep.",
    country == "Cote D'Ivoire" ~ "Ivory Coast",
    country == "Ivory Coast" ~ "Ivory Coast",
    country == "Korea South" ~ "Korea, Rep.",
    country == "Korea North" ~ "Korea, Dem. People’s Rep.", 
    country == "Myanmar (Burma)" ~ "Myanmar",
    country == "UAE" ~ "United Arab Emirates",
    country == "Venezuela" ~ "Venezuela, RB",
    TRUE ~ country)) %>% ## その他はcountryと同じに
  group_by(country2) %>%
  summarise(avg_polity2 = mean(polity2)) # 期間平均

# WDIデータとPolityデータを結合 merge

## WDIのctyとPolityのcountry2が両方に共通するキー変数

data3 <- data13 %>%
  inner_join(data22, by = c("cty" = "country2"))

data3

## country2という変数はなくなっている。これを残したいなら
data4 <- data13 %>%
  inner_join(data22, by = c("cty" = "country2"), keep = TRUE)

data4

### 演習３のためにcsvファイルとしてエクスポート
write.csv(data4, file = "world_data3.csv")

## inner_joinでは、両方のデータにキー変数が存在する観測のみを残す。
## 例では、WDIとPolityの両方にある国だけ残す。
## どちらかのデータにある観測をすべて残したい場合にはfull_join

data5 <- data13 %>%
  full_join(data22, by = c("cty" = "country2"))

data5

## data3とdata5の観測数の違いを確認しよう。
summary(data3)
summary(data5)

## 新古典派成長論の実証

reg1 <- lm(data = data3, formula = growth_GDPpc ~ first_GDPpc + avg_invrate + growth_pop)
summary(reg1)

## 若干変数を付け加える
reg2 <- lm(data = data3, 
           formula = growth_GDPpc ~ first_GDPpc + avg_invrate + growth_pop 
           + avg_educ + avg_export + avg_FDI + avg_polity2)
summary(reg2)


## 宿題４

## インターネットから国レベルのデータセットを自由に選んで
## ２つダウンロードし、それらを結合した上で、
## 何らかの回帰分析をせよ。
## RStudioのconsoleペインの結果をコピペしたtxtファイルと、
## データセットの出所、変数の定義、記述統計の表、回帰分析の結果の表、
## および回帰分析の結果の解釈を論文の形式でまとめたMS WordもしくはPDFファイルの
## 2つのファイルを提出せよ。
## 論文形式のものは表を含んでA4で2枚以上（ただし、あまり長いものは必要ない）。



