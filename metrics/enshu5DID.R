library(tidyverse) 
library(dplyr)
library(readxl)

sink("enshu5DID_result.txt", split=T)

# WDIデータを取り込む

## 1971-2020年のGDPなどのデータ
data1 <- read_csv("data_worldbank3.csv")
data1

## 元の変数名が長いので短くする
## 補完機能を使えば、元の変数名もすぐ出てくる
data2 <- data1 %>%
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
  filter(year <= 2019 & year >= 1990) # この期間しかPPPのGDPがない

## 1990-2019年の各年のデータを10年１期のデータに変換し、
## 各期間の平均値や成長率などを計算

## まず、最初に国名で、その後各国ごとに年でソート
data2 <- data2 %>%
  mutate(period = ifelse((year >= 1990 & year <= 1999), 1,
                  ifelse((year >= 2000 & year <= 2009), 2, 3))) %>%
  arrange(cty, year) 

data3 <- data2 %>%  
  group_by(cty, period) %>% # 国×期間ごとにグループ化
  summarise(avg_invrate = mean(invrate, na.rm = TRUE), 
            avg_educ = mean(educ, na.rm = TRUE), 
            avg_export = mean(export, na.rm = TRUE), 
            avg_FDI = mean(FDI, na.rm = TRUE), 
            first_GDPpc = first(GDPpc), ## 各グループの最初の観測
            first_pop = first(pop), 
            last_GDPpc = last(GDPpc), ## 各グループの最後の観測
            last_pop = last(pop), 
            ctycode = first(ctycode)) %>% ## これがないとctycodeが消えてしまう
  mutate(growth_GDPpc = ((last_GDPpc/first_GDPpc)^(1/10)-1)*100) %>%
  mutate(growth_pop = ((last_pop/first_pop)^(1/10)-1)*100) 

summary(data3)

## avg_invrate: 期間平均投資率
## avg_educ: 期間平均高等教育就学率
## avg_export: 期間平均輸出額対GDP比（％）
## avg_FDI: 期間平均対内直接投資対GDP比（％）
## growth_GDPpc: 期間平均年率1人あたりGDP成長率（％）
## growth_pop: 期間平均年率人口成長率（％）
## first_GDPpc: 期初の1人当たりGDP

# Balanced dataを構築
#install.packages("plm")
library(plm)

data3 <- data3 %>%
  drop_na() %>%
  make.pbalanced(balance.type = "shared.individuals", index = c("cty", "period")) 
  # 毎年データがない国は削除される

summary(data3)

data3 <- data3 %>%
  mutate(logfirst_GDPpc = log(first_GDPpc))

# 新古典派成長論の実証

## Pooled OLS
reg1 <- lm(data = data3, 
           formula = growth_GDPpc ~ logfirst_GDPpc + avg_invrate + growth_pop + factor(period))
### as.factor(x)またはfactor(x)でxのカテゴリー別に全てのダミー変数を説明変数として入れる

summary(reg1)

## 若干変数を付け加える
reg2 <- lm(data = data3, 
           formula = growth_GDPpc ~ logfirst_GDPpc + avg_invrate + growth_pop 
           + avg_educ+ factor(period))
summary(reg2)

reg3 <- lm(data = data3, 
           formula = growth_GDPpc ~ logfirst_GDPpc + avg_invrate + growth_pop 
           + avg_educ + avg_export + avg_FDI+ factor(period))
summary(reg3)

## 固定効果モデル
## Within transformation

data3 <-  pdata.frame(data3, index= c("cty", "period")) #plm用にクロスセクション変数、時間変数を指定
### これはなくても、plmのそれぞれでindex= c("cty", "period")をオプションとして加えることでも可能

reg4 <- plm(data = data3, 
            formula = growth_GDPpc ~ logfirst_GDPpc + avg_invrate + growth_pop + factor(period), 
            # 固定効果モデルでは必ず時間ダミーを入れる
            index = c("cty", "period"), model = "within")
summary(reg4)

reg5 <- plm(data = data3, 
            formula = growth_GDPpc ~ logfirst_GDPpc + avg_invrate + growth_pop + avg_educ + factor(period), 
            index = c("cty", "period"), model = "within")
summary(reg5)

reg6 <- plm(data = data3, formula = growth_GDPpc ~ logfirst_GDPpc + avg_invrate + growth_pop
            + avg_educ + avg_export + avg_FDI + factor(period), 
            index = c("cty", "period"), model = "within")
summary(reg6)

## ダミー変数を利用（LSDV）
reg7 <- lm(data = data3, 
           formula = growth_GDPpc ~ logfirst_GDPpc + avg_invrate + growth_pop 
           + avg_educ + factor(cty) + factor(period))

summary(reg7)

## 結果を比較
library(stargazer)
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7,
          df = FALSE, 
          keep.stat = c("n", "Rsq", "adj.Rsq"),
          type = "text",
          dep.var.caption = "Dependent variable: Growth of GDP per capita",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          keep = c("logfirst_GDPpc", "avg_invrate", "growth_pop", 
                   "avg_educ", "avg_export", "avg_FDI"), 
          column.labels = c("OLS", "OLS", "OLS", "Within", "Within", "Within", "LSDV"))

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, 
          df = FALSE, 
          dep.var.caption = "Dependent variable: Growth of GDP per capita",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          keep.stat = c("n", "Rsq", "adj.Rsq"),
          keep = c("logfirst_GDPpc", "avg_invrate", "growth_pop", 
                   "avg_educ", "avg_export", "avg_FDI"), 
          column.labels = c("OLS", "OLS", "OLS", "Within", "Within", "Within", "LSDV"))

## Robust standard errorsの利用
## 固定効果分析の場合は、同じグループ（ここでは国）の誤差項同士の相関を考慮した
## cluster robust standard errorsを利用することが標準的

library(AER)

reg5r <- coeftest(reg5, vcov = vcovHC, type = "HC1", cluster = "group") 
  # plmの場合は変数名ではなく"group"と指定

reg7r <- coeftest(reg7, vcov = vcovHC, type = "HC1", cluster = "cty") 
  # lmの場合はclusterを指定してもcluster robustではなく単なるrobust SEになってしまう

stargazer(reg5, reg5r, reg7, reg7r,
          df = FALSE, 
          dep.var.caption = "Dependent variable: Growth of GDP per capita",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          keep.stat = c("n", "Rsq", "adj.Rsq"),
          type = "text",
          keep = c("logfirst_GDPpc", "avg_invrate", "growth_pop", "avg_educ"), 
          column.labels = c("Within", "Within robust", "LSDV", "LSDV robust"))

stargazer(reg5, reg5r, reg7, reg7r,
          df = FALSE, 
          dep.var.caption = "Dependent variable: Growth of GDP per capita",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          keep.stat = c("n", "Rsq", "adj.Rsq"),
          keep = c("logfirst_GDPpc", "avg_invrate", "growth_pop", "avg_educ"), 
          column.labels = c("Within", "Within robust", "LSDV", "LSDV robust"))

### 本来、reg5rとreg7rの結果は全く同じになるはずだが、SEが若干異なっている。
### これは前述のように、coeftestでのSEの計算が
### lmの場合にはcluster化されていない（単なるrobust SEになっている）からだ。

## OLSでcluster robust SEを得る方法その１
## lm.clusterを使う

#install.packages("miceadds")
library(miceadds)

reg7r1 <- lm.cluster(data = data3, 
                    formula = growth_GDPpc ~ logfirst_GDPpc + avg_invrate + growth_pop 
                    + avg_educ + factor(cty) + factor(period),
                    cluster = "cty")

summary(reg7r1)

## OLSでcluster robust SEを得る方法その２
## lm_robustを使う

library(estimatr)

reg7r2 <- lm_robust(data = data3, 
           formula = growth_GDPpc ~ logfirst_GDPpc + avg_invrate + growth_pop 
           + avg_educ + factor(cty) + factor(period), 
           clusters = cty, 
           se_type = "stata")

summary(reg7r2)

### LSDVを使ったその1と2とでは同じ結果だが、withinを使ったreg5rとは異なることに注意。
### これは自由度の調整の仕方に若干の差があるためである。
### 詳細は以下を参照。
### http://cameron.econ.ucdavis.edu/research/Cameron_Miller_JHR_2015_February.pdf
### なお、このLSDV推計とwithin推計をStataで行った場合も、ここでの結果と全く同じになる。

## First-differencing

reg8 <- plm(data = data3, formula = growth_GDPpc ~ logfirst_GDPpc + avg_invrate + growth_pop + 
              factor(period), 
            index = c("cty", "period"), model = "fd")
summary(reg8)

reg9 <- plm(data = data3, formula = growth_GDPpc ~ logfirst_GDPpc + avg_invrate + growth_pop
            + avg_educ + factor(period), 
            index = c("cty", "period"), model = "fd")
summary(reg9)

stargazer(reg4, reg5, reg8, reg9, 
          df = FALSE, 
          keep.stat = c("n", "Rsq"),
          type = "text",
          dep.var.caption = "Dependent variable: Growth of GDP per capita",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          omit = c("Intercept"),
          keep = c("logfirst_GDPpc", "avg_invrate", "growth_pop", "avg_educ"), 
          column.labels = c("Within", "Within", "1st dif", "1st dif"))


stargazer(reg4, reg5, reg8, reg9, 
          df = FALSE, 
          keep.stat = c("n", "Rsq"), 
          dep.var.caption = "Dependent variable: Growth of GDP per capita",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          omit = c("Intercept"),
          keep = c("logfirst_GDPpc", "avg_invrate", "growth_pop", "avg_educ"), 
          column.labels = c("Within", "Within", "1st dif", "1st dif"))

# DID

## Hjort, Jonas, and Jonas Poulsen. 2019. "The Arrival of Fast Internet and Employment in Africa." 
## American Economic Review 109 (3):1032-79.

### データは以下で公開されているが、Moodleにもアップされている
### https://www.openicpsr.org/openicpsr/project/113156/version/V1/view

library(haven)
data1 <- read_dta("afrobarometer.dta")

### country: country code
### employed: = 1 if employed
### employment: detailed employment status
### year: year
### q1: age
### submarines: connected to submarine cables
### treatment: submarinesと同じ
### connected: connected to the internet
### grid10: grid cells (detailed location)

### Replication of Table 3 (2)

### filter data following the paper
data2 <- data1 %>%
  filter(employment != -9 & q1 < 65 & distance < 0.1)

### まずはrobustでないSEを利用

DID1 <- lm(data = data2,
                    formula = employed ~ treatment 
                    + factor(country)*as.factor(year) + factor(grid10)*connected) 
summary(DID1)

### Using cluster robust standard errors

library(estimatr) 

DID2 <- lm_robust(data = data2,
                  formula = employed ~ treatment 
                  + factor(country)*factor(year) + factor(grid10)*connected, 
                  se_type = "stata", # lm_robustのrobust SEのタイプ指定ではstataとHC1は同じ
                  clusters = grid10) # coeftestと違ってclusterではなくclusters, 変数名は引用符なし

DID2
str(DID2)

# 宿題５

## この宿題では
## Balázs Égert, "Regulation, Institutions, and Productivity: 
## New Macroeconomic Evidence from OECD Countries", AMERICAN ECONOMIC REVIEW, 
## VOL. 106, NO. 5, MAY 2016 (pp. 109-13)
## を利用する。

## この論文は、国レベルのパネルデータを使って、国レベルの生産性の要因を
## 経済規制の度合いと研究開発支出、経済の開放性に焦点をあてて実証したものである。

## データは以下で公開されている。
## https://www.openicpsr.org/openicpsr/project/113447/version/V1/view
## ただし、元データはEViewsのフォーマットになっているが、
## xlsxに変換したものをMoodleにアップしている。

## なお、EViewsのデータは、RのhexViewという関数を用いて取り込むことができることがあるが、
## できないこともあり、このデータはうまくいかなかった。

## 変数の説明

# pool = country code
# id01 = year

# mfp22_		= measure of MFP (log)
# MFP (multi-factor productivity) とは、国レベルの生産性の指標
# 簡単に言えば、Y = Af(K,L)という生産関数を考えた場合、Aで表されるもの。
# Total factor productivity (TFP)と称されることがより一般的だが、
# OECDはMFPと呼んでいる（この元データはOECDのもの）。
# 
# 経済規制の指標（0から6の値をとる指標）
# etcr_new_  	= overall ETCR indicator
#             (Regulation in electricity, transport and communication (ETCR))
# etcr_en_ 	= ETCR entry barriers
# etcr_po_ 	= ETCR public ownership
# 
# 貿易に対する開放性の指標
# open_	= openness (exports and imports of goods and services as a % of GDP/2)
# 
# 研究開発支出に関する指標
# berd_			= business expenditures on R&D (% of GDP)
# gerd_		= general expenditures on R&D (% of GDP)
# gerdbasic  	= general expenditure on basic research  (% of GDP)
# 
# control variables コントロール変数
# l_hcap_ 	= log human capital
# og_hp		= output gap, using the HP filter

## 1. Moodleにアップされているeviews_workfile_egert.xlsxを取り込み、以下の手順で整理せよ。
## (1)上記の変数のみ取り出す。
## (2)1981年以降のデータのみ取り出す。
## (3)poolをctyに、id01をyearに変数名を変更する。さらに、変数名の末尾に_がついていると、
## Rでは不都合が生じることがあり、これらの変数の末尾の_はすべて削除する。
## (4)取り出した変数のいずれかにNAがある場合は、すべて削除する。

## 2. stargazerを利用して、全ての変数について記述統計の表を書け。
## ただし、25 percentile, 75 percentileは削除する。

## 3. MFPの指標（mfp22）を被説明変数とし、経済規制の指標、貿易に対する開放性の指標、
## 研究開発に関する指標、コントロール変数、年ダミーを説明変数とした固定効果モデルを
## 推計せよ。この時、以下の方法で行うこと。
## (1)貿易開放度の指標は常にopen_を利用する。
##（以前のバージョンではopen_adjを利用するように書いたが、より単純なopen_に変更したので注意）
## (2)コントロール変数は必ず2つとも利用する。
## (3)経済規制の指標３つ、研究開発の指標３つは多重共線性が予想されるため、
## まず、研究開発の指標をberdとして、経済規制の指標３つを1つずつ利用した推計を行う。
## (4)(3)の結果を踏まえて、もっともよいと思われる経済規制の指標を選び、
## それに加えて、研究開発の指標を１つずつ利用した推計を行う。
## (5)以上の手順では５つの推計を行うことになるが、これらはwithin transformationを
## 利用して行う。
## (6)最後に、５つの推計の最後のものだけを国ダミーを入れたLSDVで再推計する。
## (7)これらの結果をまとめて解釈する。その際、同様の指標のうち、どれが最も大きな効果を
## 持っているのかを確認し、それが何を意味するのかの解釈も示すこと。
## また、統計的な有意性だけではなく、係数の大きさが何を表すのか、
## またその数量的な有意性（economic significance）についても論じること。
## なお、論文そのものと若干推計方法やサンプルが違うために、
## この分析での結果は論文の結果とは異なる。
