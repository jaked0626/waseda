library(tidyverse) 
library(readxl)
sink("enshu3OLS_result.txt", split=T)

# メールマーケティングの分析

## バイアスのあるデータでの回帰分析

## まずはenshu2RCT.Rでやったようにバイアスのあるデータを生成
email_data <- read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

male_df <- email_data %>%
  filter(segment != "Womens E-Mail") %>% # != はnot equal
  mutate(treatment = if_else(segment == "Mens E-Mail", 1, 0))

set.seed(1) #これ忘れないように

biased_data <- male_df %>%
  mutate(obs_rate_c = if_else(
    (history > 300) | (recency < 6) | (channel == "Multichannel"), 
    0.5, 1),
    obs_rate_t = if_else(
      (history > 300) | (recency < 6) | (channel == "Multichannel"), 
      1, 0.5),
    random_number = runif(n=NROW(male_df))) %>%
  filter((treatment == 0 & random_number < obs_rate_c) | 
           (treatment == 1 & random_number < obs_rate_t))

## 回帰分析

### アウトカム：spend（購入額）
### treatment変数：treatment（メール送付）
### コントロール変数：history（過去の購入額）

biased_reg <- lm(data = biased_data, 
    formula = spend ~ treatment + history)

## 分析結果のレポート
summary(biased_reg)
### treatmentもhistoryもその係数が1%水準で有意であることを確認

### 演習2で、
### バイアスのないデータを使ったメール送付の効果の推計値は0.7698272、
### バイアスのあるデータを使ったメール送付の効果の推計値は0.9794465
### であった。

### これらの推計値は回帰分析によっても得ることができる
rct_reg <- lm(data = male_df, formula = spend ~ treatment)
summary(rct_reg)

norct_reg <- lm(data = biased_data, formula = spend ~ treatment)
summary(norct_reg)

### つまり、バイアスのあるデータの場合、
### メールが「常連」を中心に送られているため、
### メール送付の効果が過大評価されていた。

### この回帰分析では、メール送付の効果の推計値は0.9026109となっている。
### つまり、メール送付過去の購買力をコントロールすることで
### 推計の偏りをある程度修正できている。

## さらにコントロール変数を増やすと
norct_mreg <- lm(data = biased_data, 
    formula = spend ~ treatment + recency + factor(channel) + history)
summary(norct_mreg)

### この結果、メール送付の効果の推計値は0.8465757となり、
### コントロール変数を増やすことで、さらに推計の偏りを修正できている。

## 回帰分析の結果を後で使いやすいtibbleのデータフレームに変換
library("broom")

biased_reg_coef <- tidy(biased_reg)
biased_reg_coef
biased_reg_coef[2,2]

## ただし、treatment変数と相関する変数をコントロール変数として
## 入れると、偏りが増す

### visit: サイト訪問したかどうか
cor_visit_treatment <- lm(
  data = biased_data,
  formula = treatment ~ visit + channel + recency + history) %>%
  tidy()
cor_visit_treatment

bad_control_reg <- lm(
  data = biased_data,
  formula = spend ~ treatment + channel + recency + history + visit) %>%
  tidy()
bad_control_reg

### treatmentの係数が著しく減少したことがわかる。
### これは、treatmentの効果がvisitの効果に「浸食」されてしまったから。

## stargazerパッケージを使って、回帰分析の結果を表にする

### インストール
#install.packages("stargazer")
library(stargazer)

### 記述統計も表にできる
### ただし、tidyverseのtibbleには対応しておらず、
### その場合はシンプルなデータフレーム化する必要がある

stargazer(as.data.frame(biased_data), type = "text")
stargazer(as.data.frame(biased_data), omit.summary.stat = c("p25", "p75"), type = "text")

### デフォルトではLaTeX用のアウトプットになる
stargazer(as.data.frame(biased_data))

### 上で行った2つの回帰分析の結果を1つの表に
stargazer(norct_reg, biased_reg, type = "text")

### 結果がデータフレーム化されているとstargazerが使えないので、
### tidy化しない
bad_control_reg2 <- lm(
  data = biased_data,
  formula = spend ~ treatment + channel + recency + history + visit) 

stargazer(norct_reg, biased_reg, bad_control_reg2, type = "text")

### LaTeX用の出力
stargazer(norct_reg, biased_reg, bad_control_reg2)

#### stargazerではきだしたTeXコードをWord等に張り付ける方法
#### この結果をCloudLatex (https://cloudlatex.io/)やOverleaf (https://www.overleaf.com/login)
#### などのウェブ上でTeXコードをコンパイルできるサイトでコンパイルし、PDF化したものをコピペ
#### https://qiita.com/zakkiiii/items/d543489676b27712687a

### htmlの出力
stargazer(norct_reg, biased_reg, bad_control_reg2, type = "html")
#### 出力結果をコピーして、Excelに張り付けると表になる

### オプションをつけるとより見やすくなる
stargazer(norct_reg, biased_reg, bad_control_reg2, type = "text",
          title = "メール送付の効果", 
          keep.stat = c("n", "adj.rsq", "f"),  # 必要な統計量を指定
          df = FALSE, # 自由度を省略するので、列がせまくなってよい。これは常にマスト。
          dep.var.caption = "",
          dep.var.labels = "購買額")

##############################################
# 学費割引券（バウチャー）の効果推計
# Angrist, Bettinger, Bloom, King, and Kremer (2002).
# “Vouchers for Private Schooling in Colombia: Evidence from a Randomized Natural Experiment”
# American Economic Review. 
 
## experimentdatarをインストール

#install.packages("devtools")
library(devtools)
install_github("itamarcaspi/experimentdatar")

library("experimentdatar")
data(vouchers)
vouchers

## vouchersデータセット

## VOUCH0: バウチャーをもらったかどうか
## SVY: 電話による調査が行われたか
## HSVISIT: 対面による調査が行われたか
## DMONTH1-12: 調査月
## AGE: 調査時の学生の年齢
## SEX2: 学生の性別
## STRATA1-6, MS: 調査時の親の社会的地位の分類

## USNGSCH: 奨学金を使った
## PRSCHA_1: 6年の開始時に私立校に在籍
## PRSCH_C: 3年後に私立校に在籍
## REPT: 一度でも留年したか
## NREPT: 何回留年したか

## Bogota市で1995年に行われたバウチャー配布に限定
regression_data <- vouchers %>%
  filter(TAB3SMPL == 1, BOG95SMP == 1)

stargazer(as.data.frame(regression_data), omit.summary.stat = c("p25", "p75"), type = "text")

## 同じような回帰分析を、アウトカム変数を変えながら行うことがよくある。
## この時、説明変数をまとまりとして定義すると便利。

formula_x_covariate <- "SVY + HSVISIT + AGE + STRATA1 + STRATA2 + STRATA3 + 
  STRATA4 + STRATA5 + STRATA6 + STRATAMS + 
  D1993 + D1995 + D1997 + DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + 
  DMONTH5 + DMONTH6 + DMONTH7 + DMONTH8 + DMONTH9 + DMONTH10 + 
  DMONTH11 + DMONTH12 + SEX2" # コントロール変数

## 回帰式を関数として定義

vreg <- function(y) {
  lm(data = regression_data, 
            formula = paste(y, "~", "VOUCH0", "+", formula_x_covariate)) 
}

vreg1 <- vreg("USNGSCH")
vreg2 <- vreg("PRSCHA_1")
vreg3 <- vreg("PRSCH_C")
vreg4 <- vreg("REPT")
vreg5 <- vreg("NREPT")

stargazer(vreg1, vreg2, vreg3, vreg4, vreg5, type = "text",
          title = "教育バウチャーの効果", 
          keep = c("VOUCH0"),
          covariate.labels = c("バウチャー"), 
          keep.stat = c("n", "adj.rsq"),  
          df = FALSE, 
          dep.var.caption = "",
          column.labels = c("奨学金", "私立入学", "私立在籍", "留年歴", "留年回数"),
          dep.var.labels.include = FALSE)

stargazer(vreg1, vreg2, vreg3, vreg4, vreg5, 
          title = "教育バウチャーの効果", 
          keep = c("VOUCH0"),
          covariate.labels = c("バウチャー"), 
          keep.stat = c("n", "adj.rsq"),  
          df = FALSE, 
          dep.var.caption = "",
          column.labels = c("奨学金", "私立入学", "私立在籍", "留年歴", "留年回数"),
          dep.var.labels.include = FALSE)

#################################################
## 教科書に準じた上級編（授業では扱わない）

## 回帰式のためにtreatment変数、アウトカム変数を定義

formula_x_base <- "VOUCH0" # treatment

formula_y <- c("USNGSCH", "PRSCHA_1", "PRSCH_C", "REPT", "NREPT") 
# 様々なアウトカム変数のベクトル

## 各アウトカム変数についての単回帰式の並んだベクトルを作成
base_reg_formula <- paste(formula_y, "~", formula_x_base)
names(base_reg_formula) <- paste(formula_y, "base", sep = "_")
### 各成分に名前を付ける

base_reg_formula

## 各アウトカム変数についてコントロール変数を含む重回帰式が
## 並んだベクトルを作成
covariate_reg_formula <- paste(formula_y, "~", formula_x_base, 
      "+", formula_x_covariate)
names(covariate_reg_formula) <- paste(formula_y, "covariate", 
      sep = "_")

covariate_reg_formula

## 2種類の回帰式を一列に並べたベクトル
table3_formula <- c(base_reg_formula, covariate_reg_formula)

table3_formula
str(table3_formula)

## 回帰式のベクトルをデータフレーム化
### このデータフレームでは成分がformulaとして、
### その名前（上でnamesで定義したもの）がmodel_indexとして
### 定義されている
models <- table3_formula %>%
  enframe(name = "model_index", value = "formula")

## まとめて回帰式を実行
df_models <- models %>%
  mutate(model = map(.x = formula, .f = lm, data = regression_data)) %>%
  mutate(lm_result = map(.x = model, .f = tidy))

### map: .xで受け取ったデータの一つ一つの要素・成分に
###     .fで指定した関数を実行する

### 最初のmapでは、.xで指定されたformulaはtable3_formulaで定義された
### 各回帰式で、それにlm、つまりOLSを実行するということになる。
### そしてこのmapで実行した回帰分析の結果群がmodelという名前を付けて
### 定義されている

### 次のmapでは、modelとして定義された結果のそれぞれに対してtidyを
### 実行することで結果をデータフレーム化し、それをlm_resultと定義し、
### 結果の全てをdf_modelsとして定義

df_models
str(df_models)

## さらに結果を整形
df_results <- df_models %>%
  mutate(formula = as.character(formula)) %>%
  select(formula, model_index, lm_result) %>%
  unnest(cols = c(lm_result))

### まずformulaを文字列に変換
### df_modelsのformula, model_index（formulaの名前）, 
### lm_result（データフレーム化された結果）だけを取り出す
### その後、unnestという関数によってデータフレームを展開

df_results
df_models
### 違いをよく見てみよう

## 通学率（PRSCHA_1）と奨学金利用（USNGSCH）に対する効果のみを取り出す
using_voucher_results <- df_results %>%
  filter(term == "VOUCH0", 
         str_detect(model_index, "PRSCHA_1|USNGSCH")) %>%
  select(model_index, term, estimate, std.error, statistic, p.value) %>%
  arrange(model_index)

head(using_voucher_results)

### 取り出した効果をggplotで可視化
using_voucher_results %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + std.error*1.96,
                    ymin = estimate - std.error*1.96,
                    width = 0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5,1,0.5,1, "cm"))

######################################
# 講義ノート３に関するコード

## 変数の単位
### history2 = history/1000
biased_data2 <- biased_data %>%
  mutate(history2 = history/1000)

biased_reg2 <- lm(data = biased_data2, 
                 formula = spend ~ treatment + history2)
summary(biased_reg2)
summary(biased_reg)

stargazer(biased_reg, biased_reg2, type = "text")

## Economic significance

summary(rct_reg)
b_hat2 <- coef(summary(rct_reg))["treatment","Estimate"]
b_hat2

avg_y <- summary(male_df$spend)["Mean"]
avg_y

### treatment効果とアウトカムの平均を比較
b_hat2/avg_y
### 効果はアウトカムの平均の74%=>数量的にも大きな効果

## 世界各国の1人当たりGDPなどのデータを使った分析

world_data <- read.csv("world_data3.csv")

### avg_invrate: 2000-19年平均投資率
### avg_educ: 2000-19年平均高等教育就学率
### avg_export: 2000-19年平均輸出額対GDP比（％）
### avg_FDI: 2000-19年平均対内直接投資対GDP比（％）
### growth_GDPpc: 2000-19年平均年率1人あたりGDP成長率（％）
### growth_pop: 2000-19年平均年率人口成長率（％）
### first_GDPpc: 2000年の1人当たりGDP
### avg_polity2: 2000-19年平均民主化指標（-10-+10)

## 新古典派成長論の実証
world_reg1 <- lm(data = world_data,
                formula = growth_GDPpc ~ first_GDPpc + avg_invrate + growth_pop)
summary(world_reg1)

## 投資率の数量的効果
b_inv <- coef(summary(world_reg1))["avg_invrate", "Estimate"]
b_inv # 投資率の係数（効果）

summary(world_data)

mean_growth <- summary(world_data$avg_invrate)["Mean"]
mean_growth

sd <- world_data %>%
  summarise(sd_growth = sd(growth_GDPpc, na.rm = TRUE), # 1人当たりGDP成長率の標準偏差
            sd_inv = sd(avg_invrate, na.rm = TRUE)) # 投資率の標準偏差
sd

### 投資率が１標準偏差分だけ増えた時、１人当たりGDP成長率はその標準偏差の何％増えるか？
b_inv * sd[1, "sd_inv"]
b_inv * sd[1, "sd_inv"]/ sd[1, "sd_growth"]

## 2乗項の利用

### まず散布図を書いてfirst_GDPpcの分布が偏っていることを確認
world_data %>%
  ggplot(aes(first_GDPpc, growth_GDPpc)) + # x軸とy軸を指定
  geom_point() # 散布図形式を指定
### こういう場合は非線形にしたほうがよいかも

world_data <- world_data %>%
  mutate(first_GDPpc2 = first_GDPpc^2)

world_reg2 <- lm(data = world_data,
                 formula = growth_GDPpc ~ first_GDPpc + first_GDPpc2 +
                 avg_invrate + growth_pop)
summary(world_reg2)

### 2次の項はmutateせずに直接説明変数として入れ込むこともできる
world_reg2v2 <- lm(data = world_data,
                 formula = growth_GDPpc ~ first_GDPpc + I(first_GDPpc^2) +
                 avg_invrate + growth_pop)
summary(world_reg2v2)

### 初期の1人当たりGDPの効果がプラスになる変曲点の値
b1_hat <- coef(summary(world_reg2))["first_GDPpc", "Estimate"]
b2_hat <- coef(summary(world_reg2))["first_GDPpc2", "Estimate"]
-b1_hat/(2*b2_hat)

summary(world_data$first_GDPpc)
### 変曲点が55098だが、上位1/4が18241で最大が102494であることから
### 実際には効果は非線形だがほぼ常にマイナスであることがわかる

## 対数の利用

world_data <- world_data %>%
  mutate(ln_first_GDPpc = log(first_GDPpc))

world_reg3 <- lm(data = world_data,
                 formula = growth_GDPpc ~ ln_first_GDPpc + avg_invrate + growth_pop)
summary(world_reg3)

world_reg3v2 <- lm(data = world_data,
                 formula = growth_GDPpc ~ I(log(first_GDPpc)) + avg_invrate + growth_pop)
summary(world_reg3v2)

world_data %>%
  ggplot(aes(ln_first_GDPpc, growth_GDPpc)) + 
  geom_point()

world_data %>%
  ggplot(aes(I(log(first_GDPpc)), growth_GDPpc)) + 
  geom_point()

## ダミー変数

### 民主主義ダミー
### avg_polity2が正の場合、民主主義国家と定義
world_data <- world_data %>%
  mutate(democ = ifelse(avg_polity2 > 0, 1, 0))

world_reg4 <- lm(data = world_data,
                 formula = growth_GDPpc ~ ln_first_GDPpc + avg_invrate + growth_pop + democ)
summary(world_reg4)

## 交差項

world_reg5 <- lm(data = world_data,
                 formula = growth_GDPpc ~ ln_first_GDPpc + avg_invrate*avg_FDI 
                 + growth_pop) # varA*varBで交差項もvarA, varBの単独項も説明変数となる
summary(world_reg5)

### 交差項のみ入れたい場合は
world_reg6 <- lm(data = world_data,
                 formula = growth_GDPpc ~ ln_first_GDPpc + avg_invrate:avg_FDI 
                 + growth_pop) # varA:varBで交差項のみが説明変数となる
summary(world_reg6)
### ただし、基本的には交差項のみを説明変数とすることはしない

stargazer(world_reg1, world_reg2, world_reg3, world_reg4, world_reg5, world_reg6, 
          type = "text",
          title = "新古典派成長論の実証", 
          keep.stat = c("n", "adj.rsq", "f"),  
          df = FALSE, 
          dep.var.caption = "",
          dep.var.labels = "1人当たりGDP成長率")

## Robust standard errors

### 方法その1
#install.packages("estimatr")
library("estimatr")

world_reg7 <- lm_robust(data = world_data,
                 formula = growth_GDPpc ~ ln_first_GDPpc + avg_invrate + growth_pop, 
                 se_type = "HC1")
summary(world_reg7)
### ただこれだとstargazerが使えない

## RではSEのタイプをHC0, HC1, HC2などと指定できる。HC: heteroscedasticity consistent
## 詳細はhttps://cran.r-project.org/web/packages/sandwich/vignettes/sandwich.pdfを参照のこと。
## この授業では基本的にはより単純なHC1を使うが、小さなサンプルではHC3やHC4の方がよいとする研究もある。

### modelsummaryを使う
### https://keita43a.hatenablog.com/entry/2020/05/29/210250

#install.packages("modelsummary")
library(modelsummary)

modelsummary(world_reg7) # デフォルトでは、右下のViewerに表示される

### modelsummaryで複数の結果を表示させるためには、リストに収める必要がある

regs <- list()
regs[["(1)"]] <- world_reg3
regs[["(2) robust se"]] <- world_reg7

modelsummary(regs, stars = TRUE, 
             gof_omit = "AIC|BIC|Log.Lik.|F|se_type",
             title = "新古典派成長論の実証",
             coef_map = c("ln_first_GDPpc" = "Initial GDP per capita (log)",
                          "avg_invrate" = "Investment rate",
                          "growth_pop" = "Population growth rate"))
#### stars = TRUE: 有意な係数に星をつける
#### gof_omit: 不要な統計量を削除
#### coef_map: 変数名を変更

modelsummary(regs, stars = TRUE, gof_omit = "AIC|BIC|Log.Lik.|F|se_type", output ="latex")
#### TeX形式で表示

### 現状では、stargazerがより使われており、また両者は一長一短であるが、
### stargazerの更新が止まっていることから、今後はmodelsummaryがより標準的になるかも

### 方法その2
#install.packages("AER")
library("AER")

world_reg8 <- coeftest(world_reg3, vcov. =vcovHC, type = "HC1") # world_reg3は通常の標準誤差を使用した回帰結果

stargazer(world_reg3, world_reg8, 
          type = "text",
          title = "新古典派成長論の実証", 
          keep.stat = c("n", "adj.rsq", "f"),  
          df = FALSE, 
          dep.var.caption = "1人当たりGDP成長率",
          dep.var.labels.include = FALSE,
          column.labels = c("S.E.", "robust S.E."),
          covariate.labels = c("Initial GDP per capita (log)",
                               "Investment rate",
                               "Population growth rate"))

stargazer(world_reg3, world_reg8, 
          type = "text",
          title = "新古典派成長論の実証", 
          keep.stat = c("n", "adj.rsq", "f"),  
          df = FALSE, 
          dep.var.caption = "1人当たりGDP成長率",
          dep.var.labels.include = FALSE,
          model.names = FALSE, # これによってデフォルトで出てくるモデル名を削除
          column.labels = c("S.E.", "robust S.E."), # その代わり自分でモデルの種類を記す
          covariate.labels = c("Initial GDP per capita (log)",
                               "Investment rate",
                               "Population growth rate"))

### 方法その３
### 参考：https://www.jakeruss.com/cheatsheets/stargazer/#robust-standard-errors-replicating-statas-robust-option

cov1         <- vcovHC(world_reg3, type = "HC1")
robust_se    <- sqrt(diag(cov1))

stargazer(world_reg3, world_reg3, 
          type = "text",
          title = "新古典派成長論の実証", 
          keep.stat = c("n", "adj.rsq"),  
          df = FALSE, 
          se = list(NULL, robust_se), 
          # world_reg3の結果を2つ並べて、1つは通常のseを、1つはrobust seを利用
          dep.var.caption = "1人当たりGDP成長率",
          dep.var.labels.include = FALSE,
          column.labels = c("S.E.", "robust S.E."),
          covariate.labels = c("Initial GDP per capita (log)",
                               "Investment rate",
                               "Population growth rate"))

### 2つの係数と標準誤差をくらべてみよう

## モデルの選択

### 1次式vs対数

stargazer(world_reg1, world_reg3, 
          type = "text",
          title = "新古典派成長論の実証", 
          keep.stat = c("n", "adj.rsq"),  
          df = FALSE, 
          dep.var.caption = "1人当たりGDP成長率",
          dep.var.labels.include = FALSE)

world_reg9 <- lm(data = world_data,
                 formula = growth_GDPpc ~ ln_first_GDPpc + first_GDPpc  
                 + avg_invrate + growth_pop)
summary(world_reg9)

### 1次式vs2次式vs対数
stargazer(world_reg1, world_reg2, world_reg3, 
          type = "text",
          title = "新古典派成長論の実証", 
          keep.stat = c("n", "adj.rsq"),  
          df = FALSE, 
          dep.var.labels.include = FALSE,
          dep.var.caption = "1人当たりGDP成長率")

world_reg10 <- lm(data = world_data,
                 formula = growth_GDPpc ~ ln_first_GDPpc + first_GDPpc + first_GDPpc2  
                 + avg_invrate + growth_pop)
summary(world_reg10)
### ただし、この場合そもそも理論的考察により対数を使うことが多い

## 外れ値

### わざと外れ値のあるサンプルを構築

world_data_out <- world_data %>%
  filter(ln_first_GDPpc>11 | ln_first_GDPpc<9)

world_data_out %>%
  ggplot(aes(ln_first_GDPpc, growth_GDPpc)) + 
  geom_point() 
### ３つ明らかな外れ値があることが確認できる

### 外れ値ありの推計
world_reg11 <- lm(data = world_data_out,
                  formula = growth_GDPpc ~ ln_first_GDPpc + avg_invrate + growth_pop)
summary(world_reg11)

### 外れ値なしの推計

world_data_out_sum <- world_data_out %>%
  summarise(ln_first_GDPpc_m = mean(ln_first_GDPpc),
            ln_first_GDPpc_sd = sd(ln_first_GDPpc))
world_data_out_sum
m1 <- world_data_out_sum[1, 1]
sd1 <- world_data_out_sum[1, 2]
out1 <- m1 + 3 * sd1 

world_data_out2 <- world_data_out %>%
  filter(ln_first_GDPpc<out1) # 平均より3sigma大きい観測を削除

world_reg12 <- lm(data = world_data_out2,
                  formula = growth_GDPpc ~ ln_first_GDPpc + avg_invrate + growth_pop)
summary(world_reg12)

stargazer(world_reg11, world_reg12,  
          type = "text",
          title = "新古典派成長論の実証", 
          keep.stat = c("n", "adj.rsq"),  
          df = FALSE, 
          dep.var.labels.include = FALSE,
          dep.var.caption = "1人当たりGDP成長率")
### reg11と12ではln_first_GDPpcの係数が大きく違うことを確認

#############################
# 宿題3

## この宿題では以下の論文で使われたデータを利用する。
## Acemoglu, Daron, Francisco A Gallego, and James A Robinson. 2014. "Institutions, Human Capital, and Development." Annu. Rev. Econ. 6 (1):875-912.
## https://economics.mit.edu/files/9940

## 1. データは
## https://economics.mit.edu/faculty/acemoglu/data/hcapital
## に公開されているので、それをダウンロードして読みこむ。
## dtaファイルというStataという統計ソフトのデータ形式であるので、
## dtaファイルの読み込み方は以下を参照のこと。
## https://yukiyanai.github.io/jp/classes/econometrics2/contents/R/data-handling.html
## 変数の説明はMoodleにアップしたacemoglu_data_description.txtにある。

## 2．stargazerを利用して、以下の変数の記述統計の表を作成せよ。
## logpgdp05: 2005年における1人当たりGDP
## malfal94: 1994年時点でのマラリアのリスクの指標。0以上1以下で大きいほどマラリアのリスクが高い。
## prienr1900: 1900年時点での初等教育就学率
## lat_abst: 緯度の絶対値/90
## africa: アフリカの国のダミー

## 3. 2005年における1人当たりGDPが気候・自然環境や歴史的な教育レベルによって
## 決まっているかどうかをOLSによって検証する。
## 気候・自然環境の変数としてmalfal94を、歴史的な教育レベルの変数としてprienr1900を使う。
## この時、通常の標準誤差を使った場合とrobustな標準誤差を使った場合の２通りの推計を行い、
## その結果をstargazerを使って表にした上で、その2つの結果の違いについて論ぜよ。
## また、推計結果を統計的にも数量的にも解釈せよ。
## なおこれ以降はrobust SEのみを利用せよ。

## 4. 3の推計に説明変数としてmalfal94ではなくlat_abstを利用して推計をせよ。
## 気候・自然環境を示す変数としてmalfal94とlat_abstのどちらがよいか。論ぜよ。

## 5. 3の推計で（つまりlat_abstはここでは使わない）malfal94およびprienr1900の効果が
## アフリカとそれ以外の国とでは異なるかどうかを、
## アフリカダミーおよびその交差項を利用することで検証せよ。

## なお、全ての表はstargazerでtext形式でconsoleペインに表示させ、
## Consoleペインの結果をそのままコピペしてtxtファイルとして保存し、
## それに説明を加筆したものを提出すること。





