#確認套件管理軟體 pacman 有載入
if(!require("pacman")){
  install.packages("pacman", repos="https://cran.csie.ntu.edu.tw",  dependencies = TRUE)  
  library(pacman)
}

#載入共用的套件供使用
pacman::p_load(devtools, remotes, tidyverse, tidyr, jtools, gtsummary, broom, 
               purrr, sjPlot, here, forcats, printr, flextable, statmod, MASS, 
               kableExtra, confintr, Hmisc, parameters, cocor, corrplot, moments, 
               ggplot2, GGally, ragg, patchwork, webshot2, ggeffects, ggExtra, 
               KernSmooth, ggbeeswarm, ggridges, gghighlight, ggpubr, gglm, 
               ggrepel, ggokabeito, performance, see, lm.beta, insight, bda, 
               DiagrammeR, DiagrammeRsvg, rsvg, vctrs, MBESS, mediation, plyr,
               broom.mixed, lme4, effects, lavaan, lavaanPlot, semPlot, tidySEM, 
               CTT, semTools, qgraph, eRm, cowplot, psychometric, psych, rentrez, 
               ngramr, metafor, metaviz, dispmod, sandwich, mlogit)

if(!require("ggmirt")){
   devtools::install_github("masurp/ggmirt")
   library(ggmirt)
}
if(!require("brolgar")){
   remotes::install_github("njtierney/brolgar")
  library(brolgar)
}

#設定微軟 Windows 作業系統的字型
#設定蘋果 Mac 作業系統（代號 Darwin）的字型
if(Sys.info()["sysname"]=="Windows"){
 par(family = 'Microsoft Ya Hei')
}
if(Sys.info()["sysname"]=="Darwin"){
 par(family = 'Kaiti TC')
}


# set up options for formatting output
knitr::opts_chunk$set(
  tidy = FALSE,     # 程式碼顯示就與輸入格式相同
  size = "small",   # 程式碼用小一點的字
  fig.width = 7, 
  fig.asp = .7,
  fig.align = 'center',
  dev = 'ragg_png',
  comment = "",
  autodep = TRUE)   
options(show.signif.stars = FALSE, digits=4)

#底下的圖都用黑白配色（theme_minimal）
ggplot2::theme_set(theme_minimal())
