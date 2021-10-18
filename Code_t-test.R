library(tidyverse)
library(skimr)
library(readxl)
library(MASS)
library(ggpubr) # for ggqqplot()
# devtools::install_github("kassambara/rstatix")
library(rstatix) # for rm anova and more
library(naniar) # for missing data visualizations
library(conflicted)
conflict_prefer("t_test", "rstatix")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
ggplot2::theme_set(theme_classic()) # change the theme of ggplot2
options(pillar.sigfig=4) # tibble 소숫점 넷째자리까지 표기

# 1. loading and wrangling data -------------------------------------------
pole <- read_xlsx("./data/남극연구_210708_1.xlsx") %>% 
    select(대상자ID, 기지_1장보고_2세종_3파견안됨, 
              #PSQI_4실제잔시간분     
              `@1PSQI_4실제잔시간분`, `@3PSQI_4실제잔시간분`, `@4PSQI_4실제잔시간분`, `@5PSQI_4실제잔시간분`,
              #PSQI_수면효율
              `@1PSQI_수면효율`, `@3PSQI_수면효율`, `@4PSQI_수면효율`, `@5PSQI_수면효율`,
              #PSQI_총점
              `@1PSQI_총점`, `@3PSQI_총점`, `@4PSQI_총점`, `@5PSQI_총점`,
              #1ISI_총점
              `@1ISI_총점`, `@3ISI_총점`, `@4ISI_총점`, `@5ISI_총점`,
              #1ESS_총점
              `@1ESS총점`, `@3ESS총점`, `@4ESS총점`, `@5ESS총점`,
              #1BDI_총점
              `@1BDI_총점`, `@3BDI_총점`, `@4BDI_총점`, `@5BDI_총점`,
              #1BAI_총점
              `@1BAI_총점`, `@3BAI_총점`, `@4BAI_총점`, `@5BAI_총점`,
              #1knhanes_total
              `@1knhanes_total`, `@3knhanes_total`, `@4knhanes_total`, `@5knhanes_total`) %>% 
    mutate(기지_1장보고_2세종_3파견안됨 = factor(기지_1장보고_2세종_3파견안됨),
           PSQI_실제잔시간분_31 = `@3PSQI_4실제잔시간분` - `@1PSQI_4실제잔시간분`,
           PSQI_실제잔시간분_41 = `@4PSQI_4실제잔시간분` - `@1PSQI_4실제잔시간분`,
           PSQI_실제잔시간분_51 = `@5PSQI_4실제잔시간분` - `@1PSQI_4실제잔시간분`,
           PSQI_수면효율_31 = `@3PSQI_수면효율` - `@1PSQI_수면효율`,
           PSQI_수면효율_41 = `@4PSQI_수면효율` - `@1PSQI_수면효율`,
           PSQI_수면효율_51 = `@5PSQI_수면효율` - `@1PSQI_수면효율`,
           PSQI_총점_31 = `@3PSQI_총점` - `@1PSQI_총점`,
           PSQI_총점_41 = `@4PSQI_총점` - `@1PSQI_총점`,
           PSQI_총점_51 = `@5PSQI_총점` - `@1PSQI_총점`,
           ISI_총점_31 = `@3ISI_총점` - `@1ISI_총점`,
           ISI_총점_41 = `@4ISI_총점` - `@1ISI_총점`,
           ISI_총점_51 = `@5ISI_총점` - `@1ISI_총점`,
           ESS총점_31 = `@3ESS총점` - `@1ESS총점`,
           ESS총점_41 = `@4ESS총점` - `@1ESS총점`,
           ESS총점_51 = `@5ESS총점` - `@1ESS총점`,
           BDI_총점_31 = `@3BDI_총점` - `@1BDI_총점`,
           BDI_총점_41 = `@4BDI_총점` - `@1BDI_총점`,
           BDI_총점_51 = `@5BDI_총점` - `@1BDI_총점`,
           BAI_총점_31 = `@3BAI_총점` - `@1BAI_총점`,
           BAI_총점_41 = `@4BAI_총점` - `@1BAI_총점`,
           BAI_총점_51 = `@5BAI_총점` - `@1BAI_총점`,
           knhanes_total_31 = `@3knhanes_total` - `@1knhanes_total`,
           knhanes_total_41 = `@4knhanes_total` - `@1knhanes_total`,
           knhanes_total_51 = `@5knhanes_total` - `@1knhanes_total`) %>% 
    rename(sector = 기지_1장보고_2세종_3파견안됨,
           ID = 대상자ID)
str(pole)
skim(pole)

pole_sleep <- pole %>%
    select(ID, sector, `PSQI_실제잔시간분_31`:`PSQI_실제잔시간분_51`) %>% 
    pivot_longer(`PSQI_실제잔시간분_31`:`PSQI_실제잔시간분_51`, "time")
pole_eff <- pole %>% 
    select(ID, sector, `PSQI_수면효율_31`:`PSQI_수면효율_51`) %>% 
    pivot_longer(`PSQI_수면효율_31`:`PSQI_수면효율_51`, "time")
pole_tot_psqi <- pole %>% 
    select(ID, sector, `PSQI_총점_31`:`PSQI_총점_51`) %>% 
    pivot_longer(`PSQI_총점_31`:`PSQI_총점_51`, "time")
pole_tot_isi <- pole %>% 
    select(ID, sector, `ISI_총점_31`:`ISI_총점_51`) %>% 
    pivot_longer(`ISI_총점_31`:`ISI_총점_51`, "time")
pole_tot_ess <- pole %>% 
    select(ID, sector, `ESS총점_31`:`ESS총점_51`) %>% 
    pivot_longer(`ESS총점_31`:`ESS총점_51`, "time")
pole_tot_bdi <- pole %>% 
    select(ID, sector, `BDI_총점_31`:`BDI_총점_51`) %>% 
    pivot_longer(`BDI_총점_31`:`BDI_총점_51`, "time")
pole_tot_bai <- pole %>% 
    select(ID, sector, `BAI_총점_31`:`BAI_총점_51`) %>% 
    pivot_longer(`BAI_총점_31`:`BAI_총점_51`, "time")
pole_tot_knh <- pole %>% 
    select(ID, sector, `knhanes_total_31`:`knhanes_total_51`) %>% 
    pivot_longer(`knhanes_total_31`:`knhanes_total_51`, "time")

# 2. two-sample t-test with bonferroni correction ------------------------------------------
bon_two_t <- function(.tb){
    .tb %>% 
        drop_na() %>% 
        group_by(time) %>% 
        t_test(data =., value ~ sector, mu = 0) %>%
        adjust_pvalue(method = "bonferroni")
}

## (1) PSQI_4실제잔시간분
bon_two_t(pole_sleep)

## (2) PSQI_수면효율
bon_two_t(pole_eff)

## (3) PSQI_총점
bon_two_t(pole_tot_psqi)

## (4) ISI_총점
bon_two_t(pole_tot_isi)

## (5) ESS_총점
bon_two_t(pole_tot_ess)

## (6) BDI_총점
bon_two_t(pole_tot_bdi)

## (7) BAI_총점
bon_two_t(pole_tot_bai)

## (8) knhanes_total
bon_two_t(pole_tot_knh)

