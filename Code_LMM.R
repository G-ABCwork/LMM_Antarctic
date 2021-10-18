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
    mutate(기지_1장보고_2세종_3파견안됨 = factor(기지_1장보고_2세종_3파견안됨)) %>% 
    rename(sector = 기지_1장보고_2세종_3파견안됨,
           ID = 대상자ID)
str(pole)
skim(pole)

pole_sleep <- pole %>%
    select(ID, sector, `@1PSQI_4실제잔시간분`:`@5PSQI_4실제잔시간분`)
pole_eff <- pole %>% 
    select(ID, sector, `@1PSQI_수면효율`:`@5PSQI_수면효율`)
pole_tot_psqi <- pole %>% 
    select(ID, sector, `@1PSQI_총점`:`@5PSQI_총점`)
pole_tot_isi <- pole %>% 
    select(ID, sector, `@1ISI_총점`:`@5ISI_총점`)
pole_tot_ess <- pole %>% 
    select(ID, sector, `@1ESS총점`:`@5ESS총점`)
pole_tot_bdi <- pole %>% 
    select(ID, sector, `@1BDI_총점`:`@5BDI_총점`)
pole_tot_bai <- pole %>% 
    select(ID, sector, `@1BAI_총점`:`@5BAI_총점`)
pole_tot_knh <- pole %>% 
    select(ID, sector, `@1knhanes_total`:`@5knhanes_total`)

# 2. repeated measures with linear mixed models ------------------------------------------
## (1) PSQI_4실제잔시간분
skim(pole_sleep)
sleep <- pole_sleep %>% 
    pivot_longer(`@1PSQI_4실제잔시간분`:`@5PSQI_4실제잔시간분`, "time") %>% 
    mutate(
        time = recode(time, `@1PSQI_4실제잔시간분` = "t1", `@3PSQI_4실제잔시간분` = "t3", 
                      `@4PSQI_4실제잔시간분` = "t4", `@5PSQI_4실제잔시간분` = "t5")
    ) %>% 
    arrange(ID, time, sector) %>% 
    mutate_at(vars(ID:time), factor)
### summary statistics
get_summary <- function(.tb){
    .tb %>%
        drop_na() %>% 
        group_by(sector, time) %>%
        get_summary_stats(value, type = "mean_se") %>% 
        arrange(sector, time) %>% 
        mutate(
            lower_95 = mean - se*qnorm(0.975),
            upper_95 = mean + se*qnorm(0.975)
        ) %>% 
        select(time, sector, mean:upper_95, n, -variable)
}
sleep_stat <- get_summary(sleep)

### plotting means and error bars 
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right
plot_mean_errorbars <- function(.tb, .ylab){
    .tb %>% 
        ggplot(aes(x = time, y = mean, color = sector, group = sector)) +
        geom_errorbar(aes(ymin = lower_95, ymax = upper_95), colour="black", width=.1, position = pd) +
        geom_line(position = pd) +
        geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
        scale_colour_hue(name="Station",    # Legend label, use darker colors
                         labels=c("Jang Bogo", "The King Sejong"),
                         l=40) +
        xlab("Visit") +
        ylab(.ylab) +
        theme(legend.position = "bottom")
}
plot_mean_errorbars(sleep_stat, "Actual bedtime (minutes)")
ggsave("./plot/PSQI_4실제잔시간분1.jpeg", dpi = 130, device = "jpeg", type = "cairo")
### fitting a linear mixed model
sleep_lmm <- lmerTest::lmer(value ~ time * sector + (1|ID), data = sleep %>% drop_na())
sleep_lmm
anova(sleep_lmm)

## (2) PSQI_수면효율
skim(pole_eff)
eff <- pole_eff %>% 
    pivot_longer(`@1PSQI_수면효율`:`@5PSQI_수면효율`, "time") %>% 
    mutate(
        time = recode(time, `@1PSQI_수면효율` = "t1", `@3PSQI_수면효율` = "t3", 
                      `@4PSQI_수면효율` = "t4", `@5PSQI_수면효율` = "t5")
    ) %>% 
    arrange(ID, time, sector) %>% 
    mutate_at(vars(ID:time), factor)
### summary statistics
eff_stat <- get_summary(eff)

### plotting means and error bars 
plot_mean_errorbars(eff_stat, "Sleep efficiency")
ggsave("./plot/PSQI_수면효율.jpeg", dpi = 130, device = "jpeg", type = "cairo")
### fitting a linear mixed model
eff_lmm <- lmerTest::lmer(value ~ time * sector + (1|ID), data = eff %>% drop_na())
eff_lmm
anova(eff_lmm)
### 시간에 대한 사후 분석
na_id <- eff %>% # paired t-test이므로 time point에 하나라도 결측있는 ID 제거
    drop_na() %>% 
    group_by(ID) %>% 
    summarize(n = n()) %>% 
    filter(n < 4)
eff %>%
    drop_na() %>% 
    anti_join(na_id) %>% 
    group_by(sector) %>%
    t_test(data =., value ~ time, paired = TRUE) %>%
    adjust_pvalue(method = "bonferroni") %>%
    add_significance("p.adj") %>% 
    mutate(
        p.adj = ifelse(p*6 > 1, 1, p*6)
    )

## (3) PSQI_총점
skim(pole_tot_psqi)
tot_psqi <- pole_tot_psqi %>% 
    pivot_longer(`@1PSQI_총점`:`@5PSQI_총점`, "time") %>% 
    mutate(
        time = recode(time, `@1PSQI_총점` = "t1", `@3PSQI_총점` = "t3", 
                      `@4PSQI_총점` = "t4", `@5PSQI_총점` = "t5")
    ) %>% 
    arrange(ID, time, sector) %>% 
    mutate_at(vars(ID:time), factor)
### summary statistics
tot_psqi_stat <- get_summary(tot_psqi)
### plotting means and error bars 
plot_mean_errorbars(tot_psqi_stat, "Total of PSQI")
ggsave("./plot/PSQI_총점.jpeg", dpi = 130, device = "jpeg", type = "cairo")
### fitting a linear mixed model
tot_psqi_lmm <- lmerTest::lmer(value ~ time * sector + (1|ID), data = tot_psqi %>% drop_na())
tot_psqi_lmm
anova(tot_psqi_lmm)

## (4) ISI_총점
skim(pole_tot_isi)
tot_isi <- pole_tot_isi %>% 
    pivot_longer(`@1ISI_총점`:`@5ISI_총점`, "time") %>% 
    mutate(
        time = recode(time, `@1ISI_총점` = "t1", `@3ISI_총점` = "t3", 
                      `@4ISI_총점` = "t4", `@5ISI_총점` = "t5")
    ) %>% 
    arrange(ID, time, sector) %>% 
    mutate_at(vars(ID:time), factor)
### summary statistics
tot_isi_stat <- get_summary(tot_isi)
### plotting means and error bars 
plot_mean_errorbars(tot_isi_stat, "Total of ISI")
ggsave("./plot/ISI_총점.jpeg", dpi = 130, device = "jpeg", type = "cairo")
### fitting a linear mixed model
tot_isi_lmm <- lmerTest::lmer(value ~ time * sector + (1|ID), data = tot_isi %>% drop_na())
anova(tot_isi_lmm)
### 시간에 대한 사후 분석
na_id <- tot_isi %>% # paired t-test이므로 time point에 하나라도 결측있는 ID 제거
    drop_na() %>% 
    group_by(ID) %>% 
    summarize(n = n()) %>% 
    filter(n < 4)
tot_isi %>%
    drop_na() %>% 
    anti_join(na_id) %>% 
    group_by(sector) %>%
    t_test(data =., value ~ time, paired = TRUE) %>%
    adjust_pvalue(method = "bonferroni") %>%
    add_significance("p.adj") %>% 
    mutate(
        p.adj = ifelse(p*6 > 1, 1, p*6)
    )


## (5) ESS_총점
skim(pole_tot_ess)
tot_ess <- pole_tot_ess %>% 
    pivot_longer(`@1ESS총점`:`@5ESS총점`, "time") %>% 
    mutate(
        time = recode(time, `@1ESS총점` = "t1", `@3ESS총점` = "t3", 
                      `@4ESS총점` = "t4", `@5ESS총점` = "t5")
    ) %>% 
    arrange(ID, time, sector) %>% 
    mutate_at(vars(ID:time), factor)
### summary statistics
tot_ess_stat <- get_summary(tot_ess)
### plotting means and error bars 
plot_mean_errorbars(tot_ess_stat, "Total of ESS")
ggsave("./plot/ESS_총점.jpeg", dpi = 130, device = "jpeg", type = "cairo")
### fitting a linear mixed model
tot_ess_lmm <- lmerTest::lmer(value ~ time * sector + (1|ID), data = tot_ess %>% drop_na())
tot_ess_lmm
anova(tot_ess_lmm)
### 기지에 대한 사후분석
tot_ess %>%
    group_by(time) %>%
    t_test(data =., value ~ sector) %>%
    adjust_pvalue(method = "bonferroni") %>%
    add_significance("p.adj")

## (6) BDI_총점
skim(pole_tot_bdi)
tot_bdi <- pole_tot_bdi %>% 
    pivot_longer(`@1BDI_총점`:`@5BDI_총점`, "time") %>% 
    mutate(
        time = recode(time, `@1BDI_총점` = "t1", `@3BDI_총점` = "t3", 
                      `@4BDI_총점` = "t4", `@5BDI_총점` = "t5")
    ) %>% 
    arrange(ID, time, sector) %>% 
    mutate_at(vars(ID:time), factor)
### summary statistics
tot_bdi_stat <- get_summary(tot_bdi)
### plotting means and error bars 
plot_mean_errorbars(tot_bdi_stat, "Total of BDI")
ggsave("./plot/BDI_총점.jpeg", dpi = 130, device = "jpeg", type = "cairo")
### fitting a linear mixed model
tot_bdi_lmm <- lmerTest::lmer(value ~ time * sector + (1|ID), data = tot_bdi %>% drop_na())
tot_bdi_lmm
anova(tot_bdi_lmm)
### 기지에 대한 사후분석
tot_bdi %>%
    group_by(time) %>%
    t_test(data =., value ~ sector) %>%
    adjust_pvalue(method = "bonferroni") %>%
    add_significance("p.adj")

## (7) BAI_총점
skim(pole_tot_bai)
tot_bai <- pole_tot_bai %>% 
    pivot_longer(`@1BAI_총점`:`@5BAI_총점`, "time") %>% 
    mutate(
        time = recode(time, `@1BAI_총점` = "t1", `@3BAI_총점` = "t3", 
                      `@4BAI_총점` = "t4", `@5BAI_총점` = "t5")
    ) %>% 
    arrange(ID, time, sector) %>% 
    mutate_at(vars(ID:time), factor)
### summary statistics
tot_bai_stat <- get_summary(tot_bai)
### plotting means and error bars 
plot_mean_errorbars(tot_bai_stat, "Total of BAI")
ggsave("./plot/BAI_총점.jpeg", dpi = 130, device = "jpeg", type = "cairo")
### fitting a linear mixed model
tot_bai_lmm <- lmerTest::lmer(value ~ time * sector + (1|ID), data = tot_bai %>% drop_na())
tot_bai_lmm
anova(tot_bai_lmm)

## (8) knhanes_total
skim(pole_tot_knh)
tot_knh <- pole_tot_knh %>% 
    pivot_longer(`@1knhanes_total`:`@5knhanes_total`, "time") %>% 
    mutate(
        time = recode(time, `@1knhanes_total` = "t1", `@3knhanes_total` = "t3", 
                      `@4knhanes_total` = "t4", `@5knhanes_total` = "t5")
    ) %>% 
    arrange(ID, time, sector) %>% 
    mutate_at(vars(ID:time), factor)
### summary statistics
tot_knh_stat <- get_summary(tot_knh)
### plotting means and error bars 
plot_mean_errorbars(tot_knh_stat, "Total of knhanes")
ggsave("./plot/knhanes_총점.jpeg", dpi = 130, device = "jpeg", type = "cairo")
### fitting a linear mixed model
tot_knh_lmm <- lmerTest::lmer(value ~ time * sector + (1|ID), data = tot_knh %>% drop_na())
tot_knh_lmm
anova(tot_knh_lmm)
