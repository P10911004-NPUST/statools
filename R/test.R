library(tidyverse)
library(palmerpenguins)
source("./oneway_test.R")
source("./tukey_hsd.R")
source("./compact_letter_display.R")

rawdata <- readxl::read_excel("C:/jklai/project/LGL23/dev_cell/rcode/figures/PLT2_RGF1/NBT.xlsx", sheet = 1)

colnames(rawdata) <- fct_lvl$group

df0 <- as.data.frame(rawdata) %>% 
    pivot_longer(cols = everything(), names_to = "group", values_to = "val") %>% 
    drop_na() %>% 
    separate(group, into = c("genotype", "treatment"), sep = "_", remove = FALSE) %>% 
    mutate(
        val = as.numeric(val),
        group = factor(group, levels = fct_lvl$group),
        genotype = factor(genotype, levels = fct_lvl$genotype),
        treatment = factor(treatment, levels = fct_lvl$treatment)
    )

desc_df <- df0 %>% 
    group_by(group) %>% 
    summarise(AVG = mean(val)) %>% 
    ungroup()

aov_mod <- aov(val ~ group, df0)

stats_res <- ag$post_hoc
stats_res

ag <- oneway_test(df0, val ~ group)
ag$perform_test
ag$result
ag$post_hoc

res <- compact_letter_display(
    groups = desc_df$group, 
    means = desc_df$AVG, 
    comparisons = stats_res$comparisons, 
    pval = stats_res$p.adj,
    # descending = TRUE,
    comparison_symbol = " - "
)

res


