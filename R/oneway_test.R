suppressMessages({
    if (!require(dplyr)) install.packages("dplyr")
    if (!require(tidyr)) install.packages("tidyr")
    if (!require(magrittr)) install.packages("magrittr")
    if (!require(rcompanion)) install.packages("rcompanion")
    if (!require(agricolae)) install.packages("agricolae")
    if (!require(rstatix)) install.packages("rstatix")
})

check_normality <- function(x){
    v <- na.omit(x)
    if (sd(v) == 0) normality <- TRUE
    if (sd(v) != 0) normality <- shapiro.test(v)$p.value
    return(normality)
}

oneway_test <- function(data, formula){
    stopifnot(is.data.frame(data))
    y <- as.character(formula)[2]
    x <- as.character(formula)[3]
    
    df0 <- data.frame(
        y = as.numeric(data[, y, drop = TRUE]),
        x = data[, x, drop = TRUE]
    ) %>% 
        tidyr::drop_na()
    
    stopifnot(sd(df0$y) != 0)
    
    descriptive_stats <- df0 %>% 
        dplyr::summarise(
            N = length(y),
            MIN = min(y),
            AVG = mean(y),
            MED = median(y),
            MAX = max(y),
            .by = x
        ) %>% 
        dplyr::arrange(dplyr::desc(AVG))
    
    # Factor reorder with descending average value
    df0$x <- factor(df0$x, levels = descriptive_stats$x)
    
    # Check data normality and homoscedasticity
    normality <- df0 %>% 
        group_by(x) %>% 
        summarise(p = check_normality(y))
    homoscedasticity <- rstatix::levene_test(formula = y ~ x, data = df0)
    is_normal <- all(normality$p > 0.05)
    var_equal <- homoscedasticity$p > 0.05
    
    # Parametric test
    if (is_normal){
        perform_test <- ifelse(var_equal, "Fisher's ANOVA", "Welch's ANOVA")
        pre_hoc <- oneway.test(y ~ x, df0, var.equal = var_equal)
        pre_hoc_pass <- pre_hoc$p.value < 0.05
        post_hoc <- NULL
        
        # Tukey-HSD
        if (var_equal & pre_hoc_pass){
            perform_test <- paste0(perform_test, " + Tukey-HSD")
            post_hoc <- agricolae::HSD.test(
                y = aov(y ~ x, df0),
                trt = "x",
                unbalanced = length(unique(descriptive_stats$N)) > 1
            )
            
            cld <- data.frame(
                x = rownames(post_hoc$groups),
                letter = post_hoc$groups[, "groups", drop = TRUE]
            )
        }
        
        # Games-Howell
        if(!var_equal & pre_hoc_pass){
            perform_test <- paste0(perform_test, " + Games-Howell")
            post_hoc <- rstatix::games_howell_test(
                data = df0, 
                formula = y ~ x
            ) %>% 
                dplyr::mutate(comparisons = paste(group1, group2, sep = " - "))
            
            cld <- rcompanion::cldList(p.adj ~ comparisons, post_hoc, remove.zero = FALSE)
            cld <- data.frame(
                x = cld[, "Group"],
                letter = cld[, "Letter", drop = TRUE]
            )
        }
    }  # <<<<< Parametric test
    
    # Non-parametric test
    if (!is_normal){
        perform_test <- "Kruskal-Wallis"
        pre_hoc <- with(
            data = df0,
            expr = kruskal.test(x = y, g = x)
        )
        pre_hoc_pass <- pre_hoc$p.value < 0.05
        post_hoc <- NULL
        
        if (pre_hoc_pass){
            perform_test <- paste0(perform_test, " + Dunn's test")
            post_hoc <- rstatix::dunn_test(
                data = df0, 
                formula = y ~ x
            ) %>% 
                dplyr::mutate(comparisons = paste(group1, group2, sep = "-"))
            
            cld <- rcompanion::cldList(
                formula = p.adj ~ comparisons, 
                data = post_hoc, 
                remove.zero = FALSE
            )
            
            cld <- data.frame(
                x = cld[, "Group"],
                letter = cld[, "Letter", drop = TRUE]
            )
        }
    }  # <<<<< Non-parametric test
    
    if (exists("cld")) descriptive_stats <- descriptive_stats %>% dplyr::left_join(cld, by = "x")
    
    res <- list(
        normality = normality, 
        homoscedasticity = homoscedasticity, 
        perform_test = perform_test,
        pre_hoc = pre_hoc, 
        post_hoc = post_hoc,
        results = descriptive_stats
    )
    
    return(res)
}