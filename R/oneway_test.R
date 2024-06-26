suppressMessages({
    if (!require(dplyr)) install.packages("dplyr")
    if (!require(tidyr)) install.packages("tidyr")
    if (!require(magrittr)) install.packages("magrittr")
    if (!require(rcompanion)) install.packages("rcompanion")
    if (!require(agricolae)) install.packages("agricolae")
    if (!require(rstatix)) install.packages("rstatix")
    
    options(contrasts = c("contr.helmert", "contr.poly"))
})

is_normal <- function(data, formula, alpha = 0.05){
    # if the input data is a vector
    if (length(dim(data)) == 0){
        v <- na.omit(data)
        if (sd(v) == 0) res <- FALSE  # Tied data
        if (sd(v) != 0) res <- shapiro.test(v)$p.value > alpha
    }
    
    # if the input data is a dataframe
    if (is.data.frame(data)){
        aov_mod <- aov(formula = formula, data = data)
        res <- shapiro.test(aov_mod$residuals)$p.value > alpha
    }
    
    return(res)
}

oneway_test <- function(
        data, 
        formula, 
        generate_boxplot = FALSE, 
        only_tukey = FALSE
){
    stopifnot(is.data.frame(data))
    y <- as.character(formula)[2]
    x <- as.character(formula)[3]
    
    df0 <- data.frame(
        y = as.numeric(data[, y, drop = TRUE]),
        x = data[, x, drop = TRUE]
    ) %>% 
        tidyr::drop_na()
    
    is_tied_data <- sd(df0$y) == 0
    
    descriptive_stats <- df0 %>% 
        dplyr::summarise(
            N = length(y),
            AVG = mean(y),
            SD = sd(y),
            MIN = min(y),
            P25 = quantile(y, 0.25),
            MED = median(y),
            P75 = quantile(y, 0.75),
            MAX = max(y),
            .by = x
        ) %>% 
        mutate(
            letter_y_pos = MAX + ((max(MAX) * 1.21 - max(MAX)) * 0.66)
        ) %>% 
        dplyr::arrange(dplyr::desc(AVG))
    
    if (is_tied_data){
        descriptive_stats$letter <- "a"
        descriptive_stats <- descriptive_stats %>% 
            dplyr::rename(indep_var = x)
        res <- list(
            normality = FALSE, 
            homoscedasticity = TRUE, 
            perform_test = NULL,
            pre_hoc = NULL, 
            post_hoc = NULL,
            results = descriptive_stats
        )
        return(res)
    }
    
    # Factor reorder with descending average value
    df0$x <- factor(df0$x, levels = descriptive_stats$x)
    
    # Check data normality and homoscedasticity
    # normality <- df0 %>% 
    #     dplyr::group_by(x) %>% 
    #     dplyr::summarise(p = check_normality(y))
    normality <- is_normal(df0, y ~ x)
    homoscedasticity <- rstatix::levene_test(formula = y ~ x, data = df0)
    # is_normal <- all(normality$p > 0.05)
    var_equal <- homoscedasticity$p > 0.05
    
    if (only_tukey) normality <- var_equal <- TRUE
    
    # Parametric test
    if (normality){
        perform_test <- ifelse(var_equal, "Fisher's ANOVA", "Welch's ANOVA")
        pre_hoc <- oneway.test(y ~ x, df0, var.equal = var_equal)
        pre_hoc_pass <- pre_hoc$p.value < 0.05
        
        # Tukey-HSD
        if (var_equal){
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
        if(!var_equal){
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
    if (!normality){
        perform_test <- "Kruskal-Wallis + Dunn's test"
        
        pre_hoc <- with(
            data = df0,
            expr = kruskal.test(x = y, g = x)
        )
        pre_hoc_pass <- pre_hoc$p.value < 0.05
        
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
    }  # <<<<< Non-parametric test
    
    if (!pre_hoc_pass) cld$letter <- "a"
    
    descriptive_stats <- descriptive_stats %>% 
        dplyr::left_join(cld, by = "x") %>% 
        dplyr::rename(group = x)
    
    p1 <- NULL
    if (generate_boxplot){
        p1 <- df0
        desc_stat <- descriptive_stats
        
        p1$x <- factor(p1$x, levels = sort(as.character(unique(p1$x))))
        desc_stat$group <- factor(desc_stat$group, levels = unique(p1$x))
        
        p1 <- ggplot(p1, aes(x, y)) +
            theme_bw() +
            geom_point(position = position_jitter(width = 0.1)) +
            geom_boxplot(outliers = FALSE, outlier.shape = NA, alpha = 0.2) +
            geom_text(
                data = desc_stat,
                mapping = aes(group, letter_y_pos, label = letter),
                inherit.aes = FALSE,
                size = 6
            ) +
            theme(
                text = element_text(family = "sans", face = "bold", size = 18),
                axis.title.x.bottom = element_text(margin = ggplot2::margin(t = 9)),
                axis.title.y.left = element_text(margin = ggplot2::margin(r = 9))
            )
    }
    
    
    res <- list(
        perform_test = perform_test,
        results = descriptive_stats,
        normality = normality, 
        homoscedasticity = homoscedasticity, 
        pre_hoc = pre_hoc, 
        post_hoc = post_hoc,
        boxplot = p1
    )
    
    return(res)
}