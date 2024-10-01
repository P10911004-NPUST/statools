suppressMessages({
    source("./utils.R")
    source("./tukey_hsd.R")
    if (!require(rcompanion)) install.packages("rcompanion")
    if (!require(agricolae)) install.packages("agricolae")
    if (!require(rstatix)) install.packages("rstatix")
    if (!require(ARTool)) install.packages("ARTool")
})


oneway_test <- function(
        data, 
        formula, 
        p_adjust_method = p.adjust.methods,
        use_art = TRUE,  # (FALSE: Kruskal + Dunn) or (TRUE: ART + ART-C) deal with non-normal data
        only_tukey = FALSE,
        generate_boxplot = FALSE
) {
    p_adjust_method <- match.arg(p_adjust_method)
    stopifnot(is.data.frame(data))
    y_name <- as.character(formula)[2]
    x_name <- as.character(formula)[3]
    # y <- as.character(formula)[2]
    # x <- as.character(formula)[3]
    
    df0 <- data.frame(
        y = as.numeric(data[, y_name, drop = TRUE]),
        x = data[, x_name, drop = TRUE]
    ) %>% 
        tidyr::drop_na()
    
    ## Descriptive stats ====
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
            letter_pos = MAX + ((ceiling(max(MAX) * 1.15) - max(MAX)) * 0.43),
            letter_y_pos = MAX + ((max(MAX) * 1.15 - max(MAX)) * 0.66)
        ) %>% 
        dplyr::arrange(dplyr::desc(AVG))
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ## Tied data ====
    is_tied_data <- sd(df0$y) == 0
    
    if (is_tied_data) {
        descriptive_stats$letter <- "a"
        descriptive_stats <- descriptive_stats %>% 
            dplyr::rename(group = x)
        
        res <- list(
            normality = FALSE, 
            homoscedasticity = TRUE, 
            perform_test = NULL,
            pre_hoc = NULL, 
            post_hoc = NULL,
            result = descriptive_stats
        )
        
        # if (generate_boxplot) {
        #     res[["boxplot"]] <- .generate_boxplot(df0, descriptive_stats)
        # }
        if (generate_boxplot) {
            res[["boxplot"]] <- show_boxplot(
                data = df0, 
                formula = y ~ x, 
                cld = setNames(res$result$letter, res$result$group)
            ) +
                labs(
                    x = sym(x_name),
                    y = sym(y_name)
                )
        }
        
        return(res)
    }
    #< Tied data
    
    # Factor reorder with descending average value
    df0$x <- factor(df0$x, levels = descriptive_stats$x)
    
    # Check data normality and homoscedasticity
    normality <- is_normal(df0, y ~ x)
    homoscedasticity <- rstatix::levene_test(formula = y ~ x, data = df0)
    var_equal <- homoscedasticity$p > 0.05
    
    if (only_tukey) normality <- var_equal <- TRUE
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Parametric test ====
    if (normality) {
        perform_test <- ifelse(var_equal, "Fisher's ANOVA", "Welch's ANOVA")
        pre_hoc <- oneway.test(y ~ x, df0, var.equal = var_equal)
        pre_hoc_pass <- pre_hoc$p.value < 0.05
        
        ## Tukey-HSD ====
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
        
        ## Games-Howell ====
        if(!var_equal){
            perform_test <- paste0(perform_test, " + Games-Howell")
            post_hoc <- rstatix::games_howell_test(
                data = df0, 
                formula = y ~ x
            ) %>% 
                dplyr::mutate(comparisons = paste(group1, group2, sep = " - "))
            
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
    }  
    #< Parametric test
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ## Non-parametric test ====
    if (!normality) {
        
        if (use_art) {
            ### Aligned Rank Transform (ART) + ART-Contrast ====
            perform_test <- "Aligned Rank Transform (ART) + ART-Contrast"
            
            art_mod <- art_mod <- ARTool::art(formula = y ~ x, data = df0)
            pre_hoc <- anova(art_mod)
            pre_hoc_pass <- pre_hoc$`Pr(>F)` < 0.05
            
            art_c <- as.data.frame(ARTool::art.con(art_mod, ~ x, adjust = p_adjust_method))
            
            comb_mat <- descriptive_stats %>% 
                dplyr::arrange(desc(MED)) %>% 
                .$x %>% 
                as.character() %>% 
                combn(2)
            
            # If the factor names start with digit, the art.con() will automatically insert an `x` letter;
            # so, the `x` in art_c$contrast will be removed to keep in line with the original name
            art_contrast_name <- str_replace(art_c$contrast[1], "(.*) - (.*)", "\\1")
            if (!art_contrast_name %in% comb_mat[1, ]){
                art_c$contrast <- str_remove_all(art_c$contrast, "x")
            }
            
            # comb1 <- apply(stats_res[1:2], 2, function(x) paste(x, collapse = " - "))
            # comb2 <- apply(stats_res[2:1], 2, function(x) paste(x, collapse = " - "))
            
            lst0 <- list()
            for (i in 1:ncol(comb_mat)) {
                comb_1 <- paste(comb_mat[1, i], comb_mat[2, i], sep = " - ")
                comb_2 <- paste(comb_mat[2, i], comb_mat[1, i], sep = " - ")
                
                match_1 <- grepl(sprintf("^%s$", comb_1), art_c$contrast)
                match_2 <- grepl(sprintf("^%s$", comb_2), art_c$contrast)
                
                p_val <- art_c$p.value[ match_1 | match_2 ]
                lst0[comb_1] <- p_val  # comb_1: combinations based on decending median values
            }
            
            post_hoc <- data.frame(
                comparisons = names(lst0),
                p.adj = list_c(lst0)
            )
            
        } else {
            ### Kruskal-Wallis test + Dunn's test ====
            perform_test <- "Kruskal-Wallis + Dunn's test"
            
            pre_hoc <- with(
                data = df0,
                expr = kruskal.test(x = y, g = x)
            )
            pre_hoc_pass <- pre_hoc$p.value < 0.05
            
            post_hoc <- rstatix::dunn_test(
                data = df0, 
                formula = y ~ x,
                p.adjust.method = p_adjust_method
            ) %>% 
                dplyr::mutate(
                    comparisons = paste(group1, group2, sep = "-")
                )  
        }
        
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
    #<<<<< Non-parametric test
    
    # Compact letter display ====
    # cld <- rcompanion::cldList(
    #     formula = p.adj ~ comparisons, 
    #     data = post_hoc, 
    #     remove.zero = FALSE
    # )
    # 
    # cld <- data.frame(
    #     x = cld[, "Group"],
    #     letter = cld[, "Letter", drop = TRUE]
    # )
    
    if (!pre_hoc_pass) cld$letter <- "a"
    
    descriptive_stats <- descriptive_stats %>% 
        dplyr::left_join(cld, by = "x") %>% 
        dplyr::rename(group = x)
    #<<<<< compact letter display
    
    res <- list(
        perform_test = perform_test,
        result = descriptive_stats,
        normality = normality, 
        homoscedasticity = homoscedasticity, 
        pre_hoc = pre_hoc, 
        post_hoc = post_hoc,
        boxplot = NULL,
        cld = cld
    )
    
    ## Generate boxplot ====
    if (generate_boxplot) {
        res[["boxplot"]] <- show_boxplot(
            data = df0, 
            formula = y ~ x, 
            cld = setNames(res$result$letter, res$result$group)
        ) +
            labs(
                x = sym(x_name),
                y = sym(y_name)
            )
    }
    
    return(res)
}

