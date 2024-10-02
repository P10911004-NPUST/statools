tukey_hsd <- function(
        data, 
        formula, 
        alpha = 0.05, 
        p_adjust_method = "none",
        comparison_symbol = " |vs| "
){
    y_name <- as.character(formula)[2]
    x_name <- as.character(formula)[3]
    
    df0 <- data.frame(
        y = data[, y_name, drop = TRUE],
        x = data[, x_name, drop = TRUE]
    )
    
    df0 <- df0[!is.na(df0$y), ]
    
    desc_mat <- with(
        data = df0, 
        expr = sapply(
            X = c("mean", "sd", "length", "min", "median", "max"),
            FUN = function(z) tapply(X = y, INDEX = x, FUN = z)
        )
    )
    
    # .is_unbalance <- length(unique(desc_mat[, "length", drop = TRUE])) > 1
    
    fct_order <- order(desc_mat[, 1], decreasing = TRUE)
    desc_mat <- desc_mat[fct_order, ]
    
    aov_mod <- aov(formula, data)
    
    DFerror <- df.residual(aov_mod)
    MSE <- sum(residuals(aov_mod) ^ 2) / DFerror
    
    # Group sample sizes
    groups_n <- desc_mat[, "length", drop = TRUE]
    
    groups_pair <- outer(
        X = rownames(desc_mat), 
        Y = rownames(desc_mat), 
        FUN = function(x1, x2) paste(x1, x2, sep = comparison_symbol)
    )
    groups_pair <- groups_pair[upper.tri(groups_pair)]
    
    tmp_df <- strsplit(groups_pair, comparison_symbol, fixed = TRUE)
    tmp_df <- do.call(rbind.data.frame, tmp_df)
    x1 <- tmp_df[, 1, drop = TRUE]
    x2 <- tmp_df[, 2, drop = TRUE]
    
    groups_diff <- outer(
        X = desc_mat[, "mean"], 
        Y = desc_mat[, "mean"], 
        FUN = "-"
    )
    groups_diff <- groups_diff[upper.tri(groups_diff)]
    
    # groups_q <- groups_diff / sqrt(0.5 * MSE * outer2(1 / groups_n, "+"))
    groups_q <- groups_diff / sqrt(MSE / mean(groups_n))
    
    # if (.is_unbalance) groups_q <- groups_diff / sqrt(MSE / mean(groups_n))
    
    groups_p <- ptukey(abs(groups_q), length(groups_n), DFerror, lower.tail = FALSE)
    
    # Critical value of studentized range
    q_crit <- qtukey(1 - alpha, nrow(desc_mat), DFerror)
    
    # # Minimum significant difference
    # tmp_n <- outer(
    #     X = 1 / groups_n,
    #     Y = 1 / groups_n,
    #     FUN = "+"
    # )
    # tmp_n <- tmp_n[upper.tri(tmp_n)]
    # HSD <- q_crit * sqrt((MSE / 2) * tmp_n)
    HSD <- q_crit * sqrt(MSE / mean(groups_n))
    diff_lwr <- groups_diff - HSD
    diff_upr <- groups_diff + HSD
    
    res <- data.frame(
        comparisons = groups_pair,
        x1 = x1,
        x2 = x2,
        diff = groups_diff,
        lwr = diff_lwr,
        upr = diff_upr,
        pval = groups_p,
        padj = p.adjust(groups_p, p_adjust_method)
    )
    
    return(res)
}

