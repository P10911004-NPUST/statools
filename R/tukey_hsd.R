tukey_hsd <- function(data, formula, alpha = 0.05){
    
    q25 <- function(x) quantile(x, 0.25)
    q75 <- function(x) quantile(x, 0.75)
    
    outer2 <- function(X, FUN){
        v <- outer(X, X, FUN)
        v <- v[upper.tri(v)]
        return(v)
    }
    
    y <- as.character(formula)[2]
    x <- as.character(formula)[3]
    
    df0 <- data.frame(
        response_var = data[, y],
        independant_var = data[, x]
    )
    
    df0 <- df0[!is.na(df0$response_var), ]
    
    desc_mat <- with(df0, sapply(
        X = c("mean", "sd", "length", "min", "q25", "median", "q75", "max"),
        FUN = function(x) tapply(X = response_var, INDEX = independant_var, FUN = x)
    ))
    
    unbalance <- length(unique(desc_mat[, "length"])) > 1
    
    fct_order <- order(desc_mat[, 1], decreasing = TRUE)
    desc_mat <- desc_mat[fct_order, ]
    
    aov_mod <- aov(formula, data)
    
    DFerror <- df.residual(aov_mod)
    MSE <- sum(residuals(aov_mod) ^ 2) / DFerror
    
    # Group sample sizes
    groups_n <- desc_mat[, 3]
    
    # Critical value of studentized range
    q_crit <- qtukey(1 - alpha, nrow(desc_mat), DFerror)
    
    groups_pair <- outer2(rownames(desc_mat), function(x, y) paste(x, y, sep = "-"))
    groups_diff <- outer2(desc_mat[, 1], "-")
    
    groups_q <- groups_diff / sqrt((MSE / 2) * outer2(1 / groups_n, "+"))
    if (unbalance) groups_q <- groups_diff / sqrt(MSE * mean(1 / groups_n))
    groups_p <- ptukey(abs(groups_q), length(groups_n), DFerror, lower.tail = FALSE)
    
    # Minimum significant difference
    HSD <- q_crit * sqrt((MSE / 2) * outer2(1 / groups_n, "+"))
    diff_lwr <- groups_diff - HSD
    diff_upr <- groups_diff + HSD
    
    res <- list(
        descriptive = desc_mat,
        results = data.frame(
            comparisons = groups_pair,
            diff = groups_diff,
            lwr = diff_lwr,
            upr = diff_upr,
            pval = groups_p
        )
    )
    
    cat("\nFactors:", rownames(desc_mat), "\n")
    cat("\nDFerror:", DFerror, "\n")
    cat("\nMSE:", MSE, "\n")
    cat("\nCritical Value of Studentized Range:", q_crit, "\n")
    cat("\nMinimun Significant Difference:", HSD, "\n\n")
    
    return(res)
}



