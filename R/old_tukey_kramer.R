# Tukey-Kramer test


tukey_kramer <- function(data, formula, alpha = 0.05, p_adjust_method = "none") {
    
    p_adjust_method <- match.arg(p_adjust_method, p.adjust.methods)
    y_name <- as.character(formula)[2]
    x_name <- as.character(formula)[3]
    
    df0 <- data.frame(
        y = data[[y_name]],
        x = data[[x_name]]
    )
    
    df0 <- df0[!is.na(df0$y), ]
    
    aov_mod <- aov(formula = y ~ x, data = df0)
    
    
    # Descriptive ====
    group_names <- unique(as.character(df0$x))
    group_means <- tapply(df0$y, df0$x, "mean")
    group_sizes <- tapply(df0$y, df0$x, "length")
    
    # Functions ====
    .outer2 <- function(x, FUN) {
        tmp_mat <- outer(x, x, FUN)
        tmp_mat <- tmp_mat[upper.tri(tmp_mat)]
        return(tmp_mat)
    }
    
    .generate_comparisons <- function(x1, x2){
        paste(x1, x2, sep = " |vs| ")
    }
    
    .calc_qval <- function(x1, x2) {
        n1 <- group_sizes[x1]
        n2 <- group_sizes[x2]

        res <- sqrt( (MSE / 2) * (1 / n1 + 1 / n2) )
        res <- abs(res)

        return(res)
    }
    
    .calc_conf_int <- function(x1, x2) {
        var1 <- group_vars[x1]
        var2 <- group_vars[x2]
        n1 <- group_sizes[x1]
        n2 <- group_sizes[x2]
        q <- stats::qtukey(
            p = 1 - alpha,
            nmeans = length(group_names),
            df = deg_free
        )
        
        res <- q * sqrt(((var1 / n1) + (var2 / n2)) / 2)
        
        return(res)
    }
    
    # Statistics parameters ====
    comparisons <- .outer2(group_names, ".generate_comparisons")
    difference <- .outer2(group_means, "-")
    deg_free <- df.residual(aov_mod)  # degree of freedom within groups (DFerror)
    MSE <- sum(residuals(aov_mod) ^ 2) / deg_free
    confidence_interval <- .outer2(group_names, ".calc_conf_int")
    qval <- .outer2(group_names, ".calc_qval")
    
    pval <- stats::ptukey(
        q = qval,
        nmeans = length(group_names),
        df = deg_free,
        lower.tail = FALSE
    )
    
    # Output result ====
    res <- data.frame(
        comparisons = comparisons,
        diff = difference,
        diff_lower = difference - confidence_interval,
        diff_upper = difference + confidence_interval,
        qval = qval,
        pval = pval,
        padj = p.adjust(pval, p_adjust_method)
    )
    
    return(res)
}




