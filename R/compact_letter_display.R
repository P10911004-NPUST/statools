# Compact letter display (CLD)

## Piepho, H.P. (2004). 
## An Algorithm for a Letter-Based Representation of All-Pairwise Comparisons. 
## Journal of Computational and Graphical Statistics 13, 456–466. https://doi.org/10.1198/1061860043515.

compact_letter_display <- function(
        groups, 
        means, 
        comparisons, 
        pval,
        alpha = 0.05,
        comparison_symbol = " |vs| ",
        display_symbols = base::letters,
        descending = TRUE
){
    if (!isTRUE(descending) & !isFALSE(descending)) descending <- TRUE
    
    if (any(grepl(comparison_symbol, groups))) 
        stop("Comparison symbol should not exists in group names")
    
    comparisons <- strsplit(comparisons, comparison_symbol, fixed = TRUE)
    comparisons <- do.call(rbind.data.frame, comparisons)
    x1 <- comparisons[, 1, drop = TRUE]
    x2 <- comparisons[, 2, drop = TRUE]
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ## Generate a NULL matrix ====
    ## the row and col names are sorted by the mean-values of the groups
    ## the NULL matrix is filled with NAs
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    sorted_groups <- names(sort(setNames(means, groups), decreasing = descending))
    null_mat <- matrix(
        nrow = length(sorted_groups), 
        ncol = length(sorted_groups), 
        dimnames = list(sorted_groups, sorted_groups)
    )
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ## Fill in p-values ====
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    pval_mat <- null_mat
    for (i in seq_along(x1)){
        pval_mat[x1[i], x2[i]] <- pval[i]
    }
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ## Insertion step ====
    ## Non-significant comparisons were marked as TRUE,
    ## So later, non-significant comparing-pairs will be inserted with same letters.
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    bool_mat <- t(pval_mat >= alpha)
    bool_mat[is.na(bool_mat)] <- TRUE
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ## Absorption preparation ====
    ## NOT performed yet, just memorizing which columns have to be discarded
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    is_redundant <- c()
    for (n in 1:(ncol(bool_mat))){
        `mat_n` <- bool_mat[, n]
        `mat_1:n-1` <- bool_mat[, 1:(n-1), drop = FALSE]  
        is_redundant <- append(
            is_redundant, 
            any(apply(`mat_1:n-1`, 2, function(x) identical(`mat_n`, x)))
        )
        # Note: When n = 1, the first column of the `mat_1:n-1` is always identical with the `mat-1`
        # So, implement a `FALSE` to the first element of the `is_redundant` vector after the loop end
    }
    is_redundant[1] <- FALSE
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ## Remove duplicate comparison-pairs
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    bool_mat[upper.tri(bool_mat)] <- FALSE
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ## Absorption step ====
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    bool_mat <- bool_mat[, !is_redundant]
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ## Substitute with letters ====
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    letter_mat <- bool_mat
    for (i in 1:ncol(bool_mat)){
        insert_letter <- bool_mat[, i, drop = TRUE]
        letter_mat[, i] <- ifelse(
            test = insert_letter, 
            yes = display_symbols[i], 
            no = ""
        )
    }
    
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ## Output
    ## The matrix will be reduced to a named-vector 
    ## after row-wise collapsing the letter-columns
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    res <- apply(letter_mat, 1, function(x) paste(x, collapse = ""))
    res <- res[groups]
    
    return(res)
}


cld <- compact_letter_display
