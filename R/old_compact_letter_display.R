# Compact letter display (CLD)
# Piepho, H.-P. (2004). An Algorithm for a Letter-Based Representation of All-Pairwise Comparisons. 
# Journal of Computational and Graphical Statistics 13, 456–466. https://doi.org/10.1198/1061860043515.

compact_letter_display <- function(
        groups, 
        means, 
        comparisons, 
        pval,
        pval_threshold = 0.05,
        descending = TRUE,  # TRUE: Assign the letter_symbols from the highest to the lowest means values
        comparison_symbol = " |vs| ",
        letter_symbols = letters
){
    # Separate the comparisons pairs, 
    # exp. c("a |vs| b", "a |vs| c", "c |vs| d") => c("a", "a", "c") and c("b", "c", "d")
    comparisons <- strsplit(comparisons, comparison_symbol, fixed = TRUE)
    comparisons <- do.call(rbind.data.frame, comparisons)
    x1 <- comparisons[, 1, drop = TRUE]
    x2 <- comparisons[, 2, drop = TRUE]
    
    # Generate a NULL matrix, 
    # the row and col names are sorted by the mean-values of the groups
    # the NULL matrix is filled with NAs
    sorted_groups <- names(sort(setNames(means, groups), decreasing = descending))
    null_mat <- matrix(
        nrow = length(sorted_groups), 
        ncol = length(sorted_groups), 
        dimnames = list(sorted_groups, sorted_groups)
    )
    
    # Fill in p-value 
    pval_mat <- null_mat
    for (i in seq_along(x1)){
        pval_mat[x1[i], x2[i]] <- pval[i]
    }
    
    # Non-significant comparisons were marked as TRUE,
    # So later, non-significant comparing-pairs will be inserted with same letters.
    bool_mat <- t(pval_mat >= pval_threshold)
    bool_mat[is.na(bool_mat)] <- TRUE
    
    # Absorption preparation
    # NOT performed yet, just memorizing which columns have to be discarded after the insertion step
    is_redundant <- c()
    for (n in 1:(ncol(bool_mat))){
        `mat_n` <- bool_mat[, n]
        `mat_n-1` <- bool_mat[, 1:(n-1), drop = FALSE]  
        is_redundant <- append(
            is_redundant, 
            any(apply(`mat_n-1`, 2, function(x) identical(`mat_n`, x)))
        )
        # Note: When n = 1, the first column of the `mat_n-1` is always identical with the `mat-1`
        # So, implement a `FALSE` to the first element of the `is_redundant` vector after the loop end
    }
    is_redundant[1] <- FALSE
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Insertion step ====
    # Insert 1 and 0 first, not letters, because its quite un-handy to rearrange the letter
    # after removing the redundant columns
    tmp_mat <- bool_mat
    for (i in 1:ncol(bool_mat)){
        tmp_mat[, i] <- ifelse(bool_mat[, i], 1, 0)
        if (i > 1) tmp_mat[(1:i-1), i] <- 0
    }
    #<<<<< End of insertion
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Absorption step ====
    # Now we perform absoption here, according the previous column index
    tmp_mat <- tmp_mat[, !is_redundant]
    #<<<<< End of absorption
    
    # After absorption, substitute the 1 to `letter_symbols` according to the columns number
    letter_mat <- tmp_mat
    for (i in 1:ncol(tmp_mat)){
        substitute_column <- tmp_mat[, i, drop = TRUE]
        letter_mat[, i] <- ifelse(
            test = (substitute_column == 1), 
            yes = letter_symbols[i], 
            no = ""
        )
    }
    
    # The matrix will be reduced to a named-vector after row-wise collapsing the letter-columns
    letter_mat <- apply(letter_mat, 1, function(x) paste(x, collapse = ""))
    
    return(letter_mat)
}


cld <- compact_letter_display














