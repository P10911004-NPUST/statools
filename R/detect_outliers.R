if (!require(dplyr)) install.packages("dplyr")

detect_outliers <- function(
        x,
        method = "sd", 
        n.sd = 3, 
        trim = 0.1,
        returnValues = FALSE
){
    .detect_with_mean <- function(x){
        avg <- mean(x, na.rm = TRUE, trim = trim)
        ut <- quantile(x, probs = 1 - (trim / 2), na.rm = TRUE)
        lt <- quantile(x, probs = trim / 2, na.rm = TRUE)
        return(!dplyr::between(x, lt, ut))
    }
    
    .detect_with_sd <- function(x){
        avg <- mean(x, na.rm = TRUE)
        stdev <- sd(x, na.rm = TRUE)
        ut <- avg + n.sd * stdev
        lt <- avg - n.sd * stdev
        return(!dplyr::between(x, lt, ut))
    }
    
    if (is.vector(x)){
        if (method == "mean") res <- .detect_with_mean(x)
        if (method == "sd") res <- .detect_with_sd(x)
    }
    
    if (!returnValues) return(res)
    if (returnValues) return(x[res])
}

