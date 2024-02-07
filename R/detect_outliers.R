if (!require(dplyr)) install.packages("dplyr")

detect_outliers <- function(
        x,
        method = "sd", 
        n.sd = 3, 
        trim = 0.1,
        lower_quantile = 0.25,
        upper_quantile = 0.75,
        
        use_median = FALSE,
        returnBool = TRUE
){
    .detect_with_mean <- function(x){
        avg <- mean(x, na.rm = TRUE, trim = trim)
        ut <- quantile(x, probs = 1 - (trim / 2), na.rm = TRUE)
        lt <- quantile(x, probs = trim / 2, na.rm = TRUE)
        return(!dplyr::between(x, lt, ut))
    }
    
    .detect_with_sd <- function(x){
        if (use_median){
            med <- median(x, na.rm = TRUE)
            stdev <- sd(x, na.rm = TRUE)
            ut <- med + n.sd * stdev
            lt <- med - n.sd * stdev
            return(!dplyr::between(x, lt, ut)) 
        }else{
            avg <- mean(x, na.rm = TRUE)
            stdev <- sd(x, na.rm = TRUE)
            ut <- avg + n.sd * stdev
            lt <- avg - n.sd * stdev
            return(!dplyr::between(x, lt, ut)) 
        }
    }
    
    .detect_with_iqr <- function(x){
        lt <- quantile(x, lower_quantile)
        ut <- quantile(x, upper_quantile)
        IQR <- ut - lt
        return(!dplyr::between(x, (lt - 1.5 * IQR), (ut + 1.5 * IQR)))
    }
    
    if (is.vector(x)){
        x <- na.omit(x)
        if (tolower(method) == "mean") res <- .detect_with_mean(x)
        if (tolower(method) == "sd") res <- .detect_with_sd(x)
        if (tolower(method) == "iqr") res <- .detect_with_iqr(x)
    }
    
    if (returnBool) return(res)
    if (!returnBool) return(x[res])
}

