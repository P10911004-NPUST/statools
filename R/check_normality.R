if (!require(dplyr)) install.packages("dplyr")
if (!require(car)) install.packages("car")
options(contrasts = c("contr.helmert", "contr.poly"))

check_normality <- function(x){
    v <- na.omit(x)
    if (sd(v) == 0) normality <- TRUE
    if (sd(v) != 0) normality <- shapiro.test(v)$p.value
    return(normality)
}

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







