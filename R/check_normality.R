if (!require(dplyr)) install.packages("dplyr")
if (!require(car)) install.packages("car")

check_normality <- function(x){
    v <- na.omit(x)
    if (sd(v) == 0) normality <- TRUE
    if (sd(v) != 0) normality <- shapiro.test(v)$p.value
    return(normality)
}

