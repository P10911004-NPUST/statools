if (!require(dplyr)) install.packages("dplyr")
if (!require(car)) install.packages("car")

check_normality <- function(x){
    v <- na.omit(x)
    normality <- shapiro.test(v)$p.value
    return(normality)
}

