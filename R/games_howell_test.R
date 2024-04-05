if (!require(tidyr)) install.packages("tidyr")
if (!require(dplyr)) install.packages("dplyr")
if (!require(rstatix)) install.packages("rstatix")
if (!require(rcompanion)) install.packages("rcompanion")

source("./pairwise_grouping.R", chdir = TRUE)


# -------------------- games's test --------------------
games_howell_test <- function(
        formula, 
        data,
        alpha = 0.05
){
    x <- as.character(formula)[3]
    y <- as.character(formula)[2]
    
    data <- data %>% 
        rename(y = all_of(y), trt = all_of(x))
    
    desc_df <- data %>%
        tidyr::drop_na() %>% 
        dplyr::group_by(trt) %>% 
        dplyr::summarise(avg = mean(y)) %>%
        # summarise(avg = mean(!!!syms(y))) %>% 
        dplyr::ungroup() %>% 
        dplyr::arrange(desc(avg))
    
    group_max2min <- .pairwise.grouping(desc_df[, "trt", drop = TRUE])
    
    games_df <- rstatix::games_howell_test(
        data = data,
        formula = y ~ trt,
        conf.level = 1 - alpha
    )
    
    
    games_df$comparisons <- ""
    for (i in 1:nrow(group_max2min)){
        for (j in 1:nrow(games_df)){
            mm <- c(group_max2min[i, "key1"], group_max2min[i, "key2"])
            games_group <- c(games_df[j, "group1"], games_df[j, "group2"])
            if (all(mm %in% games_group))
                games_df[j, "comparisons"] <- paste(mm, collapse = " - ")
        }
    }
    
    games_df <- games_df[match(group_max2min$comparisons, games_df$comparisons), ]
    
    games_letter <- games_df %>% 
        rcompanion::cldList(
            formula = p.adj ~ comparisons,
            data = .,
            threshold = 0.05,
            remove.zero = FALSE
        )
    
    return(list(df = games_df, groups = games_letter))
}



.games_howell <- function(grp, obs) {
    
    #Create combinations
    combs <- combn(unique(grp), 2)
    
    # Statistics that will be used throughout the calculations:
    # n = sample size of each group
    # groups = number of groups in data
    # Mean = means of each group sample
    # std = variance of each group sample
    n <- tapply(obs, grp, length)
    groups <- length(n)
    Mean <- tapply(obs, grp, mean)
    std <- tapply(obs, grp, var)
    
    statistics <- lapply(
        
        X = 1:ncol(combs), 
        
        FUN = function(x){
            
            mean.diff <- Mean[combs[2,x]] - Mean[combs[1,x]]
            
            #t-values
            t <- abs(Mean[combs[1,x]] - Mean[combs[2,x]]) / sqrt((std[combs[1,x]] / n[combs[1,x]]) + (std[combs[2,x]] / n[combs[2,x]]))
            
            # Degrees of Freedom
            df <- (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]])^2 / # Numerator Degrees of Freedom
                ((std[combs[1,x]] / n[combs[1,x]])^2 / (n[combs[1,x]] - 1) + # Part 1 of Denominator Degrees of Freedom 
                     (std[combs[2,x]] / n[combs[2,x]])^2 / (n[combs[2,x]] - 1)) # Part 2 of Denominator Degrees of Freedom
            
            #p-values
            p <- ptukey(t * sqrt(2), groups, df, lower.tail = FALSE)
            
            # Sigma standard error
            se <- sqrt(0.5 * (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]]))
            
            # Upper Confidence Limit
            upper.conf <- lapply(1:ncol(combs), function(x) {
                mean.diff + qtukey(p = 0.95, nmeans = groups, df = df) * se
            })[[1]]
            
            # Lower Confidence Limit
            lower.conf <- lapply(1:ncol(combs), function(x) {
                mean.diff - qtukey(p = 0.95, nmeans = groups, df = df) * se
            })[[1]]
            
            # Group Combinations
            grp.comb <- paste(combs[1,x], ':', combs[2,x])
            
            # Collect all statistics into list
            stats <- list(grp.comb, mean.diff, se, t, df, p, upper.conf, lower.conf)
        }
    )
    
    # Unlist statistics collected earlier
    stats.unlisted <- lapply(statistics, function(x) {
        unlist(x)
    })
    
    # Create dataframe from flattened list
    results <- data.frame(matrix(unlist(stats.unlisted), nrow = length(stats.unlisted), byrow=TRUE))
    
    # Select columns set as factors that should be numeric and change with as.numeric
    results[c(2, 3:ncol(results))] <- round(as.numeric(as.matrix(results[c(2, 3:ncol(results))])), digits = 3)
    
    # Rename data frame columns
    colnames(results) <- c('groups', 'Mean Difference', 'Standard Error', 't', 'df', 'p', 'upper limit', 'lower limit')
    
    return(results)
}



######################### Testing ##################################
if (FALSE){
    data(iris)
    
    x <- iris$Species
    y <- iris$Sepal.Length
    
    combs <- combn(unique(x), 2)
    n <- tapply(y, x, length)
    groups <- length(n)
    avg <- tapply(y, x, mean)
    std <- tapply(y, x, var)
    
    
    games_howell(grp = iris$Species, obs = iris$Sepal.Length)
}






