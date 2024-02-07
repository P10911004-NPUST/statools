if (!require(dplyr)) install.packages("dplyr")
if (!require(rstatix)) install.packages("rstatix")
if (!require(dunn.test)) install.packages("dunn.test")
if (!require(rcompanion)) install.packages("rcompanion")

source("./pairwise_grouping.R", chdir = TRUE)


# -------------------- Dunn's test --------------------
dunn_test <- function(
        formula, 
        data, 
        p.adjust.method = "BH", 
        two.sided = FALSE
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
    
    if (two.sided){
        dunn_df <- rstatix::dunn_test(
            data = data,
            formula = y ~ trt,
            p.adjust.method = p.adjust.method,
            detailed = TRUE
        )
    }else{
        dunn_df <- with(
            data = data,
            expr = dunn.test::dunn.test(
                x = y,
                g = trt,
                method = p.adjust.method
            )
        ) %>% 
            as.data.frame() %>% 
            mutate(
                group1 = str_split_i(comparisons, " - ", 1),
                group2 = str_split_i(comparisons, " - ", 2)
            ) %>% 
            rename(p.adj = P.adjusted)
    }
    
    
    dunn_df$comparisons <- "ns"
    for (i in 1:nrow(group_max2min)){
        for (j in 1:nrow(dunn_df)){
            mm <- c(group_max2min[i, "key1"], group_max2min[i, "key2"])
            dunn_group <- c(dunn_df[j, "group1"], dunn_df[j, "group2"])
            if (all(mm %in% dunn_group))
                dunn_df[j, "comparisons"] <- paste(mm, collapse = " - ")
        }
    }
    
    dunn_df <- dunn_df[match(group_max2min$comparisons, dunn_df$comparisons), ]
    
    dunn_letter <- dunn_df %>% 
        rcompanion::cldList(
            formula = p.adj ~ comparisons,
            data = .,
            threshold = 0.05,
            remove.zero = FALSE
        )
    
    return(list(df = dunn_df, groups = dunn_letter))
}

















