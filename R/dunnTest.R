if (!require(dplyr)) install.packages("dplyr")
if (!require(rstatix)) install.packages("rstatix")
if (!require(dunn.test)) install.packages("dunn.test")
if (!require(rcompanion)) install.packages("rcompanion")


# ---------- Pairwise grouping of comparisons ----------
.pairwise.comparisons <- function(x){
    sort_group <- data.frame()
    for (i in 1:length(x)){
        for (j in (i + 1):length(x)){
            if (j > length(x)) next
            v <- data.frame(key1 = x[i], key2 = x[j])
            sort_group <- rbind(sort_group, v) 
        }
    }
    sort_group <- sort_group %>% 
        filter(key1 != key2) %>% 
        mutate(comparisons = paste(key1, key2, sep = " - "))
    
    return(sort_group)
}


# -------------------- Dunn's test --------------------
dunnTest <- function(
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
    
    group_max2min <- .pairwise.comparisons(desc_df[, "trt", drop = TRUE])
    
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
    
    
    dunn_df$comparisons <- ""
    for (i in 1:nrow(group_max2min)){
        for (j in 1:nrow(dunn_df)){
            mm <- group_max2min[i, c("key1", "key2")]
            dunn_group <- dunn_df[j, c("group1", "group2")]
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

















