# ---------- Pairwise grouping of comparisons ----------
.pairwise.grouping <- function(x){
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

