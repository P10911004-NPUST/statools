suppressMessages({
    if (!require(dplyr)) install.packages("dplyr")
    if (!require(tidyr)) install.packages("tidyr")
    if (!require(stringr)) install.packages("stringr")
    if (!require(purrr)) install.packages("purrr")
    if (!require(magrittr)) install.packages("magrittr")
    if (!require(ggplot2)) install.packages("ggplot2")
    if (!require(rcompanion)) install.packages("rcompanion")
    if (!require(agricolae)) install.packages("agricolae")
    if (!require(rstatix)) install.packages("rstatix")
    if (!require(ARTool)) install.packages("ARTool")
})

p25 <- function(x){
    quantile(x, 0.25)
}

p75 <- function(x){
    quantile(x, 0.75)
}

is_balance <- function(data, formula){
    x_name <- as.character(formula)[3]
    y_name <- as.character(formula)[2]
    df0 <- data.frame(
        x = data[[x_name]],
        y = data[[y_name]]
    )
    n <- unname(with(df0, tapply(y, x, "length")))
    res <- length(unique(n)) == 1
    return(res)
}

is_unbalance <- function(data, formula){
    x_name <- as.character(formula)[3]
    y_name <- as.character(formula)[2]
    df0 <- data.frame(
        x = data[[x_name]],
        y = data[[y_name]]
    )
    n <- unname(with(df0, tapply(y, x, "length")))
    res <- length(unique(n)) > 1
    return(res)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> Check data normality
is_normal <- function(data, formula, alpha = 0.05) {
    # if the input data is a vector
    if (length(dim(data)) == 0) {
        warning("The formula argument was ignored with the vector input.")
        v <- na.omit(data)
        
        is_named_vector <- !is.null(names(v))
        if (is_named_vector) 
            warning("Named vector detected.\nPlease input as a dataframe if grouping is neccesary.")
        
        if (sd(v) == 0) {
            res <- FALSE  # if it is a tied-data
            warning("This is a tied-data.")
        } else {
            res <- shapiro.test(v)$p.value > alpha
        }
    }
    
    # if the input data is a dataframe
    if (is.data.frame(data)) {
        aov_mod <- aov(formula = formula, data = data)
        res <- shapiro.test(aov_mod$residuals)$p.value > alpha
    }
    
    return(res)
}
#< is_normal()


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> Estimate the position for compact letter display
estimate_letter_pos <- function(x){
    MAX <- max(x)
    letter_pos <- MAX + ((ceiling(max(MAX) * 1.15) - max(MAX)) * 0.43)
    return(letter_pos)
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> Plot a boxplot with compact letter display (cld)
#> The cld should be a named vector, for example, c(level_01 = "a", level_02 = "b")
show_boxplot <- function(
        data, 
        formula,
        cld = NULL, 
        color = NULL,
        point_size = 3,
        triangle_size = 3,
        letter_size = 8,
        letter_pos_adjust = 0,
        x_label_order = NULL
){
    x_name <- as.character(formula)[3]
    y_name <- as.character(formula)[2]
    
    df0 <- data.frame(
        x = data[[x_name]],
        y = data[[y_name]]
    )
    
    if (is.null(x_label_order)) x_label_order <- sort(unique(as.character(df0$x)))
    factor_missing <- any(!(x_label_order %in% unique(df0$x)))
    if (factor_missing) warning("Factor not match")
    df0$x <- factor(df0$x, levels = x_label_order)
    
    if (!is.null(color)) df0$color <- data[[color]]
    
    p1 <- ggplot(df0, aes(x, y, color = color)) +
        theme_bw() +
        labs(
            x = sym(x_name),
            y = sym(y_name)
        ) +
        geom_point(
            size = point_size,
            position = position_jitter(width = 0.1),
            alpha = 0.5,
            show.legend = FALSE
        ) +
        geom_boxplot(
            outliers = FALSE,
            outlier.colour = "transparent",
            outlier.size = 0,
            outlier.shape = NA,
            fill = NA,
            size = 1,
            linewidth = 0.5
        ) +
        stat_summary(
            geom = "point",
            fun = "mean",
            size = triangle_size,
            color = "black",
            shape = 17,
            alpha = 0.7
        ) +
        theme(
            text = element_text(family = "sans", face = "bold", size = 21),
            axis.title.x.bottom = element_text(margin = ggplot2::margin(t = 9)),
            axis.title.y.left = element_text(margin = ggplot2::margin(r = 9)),
            legend.position = "none"
        )
    
    if (!is.null(cld)){
        cld <- data.frame(
            x = names(cld),
            letter = unname(cld)
        )
        
        letter_pos <- df0 %>% 
            summarise(
                letter_pos = estimate_letter_pos(y),
                .by = x
            ) %>% 
            left_join(cld, by = "x")
        
        # letter_pos$letter <- unname(cld[match(names(cld), letter_pos[["x"]])])
        
        p1 <- p1  +
            geom_text(
                data = letter_pos,
                mapping = aes(x, letter_pos + letter_pos_adjust, label = letter),
                inherit.aes = FALSE,
                size = letter_size
            )
    }
    
    return(p1)
}


