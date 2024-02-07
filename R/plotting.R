if (!require(ggplot2)) install.packages("ggplot2")

hline_grob <- function(xmin, xmax, y){
    ggplot2::annotation_custom(
        grob = grid::linesGrob(),
        xmin = xmin, 
        xmax = xmax, 
        ymin = y, 
        ymax = y
    )
}