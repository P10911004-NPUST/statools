source("./oneway_test.R")

data("iris")
stats <- iris %>% oneway_test(Petal.Length ~ Species, generate_boxplot = TRUE)
stats$boxplot
stats_res <- stats$result

# show_boxplot(iris, Petal.Length ~ Species)
