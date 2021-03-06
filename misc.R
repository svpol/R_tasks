# Task taken from: https://stepik.org/lesson/11508/step/5?unit=2531
# Write corr.calc function which accepts a data.frame with 2 numeric columns, calculates Pearson correlation coefficient and returns a vector of 2 values: correlation coefficient and p-value.
corr.calc <- function(test_data){
  fit <- cor.test(test_data[[1]], test_data[[2]])
  return(c(fit$estimate, fit$p.value))
}

# Task taken from: https://stepik.org/lesson/11508/step/12?unit=2531
# There is a data frame with 2 columns of numeric data. Buid linear regression where the first column is a dependent variable and the second one is independent.
# Calculate the values of regression coefficients: intercept and slope.
lin_mod <- function(test_data){
  fit <- lm(test_data[[1]] ~ test_data[[2]])
  return(fit$coefficients)
}

# Task taken from: https://stepik.org/lesson/26186/step/4?thread=solutions&unit=8128
# Write most_significant function which accepts a dataframe with any number of variables, where each variable is a nucleotide sequence.
# The function should check the null hypothesis that all nucleotides (A, T, G, C) can be met within the sequense with equal probability.
# The function shoul return a vector with the name(s) of the variable(s) where the minimal p-value was calculated for chi-square check of the null hypothesis.
most_significant <- function(df){
  p_values <- apply(df, 2, function(x) chisq.test(table(x))$p.value)
  min_p <- which(p_values == min(p_values))
  return(colnames(df)[min_p])
}
                    
# Task taken from: https://stepik.org/lesson/31102/step/5?unit=11515
# Write get_negative_values function which accepts a dataframe of any size with numeric variables. The function should check whether there are negative values for each variable. For each variable having negative values the function should return its negative values as a vector or a matrix if the number of negative values is the same for each variable.
# E. g., for this input: df <- as.data.frame(list(V1 = c(NA, 0.5, 0.7, 8), V2 = c(-0.3, NA, 2, 1.2), V3 = c(2, -1, -5, -1.2))) the output will be:
# $V2
# [1] -0.3
#
# $V3
# [1] -1.0 -5.0 -1.2
get_negative_values <- function(df) {
  neg_data <- apply(df, 2, function(x) x[x < 0 & is.na(x) == FALSE])
  neg_data <- neg_data[sapply(neg_data,length)>0]
  return(neg_data)
}

# Task taken from: https://stepik.org/lesson/11508/step/7?unit=2531
# Write function smart_cor which accept a dataframe with two numeric variables. Using Shapiro-Wilk's test check that data of both variables are normally distributed.
# If at least one data vector differs from normal distribution (p-value < 0.05) the function should return  Spearman correlation coefficient (a numeric vector of one element).
# If the data of both vectors are normally distributed the function should return Pearson correlation coefficient.                 
smart_cor <- function(test_data){
  if (shapiro.test(test_data[[1]])$p.value < 0.05 | shapiro.test(test_data[[2]])$p.value < 0.05){
    return(cor.test(test_data[[1]], test_data[[2]], method="spearman")$estimate)
  } else {
    return(cor.test(test_data[[1]], test_data[[2]], method="pearson")$estimate)
  }
}

# Task taken from: https://stepik.org/lesson/11508/step/15?unit=2531
# Build a scatterplot on the built-in data iris and save it to the variable my_plot. 
# X-axis - variable Sepal.Width
# Y-axis - variable Petal.Width
# Dot color - variable Species
# Also add linear smoothing for each group of observation by the variable Species.
# The plot should look like this: https://github.com/svpol/R_tasks/blob/main/iris_plot.png
library(ggplot2)

my_plot <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, group = Species, col = Species))+
  geom_point(size = 3)+
  geom_smooth(method = "lm") 
