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
