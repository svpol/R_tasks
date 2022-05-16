# Task taken from: https://stepik.org/lesson/11508/step/5?unit=2531
# Write corr.calc function which accept a data.frame with 2 numeric columns, calculates Pearson correlation coefficient and returns a vector of 2 values: correlation coefficient and p-value.

corr.calc <- function(test_data){
  fit <- cor.test(test_data[[1]], test_data[[2]])
  return(c(fit$estimate, fit$p.value))
}
