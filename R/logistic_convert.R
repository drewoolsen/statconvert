logistic_convert <- function(test_obj) {
  vars <- colnames(test_obj[["data"]])
  response_variable <- test_obj[["formula"]][2][[1]]
  predictors <- vars[vars != response_variable]

  odds_ratios <- exp(test_obj[["coefficients"]])
  upper_error_bound <- exp(test_obj[["coefficients"]])

  percent <- round((1-odds_ratios)*100, 3)
  changed <- ifelse(percent > 0, "increased", "decreased")
  e.344 +/- 1.96*.156


  cat("Logistic regression was used to analyze the relationship between ")
  for (var in predictors) {
    cat(paste0(var, ", "))
  }
  cat(paste0("and ", response_variable, ".\n\n"))

  for (var in predictors) {
    cat(paste0("It was found that, holding all other predictor variables constant, the odds of ",
               response_variable,
               " occurring ",
               changed[var],
               " by ",
               abs(percent[var]),
               "% (95% CI [Lower Limit, Upper Limit]) for a one-unit increase in ",
               var, ".\n\n"))
  }

}

breast_cancer <- read.csv("/Users/loaner/Desktop/breastcancer.csv")
breast_cancer$Class <- factor(breast_cancer$Class)
b <- glm(formula=Class~., family=binomial(), data=breast_cancer)
b
summary(b)
logistic_convert(b)
