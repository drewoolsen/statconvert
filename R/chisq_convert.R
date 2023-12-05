# example from the paper:
# A chi-square test of independence was performed to examine the relation
# between religion and college interest. The relation between these variables
# was significant, X2 (2, N = 170) = 14.14, p <.01
# here I am assuming that "170" is the number of observations
# i am not positive of that though
# TODO: should we include degrees of freedom and # of observations?
# TODO: any more info to include?

chisq_convert <- function(test_obj) {
  if (test_obj$p.value < .001) {
    test_obj$p.value.text <- "< .001"
  } else if (test_obj$p.value < .005) {
    test_obj$p.value.text <- "< .005"
  } else if (test_obj$p.value < .01) {
    test_obj$p.value.text <- "< .01"
  } else if (test_obj$p.value < .05) {
    test_obj$p.value.text <- "< .05"
  } else {
    test_obj$p.value.text <- paste("=", round(test_obj$p.value, 3))
  }

  test_obj$statistic.text <- round(test_obj$statistic, 3)
  vars <- test_obj$data.name
  signif <- test_obj$p.value < .05
  data_string <- paste0("X² (df = ", test_obj$parameter, ") = ",
        test_obj$statistic.text, ", p ", test_obj$p.value.text)

  if (grepl("Pearson's Chi-squared test", test_obj$method[1], fixed=TRUE)) {
    is_significant <- ifelse(signif, "", "not ")
    cat("A chi-square test of independence was performed.\n")
    cat("The relation between ", vars[[1]][1], " is ", is_significant,
        "significant, ", data_string, ".\n", sep="")

  } else if (test_obj$method[1] == "Chi-squared test for given probabilities") {
    is_distributed <- ifelse(signif, "not ", "")

    # is the distribution uniform?
    if (var(test_obj$expected) == 0) is_uniform = "uniformly"
    else is_uniform = "according to the distribution provided"

    cat("A chi-square test of goodness-of-fit was performed.\n")
    cat("The elements of ", vars, " are ", is_distributed, "distributed ",
        is_uniform, ", ", data_string, ".\n", sep="")
  }

  else stop()

}

