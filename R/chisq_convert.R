# example from the paper:
# A chi-square test of independence was performed to examine the relation
# between religion and college interest. The relation between these variables
# was significant, X2 (2, N = 170) = 14.14, p <.01
# here I am assuming that "170" is the number of observations
# i am not positive of that though
# TODO: should we include degrees of freedom and # of observations?

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
  signif <- ifelse(test_obj$p.value < .05, "significant", "not significant")

  if (grepl("Pearson's Chi-squared test", test_obj$method[1], fixed=TRUE)) {
    cat("A chi-square test of independence was performed.\n")
    cat("The relation between ", vars[[1]][1], " was ", signif,
        ", X² (df = ", test_obj$parameter, ") = ",
        test_obj$statistic.text, ", p ", test_obj$p.value.text, ".\n", sep="")

  } else if (test_obj$method[1] == "Chi-squared test for given probabilities") {

    # is the distribution uniform?
    if (var(test_obj$expected) == 0) {
      is_uniform = "uniformly"
    } else is_uniform = "according to a custom distribution"

    cat("A chi-square test of goodness-of-fit was performed to determine if the elements of ",
        vars, " were distributed ", is_uniform, ".\n", sep="")
    # TODO:
    cat("[TODO: fill in this section]\n")
  }

}

