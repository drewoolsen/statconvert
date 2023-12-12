#goal: take statistical test output and translate into english
#tests: anova (one way), ttest, corr, chisq

# Reporting a significant single sample t-test (μ ≠ μ0):
# Students taking statistics courses in psychology at the University of Washington reported studying more hours
# for tests (M = MEAN, SD = STANDARD DEV) than did UW college students in in general, t(DF) = T VAL, p = P VAL.

#no way to get sd from ttest object, may need to require that as well as ttest object
#alt: run ttest in the function (so parameters are just the relevant data bits), and then use results for output

statconvert <- function(test_obj){
  ttest_method = c("Welch Two Sample t-test", "One Sample t-test", "Paired t-test", " Two Sample t-test")
  cortest_method = c("Pearson's product-moment correlation","Spearman's rank correlation rho",
                     "Kendall's rank correlation tau")
  chisq_method = c("Pearson's Chi-squared test", "Chi-squared test for given probabilities")

  if(class(test_obj)[1] == "aov" | class(test_obj)[1] == "anova"){
    oneway_anova_convert(test_obj)
    return(invisible(NULL))
  }
  if(class(test_obj) == "htest" & test_obj$method[1] %in% ttest_method){
    ttest_convert(test_obj)
    return(invisible(NULL))
  }

  if(class(test_obj) == "htest" & test_obj$method[1] %in% cortest_method) {
    cor_convert(test_obj)
    return(invisible(NULL))
  }

  # as far as I can tell here are the two options for the method of
  # a chi-squared test of independence:
  # "Pearson's Chi-squared test"
  # "Pearson's Chi-squared test with simulated p-value ..."
  # so we just check if the first string is a substring of method
  if (class(test_obj) == "htest" & test_obj$method[1] %in% chisq_method |
      grepl("Pearson's Chi-squared test", test_obj$method[1], fixed=TRUE)) {
    chisq_convert(test_obj)
    return(NULL)
  }

}

# tests

data("mtcars")
cor1 <- cor.test(mtcars$mpg, mtcars$qsec)
test1 <- t.test(mtcars$mpg, mtcars$hp)
test2 <- t.test(mtcars$mpg, mtcars$qsec)



statconvert(test1)
statconvert(test2)
statconvert(test3)
statconvert(cor1)

data("starwars", package = 'dplyr')
chi1 <- chisq.test(starwars$species, starwars$homeworld)
chi2 <- chisq.test(starwars$skin_color, starwars$hair_color)
chi3 <- chisq.test(mtcars$cyl, p=rep(1,length(mtcars$cyl)), rescale.p=TRUE)
chi4 <- chisq.test(mtcars$cyl, p=seq(1,length(mtcars$cyl)), rescale.p=TRUE)

statconvert(chi1)
statconvert(chi2)
statconvert(chi3)
statconvert(chi4)

aov1 <- aov(mpg ~ wt, data = mtcars)
statconvert(aov1)

# error tests
length(test_obj$method) > 0
is.null(aov1$method)
