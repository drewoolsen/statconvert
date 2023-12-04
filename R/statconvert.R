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


  if(class(test_obj) == "htest" & test_obj$method[1] %in% ttest_method){
    ttest_convert(test_obj)
  }

  if(class(test_obj) == "htest" & test_obj$method[1] %in% cortest_method) {
    cor_convert(test_obj)
  }
  if(class(test_obj) == "anova"){
    oneway_anova_convert(test_obj)
  }
}


data("mtcars")
cor1 <- cor.test(mtcars$mpg, mtcars$qsec)
test1 <- t.test(mtcars$mpg, mtcars$hp)

statconvert(test1)
statconvert(cor1)
