#goal: take statistical test output and translate into english
#tests: anova (one way), ttest, corr, chisq

# Reporting a significant single sample t-test (μ ≠ μ0):
# Students taking statistics courses in psychology at the University of Washington reported studying more hours
# for tests (M = MEAN, SD = STANDARD DEV) than did UW college students in in general, t(DF) = T VAL, p = P VAL.

#no way to get sd from ttest object, may need to require that as well as ttest object
#alt: run ttest in the function (so parameters are just the relevant data bits), and then use results for output

statconvert <- function(test_obj){
  if(class(test_obj) == "htest"){
    ttest_convert(test_obj)
  }
}


test1 <- t.test(1:10, y = 1:10)
statconvert(test1)
