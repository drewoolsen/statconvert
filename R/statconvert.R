#goal: take statistical test output and translate into english
#tests: anova (one way), ttest, corr, chisq



statconvert <- function(formula, data, test){
  print('hi')
}




test1 <- t.test(1:10, y = c(7:20))

class(test1)

test1$p.value

#mean group 1 (x)
test1$estimate[1]

#mean group 2 (y)
test1$estimate[2]

#standard error
test1$stderr

#degrees of freedom
test1$parameter[[1]]

#T Val
test1$statistic[[1]]

print(test1)

# Reporting a significant single sample t-test (μ ≠ μ0):
# Students taking statistics courses in psychology at the University of Washington reported studying more hours
# for tests (M = MEAN, SD = STANDARD DEV) than did UW college students in in general, t(DF) = T VAL, p = P VAL.

#no way to get sd from ttest object, may need to require that as well as ttest object
#alt: run ttest in the function (so parameters are just the relevant data bits), and then use results for output






