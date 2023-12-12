#' @title statconvert
#' @description Converts common statistical test outputs into APA format
#' @param test_obj An object create by a statistical test function like aov() or t.test()
#' @return A string of the output results converted into APA format
#' @details Tests covered are oneway ANOVA tests, T tests, Correlation tests, Chi-Square tests, and Logisitic Regression tests.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE
#'  data(mtcars)
#'  md <- aov(mpg ~ wt, data = mtcars)
#'  statconvert(md)
#'  }
#' }
#' @rdname statconvert
#' @export

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
    return(invisible(NULL))
  }

}

# t-test
ttest_convert <- function(test_obj) {
  if(test_obj$p.value < .001) {
    test_obj$p.value.text = "<.001"
  } else {
    test_obj$p.value.text = round(test_obj$p.value, 3)
  }


  if(length(test_obj$estimate) == 1){

    if(1 - test_obj$p.value >= attributes(test_obj$conf.int)$conf.level[1]){
      cat("The mean of ",
          strsplit(strsplit(test_obj$data.name, split = " and ")[[1]][1], split = "\\$")[[1]][2],
          " is significantly different from the given null hypothesis (M = ",
          test_obj$null.value[[1]], "), t(", round(test_obj$parameter, 3), " ) = ",
          round(test_obj$statistic[[1]],3),", p = ", test_obj$p.value.text, sep="")
    }

    if(1 - test_obj$p.value < attributes(test_obj$conf.int)$conf.level[1]){
      cat("The mean of ",
          strsplit(strsplit(test_obj$data.name, split = " and ")[[1]][1], split = "\\$")[[1]][2],
          "is not significantly different from the given null hypothesis (M =",
          test_obj$null.value[[1]], "), t(", round(test_obj$parameter, 3), ") = ",
          round(test_obj$statistic[[1]],3),", p = ", test_obj$p.value.text, sep="")
    }
  } else {

    if(1 - test_obj$p.value >= attributes(test_obj$conf.int)$conf.level[1]){
      cat("The mean of ",
          strsplit(strsplit(test_obj$data.name, split = " and ")[[1]][1], split = "\\$")[[1]][2],
          " is significantly different from the mean of ",
          strsplit(strsplit(test_obj$data.name, split = " and ")[[1]][2], split = "\\$")[[1]][2], ", t(",
          round(test_obj$parameter, 3), ") = ", round(test_obj$statistic[[1]],3), ", p = ",
          test_obj$p.value.text, sep="")
    }

    if(1 - test_obj$p.value < attributes(test_obj$conf.int)$conf.level[1]){
      cat("The mean of ",
          strsplit(strsplit(test_obj$data.name, split = " and ")[[1]][1], split = "\\$")[[1]][2],
          " is not significantly different from the mean of ",
          strsplit(strsplit(test_obj$data.name, split = " and ")[[1]][2], split = "\\$")[[1]][2], ", t(",
          round(test_obj$parameter, 3), ") = ", round(test_obj$statistic[[1]],3), ", p = ",
          test_obj$p.value.text, sep="")
    }
  }
}
# onewayA ANOVA test
oneway_anova_convert <- function(test_obj){
  if(class(test_obj)[1] == "aov"){
    test_obj <- summary(test_obj)[[1]]
    XVAL <- row.names(test_obj)[1]
    XVAL <- gsub(" ", "", XVAL)
    FVAL <- round(test_obj$`F value`[1], 2)
    PVAL <- test_obj$`Pr(>F)`[1]

    if(test_obj$`Pr(>F)`[1] < 0.001){
      cat("An analysis of variance showed that the effect of ",XVAL,
          " was significant, ", "F(", test_obj$Df[1], ", ", test_obj$Df[2],
          ") = ", FVAL, ", p < 0.001.", sep = "")
    } else if(test_obj$`Pr(>F)`[1] < 0.05 & test_obj$`Pr(>F)`[1] > 0.01){
      cat("An analysis of variance showed that the effect of ",XVAL,
          " was significant, ", "F(", test_obj$Df[1], ", ", test_obj$Df[2],
          ") = ", FVAL, ", p = ", PVAL, ".", sep = "")
    } else{
      cat("An analysis of variance showed that the effect of ",XVAL,
          " was not significant, ", "F(", test_obj$Df[1], ", ", test_obj$Df[2],
          ") = ", FVAL, ", p = ", PVAL, ".", sep = "")
    }
  }
  else if(class(test_obj)[1] == "anova"){
    XVAL <- row.names(test_obj)[1]
    XVAL <- gsub(" ", "", XVAL)
    FVAL <- round(test_obj$`F value`[1], 2)
    PVAL <- test_obj$`Pr(>F)`[1]

    if(test_obj$`Pr(>F)`[1] < 0.001){
      cat("An analysis of variance showed that the effect of ",XVAL,
          " was significant, ", "F(", test_obj$Df[1], ", ", test_obj$Df[2],
          ") = ", FVAL, ", p < 0.001.", sep = "")
    } else if(test_obj$`Pr(>F)`[1] < 0.05 & test_obj$`Pr(>F)`[1] > 0.01){
      cat("An analysis of variance showed that the effect of ",XVAL,
          " was significant, ", "F(", test_obj$Df[1], ", ", test_obj$Df[2],
          ") = ", FVAL, ", p = ", PVAL, ".", sep = "")
    } else{
      cat("An analysis of variance showed that the effect of ",XVAL,
          " was not significant, ", "F(", test_obj$Df[1], ", ", test_obj$Df[2],
          ") = ", FVAL, ", p = ", PVAL, ".", sep = "")
    }
  }
}
# correlation test
cor_convert <- function(test_obj){
  if(test_obj$p.value < .001) {
    test_obj$p.value.text = "<.001"
  } else {
    test_obj$p.value.text = round(test_obj$p.value, 3)
  }

  if(1 - test_obj$p.value >= attributes(test_obj$conf.int)$conf.level[1]) {
    if(test_obj$estimate < 0 & abs(test_obj$estimate) > .5) {
      cat(strsplit(strsplit(test_obj$data.name, split = " and ")[[1]][1], split = "\\$")[[1]][2], " and ",
          strsplit(strsplit(test_obj$data.name, split = " and ")[[1]][2], split = "\\$")[[1]][2],
          " are strongly negatively correlated ","r(", round(test_obj$parameter, 3), ") = ",
          round(test_obj$statistic[[1]],3),", p = ", test_obj$p.value.text, sep="")
    }

    if(test_obj$estimate > 0 & abs(test_obj$estimate) > .5) {
      cat(strsplit(strsplit(test_obj$data.name, split = " and ")[[1]][1], split = "\\$")[[1]][2], " and ",
          strsplit(strsplit(test_obj$data.name, split = " and ")[[1]][2], split = "\\$")[[1]][2],
          " are strongly positively correlated ","r(", round(test_obj$parameter, 3), ") = ",
          round(test_obj$statistic[[1]],3),", p = ", test_obj$p.value.text, sep="")
    }

    if(test_obj$estimate < 0 & abs(test_obj$estimate) < .5) {
      cat(strsplit(strsplit(test_obj$data.name, split = " and ")[[1]][1], split = "\\$")[[1]][2], " and ",
          strsplit(strsplit(test_obj$data.name, split = " and ")[[1]][2], split = "\\$")[[1]][2],
          " are weakly negatively correlated ","r(", round(test_obj$parameter, 3), ") = ",
          round(test_obj$statistic[[1]],3),", p = ", test_obj$p.value.text, sep="")
    }

    if(test_obj$estimate > 0 & abs(test_obj$estimate) < .5) {
      cat(strsplit(strsplit(test_obj$data.name, split = " and ")[[1]][1], split = "\\$")[[1]][2], " and ",
          strsplit(strsplit(test_obj$data.name, split = " and ")[[1]][2], split = "\\$")[[1]][2],
          " are weakly positively correlated ","r(", round(test_obj$parameter, 3), ") = ",
          round(test_obj$statistic[[1]],3),", p = ", test_obj$p.value.text, sep="")
    }
  }
}
# chi-square test
chisq_convert <- function(test_obj) {
  if (test_obj$p.value < .001) {
    test_obj$p.value.text <- "< .001"
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

