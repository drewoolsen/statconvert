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
