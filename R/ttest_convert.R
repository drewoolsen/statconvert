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
