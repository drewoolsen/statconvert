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
