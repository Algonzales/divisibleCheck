randnumbs <- sample(10000:99999, 1000)
library(doParallel)
div_2 <- function(x) {
  check2 <- randnumbs - round(randnumbs, -1)
  check2_1 <- abs(check2)
  if(length(check2_1[check2_1 %% 2 == 0])){
    check2_2 <- randnumbs
    check2_3 <- check2_2[check2_2 %% 2 == 0]
    return(length(check2_3))
  }
}
div_3 <- function(x){
  check3 <- sapply(randnumbs, function(randnumbs) sum( as.numeric(unlist(strsplit(as.character(randnumbs), split=""))) ))
  if(length(check3[check3 %% 3 == 0])){
    check3_2 <- randnumbs
    check3_3 <- check3_2[check3_2 %% 3 == 0]
    return(length(check3_3))
  }
}
div_4 <- function(x){
  check4 <- randnumbs - round(randnumbs, -2)
  check4_1 <- abs(check4)
  if(length(check4_1[check4_1 %% 4 == 0])){
    check4_2 <- randnumbs
    check4_3 <- check4_2[check4_2 %% 4 == 0]
    return(length(check4_3))
  }
}
div_5 <- function(x){
  check5 <- randnumbs - round(randnumbs, -1)
  check5_1 <- abs(check5)
  if(length(check5_1) == 0 | length(check5_1 == 5)){
    check5_2 <- randnumbs[randnumbs %% 5 == 0]
    return(length(check5_2))
  }
}
div_6 <- function(x){
  check6 <- abs(randnumbs - round(randnumbs, -1))
  check6_1 <- sapply(randnumbs, function(randnumbs) sum( as.numeric(unlist(strsplit(as.character(randnumbs), split=""))) ))
  if(length(check6_1[check6_1 %% 2 == 0]) && length(check6[check6 %% 2 == 0])){
    check6_2 <- randnumbs
    check6_3 <- check6_2[check6_2 %% 6 == 0]
    return(length(check6_3))
  }
}
div_8 <- function(x){
  check8 <- randnumbs - round(randnumbs, -3)
  check8_1 <- abs(check8)
  if(length(check8_1[check8_1 %% 8 == 0])){
    check8_2 <- randnumbs
    check8_3 <- check8_2[check8_2 %% 8 == 0]
    return(length(check8_3))
  }
}
div_9 <- function(x){
  check9 <- sapply(randnumbs, function(randnumbs) sum( as.numeric(unlist(strsplit(as.character(randnumbs), split=""))) ))
  if(length(check9[check9 %% 9 == 0])){
    check9_2 <- randnumbs
    check9_3 <- check9_2[check9_2 %% 9 == 0]
    return(length(check9_3))
  }
}
list_data1 <- list( function(randnumbs)div_2(randnumbs),
                    function(randnumbs)div_3(randnumbs),
                    function(randnumbs)div_4(randnumbs),
                    function(randnumbs)div_5(randnumbs),
                    function(randnumbs)div_6(randnumbs),
                    function(randnumbs)div_8(randnumbs),
                    function(randnumbs)div_9(randnumbs))
sendOut <- c()
display <- c()
cl <- makeCluster(getOption("cl.cores",length(list_data1)))
clusterExport(cl,ls())
print(round(randnumbs))
sendOut<- clusterApply(cl,list_data1,fun = function(x) x(randnumbs))
print(sendOut)
plots1 <- c(div_2(), div_3(), div_4(), div_5(), div_6(),
                div_8(), div_9())
plots2 <- matrix(plots1,ncol =7,nrow =1)
colnames(plots2) <- c("Div2", "Div3", "Div4", "Div5", "Div6", "Div8",
                      "Div9")
barplot(plots2, col=c("blue"),main = "Divisibles")
stopCluster(cl)
