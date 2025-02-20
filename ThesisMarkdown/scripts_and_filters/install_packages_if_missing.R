requiredPackages <- c("bookdown","caret","cowplot","data.table","devtools","diffcor","dplyr","emmeans",
  "factoextra","flextable","ggpubr","grateful","ggsci","ggplot2", "glmnet", "gridExtra","interactions",
  "knitr","logisticPCA","lme4","lmerTest","lmtest","lsr","ltm", "kableExtra","magrittr","NeuralNetTools",
  "MASS", "mgcv", "nnet","officer","pheatmap","pracma","pROC", "psych","pwr","RColorBrewer","readxl","reticulate","rjson","ROCR",
  "rpart","rpart.plot","rstatix", "scales","stats","stringr","tibble","tidyr","tidyverse","verification","viridis")

new.packages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
lapply(requiredPackages, require, character.only = TRUE)

meanFun <- function(data, i){
  d <- data[i, ]
  return(mean(d))
  bo <- boot(data[, "xs", drop = FALSE], statistic=meanfun, R=5000)
  boot.ci(bo, conf=0.95, type="bca")
}

subsample <- function(x,n,r) {
  returnDf <- data.frame(matrix(ncol=n,nrow=r))
  for (iter in 1:r)
  {
    sampleArr <- x
    returnArr <- sample(sampleArr,size = n,replace = FALSE)
    returnDf[iter,] <- returnArr
  }
  return(returnDf)
}

binarysimilarity <- function(x,y)
{
  if (length(x) != length(y))
  {
    return (0)
  }
  else
  {
    return (sum(x==y)/length(x))
  }
}

binarysimilarityMat <- function(m) {
  mat <- data.frame(matrix(ncol = nrow(m), nrow = nrow(m) ))
  for (x in 1:nrow(m))
  {
    for (y in 1:nrow(m))
    {
      if (x == y)
      {
        mat[x,y] <- 0
      }
      else
      {
        mat[x,y] <- binarysimilarity(m[x,],m[y,])
      }
    }
  }
  return(mat)
}

binarysimilarityMean <- function(m){
  mat <- binarysimilarityMat(m)
  values <- mat[upper.tri(mat)]
  return(mean(values))
}

dicesimilarityMean <- function(m){
  mat <- as.matrix(proxy::dist(m,method = "Dice"))
  values <- mat[upper.tri(mat)]
  return(mean(values))
}

jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}

jaccardMat <- function(m) {
  retMat <- data.frame(matrix(ncol = ncol(m), nrow = nrow(m) ))
  for (x in 1:nrow(m))
  {
    for (y in 1:nrow(m))
    {
      if (x == y)
      {
        retMat[x,y] <- 0
      }
      else
      {
        retMat[x,y] <- jaccard(m[x,],m[y,])
      }
    }
  }
  return(retMat)
}

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}


inversionNumber <- function(x){
  mergeSort <- function(x){
    if(length(x) == 1){
      inv <- 0
      #printind(' base case')
    } else {
      n <- length(x)
      n1 <- ceiling(n/2)
      n2 <- n-n1
      y1 <- mergeSort(x[1:n1])
      y2 <- mergeSort(x[n1+1:n2])
      inv <- y1$inversions + y2$inversions
      x1 <- y1$sortedVector
      x2 <- y2$sortedVector
      i1 <- 1
      i2 <- 1
      while(i1+i2 <= n1+n2+1){
        if(i2 > n2 || (i1 <= n1 && x1[i1] <= x2[i2])){ # ***
          x[i1+i2-1] <- x1[i1]
          i1 <- i1 + 1
        } else {
          inv <- inv + n1 + 1 - i1
          x[i1+i2-1] <- x2[i2]
          i2 <- i2 + 1
        }
      }
    }
    return (list(inversions=inv,sortedVector=x))
  }
  if (!is.na(x))
  {
    r <- mergeSort(x)
  }
  return (r$inversions)
}

kendallTauDistance <- function(x,y){
  n <- length(x)
  ub <- (0.5*n)*(n-1)
  return(1-(inversionNumber(order(x)[rank(y)])/ub))
}
