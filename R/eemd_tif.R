if (is.element('rgdal', installed.packages()[,1]) == FALSE) { 
  install.packages('rgdal') 
} 
library(rgdal)


data <- readGDAL("data/Comp_ANPP_82_15.tif")

sum_data <- summary(data)
cells.dim <- sum_data$grid$cells.dim # colomn, row 
data.dim <- dim(data) # colomn*row, bandwidth

# 1 transfer as vector
trans_vector <- function(banddata, bandwidth){
  vec <- vector(length = bandwidth)
  na_flag <- zero_flag <- 0L
  for(i in 1:bandwidth){
    
    COMMAND <- paste("vec[i] <- banddata$band", i, sep ="")
    eval(parse(text = COMMAND))
    if(na_flag == 0){
      if(is.na(vec[i])) na_flag <- 1
      if(vec[i] == 0) zero_flag <- zero_flag + 1
    }
  }
  if(zero_flag == bandwidth)
    na_flag <- 1
  out <- list(vec, na_flag)
  return(out)
}


# construct output
pextrema <- rn <- ptr1 <- ptr2 <- tre1 <- tre2 <- sig_flag <- sig_value <- data

for(r in 1:cells.dim[2])
  for(c in 1:cells.dim [1]){
    out <- trans_vector(data[r,c], data.dim[2])
    ts <- out[[1]]
    na_flag <- out[[2]]
    if(na_flag != 1){
      
      # commpute eemd 
      res <- eemd_series(ts)
      
      # deliver res 
      pextrema[r, c]$band1 <- res[[1]]
      for(i in 1:data.dim[2]){
        COMMAND <- paste("rn[r, c]$band", i, "<- res[[2]][i])", sep = "")
        eval(parse(text = COMMAND))
        COMMAND <- paste("ptr1[r, c]$band", i, "<- res$ptr1[i])", sep = "")
        eval(parse(text = COMMAND))
        COMMAND <- paste("ptr2[r, c]$band", i, "<- res$ptr2[i])", sep = "")
        eval(parse(text = COMMAND))
        COMMAND <- paste("tre1[r, c]$band", i, "<- res$rn[i])", sep = "")
        eval(parse(text = COMMAND))
        COMMAND <- paste("tre2[r, c]$band", i, "<- res$rn[i])", sep = "")
        eval(parse(text = COMMAND))
      }
      for(i in 1:2){
        COMMAND <- paste("sig_flag[r, c]$band", i, "<- res$sig_flag[i])", sep = "")
        eval(parse(text = COMMAND))
        COMMAND <- paste("sig_value[r, c]$band", i, "<- res$sig_value[i])", sep = "")
        eval(parse(text = COMMAND))
      }
    }
}
  

