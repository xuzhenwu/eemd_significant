if (is.element('rgdal', installed.packages()[,1]) == FALSE) { 
  install.packages('rgdal') 
} 
library(rgdal)


data <- readGDAL("data/Comp_ANPP_82_15.tif")

sum_data <- summary(data)
cells.dim <- sum_data$grid$cells.dim # colomn, row 
data.dim <- dim(data) # colomn*row, bandwidth

band_trans_vector <- function(banddata, bandwidth){
  vec <- vector(length = bandwidth)
  for(i in 1:bandwidth){
    COMMAND <- paste("vec[i] <- banddata$band", i, sep ="")
    eval(parse(text = COMMAND))
  }
  return(vec)
}


for(r in 1:cells.dim[2])
  for(c in 1:cells.dim [1]){
    timeseries <- band_trans_vector(data[r,c], data.dim[2])
    out <- eemd_series(timeseries)
}
  

