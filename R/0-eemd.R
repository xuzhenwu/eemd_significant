##=========================================================
## a function used for a timeseries
## speed test: 100 grids using 281s without parallelization
##=========================================================

# require Rlibeemd as basic eemd 
if (is.element('Rlibeemd', installed.packages()[,1]) == FALSE) { 
  install.packages('Rlibeemd') 
} 
library('Rlibeemd')

eemd_series <- function(timeseries, ensemble_size = 100, noise_strength = 0.2, 
                        S_number = 5, num_siftings = 50, wne_num = 1000,
                        extrema_flag = TRUE, print_flag =  FALSE){
  
  # attributes
  timelength <- length(timeseries)
  
  rn <- eemd(input = timeseries, ensemble_size = ensemble_size, noise_strength = noise_strength,
             S_number = S_number, num_siftings = num_siftings)[,5]
  
  # 1. generate white noises & eemd trend
  wne <- matrix(nrow = wne_num, ncol = timelength)
  wne_rn <- wne_tre <- wne_tre_dif <- wne
  for(i in 1:wne_num){
    wne[i,] <- rnorm(timelength, mean = 0, sd = noise_strength* sd(timeseries))
    #wne[i,] <- rnorm(timelength, mean = 0, sd = 1)
    wne_rn[i,] <- eemd(wne[i,], ensemble_size = ensemble_size, noise_strength = noise_strength,
                       S_number = S_number, num_siftings = num_siftings)[,5] # same as timeseries
    for(j in 1:timelength){
      wne_tre[i,j] <- wne_rn[i, j] - wne_rn[i, 1] 
    }
    for(j in 1:timelength){
      wne_tre_dif[i,j] <- wne_tre[i, j] - wne_tre[i, timelength]
    }
  }
  
  # 2. compute sd of pdf-1t and pdf-2t
  sd_ratio <- 1.96 # 95% confidential range
  sd_pdf1t <- vector(length = timelength)
  up1 <- do1<- up2<- do2 <- mean_pdf1t <- mean_pdf2t <- sd_pdf2t <- sd_pdf1t 
  for(i in 1:timelength){
    sd_pdf1t[i] <- sd(wne_tre[,i])
    mean_pdf1t[i] <- mean(wne_tre[,i])
    sd_pdf2t[i] <- sd(wne_tre_dif[,i])
    mean_pdf2t[i] <- mean(wne_tre_dif[,i])
  }
  up1 <- sd_pdf1t*sd_ratio + mean_pdf1t
  up2 <- sd_pdf2t*sd_ratio + mean_pdf2t
  do1 <- mean_pdf1t - sd_pdf1t*sd_ratio
  do2 <- mean_pdf2t - sd_pdf2t*sd_ratio
  
  confid_par <- cbind(up1, do1, up2, do2)
  
  # 3. results, p value of the trend
  tre1 <- tre2 <- ptr1 <- ptr2 <- 0
  for(i in 1:timelength){ # compute trend 1
    tre1[i] <- rn[i] - rn[1] 
    if(tre1[i] > up1[i] | tre1[i] < do1[i])# check sigficance
      ptr1[i] <- 1
    else
      ptr1[i] <- 0
  }
  for(i in 1:timelength){ # compute trend 2
    tre2[i] <- tre1[i] - tre1[timelength]
    if(tre2[i] > up2[i] | tre2[i] < do2[i])# check sigficance
      ptr2[i] <- 1
    else
      ptr2[i] <- 0
  }
  
  # 4.extrema
  pextrema <- 0L # find extrema
  for(i in 2:(timelength-1)){
    if(
      (tre1[i] > tre1[i-1] & tre1[i] > tre1[i+1]) |
      (tre1[i] < tre1[i-1] & tre1[i] < tre1[i+1])
    ){
      pextrema <- i
      break # there is only one extrema
    }
  }
  
  # 5. check the significancy of the two trends
  sig_flag <- c(0, 0) 
  sig_value <- c(0, 0)
  if(extrema_flag == FALSE)
    pextrema <- 0
  if(pextrema == 0){ # 4. when there is no extrema
    sig_flag[1] <- ptr1[timelength]
    sig_value[1] <-  tre1[timelength]
  }else{ # 5. when there is one txtrema
    sig_flag[1] <-  ptr1[pextrema]
    sig_value[1] <-  tre1[pextrema]
    sig_flag[2] <-  ptr2[pextrema]
    sig_value[2] <-  tre2[pextrema]
  }
  
  
  output <- list(pextrema, # location where the extrema is, init as 0
                 rn,  # trend values of each year
                 tre1,
                 tre2,
                 ptr1, # significance: 1 significant, 0 not 
                 ptr2, # significance: 1 significant, 0 not 
                 sig_flag, # 0, not; 1 tread
                 sig_value
                 )
  
  if(print_flag == TRUE){
    print(paste(sig_flag," ", sig_value,  sep = ""))
  }
  return(output)
}


# parameters
file_loca <- "data/eemd_test.txt"
# read file input
timeseries <- read.table(file_loca)$V1
res <- eemd_series(timeseries)

plot(res[[2]])



