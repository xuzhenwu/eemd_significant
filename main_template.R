# 1 test eemd with only one series 
file_loca <- "data/eemd_test.txt"
# read file input
timeseries <- read.table(file_loca)$V1
res <- eemd_series(timeseries)
plot(res[[2]])

# 2 test tiff file
data <- readGDAL("Annual_ANPP_PNPP_HNPP_datasets_1982_2015/PNPP/Comp_PNPP_82_15_g_m2.tif")
file_output <- "PNPP1.csv"