# 1-test_eemd
file_loca <- "data/eemd_test.txt"
# read file input
timeseries <- read.table(file_loca)$V1
res <- eemd_series(timeseries)
plot(res[[2]])