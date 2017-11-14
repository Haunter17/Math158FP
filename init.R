## task 1
library(readr)
f2016wireless <- read_delim("./data/wireless/2016FallSemWireless.csv", 
                                   "\t", escape_double = FALSE, trim_ws = TRUE)
## task 2
f2016wireless[f2016wireless == '-']  <- NA

## task 3
ezDir <- "./data/ezproxy/"
csvList <- list.files(ezDir, pattern="*.csv")
for (i in 1 : length(csvList)) {
  dataname <- strsplit(csvList[i], "_|\\.")[[1]][[1]]
  new_df <- read.csv(paste(ezDir, csvList[i], sep=""), header=FALSE, col.names = c("uuid", "campus", "datetime", "session", "ipSubnet"))
  new_df <- new_df[c("uuid", "session", "campus", "datetime", "ipSubnet")]
  assign(dataname, new_df)
}


tsvList <- list.files(ezDir, pattern="*.tsv")
for (i in 1 : length(tsvList)) {
  dataname <- strsplit(tsvList[i], "_|\\.")[[1]][[1]]
  assign(dataname, read.delim(paste(ezDir, tsvList[i], sep=""), sep="\t", header=TRUE))
}

# combine into one single data frame
ccl2016 <- do.call(rbind, lapply(ls(patt="ccl"), get))
spu2016 <- do.call(rbind, lapply(ls(patt="spu"), get))

# clean uuid
ccl2016$uuid <- as.character(ccl2016$uuid)
# clean datetime -> start, end
library(tidyr)
ccl2016 <- separate(ccl2016, datetime, into=c("start", "end"), sep=19) # 19 is counted
# convert into time object
library(anytime)
start_datetime <- anytime(ccl2016$start)
end_datetime <- anytime(ccl2016$end)

# Join the data
ezdata <- merge(ccl2016, spu2016, by = "session", all.x = TRUE)

ezdata$start_ts <- as.numeric(as.POSIXct(anytime(ezdata$start), tz = "ETC/GMT-7"))
ezdata$end_ts <- as.numeric(as.POSIXct(anytime(ezdata$end), tz = "ETC/GMT-7"))
