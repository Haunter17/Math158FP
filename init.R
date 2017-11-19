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

#select variables
ezdata2<-ezdata %>% select(campus,domain,subdomain,path,query,start,end,start_ts,end_ts)

#deleting rows with NA
ezdata2<-ezdata2 %>% filter(!is.na(end_ts))

#Calculating duration
ezdata2<-ezdata2 %>% mutate(duration=(end_ts-start_ts)/60)

#One-way ANOVA on duration and campus
myap<-aov(duration~campus,data=ezdata2)
summary(myap)
lmod<-lm(duration~campus,ezdata2)
summary(lmod)

#Duration plots by Campus
pomona<-ezdata2 %>% filter(campus=="pomona" | campus=="pom")
pomona %>% ggplot(aes(x=1:dim(pomona)[1],y=duration))+geom_hline(aes(yintercept=mean(pomona$duration)),color="blue")+geom_point(size=0.5)+labs(x="index", title="Wireless Duration Plot of Pomona College")

hmc<-ezdata2%>%filter(campus=="hmc")
hmc %>% ggplot(aes(x=1:dim(hmc)[1],y=duration))+geom_hline(aes(yintercept=mean(hmc$duration)),color="brown")+geom_point(size=0.5)+labs(x="index", title="Wireless Duration Plot of Harvey Mudd College")

pitzer<-ezdata2%>%filter(campus=="pitzer"|campus=="pit")
pitzer %>% ggplot(aes(x=1:dim(pitzer)[1],y=duration))+geom_hline(aes(yintercept=mean(pitzer$duration)),color="orange")+geom_point(size=0.5)+labs(x="index", title="Wireless Duration Plot of Pitzer College")

scripps<-ezdata2%>%filter(campus=="scrippscollege"|campus=="scr")
scripps %>% ggplot(aes(x=1:dim(scripps)[1],y=duration))+geom_hline(aes(yintercept=mean(scripps$duration)),color="green")+geom_point(size=0.5)+labs(x="index", title="Wireless Duration Plot of Scripps College")

cmc<-ezdata2%>%filter(campus=="cmc")
cmc %>% ggplot(aes(x=1:dim(cmc)[1],y=duration))+geom_hline(aes(yintercept=mean(cmc$duration)),color="red")+geom_point(size=0.5)+labs(x="index", title="Wireless Duration Plot of Claremont McKenna College")

cgu<-ezdata2%>%filter(campus=="cgu")
cgu %>% ggplot(aes(x=1:dim(cgu)[1],y=duration))+geom_hline(aes(yintercept=mean(cgu$duration)),color="purple")+geom_point(size=0.5)+labs(x="index", title="Wireless Duration Plot of Claremont Graduate University")

kgi<-ezdata2%>%filter(campus=="kgi")
kgi %>% ggplot(aes(x=1:dim(kgi)[1],y=duration))+geom_hline(aes(yintercept=mean(kgi$duration)))+geom_point(size=0.5)+labs(x="index", title="Wireless Duration Plot of Keck Graduate Institute")

#Duration Histogram                   
ezdata2 %>% ggplot(aes(x=duration))+geom_histogram(binwidth=30)+coord_cartesian(xlim=c(0,600))                   
                   
# Change campus

ezdata2$campus[ezdata2$campus == "scrippscollege"] <- "scr"
ezdata2$campus[ezdata2$campus == "pomona"] <- "pom"
ezdata2$campus[ezdata2$campus == "pitzer"] <- "pit"
ezdata2$campus[ezdata2$campus == "kecksci"] <- "kec"

# School vs. Domain

par(mfrow=c(3, 2))
FREQ_LIMIT <- 500
for (campus in c("hmc", "pom", "scr", "pit", "cmc", "cgu")) {
  campus_data <- ezdata2[ezdata2$campus == campus,]
  domain_count <- as.data.frame(table(unlist(campus_data$domain)))
  
  small_domain_count <- data.frame(Var1="other",Freq=sum(domain_count[domain_count$Freq < FREQ_LIMIT,]$Freq))
  domain_data <- domain_count[domain_count$Freq >= FREQ_LIMIT,]
  levels(domain_data$Var1) <- c(levels(domain_data$Var1), "other")
  domain_data <- rbind(domain_data, small_domain_count)
  
  pie(domain_data$Freq, sapply(domain_data$Var1, as.character), main=paste(campus, "vs. Domain", sep=" "))
}
                   
                   
## partition start hr into bins
getHr <- function(datetime) {
  hms <- strsplit(trimws(datetime), ' ')[[1]][2]
  as.numeric(strsplit(hms, ':')[[1]][1])
}

ezdata2 <- ezdata2 %>% mutate(start_hr=sapply(ezdata2$start, getHr),
                              end_hr=sapply(ezdata2$end, getHr))

library(ggplot2)
# plots for all school
ggplot(ezdata2, aes(x=start_hr, fill=campus)) + geom_bar() + labs(title="Distribution of Start Time (All)") + xlab("start hour")
ggplot(ezdata2, aes(x=end_hr, fill=campus)) + geom_bar() + labs(title="Distribution of End Time (All)") + xlab("end hour")
# by individual schools
ggplot(ezdata2 %>% filter(campus=="hmc"), aes(x=start_hr)) + geom_histogram(breaks=c(seq(0, 24, 3)), color="white", fill="blue") + ggtitle("Distribution of Start Time (HMC)") + xlab("start hour")

