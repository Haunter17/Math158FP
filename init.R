## task 1
library(readr)
require(plyr)
require(dplyr)
library(ggplot2)
library(gridExtra)

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
ezdata2<-ezdata %>% dplyr::select(campus,domain,subdomain,path,query,start,end,start_ts,end_ts)

#deleting rows with NA
ezdata2<-ezdata2 %>% filter(!is.na(end_ts))

#Calculating duration
ezdata2<-ezdata2 %>% mutate(duration=(end_ts-start_ts)/60)

# Merge campus
ezdata2$campus[ezdata2$campus == "scrippscollege"] <- "scr"
ezdata2$campus[ezdata2$campus == "pomona"] <- "pom"
ezdata2$campus[ezdata2$campus == "pitzer"] <- "pit"
ezdata2$campus[(ezdata2$campus == "kecksci") | (ezdata2$campus == "kec") ] <- "kgi"
ezdata2$campus <- factor(ezdata2$campus)

## partition start hr into bins
getHr <- function(datetime) {
  hms <- strsplit(trimws(datetime), ' ')[[1]][2]
  as.numeric(strsplit(hms, ':')[[1]][1])
}

ezdata2 <- ezdata2 %>% mutate(start_hr=sapply(ezdata2$start, getHr),
                              end_hr=sapply(ezdata2$end, getHr))

### distribution of start & end time
pdf("start_distb_tot.pdf", width=8, height=6)
ggplot(ezdata2, aes(x=start_hr, fill=campus)) + geom_bar() + labs(title="Distribution of Start Time (All)") + xlab("start hour")
dev.off()
# by individual schools
pdf("start_distb_indv.pdf", width=12, height=8)
par(mfrow=c(4, 2))
for (camp in levels(ezdata2$campus)) {
  cur_data <- ezdata2 %>% filter(campus==camp)
  title <- paste("Distribution of Start Time of",  toupper(camp))
  hist(cur_data$start_hr, main=title, xlab="start hour", breaks=9, border="white", col="pink")
}
dev.off()

pdf("end_distb_tot.pdf", width=8, height=6)
ggplot(ezdata2, aes(x=end_hr, fill=campus)) + geom_bar() + labs(title="Distribution of End Time (All)") + xlab("end hour")
dev.off()

pdf("end_distb_indv.pdf", width=12, height=8)
par(mfrow=c(4, 2))
for (camp in levels(ezdata2$campus)) {
  cur_data <- ezdata2 %>% filter(campus==camp)
  title <- paste("Distribution of End Time of",  toupper(camp))
  hist(cur_data$end_hr, main=title, xlab="end hour", breaks=9, border="white", col="pink")
}
dev.off()

### duration vs campus
#One-way ANOVA on duration and campus
myap<-aov(duration~campus,data=ezdata2)
summary(myap)
lmod<-lm(duration~campus,ezdata2)
summary(lmod)
#Duration plots by Campus
pomona<-ezdata2 %>% filter(campus=="pom")
pomona %>% ggplot(aes(x=1:dim(pomona)[1],y=duration))+geom_hline(aes(yintercept=mean(pomona$duration)),color="blue")+geom_point(size=0.5)+labs(x="index", title="Wireless Duration Plot of Pomona College")

hmc<-ezdata2%>%filter(campus=="hmc")
hmc %>% ggplot(aes(x=1:dim(hmc)[1],y=duration))+geom_hline(aes(yintercept=mean(hmc$duration)),color="brown")+geom_point(size=0.5)+labs(x="index", title="Wireless Duration Plot of Harvey Mudd College")

pitzer<-ezdata2%>%filter(campus=="pit")
pitzer %>% ggplot(aes(x=1:dim(pitzer)[1],y=duration))+geom_hline(aes(yintercept=mean(pitzer$duration)),color="orange")+geom_point(size=0.5)+labs(x="index", title="Wireless Duration Plot of Pitzer College")

scripps<-ezdata2%>%filter(campus=="scr")
scripps %>% ggplot(aes(x=1:dim(scripps)[1],y=duration))+geom_hline(aes(yintercept=mean(scripps$duration)),color="green")+geom_point(size=0.5)+labs(x="index", title="Wireless Duration Plot of Scripps College")

cmc<-ezdata2%>%filter(campus=="cmc")
cmc %>% ggplot(aes(x=1:dim(cmc)[1],y=duration))+geom_hline(aes(yintercept=mean(cmc$duration)),color="red")+geom_point(size=0.5)+labs(x="index", title="Wireless Duration Plot of Claremont McKenna College")

cgu<-ezdata2%>%filter(campus=="cgu")
cgu %>% ggplot(aes(x=1:dim(cgu)[1],y=duration))+geom_hline(aes(yintercept=mean(cgu$duration)),color="purple")+geom_point(size=0.5)+labs(x="index", title="Wireless Duration Plot of Claremont Graduate University")

kgi<-ezdata2%>%filter(campus=="kgi")
kgi %>% ggplot(aes(x=1:dim(kgi)[1],y=duration))+geom_hline(aes(yintercept=mean(kgi$duration)))+geom_point(size=0.5)+labs(x="index", title="Wireless Duration Plot of Keck Graduate Institute")

#Duration Histogram                   
ezdata2 %>% ggplot(aes(x=duration))+geom_histogram(binwidth=30,color="pink")+coord_cartesian(xlim=c(0,600))                   


### Duration vs.Domain
par(mfrow=c(1,1))
# ebscohost_domain_data <- ezdata2[ezdata2$domain == "ebscohost" | ezdata2$domain == "ebrary",]
ebscohost_domain_data <- ezdata2[(ezdata2$domain %in% c("ebscohost", "ebrary", "jstor", "oclc", "proquest")) & (ezdata2$duration < 20000),]

ebscohost_domain_data$domain <- factor(ebscohost_domain_data$domain)
boxplot(duration~domain, data = ebscohost_domain_data, main = "ebscohost vs. ebrary")

domain_aov<-aov(duration~domain,data=ebscohost_domain_data)
summary(domain_aov)
lmod<-lm(duration~domain, ebscohost_domain_data)
summary(lmod)


### Domain Duration vs. Domain Count
domain_duration_count <- as.data.frame(table(unlist(ezdata2$domain)))
domain_duration_count_top <- head(arrange(domain_duration_count, desc(Freq)), n = 30)
domain_duration_means <- sapply(domain_duration_count_top$Var1, function(domain) {
  domain_data <- ezdata2[ezdata2$domain == domain,] %>% filter(!is.na(duration))
  mean(domain_data$duration)
})

domain_duration_df <- data.frame(mean=domain_duration_means, count=domain_duration_count_top$Freq)
domain_duration_df <- domain_duration_df[domain_duration_df$mean < 15000,]
summary(domain_duration_df)

pdf("domain-count-boxcox.pdf",width=8,height=6)
mdl_old <- lm(count~mean, data = domain_duration_df)
require(MASS)
par(mfrow=c(1,2))
boxcox(mdl_old, plotit=T)
boxcox(mdl_old, plotit=T, lambda=seq(-1,1,by=0.1))
dev.off()

pdf("domain-count-fit.pdf",width=8,height=6)
par(mfrow=c(1,1))
mdl <- lm(count^(-1/2)~mean, data = domain_duration_df)
summary(mdl)
plot(count^(-1/2)~mean, data = domain_duration_df, main="Domain Visit Count vs. Mean of Duration", xlab = "Duration", ylab = "Count")
abline(mdl)
dev.off()

### School vs. Domain
pdf("school-vs-domain.pdf",width=8,height=15)
par(mfrow=c(4, 2))
FREQ_LIMIT <- 500
for (campus in levels(ezdata2$campus)) {
  campus_data <- ezdata2[ezdata2$campus == campus,]
  domain_count <- as.data.frame(table(unlist(campus_data$domain)))
  
  small_domain_count <- data.frame(Var1="other",Freq=sum(domain_count[domain_count$Freq < FREQ_LIMIT,]$Freq))
  domain_data <- domain_count[domain_count$Freq >= FREQ_LIMIT,]
  levels(domain_data$Var1) <- c(levels(domain_data$Var1), "other")
  domain_data <- rbind(domain_data, small_domain_count)
  
  pie(domain_data$Freq, sapply(domain_data$Var1, as.character), radius = 1, main=paste(campus, "vs. Domain", sep=" "))
}
dev.off()

### Duration vs Domain
dur_by_domain <- aggregate(duration~domain, ezdata2, sum)
names(dur_by_domain)[2] <- 'duration'
domain_accesses <- aggregate(duration~domain, ezdata2, length)
names(domain_accesses)[2] <- 'num accesses'
domain_durations <- merge(dur_by_domain, domain_accesses, by="domain")
freq_used_domains <- domain_durations[match(domain_data$Var1, domain_durations$domain), ]

title <- paste("Domain total accesses")
hist(domain_durations$duration, xlab="domain", main=title, border="white", col="pink")