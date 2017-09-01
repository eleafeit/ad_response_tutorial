# ===== Is My Advertising Working?=====
# Elea McDonnell Feit
# efeit@wharton.upenn.edu
# Last updated 8 May 2017, Version 1.0

# This file is a companion to the workshop on "Is my advertising working?" 
# first offered in conjunction with the Wharton Customer Analytics Initiative
# annual conference. 

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
#
# You may obtain a copy of the License at
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

rm(list=ls()) # clears the workspace
set.seed("20030603") #ensures repeatable results for attribution rules
setwd("~/Google Drive/_Workshops/2017 Ad response tutorial/R code") # set your working directory
options(scipen=999) # supress scientific notation

# ====== Read in data and explore ========
# == Customer table ==
cust <- read.csv("https://goo.gl/mqy8NR")
nrow(cust)
summary(cust)
xtabs(~email+direct+past.purchase, data=cust)

# == Impressions table ==
impress <- read.csv("https://goo.gl/74qIxy")
impress$date <- as.Date(impress$date) # change type
nrow(impress)
summary(impress)

# Cadence
(cadence <- xtabs(~date+channel, data=impress))

# Visualize the cadence
cadence <- cadence[,c(2,5,1,3,4)] # reorder the columns
mycols <- c("gray50", "wheat2", "darkorange2", "steelblue2", "steelblue4")
barplot(t(cadence), col=mycols, ylab="impressions") 
legend("topright", legend=colnames(cadence), fill=mycols, cex=0.6)

# Impressions per person
hist(xtabs(~id, data=impress), xlab="Impressions", ylab="Count of Users",
     main="Histogram of Impressions per User")

# Click through rates by channel
xtabs(click~channel, data=impress)/xtabs(~channel, data=impress) 

# ==Transactions table==
trans <- read.csv("https://goo.gl/lIAuZu")
trans$date <- as.Date(trans$date)
summary(trans)

# Transactions over time
(transbyday <- xtabs(~date, data=trans))

# Plot of transactions over time
plot(transbyday, ylab="Transactions", xlab="Date")

# Transactions per person
hist(xtabs(~id, data=trans), xlab="Number of Transactions", ylab="Count of Users", 
     main="Histogram of Transactions")

# Timeline for one customer
cust[cust$id==100,]
impress[impress$id==100,]
trans[trans$id==100,]

cust[cust$id==300,]
summary(impress[impress$id==300,])
trans[trans$id==300,]

# ====== Last-Click Attribution =======
(last.touch.att <- xtabs(~last.touch, data=trans))
barplot(last.touch.att, ylab="Transactions", 
        main="Last Touch Attribution")

# For the month of February
xtabs(~last.touch, data=trans[trans$date>as.Date("2017-01-31"),])

(last.click.att <- xtabs(~last.click, data=trans))
barplot(last.click.att, ylab="Transactions", 
        main="Last Click Attribution")

rm(last.click.att, last.touch.att)

# ======= Email Holout Test =======
# Function for pulling relavent test summaries out of the data
holdout.test <- function(test.date, delay=0, window, impress, trans) {
  test.ids <- unique(impress$id[impress$channel=="email" & impress$date==test.date])
  control.ids <- unique(impress$id[impress$channel=="email.holdout" & impress$date==test.date])
  tdata <- data.frame(ids=c(test.ids, control.ids))
  tdata$group[tdata$id %in% test.ids] <- "test"
  tdata$group[tdata$id %in% control.ids] <- "control"
  in.window <- trans$date>=(test.date+delay) & trans$date<(test.date+window+delay)
  tdata$convert <- tdata$id %in% trans$id[in.window]
  ttable <- xtabs(~ group + convert, data=tdata)
  mosaicplot(~ group + convert, data=tdata, 
             main=paste("Email Test on", test.date))
  proptest <- prop.test(x=ttable[,"TRUE"], n=xtabs(~group, data=tdata))
  diff.conv <- c(diff=proptest$estimate[2]-proptest$estimate[1], ci=-proptest$conf.int)
  out <- list(diff.conv, ttable, proptest)
}

# Different ways to analyse the "2017-01-03" test
# Window for conversions starts at test.date+delay and ends and test.date+delay+window
(holdout.test(test.date=as.Date("2017-01-03"), window=7, impress=impress, trans=trans))
(holdout.test(test.date=as.Date("2017-01-03"), window=3, impress=impress, trans=trans))

# Look at the response by day
day1 <- holdout.test(test.date=as.Date("2017-01-03"), delay=0, window=1, impress=impress, trans=trans)
day2 <- holdout.test(test.date=as.Date("2017-01-03"), delay=1, window=1, impress=impress, trans=trans)
day3 <- holdout.test(test.date=as.Date("2017-01-03"), delay=2, window=1, impress=impress, trans=trans)
day4 <- holdout.test(test.date=as.Date("2017-01-03"), delay=3, window=1, impress=impress, trans=trans)
day5 <- holdout.test(test.date=as.Date("2017-01-03"), delay=4, window=1, impress=impress, trans=trans)
day6 <- holdout.test(test.date=as.Date("2017-01-03"), delay=5, window=1, impress=impress, trans=trans)
day7 <- holdout.test(test.date=as.Date("2017-01-03"), delay=6, window=1, impress=impress, trans=trans)
incr.conv <- c(day1[[1]][1], day2[[1]][1], day3[[1]][1], day4[[1]][1], day5[[1]][1], 
               day6[[1]][1], day7[[1]][1])
plot(incr.conv, type="h", main="Email Test 2017-01-03", 
     xlab="Days after Email Sent", ylab="Increase in Conversion Rate")
abline(h=0, col="gray")

# Other email tests (not in slides)
(holdout.test(test.date=as.Date("2017-01-10"), window=3, impress=impress, trans=trans))
(holdout.test(test.date=as.Date("2017-01-17"), window=3, impress=impress, trans=trans))
(holdout.test(test.date=as.Date("2017-01-24"), window=3, impress=impress, trans=trans))
(holdout.test(test.date=as.Date("2017-01-31"), window=3, impress=impress, trans=trans))
(holdout.test(test.date=as.Date("2017-02-07"), window=3, impress=impress, trans=trans))
(holdout.test(test.date=as.Date("2017-02-14"), window=3, impress=impress, trans=trans))
(holdout.test(test.date=as.Date("2017-02-21"), window=3, impress=impress, trans=trans))

# ====== Marketing Mix Modeling ======
# Data prep: summarize impressions and transactions at the daily level
(transbyday <- xtabs(~date, data=trans))
(cadence <- xtabs(~date+channel, data=impress))
mdata <- as.data.frame(cbind(trans=transbyday[1:57], cadence[2:58,])) # aligining dates
head(mdata)
summary(mdata)

# Data exploration
plot(x=mdata$direct, y=mdata$trans, xlab="Direct Impressions", 
     ylab="Transactions", main="Direct Impressions v. Transactions")
plot(x=mdata$email, y=mdata$trans, xlab="Email Impressions", 
     ylab="Transactions", main="Email Impressions v. Transactions")
plot(x=mdata$display, y=mdata$trans, xlab="Display Impressions", 
     ylab="Transactions", main="Display Impressions v. Transactions")
plot(x=mdata$social, y=mdata$trans, xlab="Social Impressions", 
     ylab="Transactions", main="Social Impressions v. Transactions")

# Modeling
# Basic regression relating transactions to impressions
m1 <- lm(trans~direct+social+email+display+social+email.holdout, data=mdata)
summary(m1)

# Add in a day of week variable
mdata$dayofweek <- weekdays(as.Date(rownames(mdata)))
m2 <- lm(trans~email+direct+display+social+dayofweek, data=mdata)
summary(m2)

# Create and ad stock variable to account for the decay of advertising
mdata$email.stock <- as.numeric(filter(x=mdata$email, filter=0.5, method="recursive"))
mdata$display.stock <- as.numeric(filter(x=mdata$display, filter=0.3, method="recursive"))
mdata$direct.stock <- as.numeric(filter(x=mdata$direct, filter=0.75, method="recursive"))
mdata$social.stock <- as.numeric(filter(x=mdata$social, filter=0.3, method="recursive"))

head(mdata)

# Plot the ad stocks over time
plot(mdata$email.stock, type="l", xlab="Day", ylab="Email Ad Stock")
plot(mdata$display.stock, type="l", xlab="Day", ylab="Display Ad Stock")
plot(mdata$direct.stock, type="l", xlab="Days", ylab="Direct Stock")
plot(mdata$social.stock, type="l", xlab="Days", ylab="Social Stock")

# Plot the ad stocks agains the transactions
plot(mdata$email.stock, mdata$trans, xlab="Email Stock", ylab="Transactions")
plot(mdata$display.stock, mdata$trans, xlab="Display Stock", ylab="Transactions")
plot(mdata$direct.stock, mdata$trans, xlab="Direct Stock", ylab="Transactions")
plot(mdata$social.stock, mdata$trans, xlab="Social Stock", ylab="Transactions")

m3 <- lm(trans~email.stock+display.stock+direct.stock+social.stock, 
         data=mdata[5:nrow(mdata),]) # Remove first few observations to allow for "warmup" of stock
summary(m3)

# Interactions
m4 <- lm(trans~email.stock+display.stock+direct.stock+social.stock+email.stock*direct.stock, 
         data=mdata[5:nrow(mdata),]) # Remove first few observations to allow for "warmup" of stock
summary(m4)

# ======= Model-Based Attribution =======
# DATA PREP
# Summarize impressions at the day-user level
adatal <- as.data.frame(xtabs(~ id + date + channel, data=impress), stringsAsFactors=FALSE)
adatal$id <- as.integer(adatal$id)
adatal$date <- as.Date(adatal$date)
adatal$channel <- as.factor(adatal$channel)
dimnames(adatal)[[2]][4] <- "impr"
head(adatal)
str(adatal)
summary(adatal)

# Add in observations for users with no impressions. (They count, too!)
pop <- unique(cust$id)
no.impress.ids <- pop[!(pop %in% unique(impress$id))]
dates <- sort(unique(impress$date))
channels <- unique(impress$channel)
no.impress.obs <- data.frame(id=rep(no.impress.ids, each=length(dates)*length(channels)), 
                             date=rep(rep(dates, each=length(channels)), length(no.impress.ids)), 
                             channel=rep(channels, length(no.impress.ids)*length(dates)),
                             impr=rep(0, length(dates)*length(no.impress.ids)*length(channels)), 
                             stringsAsFactors=FALSE)
no.impress.obs$channel <- as.factor(no.impress.obs$channel)
summary(no.impress.obs)
adatal <- rbind(adatal, no.impress.obs)
summary(adatal)                    
summary(adatal[adatal$id==1,])

# Convert from long to wide format
adata <- reshape(adatal, direction="wide", v.names="impr", idvar=c("id", "date"), 
                timevar="channel", new.row.names=NULL)
sum(adata$impr.direct) == length(impress$channel[impress$channel=="direct"]) #quick check
nrow(adata)
head(adata)
summary(adata)
summary(adata[adata$date=="2017-01-03",])

# Add transactions
atrans <- as.data.frame(xtabs(~ id + date, data=trans), stringsAsFactors=FALSE)
atrans$id <- as.integer(atrans$id)
atrans$date <- as.Date(atrans$date)
dimnames(atrans)[[2]][3] <- "trans"
str(atrans)
adata <- merge(adata, atrans, by=c("id", "date"), all=TRUE)
adata$trans[is.na(adata$trans)] <- 0 # Fill in zeros for transactions
head(adata)

# Final tidy up
# Remove first and last days (which are incomplete)
adata <- adata[adata$date!="2016-12-31" & adata$date != "2017-02-28" & adata$date != "2017-02-27",]
# Add customer info from cust table
adata <- merge(adata, cust, by=c("id"))
# Tidy up column names
dimnames(adata)[[2]][3:11] <- c("direct", "display", "email", "email.holdout", "social", "trans", "past.purchase", "has.email", "has.direct")
rm(adatal, atrans)

# DATA EXPLORATION
summary(adata)

# Single day
summary(adata[adata$date=="2017-01-03",])

plot(aggregate(trans~direct, data=adata, FUN=mean), type="h", ylim=c(0,0.15),
     xlab="Impressions on Day", ylab="Same-Day Conversion Rate", main="Direct")
plot(aggregate(trans~email, data=adata, FUN=mean), type="h", ylim=c(0,0.15),
     xlab="Impressions on Day", ylab="Same-Day Conversion Rate", main="Email")
plot(aggregate(trans~display, data=adata, FUN=mean), type="h", ylim=c(0,0.15),
     xlab="Impressions on Day", ylab="Same-Day Conversion Rate", main="Display")
plot(aggregate(trans~social, data=adata, FUN=mean), type="h", ylim=c(0,0.15),
     xlab="Impressions on Day", ylab="Same-Day Conversion Rate", main="Social")

# MODELING
m1 <- lm(trans ~ direct + display + email + social, data=adata)
summary(m1)

m2 <- lm(trans ~ direct + display + email + social + past.purchase, data=adata)
summary(m2)

# It is more appropriate to use a logistic regression for 0-1 response
m3 <- glm(trans ~ direct + display + email + social + past.purchase, 
          family=binomial(), data=adata)
summary(m3)

