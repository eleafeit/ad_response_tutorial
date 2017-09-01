# ===== Ad Respons Workshop =====
# Synthetic Data Generation

# Elea McDonnell Feit
# efeit@wharton.upenn.edu

# features that would be nice-to-have
# clumpiness/attrition in impressions and transactions
# effect of marketing on transaction amount

rm(list=ls())
setwd("~/Google Drive/_Practitioner Presentations/2017 Ad response tutorial/R code")
set.seed("20030601")

# Customer data
ncust <- 10000
id = 1:ncust
past.purchase <- rbinom(ncust, size=1, prob=0.5)
email = rbinom(ncust, size=1, prob=0.3 + past.purchase*0.6) # has email on file
direct = rbinom(ncust, size=1, prob=0.2 + past.purchase*0.6) # had address on file
cust <- data.frame(id, past.purchase, email, direct)
summary(cust)
write.csv(cust, file="customer.csv", row.names=FALSE)

# Ad Exposure Data ("Impressions")
rimpressions <- function(id, channel, daily.rate, click.rate, start, end) {
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  diff.sec <- as.numeric(difftime(end, start, unit="sec"))
  diff.day <- as.numeric(difftime(end, start, unit="day"))
  ndate <- rpois(1, lambda = daily.rate*diff.day) 
  ev <- sort(runif(ndate, 0, diff.sec))
  rt <- start + ev
  rt <- strftime(rt, format="%Y-%m-%d", usetz=FALSE)
  click <- rbinom(ndate, size=1, prob=click.rate)
  data.frame(id=rep(id, ndate), date=as.Date(rt), channel=rep(channel, ndate),  
             click=click, stringsAsFactors=FALSE)
}
cimpressions <- function(id, channel, cadence, click.rate) {
  cadence <- as.Date(cadence)
  ndate <- length(cadence)
  click <- rbinom(ndate, size=1, prob=click.rate)
  data.frame(id=rep(id, ndate), date=cadence, channel=rep(channel, ndate), 
             click=click, stringsAsFactors=FALSE)
}
impressions <- NULL
email.cadence <- c("2017/01/03", "2017/01/10", "2017/01/17", "2017/01/24", 
                   "2017/01/31", "2017/02/07", "2017/02/14", "2017/02/21")
direct.cadence <- c("2017/01/06", "2017/02/03")
startdate <- as.Date("2017/01/01")
enddate <- as.Date("2017/02/28")
for (i in cust$id) {
  # email (treatment/control)
  if (cust$email[i]==1) {
    temp <- cimpressions(i, "email", email.cadence, click.rate=0.1)
    holdouts <- as.logical(rbinom(n=nrow(temp), size=1, p=0.20))
    temp[holdouts, "channel"] <- "email.holdout"
    temp[holdouts,"click"] <- 0
    impressions <- rbind(impressions, temp)
  }
  # directmail
  if (cust$direct[i]==1) {
    impressions <- rbind(impressions, 
                         cimpressions(i, "direct", direct.cadence, click.rate=0))
  }
  # social
  if (runif(1)<0.5) {
    social.rate <- exp(rnorm(1, mean=0.3, sd=0.5)) + cust$email[1]*2
    impressions <- rbind(impressions, 
                         rimpressions(id=i, channel="social", daily.rate=social.rate, click.rate=0.02, start=startdate, end=startdate+30))
  }
  # display
  if (runif(1)<0.3) {
    display.rate <- exp(rnorm(1, mean=0.1, sd=0.5)) + cust$email[1]*1
    impressions <- rbind(impressions, 
                         rimpressions(id=i, channel="display", daily.rate=display.rate, click.rate=0.005, start=startdate, end=enddate))
  }
}
summary(impressions)
xtabs(click~channel, data=impressions)/xtabs(~channel, data=impressions)
write.csv(impressions, file="impressions.csv", row.names=FALSE)

# Purchase Data ("Conversions")
# build up transaction rate vector by day
obs <- seq.Date(from=as.Date(startdate), to=as.Date(enddate), by="day")
transactions <- NULL
for (i in cust$id) {
  # Baseline conversion rate
  conv.rate <- rep(0.25/30 + 0.75/30*cust$past.purchase[i], length(obs))
  impr.all <- impressions[impressions$id==i,]
  impr <- impr.all[impr.all$channel=="display", ]
  if (nrow(impr)>0) {
    conv.rate[obs %in% impr$date] <- 0.01 + conv.rate[obs %in% impr$date]
  }
  impr <- impr.all[impr.all$channel=="social", ]
  if (nrow(impr)>0) {
    conv.rate[obs %in% impr$date] <- 0.02 +  conv.rate[obs %in% impr$date]
    conv.rate[obs %in% (impr$date+1)] <- 0.01 +  conv.rate[obs %in% (impr$date+1)]
  }
  impr<- impr.all[impr.all$channel=="email",]
  if (nrow(impr)>0) {
    conv.rate[obs %in% impr$date] <- 0.05 + conv.rate[obs %in% impr$date]
    conv.rate[obs %in% (impr$date+1)] <- 0.03 + conv.rate[obs %in% (impr$date+1)]
    conv.rate[obs %in% (impr$date+2)] <- 0.01 + conv.rate[obs %in% (impr$date+2)]
  }
  impr <- impr.all[impr.all$channel=="direct",]
  if (nrow(impr)>0) {
    conv.rate[obs %in% impr$date] <- 0.03 + conv.rate[obs %in% impr$date]
    conv.rate[obs %in% (impr$date+1)] <- 0.05 + conv.rate[obs %in% (impr$date+1)]
    conv.rate[obs %in% (impr$date+2)] <- 0.05 + conv.rate[obs %in% (impr$date+2)]
    conv.rate[obs %in% (impr$date+3)] <- 0.05 + conv.rate[obs %in% (impr$date+3)]
    conv.rate[obs %in% (impr$date+4)] <- 0.03 + conv.rate[obs %in% (impr$date+4)]
    conv.rate[obs %in% (impr$date+5)] <- 0.02 + conv.rate[obs %in% (impr$date+5)]
    conv.rate[obs %in% (impr$date+6)] <- 0.01 + conv.rate[obs %in% (impr$date+6)]
  }
  trans <- rbinom(length(obs), size=1, prob=conv.rate)
  if (sum(trans)>0) { 
    trans.date <- obs[as.logical(trans)]
    transactions <- rbind(transactions, 
                          data.frame(id=rep(i, length(trans.date)), date=trans.date))
  }
}
hist(xtabs(~id, data=transactions))

# Append last-click and last touch data to transactions
append.last <- function(trans, impress) {
  out <- data.frame(trans, last.touch=NA, last.click=NA)
  for (t in 1:nrow(out)) {
    impr <- impress[impress$id==out$id[t] & impress$date<=out$date[t] & impress$channel!="email.holdout", ]
    if (nrow(impr)>0) {
      out$last.touch[t] <- as.character(sample(impr$channel[impr$date==max(impr$date)], 1)) # choose randomly
    }
    impr <- impr[impr$click==1,]
    if (nrow(impr)>0){
      out$last.click[t] <- as.character(sample(impr$channel[impr$date==max(impr$date)], 1))
    }
  }
  out[is.na(out)] <- "none"
  out
}
trans <- append.last(transactions, impressions)

write.csv(transactions, file="transactions.csv", row.names=FALSE)
