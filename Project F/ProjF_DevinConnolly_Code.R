##PROJECT F - DECEMBER 10th 2019
## DEVIN CONNOLLY (15315916)


#Reading in and cleaning and organising data
data_2010 <- read.delim("2010_data.txt",
                     stringsAsFactors=FALSE, header = T, sep = ",")

data_2010$timestamp <- as.POSIXct(paste(data_2010$date, data_2010$time),
                                 format="%Y-%m-%d %H:%M:%S")

aig_2010 <- data_2010[which(data_2010$symbol == "AIG"),]
aig_2010_5th <- aig_2010[which(aig_2010$date == "2010-05-05"),]
aig_2010_6th <- aig_2010[which(aig_2010$date == "2010-05-06"),]

dia_2010 <- data_2010[which(data_2010$symbol == "DIA"),]
dia_2010_5th <- dia_2010[which(dia_2010$date == "2010-05-05"),]
dia_2010_6th <- dia_2010[which(dia_2010$date == "2010-05-06"),]

ko_2010 <- data_2010[which(data_2010$symbol == "ko"),]
ko_2010_5th <- ko_2010[which(ko_2010$date == "2010-05-05"),]
ko_2010_6th <- ko_2010[which(ko_2010$date == "2010-05-06"),]

pg_2010 <- data_2010[which(data_2010$symbol == "PG"),]
pg_2010_5th <- pg_2010[which(pg_2010$date == "2010-05-05"),]
pg_2010_6th <- pg_2010[which(pg_2010$date == "2010-05-06"),]

data_2015 <- read.delim("2015_data_new.txt",
                        stringsAsFactors=FALSE, header = T, sep = ",")

data_2015$timestamp <- as.POSIXct(paste(data_2015$date, data_2015$time_m),
                           format="%Y-%m-%d %H:%M:%OS")

data_2015$sym_root <- trimws(data_2015$sym_root)

aig_2015 <- data_2015[which(data_2015$sym_root == "AIG"),]
dia_2015 <- data_2015[which(data_2015$sym_root == "DIA"),]
ko_2015 <- data_2015[which(data_2015$sym_root == "ko"),]
pg_2015 <- data_2015[which(data_2015$sym_root == "PG"),]

day_part_start <- "09:30:00.000"
day_part_end <- "16:00:00.000"

aig_2015 = aig_2015[((aig_2015$time_m > day_part_start) & (aig_2015$time_m < day_part_end)),]
dia_2015 = dia_2015[((dia_2015$time_m > day_part_start) & (dia_2015$time_m < day_part_end)),]
ko_2015 = ko_2015[((ko_2015$time_m > day_part_start) & (ko_2015$time_m < day_part_end)),]
pg_2015 = pg_2015[((pg_2015$time_m > day_part_start) & (pg_2015$time_m < day_part_end)),]

AIG_2015_trade <- xts(aig_2015[,c("price","size", "timestamp")],order.by=aig_2015$timestamp)
DIA_2015_trade <- xts(dia_2015[,c("price","size", "timestamp")],order.by=dia_2015$timestamp)
KO_2015_trade <- xts(ko_2015[,c("price","size", "timestamp")],order.by=ko_2015$timestamp)
PG_2015_trade <- xts(pg_2015[,c("price","size", "timestamp")],order.by=pg_2015$timestamp)

##### QUESTION 1 #####

###AIG Calculations
aig_2010_5th$price[which(aig_2010_5th$time == min(aig_2010_5th$time))]
aig_2010_5th$price[which(aig_2010_5th$time == max(aig_2010_5th$time))]
min(aig_2010_5th$price)
max(aig_2010_5th$price)
sum(aig_2010_5th$size)

aig_2010_6th$price[which(aig_2010_6th$time == min(aig_2010_6th$time))]
aig_2010_6th$price[which(aig_2010_6th$time == max(aig_2010_6th$time))]
min(aig_2010_6th$price)
max(aig_2010_6th$price)
sum(aig_2010_6th$size)

AIG_2015_trade$price[1]
max(AIG_2015_trade$price)
min(AIG_2015_trade$price)
AIG_2015_trade$price[length(AIG_2015_trade$price)]
sum(as.numeric(AIG_2015_trade$size)) * 100

###DIA Calculations
table(dia_2010_5th$price[which(dia_2010_5th$time == min(dia_2010_5th$time))])
table(dia_2010_5th$price[which(dia_2010_5th$time == max(dia_2010_5th$time))])
min(dia_2010_5th$price)
max(dia_2010_5th$price)
sum(dia_2010_5th$size)

table(dia_2010_6th$price[which(dia_2010_6th$time == min(dia_2010_6th$time))])
table(dia_2010_6th$price[which(dia_2010_6th$time == max(dia_2010_6th$time))])
min(dia_2010_6th$price)
max(dia_2010_6th$price)
sum(dia_2010_6th$size)

DIA_2015_trade$price[1]
max(DIA_2015_trade$price)
min(DIA_2015_trade$price)
DIA_2015_trade$price[length(DIA_2015_trade$price)]
sum(as.numeric(DIA_2015_trade$size)) * 100

###KO Calculations
table(ko_2010_5th$price[which(ko_2010_5th$time == min(ko_2010_5th$time))])
table(ko_2010_5th$price[which(ko_2010_5th$time == max(ko_2010_5th$time))])
min(ko_2010_5th$price)
max(ko_2010_5th$price)
sum(ko_2010_5th$size)

table(ko_2010_6th$price[which(ko_2010_6th$time == min(ko_2010_6th$time))])
table(ko_2010_6th$price[which(ko_2010_6th$time == max(ko_2010_6th$time))])
min(ko_2010_6th$price)
max(ko_2010_6th$price)
sum(ko_2010_6th$size)

KO_2015_trade$price[1]
max(KO_2015_trade$price)
min(KO_2015_trade$price)
KO_2015_trade$price[length(KO_2015_trade$price)]
sum(as.numeric(KO_2015_trade$size)) * 100

###PG Calculations
table(pg_2010_5th$price[which(pg_2010_5th$time == min(pg_2010_5th$time))])
table(pg_2010_5th$price[which(pg_2010_5th$time == max(pg_2010_5th$time))])
min(pg_2010_5th$price)
max(pg_2010_5th$price)
sum(pg_2010_5th$size)

table(pg_2010_6th$price[which(pg_2010_6th$time == min(pg_2010_6th$time))])
table(pg_2010_6th$price[which(pg_2010_6th$time == max(pg_2010_6th$time))])
min(pg_2010_6th$price)
max(pg_2010_6th$price)
sum(pg_2010_6th$size)

PG_2015_trade$price[1]
max(PG_2015_trade$price)
min(PG_2015_trade$price)
PG_2015_trade$price[length(PG_2015_trade$price)]
sum(as.numeric(PG_2015_trade$size)) * 100

### Question 2 - Bid Ask Spread
aig_spread <- aig_2015$ask - aig_2015$bid
max(aig_spread)
min(aig_spread)
mean(aig_spread)

dia_spread <- dia_2015$ask - dia_2015$bid
max(dia_spread)
min(dia_spread)
mean(dia_spread)

which(dia_spread == min(dia_spread))
sort(dia_spread, decreasing = F)[1:5]

quantile(dia_spread, seq(0, 1, 0.01))

ko_spread <- ko_2015$ask - ko_2015$bid
max(ko_spread)
min(ko_spread)
mean(ko_spread)

pg_spread <- pg_2015$ask - pg_2015$bid
max(pg_spread)
min(pg_spread)
mean(pg_spread)

### Question 2 - Order Book Imbalance
aig_imbalance = (aig_2015$bidsiz - aig_2015$asksiz)/(aig_2015$bidsiz + aig_2015$asksiz)
max(aig_imbalance)
min(aig_imbalance)
mean(aig_imbalance)
plot(aig_imbalance[1:125], type = 'l')

dia_imbalance = (dia_2015$bidsiz - dia_2015$asksiz)/(dia_2015$bidsiz + dia_2015$asksiz)
max(dia_imbalance)
min(dia_imbalance)
mean(dia_imbalance)
plot(dia_imbalance[1:125], type = 'l')

ko_imbalance = (ko_2015$bidsiz - ko_2015$asksiz)/(ko_2015$bidsiz + ko_2015$asksiz)
max(ko_imbalance)
min(ko_imbalance)
mean(ko_imbalance)
plot(ko_imbalance[1:125], type = 'l')

pg_imbalance = (pg_2015$bidsiz - pg_2015$asksiz)/(pg_2015$bidsiz + pg_2015$asksiz)
max(pg_imbalance)
min(pg_imbalance)
mean(pg_imbalance)
plot(pg_imbalance[1:125], type = 'l')

### 5 Minute Series
### AIG
aig_2010_5th_xts <- xts(aig_2010_5th[,c("price")],order.by=aig_2010_5th$timestamp)
aig_2010_5th_xts[,1] <- as.numeric(aig_2010_5th_xts)
aig_5th <- (period.apply(aig_2010_5th_xts,endpoints(aig_2010_5th_xts,"mins",5),mean))
aig_5th <- align.time(aig_5th, 5*60)
aig_5th[nrow(aig_5th)-1,1] <- mean(aig_5th[(nrow(aig_5th)-1):nrow(aig_5th), ])
aig_5th <- aig_5th[1:(nrow(aig_5th)-1)]

aig_5th[which(aig_5th == min(aig_5th))]
plot(aig_5th, main = "AIG - 5 Minute Time Series")

aig_2010_6th_xts <- xts(aig_2010_6th[,c("price")],order.by=aig_2010_6th$timestamp)
aig_2010_6th_xts[,1] <- as.numeric(aig_2010_6th_xts)
aig_6th <- (period.apply(aig_2010_6th_xts,endpoints(aig_2010_6th_xts,"mins",5),mean))
aig_6th <- align.time(aig_6th, 5*60)
aig_6th[nrow(aig_6th)-1,1] <- mean(aig_6th[(nrow(aig_6th)-1):nrow(aig_6th), ])
aig_6th <- aig_6th[1:(nrow(aig_6th)-1)]

aig_6th[which(aig_6th == min(aig_6th))]
plot(aig_6th, main = "AIG - 5 Minute Time Series")

### DIA
dia_2010_5th_xts <- xts(dia_2010_5th[,c("price")],order.by=dia_2010_5th$timestamp)
dia_2010_5th_xts[,1] <- as.numeric(dia_2010_5th_xts)
dia_5th <- (period.apply(dia_2010_5th_xts,endpoints(dia_2010_5th_xts,"mins",5),mean))
dia_5th <- align.time(dia_5th, 5*60)
dia_5th[nrow(dia_5th)-1,1] <- mean(dia_5th[(nrow(dia_5th)-1):nrow(dia_5th), ])
dia_5th <- dia_5th[1:(nrow(dia_5th)-1)]

dia_5th[which(dia_5th == min(dia_5th))]
plot(dia_5th, main = "DIA - 5 Minute Time Series")

dia_2010_6th_xts <- xts(dia_2010_6th[,c("price")],order.by=dia_2010_6th$timestamp)
dia_2010_6th_xts[,1] <- as.numeric(dia_2010_6th_xts)
dia_6th <- (period.apply(dia_2010_6th_xts,endpoints(dia_2010_6th_xts,"mins",5),mean))
dia_6th <- align.time(dia_6th, 5*60)
dia_6th[nrow(dia_6th)-1,1] <- mean(dia_6th[(nrow(dia_6th)-1):nrow(dia_6th), ])
dia_6th <- dia_6th[1:(nrow(dia_6th)-1)]

dia_6th[which(dia_6th == min(dia_6th))]
plot(dia_6th, main = "DIA - 5 Minute Time Series")

### KO
ko_2010_5th_xts <- xts(ko_2010_5th[,c("price")],order.by=ko_2010_5th$timestamp)
ko_2010_5th_xts[,1] <- as.numeric(ko_2010_5th_xts)
ko_5th <- (period.apply(ko_2010_5th_xts,endpoints(ko_2010_5th_xts,"mins",5),mean))
ko_5th <- align.time(ko_5th, 5*60)
ko_5th[nrow(ko_5th)-1,1] <- mean(ko_5th[(nrow(ko_5th)-1):nrow(ko_5th), ])
ko_5th <- ko_5th[1:(nrow(ko_5th)-1)]

ko_5th[which(ko_5th == min(ko_5th))]
plot(ko_5th, main = "KO - 5 Minute Time Series")

ko_2010_6th_xts <- xts(ko_2010_6th[,c("price")],order.by=ko_2010_6th$timestamp)
ko_2010_6th_xts[,1] <- as.numeric(ko_2010_6th_xts)
ko_6th <- (period.apply(ko_2010_6th_xts,endpoints(ko_2010_6th_xts,"mins",5),mean))
ko_6th <- align.time(ko_6th, 5*60)
ko_6th[nrow(ko_6th)-1,1] <- mean(ko_6th[(nrow(ko_6th)-1):nrow(ko_6th), ])
ko_6th <- ko_6th[1:(nrow(ko_6th)-1)]

ko_6th[which(ko_6th == min(ko_6th))]
plot(ko_6th, main = "KO - 5 Minute Time Series")

### PG
pg_2010_5th_xts <- xts(pg_2010_5th[,c("price")],order.by=pg_2010_5th$timestamp)
pg_2010_5th_xts[,1] <- as.numeric(pg_2010_5th_xts)
pg_5th <- (period.apply(pg_2010_5th_xts,endpoints(pg_2010_5th_xts,"mins",5),mean))
pg_5th <- align.time(pg_5th, 5*60)
pg_5th[nrow(pg_5th)-1,1] <- mean(pg_5th[(nrow(pg_5th)-1):nrow(pg_5th), ])
pg_5th <- pg_5th[1:(nrow(pg_5th)-1)]

pg_5th[which(pg_5th == min(pg_5th))]
plot(pg_5th, main = "PG - 5 Minute Time Series")

pg_2010_6th_xts <- xts(pg_2010_6th[,c("price")],order.by=pg_2010_6th$timestamp)
pg_2010_6th_xts[,1] <- as.numeric(pg_2010_6th_xts)
pg_6th <- (period.apply(pg_2010_6th_xts,endpoints(pg_2010_6th_xts,"mins",5),mean))
pg_6th <- align.time(pg_6th, 5*60)
pg_6th[nrow(pg_6th)-1,1] <- mean(pg_6th[(nrow(pg_6th)-1):nrow(pg_6th), ])
pg_6th <- pg_6th[1:(nrow(pg_6th)-1)]

pg_6th[which(pg_6th == min(pg_6th))]
plot(pg_6th, main = "PG - 5 Minute Time Series")

### Question 4 Volatilities
# AIG
aig_5th_returns <- exp(diff(log(aig_5th))) - 1
aig5_vol <- sd(aig_5th_returns[-1]) * sqrt(252)

aig_6th_returns <- exp(diff(log(aig_6th))) - 1
aig6_vol <- sd(aig_6th_returns[-1]) * sqrt(252)

# DIA
dia_5th_returns <- exp(diff(log(dia_5th))) - 1
dia5_vol <- sd(dia_5th_returns[-1]) * sqrt(252)

dia_6th_returns <- exp(diff(log(dia_6th))) - 1
dia6_vol <- sd(dia_6th_returns[-1]) * sqrt(252)

# KO
ko_5th_returns <- exp(diff(log(ko_5th))) - 1
 ko5_vol <- sd(ko_5th_returns[-1]) * sqrt(252)

ko_6th_returns <- exp(diff(log(ko_6th))) - 1
ko6_vol <- sd(ko_6th_returns[-1]) * sqrt(252)

# PG
pg_5th_returns <- exp(diff(log(pg_5th))) - 1
pg5_vol <- sd(pg_5th_returns[-1]) * sqrt(252)

pg_6th_returns <- exp(diff(log(pg_6th))) - 1
pg6_vol <- sd(pg_6th_returns[-1]) * sqrt(252)


#Reading in Actual Prices
price_data <- read.csv("2010_Prices.csv", header = T)
price_data <- price_data[6:47,] ##Using 42 trading days (2-months)

#Calculating Realized Volatility
dia_realized <- sd(exp(diff(log(price_data[,2]))) - 1) * sqrt(252)
aig_realized <- sd(exp(diff(log(price_data[,4]))) - 1) * sqrt(252)
ko_realized <- sd(exp(diff(log(price_data[,3]))) - 1) * sqrt(252)
pg_realized <- sd(exp(diff(log(price_data[,5]))) - 1) * sqrt(252)



