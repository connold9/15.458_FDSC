setwd("/Users/devinconnolly/Documents/MIT Academic/Fall 2019/Data Science and Computing/Assignment 1")

#Read in two datasets - one for SPX and one for INDU
spx_data <- read.csv("spx.csv")
dowjones_data <- read.csv("dowjones.csv")

#Look at date range
head(spx_data)
tail(spx_data)

#Converting date column to date type
spx_data$Dates = as.Date(spx_data$Dates, "%d/%m/%Y")
dowjones_data$Dates = as.Date(dowjones_data$Dates, "%d/%m/%Y")

#SPX began trading in 1928, but did not become S&P500 until March 4, 1957. 4/3/1957, lets subset
start <- which(spx_data$Dates == "1957-04-03")
spx_data <- spx_data[start:length(spx_data$Dates),]
rownames(spx_data) <- seq(length=nrow(spx_data))

#Testing the completeness and integrity of the data
spx_na <- sum(is.na(spx_data))
dowjones_na <- sum(is.na(dowjones_data))
sapply(spx_data, function(x) sum(is.na(x)))
sapply(dowjones_data, function(x) sum(is.na(x)))

spx_diff <- diff(spx_data$PX_LAST)
head(which(spx_diff == 0))
length(which(spx_diff == 0))

spx_data[106:111,]

dow_diff <- diff(dowjones_data$PX_LAST)
head(which(dow_diff == 0))
tail(which(dow_diff == 0))
length(which(dow_diff == 0))

dowjones_data[17:22,]
dowjones_data[31327:31330,]

#Check for Duplicate Values, with and without date
sum(duplicated(spx_data))
sum(duplicated(dowjones_data))

sum(duplicated(spx_data[,-1]))
sum(duplicated(dowjones_data[,-1]))

tail(which(duplicated(spx_data[,-1])))
spx_data[16280:16289,]

#Looking at the these duplicates, we see that for the 2nd of September is because of Labor day,
#while september 9th is today (day data was received)

#Check for negative values - ignore dates
sapply(spx_data[,-1], function(x) sum(x <= 0))
sapply(dowjones_data[,-1], function(x) sum(x <= 0))

#Does Hi Low make sense
length(which(spx_data$PX_LOW > spx_data$PX_HIGH))
length(which(dowjones_data$PX_LOW > dowjones_data$PX_HIGH))

#Signicant number where high = low, as intraday data not recorded for beginning of data set. 
length(which(spx_data$PX_LOW == spx_data$PX_HIGH))
length(which(dowjones_data$PX_LOW == dowjones_data$PX_HIGH))

sum(spx_data$PX_HIGH != apply(spx_data[,-1], 1, max))
sum(dowjones_data$PX_HIGH != apply(dowjones_data[,-1], 1, max))
which(dowjones_data$PX_HIGH != apply(dowjones_data, 1, max))
head(which(dowjones_data$PX_HIGH < dowjones_data$PX_LAST))
dowjones_data[4084:4088,]

#There exists rows where the last price is GREATER than the HIGH price. 
#To correct these, we could replace high with last, or likewise last with high
#It is difficult to judge as we do not know which contains the error

#Q1 (C) PROBABILITY ESTIMATION

#Here, it may be better to subset the data to only those days which intraday prices are recorded. 
#Or else the probabilities will be overstated due to this data insufficiency.
length(which(spx_data$PX_OPEN == spx_data$PX_LAST))
index <- which(spx_data$PX_OPEN == spx_data$PX_LAST)

#Can see that daily data is available from index 6536 onwards, which is 21st April 1982
index[6500:6550]
spx_data[6530:6540,]

#Take the necessary subset
spx_data_sub = spx_data[6536:nrow(spx_data),]

#Estimate the probabilities using occurences in sample
# P(High = Open)

length_HO <- length(which(spx_data_sub$PX_HIGH == spx_data_sub$PX_OPEN))
prob_HO <- length_HO/nrow(spx_data_sub)

length_HC <- length(which(spx_data_sub$PX_HIGH == spx_data_sub$PX_LAST))
prob_HC <- length_HC/nrow(spx_data_sub)

length_LO <- length(which(spx_data_sub$PX_LOW == spx_data_sub$PX_OPEN))
prob_LO <- length_LO/nrow(spx_data_sub)

length_LC <- length(which(spx_data_sub$PX_LOW == spx_data_sub$PX_LAST))
prob_LC <- length_LC/nrow(spx_data_sub)

results <- c(prob_HO, prob_HC, prob_LO, prob_LC)
names(results) <- c("Hi = Open", "Hi = Close", "Lo = Open", "Lo = Close")
results

# Testing the RW Hypothesis:
#
#
#

#Question 1 D INTRADAY RANGES
#Find date ranges for this test

start = which(spx_data$Dates == "1980-01-01")
end = which(spx_data$Dates == "2011-08-30")

spx_data_intra <- spx_data[start:end,]
spx_data_intra$intraday <- (spx_data_intra$PX_HIGH - spx_data_intra$PX_LAST) / spx_data_intra$PX_LAST

ordered_spx_intra <- spx_data_intra[order(spx_data_intra$intraday, decreasing = T),]
top_20_intra <- ordered_spx_intra[1:20,]
top_20_dated <- top_20_intra[order(top_20_intra$Dates),]

count_2008_onwards <- length(which(top_20_dated$Dates > "2008-01-01"))

#Question 1 E Overnight Returns
spx_data$overnight <- c(0, (spx_data$PX_OPEN[2:nrow(spx_data)] / spx_data$PX_LAST[1:nrow(spx_data)-1]) - 1)

start = which(spx_data$Dates == "1980-01-01")
end = which(spx_data$Dates == "2011-08-30")

spx_data_overnight <- spx_data[start:end,]

ordered_overnight <- spx_data_overnight[order(spx_data_overnight$overnight, decreasing = T),]
head(ordered_overnight)
tail(ordered_overnight)

top_20_ordered_overnight <- ordered_overnight[1:20,]
bottom_20_ordered_overnight <- ordered_overnight[(nrow(ordered_overnight)-19):nrow(ordered_overnight),]

top_20_overnight_dated <- top_20_ordered_overnight[order(top_20_ordered_overnight$Dates),]
bottom_20_overnight_dated <- bottom_20_ordered_overnight[order(bottom_20_ordered_overnight$Dates),]

top_20_overnight_dated
bottom_20_overnight_dated

length(which(top_20_overnight_dated$Dates < "1983-01-01"))
length(which(bottom_20_overnight_dated$Dates < "1983-01-01"))

#Question 1 F MARKET VOLATILITY
spx_RET <- diff(log(spx_data$PX_LAST))
spx_data$RET <- c(0, spx_RET)

library(RcppRoll)
rolled_sd <- roll_sd(spx_data$RET, 63)
zeros <- c(rep(0, 62))
spx_data$roll_sd <- c(zeros, rolled_sd)
spx_data$roll_2<- rollapply(spx_data$RET, width = 63, align = "right", FUN = sd, fill = NA)

jumps <- spx_data$RET[63:nrow(spx_data)] / spx_data$roll_sd[63:nrow(spx_data)]

spx_data$jumps <- c(zeros, jumps)

#Black Monday top of the list, ~7.3
spx_jump_ordered <- spx_data[order(abs(spx_data$jumps), decreasing = T),]
top_20_jumps <- spx_jump_ordered[1:20,]
top_20_jumps_dated <- top_20_jumps[order(top_20_jumps$Dates),]

length(which((top_20_jumps_dated$Dates < "2011-08-30") & (top_20_jumps_dated$Dates > "2008-08-30")))

#Q2 DATA DISCREPANCY
#Start by reading in Data from other sources

symbol = "^GSPC"
startday = as.Date("1982-10-1") # Year-Month-Day
endday = as.Date("1982-10-31") # Year-Month-Day
loadSymbols(Symbols = symbol, # to get the documentation to any function, you can type into console ?<function>
            src = "yahoo",
            from = startday,
            to = endday)

#GSPC for October
index <- which(index(GSPC) == '1982-10-06')
yh_oct6 <- GSPC[(index-1):(index+1),]

#Isolate this date in Bloomberg data
bb_index = which(spx_data$Dates == "1982-10-6")
bb_oct6 <- spx_data[(bb_index-1):(bb_index+1),1:5 ]

bb_oct6
yh_oct6

# QUESTION 3 - DOW JONES INDEX
install.packages("readxl")
library(readxl)

march6 <- read_excel("Mar 06 2015.xlsx")
march9 <- read_excel("Mar 09 2015.xlsx")
march18 <- read_excel("Mar 18 2015.xlsx")
march19 <- read_excel("Mar 19 2015.xlsx")

library(quantmod)
symbol = "AAPL"
startday = as.Date("2015-03-01")
endday = as.Date("2015-03-31")
loadSymbols(Symbols = symbol,
            src = "yahoo",
            from = startday,
            to = endday)

symbol = "T"
startday = as.Date("2015-03-01")
endday = as.Date("2015-03-31")
loadSymbols(Symbols = symbol,
            src = "yahoo",
            from = startday,
            to = endday)

#Subsetting INDU data to just March 2015
start = which(dowjones_data$Dates == "2015-03-02")
end = which(dowjones_data$Dates == "2015-03-31")
dow_sub <- dowjones_data[start:end,]

#Extracting INDU prices for 6th and 9th of March
mv6_march <- sum(march6$Price)
mv9_march <- sum(march9$Price)

#Get AAPL and T prices for the 6th of March
p_AAPL_6 <- as.numeric(AAPL[which(index(AAPL) == "2015-03-06"),4])
p_T_6 <- as.numeric(T[which(index(T) == "2015-03-06"),4])

#Calculate new MV given adding AAPL and removing T
new_mv  <- mv6_march + p_AAPL_6 - p_T_6

#Formula -> D = Old MV / Index Price
dow_6th <- dow_sub[(dow_sub$Dates) == '2015-03-06',]
dow_6th_price <- dow_6th$PX_LAST

#Current divisor, and new divisor given the change
divisor <- mv6_march / dow_6th_price
divisor_new <- new_mv /dow_6th_price

#Testing the calculation with other formula
divisor * (new_mv/mv6_march)

#QUESTION 3 B - Weight of T - Departing Company
T_weight <- p_T_6 / mv6_march

#QUESTION 3 C - Weights of Remaining Companies
before_29 <- sum(march6$Weight) - T_weight*100

AAPL_weight <- p_AAPL_6 / mv6_march

after_29 <- sum(march9$Weight) - AAPL_weight*100

#QUESTION 3 D - AMZN, BERK instead of AAPL
symbol = "AMZN"
startday = as.Date("2015-03-01")
endday = as.Date("2015-03-31")
loadSymbols(Symbols = symbol,
            src = "yahoo",
            from = startday,
            to = endday)

symbol = "BRK-A"
startday = as.Date("2015-03-01")
endday = as.Date("2015-03-31")
loadSymbols(Symbols = symbol,
            src = "yahoo",
            from = startday,
            to = endday)

p_AMZN_6 <- as.numeric(AMZN[which(index(AMZN) == "2015-03-06"),4])
p_BRK_6 <- as.numeric(`BRK-A`[which(index(`BRK-A`) == "2015-03-06"),4])

new_mv_AMZN  <- mv6_march + p_AMZN_6 - p_T_6
new_mv_BERK <- mv6_march + p_BRK_6 - p_T_6
divisor_new_AMZN <- new_mv_AMZN /dow_6th_price
divisor_new_BERK <- new_mv_BERK /dow_6th_price
