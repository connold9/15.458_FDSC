gold_oil <- read.csv("gold_oil.csv")
oil_future <- read.csv("oil_futures.csv")
spx_sp1 <- read.csv("spx_sp1.csv")
spy <- read.csv("spy.csv")

oil_future$Dates <- as.Date(oil_future$Dates, format = "%m/%d/%Y")
spy$Dates <- as.Date(spy$Dates, format = "%m/%d/%Y")
gold_oil$Dates <- as.Date(gold_oil$Dates, format = "%m/%d/%Y")
spx_sp1$Dates <- as.Date(spx_sp1$Dates, format = "%m/%d/%Y")

oil_price <- gold_oil[,c(1,10)]
futures_price <- oil_future[,c(1,2)]
oil <- merge(oil_price, futures_price, by="Dates")

spx_sp1$SPX_return <- (spx_sp1$SPX_PX_LAST-lag(spx_sp1$SPX_PX_LAST))/lag(spx_sp1$SPX_PX_LAST)
spx_sp1$SP1_return <- (spx_sp1$SP1_PX_LAST-lag(spx_sp1$SP1_PX_LAST))/lag(spx_sp1$SP1_PX_LAST)

spx_reg<-lm(SPX_return~SP1_return, data = spx_sp1)
cf_spx <- round(coef(spx_reg), 2) 
eq_spx <- paste0("SPX = ",abs(cf_spx[2]), " x SP1 ")

#SPX and SP1 Comparison
ggplot(spx_sp1) + geom_point(aes(x = SP1_return, y = SPX_return)) + 
  geom_smooth(aes(y = SPX_return, x = SP1_return), method = lm, formula = y ~ x) +
  labs(x="SP1 Returns",y="SPX Returns") + theme_minimal() +
  geom_text(aes(0.04, -0.045, label = eq_spx, family = "Arial"))

#SPX and SPY Comparison
spy<-merge(spy, spx_sp1[,c(1,2)], by = "Dates")
spy$SPX_return <- (spy$SPX_PX_LAST-lag(spy$SPX_PX_LAST))/lag(spy$SPX_PX_LAST)
spy$SPY_return <- (spy$SPY_PX_LAST-lag(spy$SPY_PX_LAST))/lag(spy$SPY_PX_LAST)

spy_reg<-lm(SPX_return~SPY_return, data = spy)
cf_spy <- round(coef(spy_reg), 4) 
eq_spy <- paste0("SPX = ",abs(cf_spy[2]), "x SPY ")

ggplot(spy) + geom_point(aes(x = SPY_return, y = SPX_return)) + 
  geom_smooth(aes(y = SPX_return, x = SPY_return), method = lm, formula = y ~ x) +
  labs(x="SPY Returns",y="SPX Returns") + theme_minimal() + 
  geom_text(aes(0.04, -0.045, label = eq_spy, family="Arial"))

#Gold Commodity vs Gold Futures
gold_oil$GOLDS_return <- (gold_oil$GOLDS_PX_LAST-lag(gold_oil$GOLDS_PX_LAST))/lag(gold_oil$GOLDS_PX_LAST)
gold_oil$GLD_return <- (gold_oil$GLD_PX_LAST-lag(gold_oil$GLD_PX_LAST))/lag(gold_oil$GLD_PX_LAST)

gold_reg<-lm(GOLDS_return~GLD_return, data = gold_oil)
cf_gold <- round(coef(gold_reg), 4) 
eq_gold <- paste0("GOLDS Cmdty = ",abs(cf_gold[2]), " x GLD Equity")

ggplot(gold_oil) + geom_point(aes(x = GLD_return, y = GOLDS_return)) + 
  geom_smooth(aes(y = GOLDS_return, x = GLD_return), method = lm, formula = y ~ x) +
  labs(x="GLD Returns",y="GOLDS Returns") + theme_minimal() + 
  geom_text(aes(0.01, -0.045, label = eq_gold, family="Arial"))

#Oil vs Oil Futures
oil$OIL_return <- (oil$CL1_CL1_PX_LAST-lag(oil$CL1_CL1_PX_LAST))/lag(oil$CL1_CL1_PX_LAST)
oil$CLG0_return <- (oil$CLG0_PX_LAST-lag(oil$CLG0_PX_LAST))/lag(oil$CLG0_PX_LAST)

oil_reg<-lm(OIL_return~CLG0_return, data = oil)
cf_oil <- round(coef(oil_reg), 4) 
eq_oil <- paste0("WTI Spot = ",abs(cf_oil[2]), " x One-Month Futures")

ggplot(oil) + geom_point(aes(x = CLG0_return, y = OIL_return)) + 
  geom_smooth(aes(y = OIL_return, x = CLG0_return), method = lm, formula = y ~ x) +
  labs(x="WTI Spot Returns",y="One-Month Futures Returns") + theme_minimal() +
  geom_text(aes(0.04, -0.08, label = eq_oil, family="Arial"))


####################  
##QUESTION 3 START##
####################
##### PART A #######

q3_data <- read.csv("q3_option_data.csv")
k90 <- q3_data[,c(1:10)]
k95 <- q3_data[,c(1,11:19)]
k100 <- q3_data[,c(1,20:28)]
k90$TradeDate <- as.Date(k90$TradeDate, format = "%m/%d/%Y")
k95$TradeDate <- as.Date(k95$TradeDate, format = "%m/%d/%Y")
k100$TradeDate <- as.Date(k100$TradeDate, format = "%m/%d/%Y")
names <- as.vector(colnames(k90))
colnames(k95) <- names
colnames(k100) <- names

d_rf <- read.csv("d_rf_data.csv", header = F)
colnames(d_rf) <- c("Date", "Ticker", "Expiry", "Strike","CallPut", "Bid", "Mid", "Ask",
                    "Delta", "IV", "RF", "Spot", "Dividend")
d_rf <- d_rf[,1:12]
d_rf$Date<- as.Date(d_rf$Date, format = "%Y-%m-%d")
d_rf$Expiry<- as.Date(d_rf$Expiry, format = "%Y-%m-%d")

div_data <- read.csv("dia_div.csv", col.names = c("Date", "Dividend"), header = F, stringsAsFactors = F)
div_data$Date <- as.Date(div_data$Date, format = "%Y-%m-%d")
div_data[which(div_data$Dividend == "NULL"),2] <- 0
div_data$Dividend <- as.double(div_data$Dividend)

duplicates <- which(duplicated(d_rf$Date))
daily_data <- d_rf[-duplicates,]

daily_rf_spot <- daily_data[,c(1,11,12)]

k90_dates<- as.data.frame(k90$TradeDate)
colnames(k90_dates) <- "Date"
k90_dates <- merge(k90_dates, daily_rf_spot, by = "Date", all.x = T)
k90_dates[196,2:3] <- k90_dates[195, 2:3]

daily_rf_spot <- k90_dates
div_data <- div_data[1:262,]


### BUILDING THE ALGORITHM ###
days <- nrow(k100)
daily_data <- daily_data[1:days,]

cash_pos <- rep(0, days)
shares <- rep(0, days)
options <- rep(0, days)
mv <- rep(0, days)
profit <- rep(0, days)

options[1] <- -45*100
shares[1] <- round(-options[1]*k90$Delta[1],0)
cash_pos[1] <- -options[1]*k90$Ask[1] - shares[1]*daily_rf_spot$Spot[1]
mv[1] <- options[1]*(k90$P[1] - k90$Ask[1])
profit[1] <- mv[1]
delta_t <- 1/252

#big Q = options
#little q = stocks
for(i in 2:days){
  options[i] <- options[i-1]
  shares[i] <- round(-options[i]*k90$Delta[i],0)
  
  #cash_pos[i] <- (1+daily_rf_spot$RF[i]*delta_t)*cash_pos[i-1] - 
    #(shares[i] - shares[i-1])*daily_rf_spot$Spot[i] + shares[i]*div_data$Dividend[i]
  
  cash_pos[i] <- (1+daily_rf_spot$RF[i-1]*delta_t)*cash_pos[i-1] - 
  (shares[i] - shares[i-1])*daily_rf_spot$Spot[i]
  
  mv[i] <- options[i]*k90$P[i] + shares[i]*daily_rf_spot$Spot[i] + cash_pos[i] + shares[i]*div_data$Dividend[i-1]
  
  profit[i] <- mv[i] - mv[i-1]
}

final_cash_90 = (1+daily_rf_spot$RF[days-1]*delta_t)*cash_pos[days-1] +
             (shares[days]*daily_rf_spot$Spot[days] + options[days-1]*k90$P[days])

k90$MV <- mv
k90$profit <- profit
k90$cash <- cash_pos
k90$shares <- shares

gg_market_90 <- ggplot(k90, aes(x=TradeDate)) + 
  geom_line(aes(y=MV)) + 
  labs(title="Market Value Evolution (K = 90)", 
       subtitle="Time Series of Market Value over Duration of Trade", 
       y="Market Value")

annual_sd_90 <- (sd(k90$profit)*sqrt(252))
sharpe90 <- (mean(k90$profit)*252) / (sd(k90$profit)*sqrt(252))

#25th of February is missing data for IV, Delta, Gamma
#For the graph, remove days where there is no IV:
no_IV_days <- which(k90$IV == 0)

gg_delta_90 <- ggplot(k90[-(no_IV_days),], aes(x=TradeDate)) + 
  geom_line(aes(y=Delta)) + 
  labs(title="Delta Evolution (K = 90)", 
       subtitle="Time Series of Delta over Duration of Trade", 
       y="Delta")

daily_rf_spot$Return <- log((daily_rf_spot$Spot/lag(daily_rf_spot$Spot)))
k90$roll_sd <- roll_sd(daily_rf_spot$Return[1:262], width = 21)
k90$roll_sd <- k90$roll_sd * sqrt(252)

gg_IV_90 <- ggplot(k90[-(no_IV_days),], aes(x=TradeDate)) + 
  geom_line(aes(y = IV), color = "darkred") + 
  geom_line(aes(y = roll_sd), color="steelblue", linetype="twodash") 

### K = 95 ###
cash_pos <- rep(0, days)
shares <- rep(0, days)
options <- rep(0, days)
mv <- rep(0, days)
profit <- rep(0, days)

options[1] <- -45*100
shares[1] <- round(-options[1]*k95$Delta[1],0)
cash_pos[1] <- -options[1]*k95$Ask[1] - shares[1]*daily_rf_spot$Spot[1]
mv[1] <- options[1]*(k95$P[1] - k95$Ask[1])
profit[1] <- mv[1]
delta_t <- 1/252

for(i in 2:days){
  options[i] <- options[i-1]
  shares[i] <- round(-options[i]*k95$Delta[i],0)
  
  cash_pos[i] <- (1+daily_rf_spot$RF[i-1]*delta_t)*cash_pos[i-1] - 
    (shares[i] - shares[i-1])*daily_rf_spot$Spot[i]
  
  mv[i] <- options[i]*k95$P[i] + shares[i]*daily_rf_spot$Spot[i] + cash_pos[i] + shares[i]*div_data$Dividend[i-1]
  
  profit[i] <- mv[i] - mv[i-1]
}

final_cash_95 = (1+daily_rf_spot$RF[days-1]*delta_t)*cash_pos[days-1] +
  (shares[days]*daily_rf_spot$Spot[days] + options[days-1]*k95$P[days])

k95$MV <- mv
k95$profit <- profit
k95$cash <- cash_pos
k95$shares <- shares

gg_market_95 <- ggplot(k95, aes(x=TradeDate)) + 
  geom_line(aes(y=MV)) + 
  labs(title="Market Value Evolution (K = 95)", 
       subtitle="Time Series of Market Value over Duration of Trade", 
       y="Market Value")

annual_sd_95 <- (sd(k95$profit)*sqrt(252))
sharpe95 <- (mean(k95$profit)*252) / (sd(k95$profit)*sqrt(252))

no_IV_days_95 <- which(k95$IV == 0)

gg_delta_95 <- ggplot(k95[-(no_IV_days_95),], aes(x=TradeDate)) + 
  geom_line(aes(y=Delta)) + 
  labs(title="Delta Evolution (K = 95)", 
       subtitle="Time Series of Delta over Duration of Trade", 
       y="Delta")

k95$roll_sd <- k90$roll_sd

gg_IV_95 <- ggplot(k95[-(no_IV_days_95),], aes(x=TradeDate)) + 
  geom_line(aes(y = IV), color = "darkred") + 
  geom_line(aes(y = roll_sd), color="steelblue", linetype="twodash") 

##############

### K = 100 ###
cash_pos <- rep(0, days)
shares <- rep(0, days)
options <- rep(0, days)
mv <- rep(0, days)
profit <- rep(0, days)

options[1] <- -45*100
shares[1] <- round(-options[1]*k100$Delta[1],0)
cash_pos[1] <- -options[1]*k100$Ask[1] - shares[1]*daily_rf_spot$Spot[1]
mv[1] <- options[1]*(k100$P[1] - k100$Ask[1])
profit[1] <- mv[1]
delta_t <- 1/252

#big Q = options
#little q = stocks
for(i in 2:days){
  options[i] <- options[i-1]
  shares[i] <- round(-options[i]*k100$Delta[i],0)
  
  cash_pos[i] <- (1+daily_rf_spot$RF[i-1]*delta_t)*cash_pos[i-1] - 
    (shares[i] - shares[i-1])*daily_rf_spot$Spot[i]
  
  mv[i] <- options[i]*k100$P[i] + shares[i]*daily_rf_spot$Spot[i] + cash_pos[i] + shares[i]*div_data$Dividend[i-1]
  
  profit[i] <- mv[i] - mv[i-1]
}

final_cash_100 = (1+daily_rf_spot$RF[days-1]*delta_t)*cash_pos[days-1] +
  (shares[days]*daily_rf_spot$Spot[days] + options[days-1]*k100$P[days])

k100$MV <- mv
k100$profit <- profit
k100$cash <- cash_pos
k100$shares <- shares

gg_market_100 <- ggplot(k100, aes(x=TradeDate)) + 
  geom_line(aes(y=MV)) + 
  labs(title="Market Value Evolution (K = 100)", 
       subtitle="Time Series of Market Value over Duration of Trade", 
       y="Market Value")

annual_sd_100 <- (sd(k100$profit)*sqrt(252))
sharpe100 <- (mean(k100$profit)*252) / (sd(k100$profit)*sqrt(252))

gg_delta_100 <- ggplot(k100, aes(x=TradeDate)) + 
  geom_line(aes(y=Delta)) + 
  labs(title="Delta Evolution (K = 100)", 
       subtitle="Time Series of Delta over Duration of Trade", 
       y="Delta")

no_IV_days_100 <- which(k100$IV == 0)

k100$roll_sd <- k90$roll_sd

ggplot() + 
  geom_line(data=k90[-(no_IV_days),], aes(x=TradeDate, y=IV, col = "Strike 90 IV")) + 
  geom_line(data=k95[-(no_IV_days_95), ], aes(x=TradeDate, y=IV, col = "Strike 95 IV")) +
  geom_line(data=k100[-(no_IV_days_100),], aes(x=TradeDate, y=IV, col = "Strike 100 IV")) +
  geom_line(data=k90, aes(x=TradeDate, y=roll_sd, col = "Realised Volatility")) +
  theme_minimal() + theme(legend.title = element_blank()) +
  scale_color_manual(values = c("darkred", "steelblue", "thistle4", "lightpink2"))

ggplot() + 
  geom_line(data=k90[-c(no_IV_days),], aes(x=TradeDate, y=Delta, col = "Strike 90 Delta")) + 
  geom_line(data=k95[-(no_IV_days_95), ], aes(x=TradeDate, y=Delta, col = "Strike 95 Delta")) +
  geom_line(data=k100[-(no_IV_days_100),], aes(x=TradeDate, y=Delta, col = "Strike 100 Delta")) +
  theme_minimal() + theme(legend.title = element_blank()) +
  scale_color_manual(values = c("darkred", "steelblue", "thistle4", "lightpink2"))

ggplot() + 
  geom_line(data=k90, aes(x=TradeDate, y=MV, col = "Strike 90 MV")) + 
  geom_line(data=k95, aes(x=TradeDate, y=MV, col = "Strike 95 MV")) +
  geom_line(data=k100, aes(x=TradeDate, y=MV, col = "Strike 100 MV")) +
  theme_minimal() + theme(legend.title = element_blank()) +
  scale_color_manual(values = c("darkred", "steelblue", "thistle4", "lightpink2"))

###############

### PART B ###
#Can use the formula in lecture slides:
vol <- sd(daily_rf_spot$Spot)
implied <- k90$IV[1]
expected <- 0.5*daily_rf_spot$Spot[1]^2 * k90$Gamma[1] * (vol - implied)

### PART (i) ###
plot_data_i <- as.data.frame(cbind(daily_rf_spot$Return[1:262], k90$profit, k95$profit, k100$profit))

gg_part_i <- ggplot(plot_data_i[-1,], aes(x=V1, y=V2)) + geom_point(col = "darkred") + 
  labs(x = "DIA Return", y = "P & L", title = "DIA Returns vs P&L K = 90") + theme_minimal()
gg_part_ii <- ggplot(plot_data_i[-1,], aes(x=V1, y=V3)) + geom_point(col = "steelblue") + 
  labs(x = "DIA Return", y = "P & L", title = "DIA Returns vs P&L K = 95")+ theme_minimal()
gg_part_iii <- ggplot(plot_data_i[-1,], aes(x=V1, y=V4)) + geom_point(col = "thistle4") + 
  labs(x = "DIA Return", y = "P & L", title = "DIA Returns vs P&L K = 100")+ theme_minimal()



##Q4
#Sell 45 Butterflies - Sell 45 of 90, 100, buy 90 of 95
#Close on November 13th 2009
#Buy at Bid, Sell at Ask
#Find Index of Nov 15
end_date <- as.Date("2009-11-13")
new_days <- which(k90$TradeDate == end_date)

cash_pos <- rep(0, new_days)
shares <- rep(0, new_days)
options_90 <- rep(0, new_days)
options_95 <- rep(0, new_days)
options_100 <- rep(0, new_days)
mv <- rep(0, new_days)
profit <- rep(0, new_days)

#Sell option, buy share
#Buy option, sell shares 
options_90[1] <- -45*100
options_95[1] <- 90*100
options_100[1] <- -45*100
shares[1] <- round(-options_90[1]*k90$Delta[1],0) + round(-options_95[1]*k95$Delta[1],0) + 
             round(-options_100[1]*k100$Delta[1],0)

cash_pos[1] <- -options_90[1]*k90$Ask[1] - options_100[1]*k100$Ask[1] - 
                options_95[1]*k95$Bid[1] - shares[1]*daily_rf_spot$Spot[1]

#mv[1] <- options_100[1]*(k100$P[1] - k100$Bid[1]) + options_90[1]*(k90$P[1] - k90$Bid[1]) + 
#        options_95[1]*(k95$P[1] - k95$Ask[1])
mv[1] = options_100[1]*(k100$P[1]) + options_90[1]*(k90$P[1]) + 
        options_95[1]*(k95$P[1]) + cash_pos[1] + shares[1]*daily_rf_spot$Spot[1]

#add cash and shares
#sum of profit 2k 

profit[1] <- mv[1]
delta_t <- 1/252

#big Q = options
#little q = stocks

for(i in 2:(new_days)){
  options_90[i] <- options_90[i-1]
  options_95[i] <- options_95[i-1]
  options_100[i] <- options_100[i-1]
  
  shares[i] <- round(-options_90[i]*k90$Delta[i],0) + round(-options_95[i]*k95$Delta[i],0) + 
    round(-options_100[i]*k100$Delta[i],0)
  
  cash_pos[i] <- (1+daily_rf_spot$RF[i-1]*delta_t)*cash_pos[i-1] - 
    (shares[i] - shares[i-1])*daily_rf_spot$Spot[i] 
  
  mv[i] <- options_100[i]*(k100$P[i]) + options_90[i]*(k90$P[i]) + 
    options_95[i]*(k95$P[i]) + shares[i]*daily_rf_spot$Spot[i] + cash_pos[i] + shares[i]*div_data$Dividend[i-1]
  
  profit[i] <- mv[i] - mv[i-1]
}

final_cash_butterfly = (1+daily_rf_spot$RF[new_days-1]*delta_t)*cash_pos[new_days-1] +
  shares[new_days]*daily_rf_spot$Spot[new_days] + options_90[new_days-1]*k90$Bid[new_days] +
  options_95[new_days-1]*k95$Ask[new_days] + options_100[new_days-1]*k100$Bid[new_days]

cash_pos[new_days] + shares[new_days]*daily_rf_spot$Spot[new_days] + 
options_90[new_days-1]*k90$Bid[new_days] + options_95[new_days-1]*k95$Ask[new_days] + 
options_100[new_days-1]*k100$Bid[new_days]

butterfly_data <- k90[1:220,1]
butterfly_data <- (cbind.data.frame(butterfly_data, cash_pos, profit, mv, shares))

ggplot() + 
  geom_line(data=butterfly_data, aes(x=butterfly_data, y=mv, col = "Butterfly MV")) + 
  theme_minimal() + theme(legend.title = element_blank()) +
  scale_color_manual(values = c("darkred", "steelblue", "thistle4", "lightpink2")) +
  labs(title="Market Value Evolution (Butterfly)", 
       subtitle="Time Series of Market Value over Duration of Butterfly Trade", 
       y="Market Value", x = "Date")

butterfly <- as.data.frame(cbind(profit, mv, cash_pos, shares))
annual_sd_but <- (sd(butterfly$profit)*sqrt(252))
sharpe_but <- (mean(butterfly$profit)*252) / (sd(butterfly$profit)*sqrt(252))

butterfly_delta <- 2*k95$Delta - k90$Delta - k100$Delta
butterfly_data$delta <- butterfly_delta[1:220]

but_delta_gg <- ggplot() + 
  geom_line(data=butterfly_data, aes(x=butterfly_data, y=delta, col = "Delta")) + 
  theme_minimal() + theme(legend.title = element_blank()) +
  scale_color_manual(values = c("darkred", "steelblue", "thistle4", "lightpink2")) +
  labs(title="Aggregate Delta Evolution (Butterfly)", 
       subtitle="Time Series of Aggregate Delta over Duration of Butterfly Trade", 
       y="Delta", x = "Date")

butterfly_gamma <- 2*k95$Gamma - k90$Gamma - k100$Gamma
butterfly_vega <- 2*k95$Vega - k90$Vega - k100$Vega
butterfly_data$vega <- butterfly_vega[1:220]

but_vega_gg <- ggplot() + 
  geom_line(data=butterfly_data, aes(x=butterfly_data, y=vega, col = "Vega")) + 
  theme_minimal() + theme(legend.title = element_blank()) +
  scale_color_manual(values = c("steelblue", "darkred", "thistle4", "lightpink2")) +
  labs(title="Aggregate Vega Evolution (Butterfly)", 
       subtitle="Time Series of Aggregate Vega over Duration of Butterfly Trade", 
       y="Vega", x = "Date")

plot_data_i_butterfly <- as.data.frame(cbind(daily_rf_spot$Return[1:220], profit))

gg_part_i <- ggplot(plot_data_i_butterfly[-1,], aes(x=V1, y=profit)) + geom_point(col = "thistle4") + 
  labs(x = "DIA Return", y = "Butterfly P & L", title = "DIA Returns vs Butterfly P&L") + theme_minimal()
