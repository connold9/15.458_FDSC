stocks <- read.table("data.txt", sep = ",", header = T)

pid <- 929315784
vid <- 1

mss <- function(d) {
  mean <- 252*mean(d) * 100
  print(paste("The annualised mean was:", round(mean,2), "%"))
  sd <- sqrt(252)*sd(d) * 100
  print(paste("The annualised mean was:", round(sd,2), "%"))
  sr <- mean/sd
  print(paste("The annualised Sharpe ratio was:", round(sr,2)))
  return(c(mean, sd, sr))
}

stocks$d <- as.Date(stocks$d, format = "%m/%d/%Y")

DAYS  <- sort(unique(stocks$d))
N_DAYS  <- sort(unique(stocks$t))
UNIVERSE  <- sort(unique(stocks$id))
N_PERIODS <- length(N_DAYS)
N_STOCKS <- length(UNIVERSE)

g_returns <- spread(stocks,id,r)
g_returns <- g_returns[,-2]
g_returns$d <- as.Date(g_returns$d, format = "%m/%d/%Y")
#daily_means = (aggregate((exp(stocks[,4])-1), list(stocks$d), mean))

#Removing first two colums (date, days passed), store as matrix
#g_returns <- as.matrix(g_returns[,-1])

Returns <- exp(g_returns[,-1]) - 1 #covert geometric to simple
simple_means <- apply(Returns, 1 , mean)

appended = as.data.frame(cbind(Returns, simple_means))
signal = -(appended[,1:690] - appended$simple_means)

#row.names(Returns) <- DAYS
#excess_returns = Returns[,-1] - daily_means[which(daily_means$Group.1 == row.names(Returns)), 2]
#trading signal

#writing a funcion that calculates the respective weights for each stock
ordered_equal <- function(vec) {
  indices = order(vec)
  order_vec = (vec[order(vec, decreasing = T)])
  
  top10 = ceiling(length(vec)/10)
  bottom10 = length(vec) + 1 - top10
  top = order_vec[1:top10]
  top = (top/top) / length(top)
  
  bottom = order_vec[bottom10:length(vec)]
  bottom = -(bottom/bottom) / length(bottom)
  
  weights = c(top, rep(0, bottom10-top10-1), bottom)
  
  return(weights[indices])
}

test_equal <- function(test_vec){
  indices = order(test_vec, decreasing = T)
  order_vec = (test_vec[order(test_vec, decreasing = T)])
  top10 = ceiling(length(test_vec)/10)
  w <- c(rep.int(1/top10, top10), rep.int(0, length(test_vec)-2*top10), rep.int(-1/top10, top10))
  order_vec <- rbind(order_vec, w)
  order_vec <- order_vec[-1,]
  test_vec[indices] <- order_vec
  return(test_vec)
}

weight_matrix = apply(signal, 1, test_equal)
weight_matrix = t(weight_matrix)

#exposure <- weight_matrix * 0
#exposure[2:N_PERIODS, ] <- weight_matrix[1:(N_PERIODS-1),]

Returns = Returns[-1,]
weight_matrix =  weight_matrix[1:nrow(weight_matrix)-1,]
#portfolio_Return = Returns*weight_matrix
Return.portfolio <- apply(weight_matrix * Returns, 1, sum)
Returns_bystock <- apply(weight_matrix * Returns, 2, sum)

#portfolio_Return <- apply(exposure * Returns, 1, sum)
#daily_returns_fin <- rowSums(Return.portfolio)
daily_returns_log <- log(Return.portfolio+1)

plot(cumprod(1+daily_returns_log), type = "l", col = "red")
plot(daily_returns_log, type = "l", col = "red")

plot_data = as.data.frame(cbind(DAYS[-1], daily_returns_log))
plot_data$V1 <- DAYS[-1]

ggplot(plot_data, aes(x=V1)) + 
  geom_line(aes(y=daily_returns_log)) + 
  labs(title="Portfolio Returns", 
       subtitle="Returns for the Lag 1 Long/Short Contrarian Strategy", 
       y="Returns %",
       x="Date")

plot_data2 = as.data.frame(cbind(DAYS, simple_means))
plot_data2$V1 <- DAYS

ggplot(plot_data2, aes(x=V1)) + 
  geom_line(aes(y=simple_means)) + 
  labs(title="Portfolio Returns", 
       subtitle="Returns for the Market Portfolio", 
       y="Returns %",
       x="Date")

#Checking Summary Statistics
#Part 1 - Overall
mss(daily_returns_log)
mss(log(1+simple_means))

#Part 2 - First Half and Second Half - Big Difference
mss(daily_returns_log[1:(length(daily_returns_log)/2)])
mss(daily_returns_log[(length(daily_returns_log)/2):length(daily_returns_log)])

#Checking for stationarity
Box.test(daily_returns_log, lag=5, type="Ljung-Box")
Box.test(simple_means, lag=5, type="Ljung-Box")

#Very small p-value -> non-stationary
acf(daily_returns_log,lag.max = length(daily_returns_log))
acf(simple_means,lag.max = 100)

#Looking for outliers
max_return = (max(daily_returns_log))
paste("Max Return = ", round(max_return*100,2), "%")

min_return = (min(daily_returns_log))
paste("Max Return = ", round(min_return*100,2), "%")

#To check if they have a sizeable effect, we could remove these days and recalculate Sharpe
#and other summary stats.

low_bar = mean(daily_returns_log) - 2*(sd(daily_returns_log))
high_bar = mean(daily_returns_log) + 2*(sd(daily_returns_log))

ggplot(plot_data, aes(x=V1)) + 
  geom_line(aes(y=daily_returns_log)) + 
  geom_hline(aes(yintercept=low_bar), col = "red", lty = 2) + 
  geom_hline(aes(yintercept=high_bar), col = "red", lty = 2) + 
  labs(title="Portfolio Returns", 
       subtitle="Returns for the Lag 1 Long/Short Contrarian Strategy", 
       y="Returns %",
       x="Date")

outliers_removed = daily_returns_log[which((daily_returns_log < high_bar) & (daily_returns_log > low_bar))]

mss(outliers_removed)

#Check correlation between portfolio and market
mrkt_corr = cor(simple_means[-1], daily_returns_log)

#Long and Short Sub Portfolios
long_weights  = weight_matrix
long_weights[long_weights < 0] <- 0

short_weights  = weight_matrix
short_weights[short_weights > 0] <- 0

Return_long <- apply(long_weights * Returns, 1, sum)
Return_long_log <- log(1 + apply(long_weights * Returns, 1, sum))
Return_short <- apply(short_weights * Returns, 1 , sum)
Return_short_log <- log(1 + apply(short_weights * Returns, 1, sum))

mss(Return_long)
mss(Return_short)

cor(Return_long, Return_short)
cor(Return_long_log, Return_short_log)

ls_df <- as.data.frame(cbind(Return_long, Return_short))
#Plotting Long vs Short 
#plot(x = Return_short, y = Return_long)

gg <- ggplot(ls_df, aes(x=Return_long, y=Return_short)) + 
  geom_point() + 
  geom_smooth(method="loess", se=F) + 
  #xlim(c(0, 0.1)) + 
  #ylim(c(0, 500000)) + 
  labs(subtitle="Long vs Short Portfolios", 
       y="Short Portfolio", 
       x="Long Portfolio", 
       title="Scatterplot")
plot(gg)

#Generalise to Lag k -> Sigal today is excess returns in the past
#Shift excess signal - lag 2
Returns_lag2 = Returns[-1,]
weight_matrix_lag2 =  weight_matrix[1:nrow(weight_matrix)-1,]
Return_portfolio_lag2 <- apply(weight_matrix_lag2 * Returns_lag2, 1, sum)

daily_returns_log_lag2 <- log(Return_portfolio_lag2+1)
mss(daily_returns_log_lag2)
mss(daily_returns_log)

#Lag 3
Returns_lag3 = Returns_lag2[-1,]
weight_matrix_lag3 =  weight_matrix_lag2[1:nrow(weight_matrix_lag2)-1,]
Return_portfolio_lag3 <- apply(weight_matrix_lag3 * Returns_lag3, 1, sum)

daily_returns_log_lag3 <- log(Return_portfolio_lag3+1)
mss(daily_returns_log_lag3)
mss(daily_returns_log)

#Lag 4
Returns_lag4 = Returns_lag3[-1,]
weight_matrix_lag4 =  weight_matrix_lag3[1:nrow(weight_matrix_lag3)-1,]
Return_portfolio_lag4 <- apply(weight_matrix_lag4 * Returns_lag4, 1, sum)

daily_returns_log_lag4 <- log(Return_portfolio_lag4+1)
mss(daily_returns_log_lag4)
mss(daily_returns_log)

#Lag 5
Returns_lag5 = Returns_lag4[-1,]
weight_matrix_lag5 =  weight_matrix_lag4[1:nrow(weight_matrix_lag4)-1,]
Return_portfolio_lag5 <- apply(weight_matrix_lag5 * Returns_lag5, 1, sum)

daily_returns_log_lag5 <- log(Return_portfolio_lag5+1)
mss(daily_returns_log_lag5)
mss(daily_returns_log)

#Now need to create the full dataframe for the flat file
dates = c(DAYS[-1], tail(DAYS,-2), tail(DAYS,-3), tail(DAYS,-4), tail(DAYS,-5))
lags = c(rep(1, length(DAYS[-1])),rep(2, length(tail(DAYS,-2))), rep(3, length(tail(DAYS,-3))), 
         rep(4, length(tail(DAYS,-4))), rep(5, length(tail(DAYS,-5))))

test_merge = rbind(weight_matrix, weight_matrix_lag2, weight_matrix_lag3, weight_matrix_lag4, weight_matrix_lag5)

total_portfolio = as.data.frame(cbind(dates, lags, test_merge))
total_portfolio$dates <- dates

flat_portfolio <- melt(total_portfolio, id.vars = c("lags","dates"))

pf_id = c(rep(pid, nrow(flat_portfolio)))
v_id = c(rep(vid, nrow(flat_portfolio)))

flat_portfolio <- cbind(pf_id, flat_portfolio, v_id)
flat_portfolio <- flat_portfolio[, c(1, 3, 4, 2, 5, 6)]

write.table(flat_portfolio, "portfolio.txt", append = FALSE, sep = ",", dec = ".",
            row.names = F, col.names = TRUE)

write.table(flat_portfolio,file="output_file.txt",sep="\t",eol="\r\n",quote=FALSE,row.names=FALSE,col.names=TRUE,append=FALSE)

#Reforming the same strategy using top third and bottom third portfolios instead of deciles.
extra_weights <- function(test_vec){
  indices = order(test_vec, decreasing = T)
  order_vec = (test_vec[order(test_vec, decreasing = T)])
  top10 = ceiling(length(test_vec)/3)
  w <- c(rep.int(1/top10, top10), rep.int(0, length(test_vec)-2*top10), rep.int(-1/top10, top10))
  order_vec <- rbind(order_vec, w)
  order_vec <- order_vec[-1,]
  test_vec[indices] <- order_vec
  return(test_vec)
}

ex_weights = apply(signal, 1, extra_weights)
ex_weights = t(ex_weights)

#exposure <- weight_matrix * 0
#exposure[2:N_PERIODS, ] <- weight_matrix[1:(N_PERIODS-1),]

ex_weights =  ex_weights[1:nrow(ex_weights)-1,]
#portfolio_Return = Returns*weight_matrix
Return.extra <- apply(ex_weights * Returns, 1, sum)

#portfolio_Return <- apply(exposure * Returns, 1, sum)
daily_returns_extra_log <- log(Return.extra+1)

mss(daily_returns_extra_log)

unequal_weights = apply(signal, 1, ordered_equal)
unequal_weights = t(unequal_weights)
unequal_weights =  unequal_weights[1:nrow(unequal_weights)-1,]

Return.unequal <- apply(unequal_weights * Returns, 1, sum)
mss(Return.unequal)
