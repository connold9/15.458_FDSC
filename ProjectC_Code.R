#Question 1 - Beta

data <- read.csv("jpm_spx.csv", header = F)
vix <- read.csv("vix.csv", header = F)
colnames(data) <- c("DATE", "JPM", "SPX")
colnames(vix) <- c("DATE", "VIX")
data$VIX <- vix$VIX

#Strong correlation with market - not a good factor. 
pairs(data[,-1])
cor(data$SPX, data$VIX)

data$DATE <- as.Date(data$DATE, format = "%Y-%m-%d")

jpm_spx <- lm(JPM ~ SPX, data)
plot(x = data$SPX, y = data$JPM, xlab = "SPX Returns", ylab = "JPM Returns", main = "JPM vs SPX Returns w/ Regression Line")
abline(jpm_spx)
text(x = 0.07, y = -.2, labels = eqn)

cc <- jpm_spx$coefficients
eqn <- paste("JPM =", paste(round(cc[1],2), paste(round(cc[-1],2), names(cc[-1]), sep=" * ", collapse=" + "), sep=" + "), "+ e")

install.packages("rollRegres")
library(rollRegres)

fits <- roll_regres(JPM ~ SPX, data, width = 63)
plot(fits$coefs[,2], type = "l", x = data$DATE, xlab = "Date", ylab = "Beta", main = "Rolling 3-month JPM Beta")


#VIX Betas
jpm_vix <- lm(JPM ~ VIX, data)
plot(x = data$VIX, y = data$JPM, xlab = "VIX Returns", ylab = "JPM Returns", main = "JPM vs VIX Returns w/ Regression Line")
abline(jpm_vix)
text(x = -0.15, y = -.2, labels = eqn2)

cc2 <- jpm_vix$coefficients
eqn2 <- paste("JPM =", paste(round(cc2[1],2), paste(round(cc2[-1],2), names(cc2[-1]), sep=" * ", collapse=" + "), sep=" + "), "+ e")

#Question 2 - Performance Modelling
ff_data <- read.csv("ff_data.csv", header = F)
colnames(ff_data) <- c("DATE", "MKTRF", "SMB", "HML", "RF", "UMD")

strat <- read.csv("strat_returns.csv", header = T)
strat$Date <- ff_data$DATE

plot(x = dj_vol$DATE, y = dj_vol$AVG1, type = "l", xlab = "", ylab = "Correlation", main = "1 Month Volatility Plot", ylim = c(0,1))
par(new = T)
lines(x = dj_vol$DATE, y = dj_vol$VAR0_21, col = "red")
lines(x = dj_vol$DATE, y = dj_vol$VAR1_21, col = "green")
lines(x = dj_vol$DATE, y = dj_vol$MONTH_VOL, col = "blue")


plot(x = dj_vol$DATE, y = dj_vol$AVG3, type = "l", xlab = "", ylab = "Correlation", main = "3 Month Volatility Plot", ylim = c(0,1))
par(new = T)
lines(x = dj_vol$DATE, y = dj_vol$VAR0_63, col = "red")
lines(x = dj_vol$DATE, y = dj_vol$VAR1_63, col = "green")
lines(x = dj_vol$DATE, y = dj_vol$THREE_VOL, col = "blue")

