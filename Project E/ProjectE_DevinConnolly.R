gold_equity <- read.csv("GLD_US_EQ.csv", stringsAsFactors = F)
gold_equity <- gold_equity[,1:5]
gold_commodity <- read.csv("GLD_CMDTY.csv", stringsAsFactors = F)
gold_commodity <- gold_commodity[-1,]
cl1 <- read.csv("CL1.csv", stringsAsFactors = F)
clg0 <-read.csv("CLG0.csv", stringsAsFactors = F)
SP1_index <- read.csv("SP1_index.csv", stringsAsFactors = F)
SPY_US <- read.csv("SPY_US.csv", stringsAsFactors = F)
SPX_index <- read.csv("SPX_INDEX.csv", stringsAsFactors = F)

full_data_gold <- cbind(gold_equity[,1:2], gold_commodity[,2])
colnames(full_data_gold) <- c("Date", "GOLD_EQUITY", "GOLD_COMDTY")

full_data_sp <- cbind(SP1_index[,1:2], SPY_US[,2], SPX_index[,2])
colnames(full_data_sp) <- c("Date", "SP1", "SPY_US", "SPX")
full_data_sp$SP1 <- as.double(full_data_sp$SP1)
full_data_sp$SPY_US <- as.numeric(full_data_sp$SPY_US)
full_data_sp$SPX <- as.double(full_data_sp$SPX)

n <- max(nrow(cl1), nrow(clg0))
clg0_prices <- clg0$PX_LAST
small_n <- length(clg0_prices)
clg0_prices <- c(rep(NA, (n-small_n)), clg0_prices)
full_data_oil <- cbind(cl1[,1:2], clg0_prices)
colnames(full_data_oil) <- c("Date", "CL1", "CLG0")
full_data_oil$CL1 <- as.numeric(full_data_oil$CL1)
full_data_oil$CLG0 <- as.numeric(full_data_oil$CLG0)
full_data_oil$CL1_R <- c(0, ((full_data_oil[2:n, 2] - full_data_oil[1:(n-1), 2])/full_data_oil[1:(n-1), 2]))
full_data_oil$CLG0_R <- c(0, ((full_data_oil[2:n, 3] - full_data_oil[1:(n-1), 3])/full_data_oil[1:(n-1), 3]))



