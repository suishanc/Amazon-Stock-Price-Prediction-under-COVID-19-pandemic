install.packages("pbapply")
install.packages("quantmod")
install.packages("lubridate")
require("quantmod"); require("lubridate"); require("pbapply")
# library(quantmod)
# library(lubridate)
# library(pbapply)

ticker <- "AMZN"
stock <- getSymbols(ticker,
                    auto.assign = FALSE, 
                    from = '2017-01-30', 
                    to = Sys.Date())

tail(stock)
tmp <- getQuote(ticker)
stock <- rbind(stock, xts(cbind(tmp$Open,tmp$High,tmp$Low,tmp$Last,tmp$Volume,tmp$Last), order.by = Sys.Date()))
tail(stock) # today data
tmp <- Ad(stock) # store the adjusted price
rets <- ROC(tmp,type="discrete") # calculate the returns
rets[is.na(rets)]<-0
mean(rets)
sd(rets)

# build a function where we pass in the stock price and the number periods, we want to predict the mean and standard deviation and it is going to return the predicted price
# pass in stock price
# N = the period we're trying to predict
stk_ret = function(STK_PRC, N, MEAN, STDEV)
{
  delta_t = 1/N # for 1 period
  for(i in seq(N))
  {
    epsilon <- runif(n=1,min=0,max=1) # random probability from 0 to 1
    STK_PRC <- STK_PRC * (1 + qnorm(epsilon, MEAN*delta_t, STDEV*sqrt(delta_t))) # use qnorm along epsilon prob, mean, sd
  }
  
  STK_PRC
  
}

last(tmp) # the close price on this day
simulations <- 1000 # simulation times
N = 18 # look back 18 days to see the predict stock price for today
STK_PRC <- as.numeric(coredata(tmp[Sys.Date() - days(18)]))
MEAN = mean(rets)
STDEV = sd(rets)

stock_prices <- c() # empty vector to store the stock prices
for(i in seq(simulations))
{
  stock_prices <- c(stock_prices,stk_ret(STK_PRC = STK_PRC, N=N,MEAN=MEAN,STDEV = STDEV))
}

stock_prices # predictions for today's closing price
quantile(stock_prices) # summary
# the lowest possible stock price of today would have been
# the highest possible stock price of today
# > 100% -> this happens beacuse we using the mean and sd to predict future prices and we know mean and sd are static and they constanly change when there's periods of volatitlity these numbers may be off but when the prices don't fluctuate and stay static so does the median standard deviation and prices remian within this range

# predicted prices every month
EXPIRY <- tmp[options.expiry(tmp)] # monthly options expiration dates along with the close of that day
EXPIRY <- EXPIRY["2007::"]
IDX <- index(EXPIRY)
NEXT.EXPIRY <- as.Date("2021-5-30")
IDX <- c(IDX,NEXT.EXPIRY)

MEAN = function(calculateUNTIL)
{
  tmp <- tmp[paste0("::",calculateUNTIL)]
  tmp <- ROC(tmp,type="discrete")
  tmp[is.na(tmp)]<-0
  mean(tmp)
}

STDEV = function(calculateUNTIL)
{
  tmp <- tmp[paste0("::",calculateUNTIL)]
  tmp <- ROC(tmp,type="discrete") # calculate the return
  tmp[is.na(tmp)]<-0
  sd(tmp)
}

# calculate the mean and sd for each of the options expiration dates
means <- do.call(rbind,lapply(as.list(IDX), MEAN))
stdevs <- do.call(rbind,lapply(as.list(IDX), STDEV))
days = as.numeric(diff(IDX)) # calculate the difference of days in between the options expiration dates

# pass the number of simulations, or iteration, if's the last iteration
MONTE.CARLO = function(sim,iter,LastIter)
{
  simulations <- sim
  N <- days[iter]
  STK_PRC <- as.numeric(EXPIRY[iter])
  MEAN <- means[iter]
  STDEV <- stdevs[iter]
  stock_prices <- c()
  
  # do simulation
  for(i in seq(simulations))
  {
    stock_prices <- c(stock_prices, stk_ret(STK_PRC = STK_PRC,N=N,MEAN=MEAN,STDEV=STDEV))
  }
  
  # data frame
  # store opening price and closing price
  START <- as.data.frame(round(STK_PRC,2))
  START.DATE = index(EXPIRY[iter])
  PROBS = as.data.frame(t(round(quantile(stock_prices,probs = seq(0,1,0.05)),2)))
  
  if(iter == LastIter)
  {
    END <- as.data.frame(NA)
    END.DATE = as.data.frame(NA)
  }else{
    END <- as.data.frame(as.numeric(round(EXPIRY[iter+1],2)))
    END.DATE = index(EXPIRY[iter+1])
  }
  all <- cbind(START,START.DATE,PROBS,END,END.DATE)
  colnames(all) <- c("START.PRC","START.DATE","0%","5%","10%","15%","20%","25%","30%","35%","40%","45%","50%","55%",
                     "60%","65%","70%","75%","80%","85%","90%","95%","100%","END.PRC","END.DATE")
  all
}

p <- pblapply(as.list(1:length(days)), function(x){
  MONTE.CARLO(sim=10000,iter = x, LastIter = length(days))
})

p <- do.call(rbind,p)

# plot the ending prices along with the prob
plot(p$END.PRC, type="l", main="The ending prices along with the probability of zero and one hundred percent", 
     ylab="close")
lines(p$`0%`, col='red')
lines(p$`100%`,col='green')
legend("topleft", inset=.02, legend=c("zero percent","one hundred percent"),
       col=c("red","green"), lty=1:1, cex=0.6, box.lty=0)

# number of months
nMo <- nrow(p)

# numbers of times it closes above 100%
sum(as.numeric(na.omit(ifelse(p$END.PRC > p$`100%`,1,0))))/nMo

# numbers of times it closes below 0%
sum(as.numeric(na.omit(ifelse(p$END.PRC < p$`0%`,1,0))))/nMo

write.csv(p,"Amazon.csv")

# Test for model accuracy
y_true <- p$END.PRC[1:49]
y_pred <- p$`100%`[1:49]

# MAPE
MAPE <- mean(abs((y_pred - y_true) / y_true)) * 100
MAPE

# SMAPE
SMAPE <- mean(abs(y_pred - y_true) / (abs(y_pred) + abs(y_true))/2) * 100
SMAPE

row1 <- c("Mean Absolute Percentage Error (MAPE)","Symmetric Mean Absolute Percentage Error (SMAPE)")
row2 <- c(MAPE,SMAPE)
x <- cbind(row1,row2)
colnames(x) <- c("Forecast Accuracy Method", "Accuracy")
x


