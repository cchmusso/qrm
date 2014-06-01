
# load functions
source("~/Dropbox/QRM/Project/Functions.R")

#import all data
Equity_CH <- qrm.load.data("~/Dropbox/QRM/Project/QRM_Project_Dataset/Equity_CH.csv", ";")
Equity_CH_Infos <- qrm.load.data("~/Dropbox/QRM/Project/QRM_Project_Dataset/Equity_CH_Infos.csv", "\t")
Equity_US <- qrm.load.data("~/Dropbox/QRM/Project/QRM_Project_Dataset/Equity_US.csv", ";")
Equity_US_Infos <- qrm.load.data("~/Dropbox/QRM/Project/QRM_Project_Dataset/Equity_US_Infos.csv", "\t")

ETF <- qrm.load.data("~/Dropbox/QRM/Project/QRM_Project_Dataset/ETF.csv", "\t")
ETF_Infos <- qrm.load.data("~/Dropbox/QRM/Project/QRM_Project_Dataset/ETF_Infos.csv", "\t")
FX <- qrm.load.data("~/Dropbox/QRM/Project/QRM_Project_Dataset/FX.csv", "\t")

stockUS <- qrm.load.data("~/Dropbox/QRM/Project/QRM_Project_Dataset/Stock_US.csv~", ";")
stockCH <- qrm.load.data("~/Dropbox/QRM/Project/QRM_Project_Dataset/Stocks_CH.csv", ";")

#download packages
require(moments)
require(zoo)
require(rugarch)
require(quantmod)
require(ghyp)
require(copula)
require(QRM)

# Choose stock and order nbNa by stock
stock <- Equity_US
nb_NAs <- matrix (, nrow = ncol(stock),2)
rownames(nb_NAs) <- colnames(stock)

for (i in 1:ncol(stock)) {

  s <- as.matrix(stock[,i])
  
  #remove NAs from the begining
  nonNA <- which(!is.na(s))
  if (nonNA[1] != 1) {
    s <- s[-(1:nonNA[1]-1), ]
  }
  s <- as.matrix(s)
  

  # Compute the log-return
  s_Return <- qrm.price2ret(s)
  
  #clean data
  cleanReturns <- qrm.clean.prices(as.matrix(s_Return), "remove")
  prop <- cleanReturns$na.prop #nb.na
  run <- cleanReturns$max.na.run #na.run
  
  nb_NAs[i, 1] <- prop
  nb_NAs[i, 2] <- run
  
}

######################################################################################################################################################
# Reliable Stocks US
######################################################################################################################################################

off_days <- 13/365
max.na.run <- 6
reliableStock_US <-c()
for (i in 1:nrow(nb_NAs)) {
  if ((nb_NAs[i, 1] < off_days) && (nb_NAs[i, 2] < max.na.run)) {
    #cat("missing nas ", nb_NAs[i, 1], " max long run: " , nb_NAs[i, 2], " ")
    #print(rownames(nb_NAs)[i]);
    reliableStock_US <- c(reliableStock_US, rownames(nb_NAs)[i])
  }
}
#View(reliableStock_US)

######################################################################################################################################################
# Sector of Stocks US 
######################################################################################################################################################
library(RCurl)
x <- getURL("http://www.nyse.com/indexes/nyaindex.csv")
nyaindex <- read.csv(text = x)
#Retrieve sectors for each stocks by matching with nyaindex

matchedStock_US <-matrix(ncol=2)
sector = array(list(), 12)

for (i in 1:length(reliableStock_US)) {   
  for (j in 1:nrow(nyaindex[2])) {
    if (nyaindex[j, 2] == reliableStock_US[i]) {
      tmp <- c(reliableStock_US[i], nyaindex[j, 5])
      matchedStock_US <- rbind(matchedStock_US, tmp)   
      sector[[nyaindex[j, 5]]] <- c(sector[[nyaindex[j, 5]]], list(reliableStock_US[i]))
    }
  }
}
#View(matchedStock_US)


######################################################################################################################################################
# Market Capitalization of Stocks US
######################################################################################################################################################
n<- 0
stock_characteristics <- vector('list', 12)

for (i in 2:length(sector)) { #iterate over each type of sector
  car1 <- c() #largeMarketCap
  car2 <- c() #mediumMarketCap
  car3 <- c() #smallMarketCap
  if (i!=7) {
    sec <- as.matrix(sector[[i]])
    for (j in 1:length(sec)) { #iterate over each sector of a certain type
      n <- n+1
      tickers <- sec[[j]]
      
      stats <- ldply(tickers, getKeyStats_xpath) #get all stats from yahoo
      rownames(stats) <- tickers
      
      marketCap <- stats[1, 1] #get marketCap as a factor w/ 1 -> need to be parsed
      tmp <- as.character(marketCap)
      
      mCap <- as.numeric(substr(tmp, 1, nchar(tmp)-1))
      quant <- substr(tmp, nchar(tmp), nchar(tmp))
      #sort by market cap
      if(mCap < 2 || quant == "M") {
        car3 <- append(car3, list(tickers))
      } else if (mCap < 10) {
        car2 <- append(car2, list(tickers))
      } else {
        car1 <- append(car1, list(tickers))
      }
    } 
    stock_characteristics_per_sector <- vector("list", 3)   # create a 1d array
    stock_characteristics_per_sector[[1]] <- car1 #big cap
    stock_characteristics_per_sector[[2]] <- car2 #medium cap
    stock_characteristics_per_sector[[3]] <- car3 #samll cap

    stock_characteristics[[i]] <- stock_characteristics_per_sector
  }
}


######################################################################################################################################################
# Test Stylized Facts on 60 US stocks
######################################################################################################################################################
listUSstocks <- c("APD" , "ASH" , "AP" , "KO" , "GT", "SKY" , "MCD" , "ALK" , "RSH" , "AXP" , "WRB" , "ARL" , "HUM" , "COO" , "MMM" , "CSX" , "AVT" , "BMI" , "XOM" , "TSO" , "PKD" , "IBM" , "HRS" , "UIS" , "BT" ,"AEP" , "CMS")
listCHstocks <- c("CH0012214059" , "CH0012255151" , "CH0012138605" , "CH0012268360" , "CH0024899483" ,"CH0015251710" , "CH0012549785" , "CH0012221716" , "CH0025751329" , "CH0008742519" , "CH0030380734" , "CH0010567961" , "CH0011339204")


for (j in 1:2) {
  if (j==1) { #selection of US stocks
    stock <- Equity_US
    lenStock <- length(listUSstocks)
    prices <- stock[, listUSstocks]
    
  } else{ #selection of CH stocks
    stock <- Equity_CH
    lenStock <- length(listCHstocks)
    prices <- stock[, listCHstocks]
    
  }

  stylizedFacts <- array(dim=c(lenStock,1))
  sum.stat <- list()
  
  for (k in 1:lenStock) {
    prices_k <- prices[, k]
    prices_k <- as.matrix(prices_k)
    rownames(prices_k) <- rownames(stock)
    
    # Compute the log-return
    returns <- qrm.price2ret(prices_k, ret.type = "log")  
    returns <- qrm.clean.prices(returns, "remove")$prices.clean
    
    # ----------------------------------------------------------------
    # PART1
    # Return Stylized Facts
    # ----------------------------------------------------------------
    facts <- vector('list', 5)
    limit <- 100
    name <- paste("Ticker", colnames(prices)[k]) 
    
    
    #auto correlation function  
    save <- paste("Autocorrelation - ", name, ".pdf") 
    pdf(save)
    save <- paste("Autocorrelation - ", name) 
    acf <- acf(returns, lag.max=limit, main=save)
    facts[[1]] <- acf
    dev.off()
    
    # heavy tails
    kurtosis <- kurtosis(returns)-3
    facts[[2]] <- kurtosis
    
    # symmetric
    skewness <- skewness(returns)
    facts[[3]] <- skewness
    
    # volatility
    save <- paste("Volatility - ", name, ".pdf") 
    pdf(save)
    save <- paste("Volatility - ", name) 
    volatility<-returns^2
    volatility <- acf(volatility, lag.max =limit, main=save)
    facts[[4]] <- volatility
    dev.off()
    
    #leverage effect
    save <- paste("Leverage - ", name, ".pdf") 
    pdf(save)
    save <- paste("Leverage - ", name) 
    leverage = ccf(returns, (returns^2), lag.max=limit, main=save)
    facts[[5]] <- leverage
    dev.off()
    
    stylizedFacts[[k]] <- list(facts)
  }
  
  names(stylizedFacts[[k]][[1]]) = c("Auto-correlation", "Kurtosis", "Skewness", "Volatility", "Leverage")
  
  # extreme returns
  for (freq in c("asis", "week", "month")) {
    i.ret <- qrm.price2ret(prices, ret.type = "log", freq = freq)
    colnames(i.ret) <- colnames(prices)
    nonNA <- which(!is.na(i.ret))
    if (nonNA[1] != 1) {
      i.ret <- i.ret[-(1:nonNA[1]-1), ]
    }
    sum.stat[[freq]] <- list(min  = apply(i.ret, 2, min, na.rm=TRUE), #Minimum
                             max  = apply(i.ret, 2, max, na.rm=TRUE), #Maximum
                             mean = apply(i.ret, 2, mean, na.rm=TRUE), #Mean
                             med = apply(i.ret, 2, median, na.rm=TRUE), # Median 
                             var = apply(i.ret, 2, var, na.rm=TRUE), # Variance 
                             skew = apply(i.ret, 2, skewness, na.rm=TRUE), #Skewness
                             kurt = apply(i.ret, 2, kurtosis, na.rm=TRUE)) #Kurtosis
    
  }
}

######################################################################################################################################################
# Portfolio Creation
######################################################################################################################################################
money <- 1000000000
listUSstocks <- c("APD" , "ASH" , "KO" , "GT", "MCD" , "ALK" , "AXP" , "WRB" , "HUM" , "COO" , "MMM" , "CSX" , "AVT" , "XOM" , "TSO" , "IBM" , "HRS" , "BT" ,"AEP" , "CMS")
listCHstocks <- c("CH0012214059" , "CH0012255151" , "CH0012138605" , "CH0024899483" ,"CH0015251710" , "CH0012549785" , "CH0012221716" , "CH0008742519" , "CH0030380734" , "CH0010567961")
listETF <- c("BIV.US.Equity", "BLV.US.Equity", "BND.US.Equity", "BSV.US.Equity", "TIP.US.Equity", "PCY.US.Equity")

#compute return
ETFReturn <- qrm.price2ret(ETF[, listETF])
ETFReturn <- qrm.clean.prices(ETFReturn, "remove")$prices.clean

stockUSReturn <- qrm.price2ret(Equity_US[, listUSstocks])
stockUSReturn <- qrm.clean.prices(stockUSReturn, "remove")$prices.clean

stockCHReturn <- qrm.price2ret(Equity_CH[, listCHstocks])
stockCHReturn <- qrm.clean.prices(stockCHReturn, "remove")$prices.clean

tmp<-data.frame(merge(stockCHReturn,stockUSReturn,by="row.names"))
#1st column become row.names
rownames(tmp)<-tmp[, 1]
tmp <- tmp[, 2:length(colnames(tmp))]

portfolio<-merge(ETFReturn,tmp,by="row.names")
rownames(portfolio)<-portfolio[, 1]
portfolio <- portfolio[, 2:length(colnames(portfolio))]

portfolio<-data.matrix(portfolio)

#compute weight
tiers = 1/300
w1=matrix(0.1,nrow=length(listETF))

w2=matrix(0.01+tiers,nrow=(length(listUSstocks)+length(listCHstocks)))
weights=c(w1,w2)

Rport=portfolio%*%weights

sum(Rport)

######################################################################################################################################################
# Rugarch
######################################################################################################################################################
openDays <- 249
len <- length(Rport)
#Normal
specNorm <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(1, 0)))
fitNorm <- ugarchfit(spec=specNorm, data=Rport, out.sample = openDays)
VaRNorm <-fitNorm@fit$sigma*qnorm(0.95)+fitNorm@fit$fitted

#Student
specStud <- ugarchspec(variance.model=list(model='sGARCH',garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,0)),distribution.model="std")
fitStud=ugarchfit(spec=specStud,data=Rport,out.sample=openDays)
VaRStud=fitStud@fit$sigma*qt(.95, df = c(2))+fitStud@fit$fitted

#Empirical


# Backtesting
ReturnOfPortfolio <- Rport[openDays:len, ]
diff <- len-openDays

#normal
count<-0
for (l in 1:diff) {
  if(ReturnOfPortfolio[[l]]>VaRNorm[l]) {
    count <- count + 1
  }
}
exceedVaRNormal <- count/diff
#exceedVaRNormal = 0.0797227

#student
count<-0
for (l in 1:diff) {
  if(ReturnOfPortfolio[[l]]>VaRStud[l]) {
    count <- count + 1
  }
}
exceedVaRStudent <- count/diff
# exceedVaRStudent = 0.03812825

plot(ReturnOfPortfolio, type="h", ylim=c(min(ReturnOfPortfolio), max(VaRStud)))

lines(VaRNorm,col="green")
lines(VaRStud,col="red")

######################################################################################################################################################
# Project PART 2
######################################################################################################################################################
# ----------------------------------------------------------------
# Data
# ----------------------------------------------------------------
getSymbols(listUSstocks, from="2006-12-19", to="2013-12-31", src="yahoo")
adjustedPrices <- cbind(KO[, 6], MMM[, 6], MCD[, 6], ALK[, 6], AXP[, 6], WRB[, 6], HUM[, 6],HRS[, 6], CSX[, 6], IBM[, 6], BT[, 6], AEP[, 6], CMS[, 6])

returnOnAdjustedPrices <- qrm.price2ret(adjustedPrices)
returnCleaned <- qrm.clean.prices(returnOnAdjustedPrices, method="remove")

returnOnAdjustedPrices <- returnCleaned$prices.clean
#see number of consecutive runs
returnCleaned$na.prop
returnCleaned$max.na.run
#no additonal information -> choose the first 10
returnOnAdjustedPrices <- returnOnAdjustedPrices[, 1:10]
logLosses <- returnOnAdjustedPrices *-1

# ----------------------------------------------------------------
# Filtering
# ----------------------------------------------------------------
windowSize <- 1000
len <- length(logLosses)

# residuals <- array(dim=length(logLosses[1, ]),1)
arch.list = list()
stock_orders <- vector('list', ncol(logLosses))

#For each window, fit an appropriate ARMA-GARCH model to the log-losses of each stock separately
for (s in 1:ncol(logLosses)) {
  print (s)
  #test on the first window
  
  #fist value in 2012(2012-01-03): 1268th element of loglosse
  #fist value to start with: 268th element of loglosse
  start <- 268
  len <- 1267
  
  #Find the best ARMA-GARCH Orders
  for (p1 in 0:2) {
    for (q1 in 0:2) {
      for (p2 in 0:2) {
        for (q2 in 0:2) {
          iter <- c(p1, q1, p2, q2)
          print(iter)
          iter <- paste(iter, collapse=" ")
          specStud <- ugarchspec(variance.model=list(model='sGARCH',garchOrder=c(p1,q1)), mean.model=list(armaOrder=c(p2,q2)),distribution.model="std")
          fitStud <- ugarchfit(spec=specStud,data=logLosses[start:len, s],out.sample=0)
          VaRStud <- fitStud@fit$sigma*qt(.95, df = c(1))+fitStud@fit$fitted
          if (length(VaRStud) > 0 ){ #check convergence
            arch.list[[iter]] = fitStud
          }
        }
      }
    }
  }
  info.mat <- sapply(arch.list, infocriteria)
  #bic value :)
  BIC <- info.mat[2, ]
  min <-which(BIC == min(BIC))
  orders <- as.numeric(unlist(strsplit(names(min), " ")))
  stock_orders[[s]] <- orders
  
}

nWindow <- nrow(logLosses)-len
#all parameters for the fitted multivariate t distribution to the residuals
fit_per_window <- vector('list', nWindow)
#fit a multivariate t distribution to the residuals of the ARMA-GARCH fits

#nu for copula
nu_2M <- matrix(nrow=ncol(logLosses), ncol=(nWindow-1))
nu_2D <- matrix(nrow=1, ncol=(nWindow-1))
nu_3M <- matrix(nrow=1, ncol=(nWindow-1))
nu_3D <- matrix(nrow=1, ncol=(nWindow-1))
res <- matrix(nrow=windowSize, ncol=ncol(logLosses))
nu_2Ms <- matrix(nrow=1, ncol=ncol(logLosses))
for (k in 0:(nWindow-1)) {
  cat(k, "computed nu")
  
  join_res <- c()
  for (s in 1:ncol(logLosses)) {
    cat(" stock", s)
    
    specStud <- ugarchspec(variance.model=list(model='sGARCH',garchOrder=c(stock_orders[[s]][1],stock_orders[[s]][2])), mean.model=list(armaOrder=c(stock_orders[[s]][3],stock_orders[[s]][4])),distribution.model="std")
    fitStud <- ugarchfit(spec=specStud,data=logLosses[(start+k):(len+k), s],out.sample=0)
    
    #if converge
    if (fitStud@fit$convergence ==0) {
      residuals_s <- residuals(fitStud, standardize=TRUE)
      
      res[, s] <- residuals_s
      
      #nu
      nu_2Ms[, s] <- fitStud@fit$coef[["shape"]]
    } else {
      print ("not converged")
    }
    #join_res <-rbind(join_res, res[, s])
    
    #fit a multvariate t distribution to the residuals of the ARMA-GARCH fits
    #univ_fit <- fit.tmv(residuals_s)   
  }
  join_res <- do.call(rbind,as.list(res))
  
  #Fit a skewed Student-t distribution to multivariate data.
  my_fit <- fit.tmv(res,symmetric=TRUE,silent=TRUE, na.rm=TRUE)
  #in case we need other functions than nu
  fit_per_window[[k+1]] <- my_fit
  nu_1 <- coef(my_fit)$nu
  
  nu_2M[, (k+1)] <-nu_2Ms

  
  # ----------------------------------------------------------------
  # Improved dependence modeling
  # ----------------------------------------------------------------  
  #distribution function of Student
  obs <- pt(res, df=nu_2Ms)
  
  fit_cop <- fit.tcopula(obs,  method = "Kendall")
  nu_2D[, (k+1)] <- fit_cop$nu #500 items stocked
  
  #Fit a skewed Student-t distribution to univariate data.
  my_fit <- fit.tuv(join_res,symmetric=TRUE,silent=TRUE, na.rm=TRUE)
  nu_3M[, (k+1)] <- coef(my_fit)$nu
  
  obs <- pt(res, df=nu_3M[, (k+1)])
  fit_cop <- fit.tcopula(obs,  method = "Kendall")
  nu_3D[, (k+1)] <- fit_cop$nu #500 items stocked
  cat("\n")
}

# 23.05.14
# TODO: stock nu for each window
# TODO : plot nu with confidence
