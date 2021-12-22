#Loading Libraries
library(base)
library(caret)
library(datasets)
library(dplyr)
library(forecast)
library(GGally)
library(ggplot2)
library(graphics)
library(grDevices)
library(lattice)
library(lubridate)
library(methods)
library(Metrics)
library(mlbench)
library(mltools)
library(stats)
library(tseries)
library(utils)
library(repr)
library(ggplot2)
library(ggpubr)
library(hrbrthemes)
library(reshape2)

# Loading the dataset
eth_raw_data = read.csv('ethusdt.csv')


idx =1
# creating new data frame to copy the cleaned dataset
new_data_frame = data.frame(Date = as.Date(character()),Open = double(),High = double(),Low=double(),Close = double(),Volume = double(),Cryptocurrency = character())

volume = 0;
low = 0
high = 0
for(row in 1:nrow(eth_raw_data)){
  date_time = eth_raw_data[row,"Date"]
  date = strsplit(date_time," ")[[1]][1]
  time = strsplit(date_time," ")[[1]][2]
  if(grepl("00:00:00",time,fixed=TRUE)){
    new_data_frame[idx,"Date"] = date
    new_data_frame[idx,"Open"] = eth_raw_data[row,"Open"]
    low = eth_raw_data[row,"Low"]
    high = eth_raw_data[row,"High"]
    volume = volume + eth_raw_data[row,"Volume"]
  }
  else if(grepl("23:30:00",time,fixed=TRUE)){
    new_data_frame[idx,"Close"] = eth_raw_data[row,"Close"]
    volume = volume + eth_raw_data[row,"Volume"]
    if(low>eth_raw_data[row,"Low"]){
      low = eth_raw_data[row,"Low"]
    }
    if(high < eth_raw_data[row,"High"]){
      high = eth_raw_data[row,"High"]
    }
    new_data_frame[idx,"Volume"] = volume
    new_data_frame[idx,"Low"] = low
    new_data_frame[idx,"High"] = high
    new_data_frame[idx,"Cryptocurrency"] = 'Ethereum'
    volume = 0;
    idx = idx+1;
  }
  else{
    if(low>eth_raw_data[row,"Low"]){
      low = eth_raw_data[row,"Low"]
    }
    if(high < eth_raw_data[row,"High"]){
      high = eth_raw_data[row,"High"]
    }
    volume = volume + eth_raw_data[row,"Volume"]
  }
}

eth_imputed_data = new_data_frame
eth_imputed_data = na.omit(eth_imputed_data)

eth_imputed_data$Date = as.Date(eth_imputed_data$Date,format='%Y-%m-%d')

########### Data processing for Cardano dataset ##########

cardano_raw_data = read.csv('adausdt.csv')


idx =1
new_data_frame = data.frame(Date = as.Date(character()),Open = double(),High = double(),Low=double(),Close = double(),Volume = double(),Cryptocurrency = character())
volume = 0;
low = 0
high = 0
for(row in 1:nrow(cardano_raw_data)){
  date_time = cardano_raw_data[row,"Date"]
  date = strsplit(date_time," ")[[1]][1]
  time = strsplit(date_time," ")[[1]][2]
  if(grepl("00:00:00",time,fixed=TRUE)){
    new_data_frame[idx,"Date"] = date
    new_data_frame[idx,"Open"] = cardano_raw_data[row,"Open"]
    low = cardano_raw_data[row,"Low"]
    high = cardano_raw_data[row,"High"]
    volume = volume + cardano_raw_data[row,"Volume"]
  }
  else if(grepl("23:30:00",time,fixed=TRUE)){
    new_data_frame[idx,"Close"] = cardano_raw_data[row,"Close"]
    volume = volume + cardano_raw_data[row,"Volume"]
    if(low>cardano_raw_data[row,"Low"]){
      low = cardano_raw_data[row,"Low"]
    }
    if(high < cardano_raw_data[row,"High"]){
      high = cardano_raw_data[row,"High"]
    }
    new_data_frame[idx,"Volume"] = volume
    new_data_frame[idx,"Low"] = low
    new_data_frame[idx,"High"] = high
    new_data_frame[idx,"Cryptocurrency"] = 'Cardano'
    volume = 0;
    idx = idx+1;
  }
  else{
    if(low>cardano_raw_data[row,"Low"]){
      low = cardano_raw_data[row,"Low"]
    }
    if(high < cardano_raw_data[row,"High"]){
      high = cardano_raw_data[row,"High"]
    }
    volume = volume + cardano_raw_data[row,"Volume"]
  }
}



cardano_imputed_data = new_data_frame
cardano_imputed_data$Date = as.Date(cardano_imputed_data$Date,format='%Y-%m-%d')
cardano_imputed_data = na.omit(cardano_imputed_data)


######## Data processing for Binance #############

binance_raw_data = read.csv('bnbusdt.csv')


idx =1
new_data_frame = data.frame(Date = as.Date(character()),Open = double(),High = double(),Low=double(),Close = double(),Volume = double(),Cryptocurrency = character())
volume = 0;
low = 0
high = 0
for(row in 1:nrow(binance_raw_data)){
  date_time = binance_raw_data[row,"Date"]
  date = strsplit(date_time," ")[[1]][1]
  time = strsplit(date_time," ")[[1]][2]
  if(grepl("00:00:00",time,fixed=TRUE)){
    new_data_frame[idx,"Date"] = date
    new_data_frame[idx,"Open"] = binance_raw_data[row,"Open"]
    low = binance_raw_data[row,"Low"]
    high = binance_raw_data[row,"High"]
    volume = volume + binance_raw_data[row,"Volume"]
  }
  else if(grepl("23:30:00",time,fixed=TRUE)){
    new_data_frame[idx,"Close"] = binance_raw_data[row,"Close"]
    volume = volume + binance_raw_data[row,"Volume"]
    if(low>binance_raw_data[row,"Low"]){
      low = binance_raw_data[row,"Low"]
    }
    if(high < binance_raw_data[row,"High"]){
      high = binance_raw_data[row,"High"]
    }
    new_data_frame[idx,"Volume"] = volume
    new_data_frame[idx,"Low"] = low
    new_data_frame[idx,"High"] = high
    new_data_frame[idx,"Cryptocurrency"] = 'Binance Coin'
    volume = 0;
    idx = idx+1;
  }
  else{
    if(low>binance_raw_data[row,"Low"]){
      low = binance_raw_data[row,"Low"]
    }
    if(high < binance_raw_data[row,"High"]){
      high = binance_raw_data[row,"High"]
    }
    volume = volume + binance_raw_data[row,"Volume"]
  }
}

binance_imputed_data = new_data_frame
binance_imputed_data$Date = as.Date(binance_imputed_data$Date,format='%Y-%m-%d')
binance_imputed_data = na.omit(binance_imputed_data)


##############3 data processing for bitcoin #########

bitcoin_raw_data = read.csv('btcusdt.csv')


idx =1
new_data_frame = data.frame(Date = as.Date(character()),Open = double(),High = double(),Low=double(),Close = double(),Volume = double(),Cryptocurrency = character())
volume = 0;
low = 0
high = 0
for(row in 1:nrow(bitcoin_raw_data)){
  date_time = bitcoin_raw_data[row,"Date"]
  date = strsplit(date_time," ")[[1]][1]
  time = strsplit(date_time," ")[[1]][2]
  if(grepl("00:00:00",time,fixed=TRUE)){
    new_data_frame[idx,"Date"] = date
    new_data_frame[idx,"Open"] = bitcoin_raw_data[row,"Open"]
    low = bitcoin_raw_data[row,"Low"]
    high = bitcoin_raw_data[row,"High"]
    volume = volume + bitcoin_raw_data[row,"Volume"]
  }
  else if(grepl("23:30:00",time,fixed=TRUE)){
    new_data_frame[idx,"Close"] = bitcoin_raw_data[row,"Close"]
    volume = volume + bitcoin_raw_data[row,"Volume"]
    if(low>bitcoin_raw_data[row,"Low"]){
      low = bitcoin_raw_data[row,"Low"]
    }
    if(high < bitcoin_raw_data[row,"High"]){
      high = bitcoin_raw_data[row,"High"]
    }
    new_data_frame[idx,"Volume"] = volume
    new_data_frame[idx,"Low"] = low
    new_data_frame[idx,"High"] = high
    new_data_frame[idx,"Cryptocurrency"] = 'BitCoin'
    volume = 0;
    idx = idx+1;
  }
  else{
    if(low>bitcoin_raw_data[row,"Low"]){
      low = bitcoin_raw_data[row,"Low"]
    }
    if(high < bitcoin_raw_data[row,"High"]){
      high = bitcoin_raw_data[row,"High"]
    }
    volume = volume + bitcoin_raw_data[row,"Volume"]
  }
}


bitcoin_imputed_data = new_data_frame
bitcoin_imputed_data$Date = as.Date(bitcoin_imputed_data$Date,format='%Y-%m-%d')
bitcoin_imputed_data = na.omit(bitcoin_imputed_data)

########## Data processing for XRP ###########

dodge_raw_data = read.csv('dogeusdt.csv')


idx =1
new_data_frame = data.frame(Date = as.Date(character()),Open = double(),High = double(),Low=double(),Close = double(),Volume = double(),Cryptocurrency = character())
volume = 0;
low = 0
high = 0
for(row in 1:nrow(dodge_raw_data)){
  date_time = dodge_raw_data[row,"Date"]
  date = strsplit(date_time," ")[[1]][1]
  time = strsplit(date_time," ")[[1]][2]
  if(grepl("00:00:00",time,fixed=TRUE)){
    new_data_frame[idx,"Date"] = date
    new_data_frame[idx,"Open"] = dodge_raw_data[row,"Open"]
    low = dodge_raw_data[row,"Low"]
    high = dodge_raw_data[row,"High"]
    volume = volume + dodge_raw_data[row,"Volume"]
  }
  else if(grepl("23:30:00",time,fixed=TRUE)){
    new_data_frame[idx,"Close"] = dodge_raw_data[row,"Close"]
    volume = volume + dodge_raw_data[row,"Volume"]
    if(low>dodge_raw_data[row,"Low"]){
      low = dodge_raw_data[row,"Low"]
    }
    if(high < dodge_raw_data[row,"High"]){
      high = dodge_raw_data[row,"High"]
    }
    new_data_frame[idx,"Volume"] = volume
    new_data_frame[idx,"Low"] = low
    new_data_frame[idx,"High"] = high
    new_data_frame[idx,"Cryptocurrency"] = 'DogeCoin'
    volume = 0;
    idx = idx+1;
  }
  else{
    if(low>dodge_raw_data[row,"Low"]){
      low = dodge_raw_data[row,"Low"]
    }
    if(high < dodge_raw_data[row,"High"]){
      high = dodge_raw_data[row,"High"]
    }
    volume = volume + dodge_raw_data[row,"Volume"]
  }
}

dodge_imputed_data = new_data_frame
dodge_imputed_data$Date = as.Date(dodge_imputed_data$Date,format='%Y-%m-%d')
dodge_imputed_data = na.omit(dodge_imputed_data)

################# Data processing for XRP #############

xrp_raw_data = read.csv('xrpusdt.csv')


idx =1
new_data_frame = data.frame(Date = as.Date(character()),Open = double(),High = double(),Low=double(),Close = double(),Volume = double(),Cryptocurrency = character())
volume = 0;
low = 0
high = 0
for(row in 1:nrow(xrp_raw_data)){
  date_time = xrp_raw_data[row,"Date"]
  date = strsplit(date_time," ")[[1]][1]
  time = strsplit(date_time," ")[[1]][2]
  if(grepl("00:00:00",time,fixed=TRUE)){
    new_data_frame[idx,"Date"] = date
    new_data_frame[idx,"Open"] = xrp_raw_data[row,"Open"]
    low = xrp_raw_data[row,"Low"]
    high = xrp_raw_data[row,"High"]
    volume = volume + xrp_raw_data[row,"Volume"]
  }
  else if(grepl("23:30:00",time,fixed=TRUE)){
    new_data_frame[idx,"Close"] = xrp_raw_data[row,"Close"]
    volume = volume + xrp_raw_data[row,"Volume"]
    if(low>xrp_raw_data[row,"Low"]){
      low = xrp_raw_data[row,"Low"]
    }
    if(high < xrp_raw_data[row,"High"]){
      high = xrp_raw_data[row,"High"]
    }
    new_data_frame[idx,"Volume"] = volume
    new_data_frame[idx,"Low"] = low
    new_data_frame[idx,"High"] = high
    new_data_frame[idx,"Cryptocurrency"] = 'XRP'
    volume = 0;
    idx = idx+1;
  }
  else{
    if(low>xrp_raw_data[row,"Low"]){
      low = xrp_raw_data[row,"Low"]
    }
    if(high < xrp_raw_data[row,"High"]){
      high = xrp_raw_data[row,"High"]
    }
    volume = volume + xrp_raw_data[row,"Volume"]
  }
}


xrp_imputed_data = new_data_frame
xrp_imputed_data$Date = as.Date(xrp_imputed_data$Date,format='%Y-%m-%d')
xrp_imputed_data = na.omit(xrp_imputed_data)


################# Merging all the data #################

complete_data = rbind(eth_imputed_data, cardano_imputed_data, binance_imputed_data, bitcoin_imputed_data , dodge_imputed_data, xrp_imputed_data)

complete_data <- na.omit(complete_data) # removing null values


write.csv(x=complete_data, file = "CryptoFinal.csv")



data = read.csv("CryptoFinal.csv")

data = subset(data, select = c(-1))

###################### Exploratory Data Analysis ####################


eth_plot = eth_imputed_data %>%
  ggplot(aes(x=Date,y=Close))+geom_area(fill = "#69b3a2",alpha = 0.5)+geom_line(color="#69b3a2")+theme_ipsum()+ylab("Closing Price ($)")+ylim(0,max(eth_imputed_data$Close)) + ggtitle("Date vs Closing Price")


ggplotly(eth_plot)

cardano_plot = cardano_imputed_data %>%
  ggplot(aes(x=Date,y=Close))+geom_area(fill = "#69b3a2",alpha = 0.5)+geom_line(color="#69b3a2")+ylab("Closing Price ($)")+ylim(0,max(cardano_imputed_data$Close)) + ggtitle("Date vs Closing Price")


ggplotly(cardano_plot)

binance_plot = binance_imputed_data %>%
  ggplot(aes(x=Date,y=Close))+geom_area(fill = "#69b3a2",alpha = 0.5)+geom_line(color="#69b3a2")+ylab("Closing Price ($)")+ylim(0,max(binance_imputed_data$Close)) + ggtitle("Date vs Closing Price")


ggplotly(binance_plot)

bitcoin_plot = bitcoin_imputed_data %>%
  ggplot(aes(x=Date,y=Close))+geom_area(fill = "#69b3a2",alpha = 0.5)+geom_line(color="#69b3a2")+ylab("Closing Price ($)")+ylim(0,max(bitcoin_imputed_data$Close)) + ggtitle("Date vs Closing Price")


ggplotly(bitcoin_plot)

doge_plot = dodge_imputed_data %>%
  ggplot(aes(x=Date,y=Close))+geom_area(fill = "#69b3a2",alpha = 0.5)+geom_line(color="#69b3a2")+ylab("Closing Price ($)")+ylim(0,max(dodge_imputed_data$Close)) + ggtitle("Date vs Closing Price")


ggplotly(doge_plot)

xrp_plot = xrp_imputed_data %>%
  ggplot(aes(x=Date,y=Close))+geom_area(fill = "#69b3a2",alpha = 0.5)+geom_line(color="#69b3a2")+ylab("Closing Price ($)")+ylim(0,max(xrp_imputed_data$Close)) + ggtitle("Date vs Closing Price")


ggplotly(xrp_plot)


figure <- ggarrange(cardano_plot, binance_plot, bitcoin_plot, doge_plot, eth_plot, xrp_plot,
                    labels = c("CARDANO", "Binance Coin", "Bitcoin", "Doge Coin", "Ethereum", "XRP"),
                    ncol = 2, nrow = 3)
figure



### Pie chart for highly traded crypto ###

pi = as.data.frame(data %>% 
                     group_by(Cryptocurrency) %>% 
                     summarise(Frequency = sum(Volume*Close)))
pct <- round(pi$Frequency/sum(pi$Frequency)*100)
lbls = pi$Cryptocurrency
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie3D(pi$Frequency,labels=lbls, col=c("skyblue","skyblue3","paleturquoise3","paleturquoise4","grey","white"),main = "Volume of Cryptocurrencies traded")

#### Correlation of all cryptocurrencies ####
close.cryp <- reshape(complete_data[c(1,5,7)], timevar = "Cryptocurrency", idvar = "Date", direction = "wide")
close.cryp[,"Close.Cryptocurrency"] <- NULL

close <- data.frame(sapply(close.cryp, function(z){as.numeric(as.character(z))}))
#Change names of cols
colnames(close) <- sub("Close.", "", colnames(close))

#Change the date column into POSIXct style
close$Date <- close.cryp$Date

#Number of NA's from Bit Coin starting date
length.col <- colSums(!is.na(close[,-1]))
#Newest cryptocurrency in this dataset
sort(length.col)[1]


close.180 <- close[,colSums(!is.na(close)) >= 180]
corr <- cor(close[,-1], use = "pairwise.complete")

# Correlation between closing prices of different cryptocurrencies
library(corrplot)
corrplot(corr, method = "pie", order="hclust", diag = TRUE, tl.col = "black", tl.cex = 0.7,
         title = "Correlation matrix between different cryptocurrencies",
         mar = c(0,1,2,0))

####### Creating Time-series object for each cryptocurrencies #######

close_ts_eth <- eth_imputed_data[,c("Date", "Close")] %>% arrange(Date)
eth_ts_data = close_ts_eth
close_ts_eth <- ts(eth_ts_data$Close,
                   start = c(as.numeric(format(eth_ts_data$Date[1], "%Y")), as.numeric(format(eth_ts_data$Date[1], "%j"))),
                   end = c(as.numeric(format(eth_ts_data$Date[nrow(eth_ts_data)], "%Y")), as.numeric(format(eth_ts_data$Date[nrow(eth_ts_data)], "%j"))),
                   frequency = 365)


close_ts_btc <- bitcoin_imputed_data[,c("Date", "Close")] %>% arrange(Date)
btc_ts_data = close_ts_btc
close_ts_btc <- ts(btc_ts_data$Close,
                   start = c(as.numeric(format(btc_ts_data$Date[1], "%Y")), as.numeric(format(btc_ts_data$Date[1], "%j"))),
                   end = c(as.numeric(format(btc_ts_data$Date[nrow(btc_ts_data)], "%Y")), as.numeric(format(btc_ts_data$Date[nrow(btc_ts_data)], "%j"))),
                   frequency = 365)


close_ts_ada <- cardano_imputed_data[,c("Date", "Close")] %>% arrange(Date)
ada_ts_data = close_ts_ada
close_ts_ada <- ts(ada_ts_data$Close,
                   start = c(as.numeric(format(ada_ts_data$Date[1], "%Y")), as.numeric(format(ada_ts_data$Date[1], "%j"))),
                   end = c(as.numeric(format(ada_ts_data$Date[nrow(ada_ts_data)], "%Y")), as.numeric(format(ada_ts_data$Date[nrow(ada_ts_data)], "%j"))),
                   frequency = 365)


close_ts_bin <- binance_imputed_data[,c("Date", "Close")] %>% arrange(Date)
bin_ts_data = close_ts_bin
close_ts_bin <- ts(bin_ts_data$Close,
                   start = c(as.numeric(format(bin_ts_data$Date[1], "%Y")), as.numeric(format(bin_ts_data$Date[1], "%j"))),
                   end = c(as.numeric(format(bin_ts_data$Date[nrow(bin_ts_data)], "%Y")), as.numeric(format(bin_ts_data$Date[nrow(bin_ts_data)], "%j"))),
                   frequency = 365)

close_ts_doge <- dodge_imputed_data[,c("Date", "Close")] %>% arrange(Date)
doge_ts_data = close_ts_doge
close_ts_doge <- ts(doge_ts_data$Close,
                    start = c(as.numeric(format(doge_ts_data$Date[1], "%Y")), as.numeric(format(doge_ts_data$Date[1], "%j"))),
                    end = c(as.numeric(format(doge_ts_data$Date[nrow(doge_ts_data)], "%Y")), as.numeric(format(doge_ts_data$Date[nrow(doge_ts_data)], "%j"))),
                    frequency = 365)

close_ts_xrp <- xrp_imputed_data[,c("Date", "Close")] %>% arrange(Date)
xrp_ts_data = close_ts_xrp
close_ts_xrp <- ts(xrp_ts_data$Close,
                   start = c(as.numeric(format(xrp_ts_data$Date[1], "%Y")), as.numeric(format(xrp_ts_data$Date[1], "%j"))),
                   end = c(as.numeric(format(xrp_ts_data$Date[nrow(xrp_ts_data)], "%Y")), as.numeric(format(xrp_ts_data$Date[nrow(xrp_ts_data)], "%j"))),
                   frequency = 365)

par(mfrow=c(3,2))

#### Finding stationaity of cryptocurrencies using difference ####

close_arima_eth_d = diff(close_ts_eth, differences = 1)
close_arima_btc_d = diff(close_ts_btc, differences = 1)
close_arima_ada_d = diff(close_ts_ada, differences = 1)
close_arima_xrp_d = diff(close_ts_xrp, differences = 1)
close_arima_bin_d = diff(close_ts_bin, differences = 1)
close_arima_doge_d = diff(close_ts_doge, differences = 1)

#### Plotting Stationary time series ####
plot(close_arima_eth_d, main = "Ethereum - Stationarity after differencing")
plot(close_arima_btc_d, main = "Bitcoin - Stationarity after differencing")
plot(close_arima_ada_d, main = "Cardano - Stationarity after differencing")
plot(close_arima_xrp_d, main = "XRP - Stationarity after differencing")
plot(close_arima_bin_d, main = "Binance Coin - Stationarity after differencing")
plot(close_arima_doge_d, main = "Doge Coin - Stationarity after differencing")

#### Autocorrelation plot of Cryptocurrencies ####
acf(close_arima_eth_d,lag.max = 30, main = "Ethereum - ACF", ylim = c(-0.2,0.2))
acf(close_arima_btc_d,lag.max = 30, main = "Bitcoin - ACF",ylim = c(-0.2,0.2))
acf(close_arima_ada_d,lag.max = 30, main = "Cardano - ACF",ylim = c(-0.2,0.2))
acf(close_arima_xrp_d,lag.max = 30, main = "XRP - ACF",ylim = c(-0.2,0.2))
acf(close_arima_bin_d,lag.max = 30, main = "Binance Coin - ACF",ylim = c(-0.2,0.2))
acf(close_arima_doge_d,lag.max = 30, main = "Doge Coin - ACF",ylim = c(-0.2,0.2))

#### Partial autocorrelation plot of Cryptocurrencies ####
pacf(close_arima_eth_d,lag.max = 30, main = "Ethereum - PACF")
pacf(close_arima_btc_d,lag.max = 30, main = "Bitcoin - PACF")
pacf(close_arima_ada_d,lag.max = 30, main = "Cardano - PACF")
pacf(close_arima_xrp_d,lag.max = 30, main = "XRP - PACF")
pacf(close_arima_bin_d,lag.max = 30, main = "Binance Coin - PACF")
pacf(close_arima_doge_d,lag.max = 30, main = "Doge Coin - PACF")


dects <- decompose(close_ts_eth) #Obtaining the trends and seasonality
plot(dects)


dects <- decompose(close_ts_btc) #Obtaining the trends and seasonality
plot(dects)

dects <- decompose(close_ts_ada) #Obtaining the trends and seasonality
plot(dects)

close_tr_ts_bin
dects <- decompose(close_ts_doge) #Obtaining the trends and seasonality
plot(dects)


##### model building for each crypto #####
(close_arima_eth <- auto.arima(close_ts_eth,D=1))


(close_arima_btc <- auto.arima(close_ts_btc,D=1))


(close_arima_bin <- auto.arima(close_ts_bin,D=1))


(close_arima_ada <- auto.arima(close_ts_ada,D=1))


(close_arima_doge <- auto.arima(close_ts_doge,D=1))


(close_arima_xrp <- auto.arima(close_ts_xrp,D=1))

par(mfrow=c(3,2))

#### Forecasting model ####

plot(forecast(close_arima_eth),main="Ethereum Coin Forecasting")
plot(forecast(close_arima_btc),main="Bitcoin Forecasting")
plot(forecast(close_arima_ada),main="Cardano Forecasting")
plot(forecast(close_arima_xrp),main="XRP coin Forecasting")
plot(forecast(close_arima_bin),main="Binance coin Forecasting")
plot(forecast(close_arima_doge),main="Dogecoin Forecasting")

