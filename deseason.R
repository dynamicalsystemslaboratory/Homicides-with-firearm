rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(xlsx)
library(tidyr)
library(pracma)
library(forecast)
data0 <- openxlsx::read.xlsx("times_series_original_&_sa_det.xlsx",1)

series <- c("Homicides.with.Guns","Guns.Archive","Cities","BCs","GT_hom","GT_Guns","MO.riots", "EPU","MO_unemp","Twitter.number.Homicide")

df_adjusted <- data.frame(data0$Date)
colnames(df_adjusted) <- "Date"

for (i in 1:length(series)){

data <- subset(data0,select=c("Date",series[i]))   #choose the variable needed. 
#we can do all within a loop also
data <-drop_na(data)

#We detrend a linear trend from the series (if not needed, is just a change in level):
x <- detrend(as.numeric(data[,2]),tt="linear")

#we specify  monthly 
x_2 <- msts(x, seasonal.periods=c(12))
data_seas <- mstl(x_2)    


# With the trend that the decomposition still capture(non linear)+ remainder, we get the detrended and SA series 
#df <- data.frame(data$MO.riots,data_seas[,c(1,2,6)],data_seas[,2]+data_seas[,6])

#If we also want to output the SA series (no detrended), don't take the detrended one
x_3 <- msts(data[,2],seasonal.periods=c(12))

data_seas_3 <- mstl(x_3)  
df_3 <- data.frame(data_seas_3)
#so we include the dates, the series, the linearly detrended from data_seas,
#the detrended and SA from data_seas as mentioned before, and the same from the non detrended one, so the SA series
#df <- data.frame(data$Date,data[,2],data_seas_3[,3]+data_seas_3[,4],data_seas_3[,4],data_seas_3[,2]+data_seas_3[,4])

df <- data.frame(data$Date,data[,2],x,x-data_seas[,3],data_seas_3[,2]+data_seas_3[,4])

#And everything as output
colnames(df) <- c("dates","data","detrended","detrended_sa","sa")

if(series[i] %in% c("Homicides.with.Guns")){
  HF_ts <- ts(data[,2],start = c(1999,1),end=c(2020,12),frequency=12)
  HF_ts_sa <- HF_ts- stl(HF_ts,s.window=12)[["time.series"]][,'seasonal']
  df_adjusted <- cbind(df_adjusted,c(rep(NA,dim(df_adjusted)[1]-length(df$sa))+1,HF_ts_sa))
  colnames(df_adjusted)[i+1] <- paste0(series[i]," sa")
} else if(series[i] %in% c("Guns.Archive")){
  GA_ts <- ts(data[,2],start = c(2014,1),end=c(2020,12),frequency=12)
  GA_ts_sa <- GA_ts- stl(GA_ts,s.window=12)[["time.series"]][,'seasonal']
  df_adjusted <- cbind(df_adjusted,c(rep(NA,dim(df_adjusted)[1]-length(df$sa))+1,GA_ts_sa))
  colnames(df_adjusted)[i+1] <- paste0(series[i]," sa")
} else if (series[i] %in% c("Cities","BCs","GT_hom")){
  df_adjusted <- cbind(df_adjusted,c(rep(NA,dim(df_adjusted)[1]-length(df$detrended_sa))+1,df$detrended_sa))
  colnames(df_adjusted)[i+1] <- paste0(series[i],"_sa_det")
} else if (series[i] %in% c("GT_Guns")){
  df_adjusted <- cbind(df_adjusted,c(rep(NA,dim(df_adjusted)[1]-length(df$detrended))+1,df$detrended))
  colnames(df_adjusted)[i+1] <- paste0(series[i],"_det")
} else if (series[i] %in% c("MO.riots","EPU","MO_unemp","Twitter.number.Homicide")){
  df <- data.frame(data$Date,data[,2],x,x-data_seas[,3],data_seas_3[,2]+data_seas_3[,4])
  df_adjusted <- cbind(df_adjusted,c(rep(NA,dim(df_adjusted)[1]-length(df$detrended_sa))+1,df$detrended_sa))
  colnames(df_adjusted)[i+1] <- paste0(series[i],"_sa_det")
}


}


