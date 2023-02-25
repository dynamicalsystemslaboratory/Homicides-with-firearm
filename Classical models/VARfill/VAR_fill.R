rm(list=ls())

library(vars)
library(tsDyn)
library(panelvar)
library(plm)
library(Spillover)
library("readxl")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Although it could be more condesated, we create a small look for each month
w <- c(1208,1209,1210,1211,1212,1213,1214,1215,1216,1217,1218,1219,1220,1221)
w1 <- c(1217,1218,1219,1220,1221)


for (momento in w){
  #read data
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$H3 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")

  df <-df[,-c(1,3,6)]
  df$Hom <- df$Hom*10000  #scale
  df$H3 <- df$H3*100000
  VARselect(df, lag.max = 3,type='const')
  #we estimate the model each month
  vartsDyn<- lineVar(df, lag = 3)
  #horizon with filling is 12
  nhor <- 12
  for (i in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+i),]
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,3,6)]
    df_new$H3 <- df_new$H3*100000
    
    
    df <- rbind(df,df_new)
    
  }
  
  #the 12 final observation are forecast iterative, as with no fill (no observations there)
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 12)
  
  fore_v2 <- c(tail(df$Hom,12),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento) - round(momento, -2)),paste0("2.",(momento) - round(momento, -2)),
                    paste0("3.",(momento) - round(momento, -2)),paste0("4.",(momento) - round(momento, -2)),
                    paste0("5.",(momento) - round(momento, -2)),paste0("6.",(momento) - round(momento, -2)),
                    paste0("7.",(momento) - round(momento, -2)),paste0("8.",(momento) - round(momento, -2)),
                    paste0("9.",(momento) - round(momento, -2)),paste0("10.",(momento) - round(momento, -2)),
                    paste0("11.",(momento) - round(momento, -2)),paste0("12.",(momento) - round(momento, -2)),
                    paste0("1.",(momento+1) - round(momento, -2)),paste0("2.",(momento+1) - round(momento, -2)),
                    paste0("3.",(momento+1) - round(momento, -2)),paste0("4.",(momento+1) - round(momento, -2)),
                    paste0("5.",(momento+1) - round(momento, -2)),paste0("6.",(momento+1) - round(momento, -2)),
                    paste0("7.",(momento+1) - round(momento, -2)),paste0("8.",(momento+1) - round(momento, -2)),
                    paste0("9.",(momento+1) - round(momento, -2)),paste0("10.",(momento+1) - round(momento, -2)),
                    paste0("11.",(momento+1) - round(momento, -2)),paste0("12.",(momento+1) - round(momento, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento,".xlsx"))
  
}




#Here we include the data from Guns Archive when available

for (momento1 in w1){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento1,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$GA != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,6)]
  df$Hom <- df$Hom*10000
  df$H3 <- df$H3*100000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 12
  for (j in 1:nhor){  
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+j),]
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,6)]
    
    
    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 12)
  
  fore_v2 <- c(tail(df$Hom,12),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento1) - round(momento1, -2)),paste0("2.",(momento1) - round(momento1, -2)),
                    paste0("3.",(momento1) - round(momento1, -2)),paste0("4.",(momento1) - round(momento1, -2)),
                    paste0("5.",(momento1) - round(momento1, -2)),paste0("6.",(momento1) - round(momento1, -2)),
                    paste0("7.",(momento1) - round(momento1, -2)),paste0("8.",(momento1) - round(momento1, -2)),
                    paste0("9.",(momento1) - round(momento1, -2)),paste0("10.",(momento1) - round(momento1, -2)),
                    paste0("11.",(momento1) - round(momento1, -2)),paste0("12.",(momento1) - round(momento1, -2)),
                    paste0("1.",(momento1+1) - round(momento1, -2)),paste0("2.",(momento1+1) - round(momento1, -2)),
                    paste0("3.",(momento1+1) - round(momento1, -2)),paste0("4.",(momento1+1) - round(momento1, -2)),
                    paste0("5.",(momento1+1) - round(momento1, -2)),paste0("6.",(momento1+1) - round(momento1, -2)),
                    paste0("7.",(momento1+1) - round(momento1, -2)),paste0("8.",(momento1+1) - round(momento1, -2)),
                    paste0("9.",(momento1+1) - round(momento1, -2)),paste0("10.",(momento1+1) - round(momento1, -2)),
                    paste0("11.",(momento1+1) - round(momento1, -2)),paste0("12.",(momento1+1) - round(momento1, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento1,"conGA",".xlsx"))
  
}


#We act similarly for the other months, changing the horizon for with filling is available, 
#and the iterative forecast have to be done

w <- c(0109,0110,0111,0112,0113,0114,0115,0116,0117,0118,0119,0120,0121)
w1 <- c(0117,0118,0119,0120,0121)


for (momento in w){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$H3 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,3,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 13
  for (i in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+i),]
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,3,6)]
    

    df <- rbind(df,df_new)
    
  }
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento-1) - round(momento-1, -2)),paste0("2.",(momento-1) - round(momento-1, -2)),
                    paste0("3.",(momento-1) - round(momento-1, -2)),paste0("4.",(momento-1) - round(momento-1, -2)),
                    paste0("5.",(momento-1) - round(momento-1, -2)),paste0("6.",(momento-1) - round(momento-1, -2)),
                    paste0("7.",(momento-1) - round(momento-1, -2)),paste0("8.",(momento-1) - round(momento-1, -2)),
                    paste0("9.",(momento-1) - round(momento-1, -2)),paste0("10.",(momento-1) - round(momento-1, -2)),
                    paste0("11.",(momento-1) - round(momento-1, -2)),paste0("12.",(momento-1) - round(momento-1, -2)),
                    paste0("1.",(momento) - round(momento, -2)),paste0("2.",(momento) - round(momento, -2)),
                    paste0("3.",(momento) - round(momento, -2)),paste0("4.",(momento) - round(momento, -2)),
                    paste0("5.",(momento) - round(momento, -2)),paste0("6.",(momento) - round(momento, -2)),
                    paste0("7.",(momento) - round(momento, -2)),paste0("8.",(momento) - round(momento, -2)),
                    paste0("9.",(momento) - round(momento, -2)),paste0("10.",(momento) - round(momento, -2)),
                    paste0("11.",(momento) - round(momento, -2)),paste0("12.",(momento) - round(momento, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento,".xlsx"))
  
}



for (momento1 in w1){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento1,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$GA != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 13
  for (j in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+j),]
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,6)]

    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento1-1) - round(momento1-1, -2)),paste0("2.",(momento1-1) - round(momento1-1, -2)),
                    paste0("3.",(momento1-1) - round(momento1-1, -2)),paste0("4.",(momento1-1) - round(momento1-1, -2)),
                    paste0("5.",(momento1-1) - round(momento1-1, -2)),paste0("6.",(momento1-1) - round(momento1-1, -2)),
                    paste0("7.",(momento1-1) - round(momento1-1, -2)),paste0("8.",(momento1-1) - round(momento1-1, -2)),
                    paste0("9.",(momento1-1) - round(momento1-1, -2)),paste0("10.",(momento1-1) - round(momento1-1, -2)),
                    paste0("11.",(momento1-1) - round(momento1-1, -2)),paste0("12.",(momento1-1) - round(momento1-1, -2)),
                    paste0("1.",(momento1) - round(momento1, -2)),paste0("2.",(momento1) - round(momento1, -2)),
                    paste0("3.",(momento1) - round(momento1, -2)),paste0("4.",(momento1) - round(momento1, -2)),
                    paste0("5.",(momento1) - round(momento1, -2)),paste0("6.",(momento1) - round(momento1, -2)),
                    paste0("7.",(momento1) - round(momento1, -2)),paste0("8.",(momento1) - round(momento1, -2)),
                    paste0("9.",(momento1) - round(momento1, -2)),paste0("10.",(momento1) - round(momento1, -2)),
                    paste0("11.",(momento1) - round(momento1, -2)),paste0("12.",(momento1) - round(momento1, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento1,"conGA",".xlsx"))
  
}



w <- c(0209,0210,0211,0212,0213,0214,0215,0216,0217,0218,0219,0220,0221)
w1 <- c(0217,0218,0219,0220,0221)


for (momento in w){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$H3 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,3,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 14   #changes
  for (i in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+i),]  #addition changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,3,6)]
    
 
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento-1) - round(momento-1, -2)),paste0("2.",(momento-1) - round(momento-1, -2)),
                    paste0("3.",(momento-1) - round(momento-1, -2)),paste0("4.",(momento-1) - round(momento-1, -2)),
                    paste0("5.",(momento-1) - round(momento-1, -2)),paste0("6.",(momento-1) - round(momento-1, -2)),
                    paste0("7.",(momento-1) - round(momento-1, -2)),paste0("8.",(momento-1) - round(momento-1, -2)),
                    paste0("9.",(momento-1) - round(momento-1, -2)),paste0("10.",(momento-1) - round(momento-1, -2)),
                    paste0("11.",(momento-1) - round(momento-1, -2)),paste0("12.",(momento-1) - round(momento-1, -2)),
                    paste0("1.",(momento) - round(momento, -2)),paste0("2.",(momento) - round(momento, -2)),
                    paste0("3.",(momento) - round(momento, -2)),paste0("4.",(momento) - round(momento, -2)),
                    paste0("5.",(momento) - round(momento, -2)),paste0("6.",(momento) - round(momento, -2)),
                    paste0("7.",(momento) - round(momento, -2)),paste0("8.",(momento) - round(momento, -2)),
                    paste0("9.",(momento) - round(momento, -2)),paste0("10.",(momento) - round(momento, -2)),
                    paste0("11.",(momento) - round(momento, -2)),paste0("12.",(momento) - round(momento, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento,".xlsx"))
  
}



for (momento1 in w1){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento1,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$GA != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 14  #nhor changes 
  for (j in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+j),] #the add changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,6)]
    
    
    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento1-1) - round(momento1-1, -2)),paste0("2.",(momento1-1) - round(momento1-1, -2)),
                    paste0("3.",(momento1-1) - round(momento1-1, -2)),paste0("4.",(momento1-1) - round(momento1-1, -2)),
                    paste0("5.",(momento1-1) - round(momento1-1, -2)),paste0("6.",(momento1-1) - round(momento1-1, -2)),
                    paste0("7.",(momento1-1) - round(momento1-1, -2)),paste0("8.",(momento1-1) - round(momento1-1, -2)),
                    paste0("9.",(momento1-1) - round(momento1-1, -2)),paste0("10.",(momento1-1) - round(momento1-1, -2)),
                    paste0("11.",(momento1-1) - round(momento1-1, -2)),paste0("12.",(momento1-1) - round(momento1-1, -2)),
                    paste0("1.",(momento1) - round(momento1, -2)),paste0("2.",(momento1) - round(momento1, -2)),
                    paste0("3.",(momento1) - round(momento1, -2)),paste0("4.",(momento1) - round(momento1, -2)),
                    paste0("5.",(momento1) - round(momento1, -2)),paste0("6.",(momento1) - round(momento1, -2)),
                    paste0("7.",(momento1) - round(momento1, -2)),paste0("8.",(momento1) - round(momento1, -2)),
                    paste0("9.",(momento1) - round(momento1, -2)),paste0("10.",(momento1) - round(momento1, -2)),
                    paste0("11.",(momento1) - round(momento1, -2)),paste0("12.",(momento1) - round(momento1, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento1,"conGA",".xlsx"))
  
}




w <- c(0309,0310,0311,0312,0313,0314,0315,0316,0317,0318,0319,0320,0321)
w1 <- c(0317,0318,0319,0320,0321)


for (momento in w){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$H3 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,3,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 15   #changes
  for (i in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+i),]  #addition changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,3,6)]
    
    
    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento-1) - round(momento-1, -2)),paste0("2.",(momento-1) - round(momento-1, -2)),
                    paste0("3.",(momento-1) - round(momento-1, -2)),paste0("4.",(momento-1) - round(momento-1, -2)),
                    paste0("5.",(momento-1) - round(momento-1, -2)),paste0("6.",(momento-1) - round(momento-1, -2)),
                    paste0("7.",(momento-1) - round(momento-1, -2)),paste0("8.",(momento-1) - round(momento-1, -2)),
                    paste0("9.",(momento-1) - round(momento-1, -2)),paste0("10.",(momento-1) - round(momento-1, -2)),
                    paste0("11.",(momento-1) - round(momento-1, -2)),paste0("12.",(momento-1) - round(momento-1, -2)),
                    paste0("1.",(momento) - round(momento, -2)),paste0("2.",(momento) - round(momento, -2)),
                    paste0("3.",(momento) - round(momento, -2)),paste0("4.",(momento) - round(momento, -2)),
                    paste0("5.",(momento) - round(momento, -2)),paste0("6.",(momento) - round(momento, -2)),
                    paste0("7.",(momento) - round(momento, -2)),paste0("8.",(momento) - round(momento, -2)),
                    paste0("9.",(momento) - round(momento, -2)),paste0("10.",(momento) - round(momento, -2)),
                    paste0("11.",(momento) - round(momento, -2)),paste0("12.",(momento) - round(momento, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento,".xlsx"))
  
}


for (momento1 in w1){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento1,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$GA != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 15  #nhor changes 
  for (j in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+j),] #the add changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,6)]
    

    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento1-1) - round(momento1-1, -2)),paste0("2.",(momento1-1) - round(momento1-1, -2)),
                    paste0("3.",(momento1-1) - round(momento1-1, -2)),paste0("4.",(momento1-1) - round(momento1-1, -2)),
                    paste0("5.",(momento1-1) - round(momento1-1, -2)),paste0("6.",(momento1-1) - round(momento1-1, -2)),
                    paste0("7.",(momento1-1) - round(momento1-1, -2)),paste0("8.",(momento1-1) - round(momento1-1, -2)),
                    paste0("9.",(momento1-1) - round(momento1-1, -2)),paste0("10.",(momento1-1) - round(momento1-1, -2)),
                    paste0("11.",(momento1-1) - round(momento1-1, -2)),paste0("12.",(momento1-1) - round(momento1-1, -2)),
                    paste0("1.",(momento1) - round(momento1, -2)),paste0("2.",(momento1) - round(momento1, -2)),
                    paste0("3.",(momento1) - round(momento1, -2)),paste0("4.",(momento1) - round(momento1, -2)),
                    paste0("5.",(momento1) - round(momento1, -2)),paste0("6.",(momento1) - round(momento1, -2)),
                    paste0("7.",(momento1) - round(momento1, -2)),paste0("8.",(momento1) - round(momento1, -2)),
                    paste0("9.",(momento1) - round(momento1, -2)),paste0("10.",(momento1) - round(momento1, -2)),
                    paste0("11.",(momento1) - round(momento1, -2)),paste0("12.",(momento1) - round(momento1, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento1,"conGA",".xlsx"))
  
}


w <- c(0409,0410,0411,0412,0413,0414,0415,0416,0417,0418,0419,0420,0421)
w1 <- c(0417,0418,0419,0420,0421)


for (momento in w){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$H3 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,3,6)]
  df$Hom <- df$Hom*10000  #to set it to a closer scale
  df$H3 <- df$H3*100000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 16   #changes
  for (i in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+i),]  #addition changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,3,6)]
    #df_new$Hom <- df_new$Hom*10000
    df_new$H3 <- df_new$H3*100000
    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento-1) - round(momento-1, -2)),paste0("2.",(momento-1) - round(momento-1, -2)),
                    paste0("3.",(momento-1) - round(momento-1, -2)),paste0("4.",(momento-1) - round(momento-1, -2)),
                    paste0("5.",(momento-1) - round(momento-1, -2)),paste0("6.",(momento-1) - round(momento-1, -2)),
                    paste0("7.",(momento-1) - round(momento-1, -2)),paste0("8.",(momento-1) - round(momento-1, -2)),
                    paste0("9.",(momento-1) - round(momento-1, -2)),paste0("10.",(momento-1) - round(momento-1, -2)),
                    paste0("11.",(momento-1) - round(momento-1, -2)),paste0("12.",(momento-1) - round(momento-1, -2)),
                    paste0("1.",(momento) - round(momento, -2)),paste0("2.",(momento) - round(momento, -2)),
                    paste0("3.",(momento) - round(momento, -2)),paste0("4.",(momento) - round(momento, -2)),
                    paste0("5.",(momento) - round(momento, -2)),paste0("6.",(momento) - round(momento, -2)),
                    paste0("7.",(momento) - round(momento, -2)),paste0("8.",(momento) - round(momento, -2)),
                    paste0("9.",(momento) - round(momento, -2)),paste0("10.",(momento) - round(momento, -2)),
                    paste0("11.",(momento) - round(momento, -2)),paste0("12.",(momento) - round(momento, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento,".xlsx"))
  
}



for (momento1 in w1){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento1,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$GA != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,6)]
  df$Hom <- df$Hom*10000
  df$H3 <- df$H3*100000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 16  #nhor changes 
  for (j in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+j),] #the add changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,6)]
    df_new$H3 <- df_new$H3*100000
    
    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento1-1) - round(momento1-1, -2)),paste0("2.",(momento1-1) - round(momento1-1, -2)),
                    paste0("3.",(momento1-1) - round(momento1-1, -2)),paste0("4.",(momento1-1) - round(momento1-1, -2)),
                    paste0("5.",(momento1-1) - round(momento1-1, -2)),paste0("6.",(momento1-1) - round(momento1-1, -2)),
                    paste0("7.",(momento1-1) - round(momento1-1, -2)),paste0("8.",(momento1-1) - round(momento1-1, -2)),
                    paste0("9.",(momento1-1) - round(momento1-1, -2)),paste0("10.",(momento1-1) - round(momento1-1, -2)),
                    paste0("11.",(momento1-1) - round(momento1-1, -2)),paste0("12.",(momento1-1) - round(momento1-1, -2)),
                    paste0("1.",(momento1) - round(momento1, -2)),paste0("2.",(momento1) - round(momento1, -2)),
                    paste0("3.",(momento1) - round(momento1, -2)),paste0("4.",(momento1) - round(momento1, -2)),
                    paste0("5.",(momento1) - round(momento1, -2)),paste0("6.",(momento1) - round(momento1, -2)),
                    paste0("7.",(momento1) - round(momento1, -2)),paste0("8.",(momento1) - round(momento1, -2)),
                    paste0("9.",(momento1) - round(momento1, -2)),paste0("10.",(momento1) - round(momento1, -2)),
                    paste0("11.",(momento1) - round(momento1, -2)),paste0("12.",(momento1) - round(momento1, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento1,"conGA",".xlsx"))
  
}



w <- c(0509,0510,0511,0512,0513,0514,0515,0516,0517,0518,0519,0520,0521)
w1 <- c(0517,0518,0519,0520,0521)


for (momento in w){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$H3 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,3,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 17   #changes
  for (i in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+i),]  #addition changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,3,6)]
    
    
    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento-1) - round(momento-1, -2)),paste0("2.",(momento-1) - round(momento-1, -2)),
                    paste0("3.",(momento-1) - round(momento-1, -2)),paste0("4.",(momento-1) - round(momento-1, -2)),
                    paste0("5.",(momento-1) - round(momento-1, -2)),paste0("6.",(momento-1) - round(momento-1, -2)),
                    paste0("7.",(momento-1) - round(momento-1, -2)),paste0("8.",(momento-1) - round(momento-1, -2)),
                    paste0("9.",(momento-1) - round(momento-1, -2)),paste0("10.",(momento-1) - round(momento-1, -2)),
                    paste0("11.",(momento-1) - round(momento-1, -2)),paste0("12.",(momento-1) - round(momento-1, -2)),
                    paste0("1.",(momento) - round(momento, -2)),paste0("2.",(momento) - round(momento, -2)),
                    paste0("3.",(momento) - round(momento, -2)),paste0("4.",(momento) - round(momento, -2)),
                    paste0("5.",(momento) - round(momento, -2)),paste0("6.",(momento) - round(momento, -2)),
                    paste0("7.",(momento) - round(momento, -2)),paste0("8.",(momento) - round(momento, -2)),
                    paste0("9.",(momento) - round(momento, -2)),paste0("10.",(momento) - round(momento, -2)),
                    paste0("11.",(momento) - round(momento, -2)),paste0("12.",(momento) - round(momento, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento,".xlsx"))
  
}


for (momento1 in w1){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento1,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$GA != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 17  #nhor changes 
  for (j in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+j),] #the add changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,6)]
    
    
    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento1-1) - round(momento1-1, -2)),paste0("2.",(momento1-1) - round(momento1-1, -2)),
                    paste0("3.",(momento1-1) - round(momento1-1, -2)),paste0("4.",(momento1-1) - round(momento1-1, -2)),
                    paste0("5.",(momento1-1) - round(momento1-1, -2)),paste0("6.",(momento1-1) - round(momento1-1, -2)),
                    paste0("7.",(momento1-1) - round(momento1-1, -2)),paste0("8.",(momento1-1) - round(momento1-1, -2)),
                    paste0("9.",(momento1-1) - round(momento1-1, -2)),paste0("10.",(momento1-1) - round(momento1-1, -2)),
                    paste0("11.",(momento1-1) - round(momento1-1, -2)),paste0("12.",(momento1-1) - round(momento1-1, -2)),
                    paste0("1.",(momento1) - round(momento1, -2)),paste0("2.",(momento1) - round(momento1, -2)),
                    paste0("3.",(momento1) - round(momento1, -2)),paste0("4.",(momento1) - round(momento1, -2)),
                    paste0("5.",(momento1) - round(momento1, -2)),paste0("6.",(momento1) - round(momento1, -2)),
                    paste0("7.",(momento1) - round(momento1, -2)),paste0("8.",(momento1) - round(momento1, -2)),
                    paste0("9.",(momento1) - round(momento1, -2)),paste0("10.",(momento1) - round(momento1, -2)),
                    paste0("11.",(momento1) - round(momento1, -2)),paste0("12.",(momento1) - round(momento1, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento1,"conGA",".xlsx"))
  
}



w <- c(0609,0610,0611,0612,0613,0614,0615,0616,0617,0618,0619,0620,0621)
w1 <- c(0617,0618,0619,0620,0621)


for (momento in w){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$H3 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,3,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 18   #changes
  for (i in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+i),]  #addition changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,3,6)]
    
    
    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento-1) - round(momento-1, -2)),paste0("2.",(momento-1) - round(momento-1, -2)),
                    paste0("3.",(momento-1) - round(momento-1, -2)),paste0("4.",(momento-1) - round(momento-1, -2)),
                    paste0("5.",(momento-1) - round(momento-1, -2)),paste0("6.",(momento-1) - round(momento-1, -2)),
                    paste0("7.",(momento-1) - round(momento-1, -2)),paste0("8.",(momento-1) - round(momento-1, -2)),
                    paste0("9.",(momento-1) - round(momento-1, -2)),paste0("10.",(momento-1) - round(momento-1, -2)),
                    paste0("11.",(momento-1) - round(momento-1, -2)),paste0("12.",(momento-1) - round(momento-1, -2)),
                    paste0("1.",(momento) - round(momento, -2)),paste0("2.",(momento) - round(momento, -2)),
                    paste0("3.",(momento) - round(momento, -2)),paste0("4.",(momento) - round(momento, -2)),
                    paste0("5.",(momento) - round(momento, -2)),paste0("6.",(momento) - round(momento, -2)),
                    paste0("7.",(momento) - round(momento, -2)),paste0("8.",(momento) - round(momento, -2)),
                    paste0("9.",(momento) - round(momento, -2)),paste0("10.",(momento) - round(momento, -2)),
                    paste0("11.",(momento) - round(momento, -2)),paste0("12.",(momento) - round(momento, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento,".xlsx"))
  
}


for (momento1 in w1){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento1,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$GA != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 17  #nhor changes 
  for (j in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+j),] #the add changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,6)]
    
 
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento1-1) - round(momento1-1, -2)),paste0("2.",(momento1-1) - round(momento1-1, -2)),
                    paste0("3.",(momento1-1) - round(momento1-1, -2)),paste0("4.",(momento1-1) - round(momento1-1, -2)),
                    paste0("5.",(momento1-1) - round(momento1-1, -2)),paste0("6.",(momento1-1) - round(momento1-1, -2)),
                    paste0("7.",(momento1-1) - round(momento1-1, -2)),paste0("8.",(momento1-1) - round(momento1-1, -2)),
                    paste0("9.",(momento1-1) - round(momento1-1, -2)),paste0("10.",(momento1-1) - round(momento1-1, -2)),
                    paste0("11.",(momento1-1) - round(momento1-1, -2)),paste0("12.",(momento1-1) - round(momento1-1, -2)),
                    paste0("1.",(momento1) - round(momento1, -2)),paste0("2.",(momento1) - round(momento1, -2)),
                    paste0("3.",(momento1) - round(momento1, -2)),paste0("4.",(momento1) - round(momento1, -2)),
                    paste0("5.",(momento1) - round(momento1, -2)),paste0("6.",(momento1) - round(momento1, -2)),
                    paste0("7.",(momento1) - round(momento1, -2)),paste0("8.",(momento1) - round(momento1, -2)),
                    paste0("9.",(momento1) - round(momento1, -2)),paste0("10.",(momento1) - round(momento1, -2)),
                    paste0("11.",(momento1) - round(momento1, -2)),paste0("12.",(momento1) - round(momento1, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento1,"conGA",".xlsx"))
  
}





w <- c(0709,0710,0711,0712,0713,0714,0715,0716,0717,0718,0719,0720,0721)
w1 <- c(0717,0718,0719,0720,0721)


for (momento in w){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$H3 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,3,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 19   #changes
  for (i in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+i),]  #addition changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,3,6)]
    
    
    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento-1) - round(momento-1, -2)),paste0("2.",(momento-1) - round(momento-1, -2)),
                    paste0("3.",(momento-1) - round(momento-1, -2)),paste0("4.",(momento-1) - round(momento-1, -2)),
                    paste0("5.",(momento-1) - round(momento-1, -2)),paste0("6.",(momento-1) - round(momento-1, -2)),
                    paste0("7.",(momento-1) - round(momento-1, -2)),paste0("8.",(momento-1) - round(momento-1, -2)),
                    paste0("9.",(momento-1) - round(momento-1, -2)),paste0("10.",(momento-1) - round(momento-1, -2)),
                    paste0("11.",(momento-1) - round(momento-1, -2)),paste0("12.",(momento-1) - round(momento-1, -2)),
                    paste0("1.",(momento) - round(momento, -2)),paste0("2.",(momento) - round(momento, -2)),
                    paste0("3.",(momento) - round(momento, -2)),paste0("4.",(momento) - round(momento, -2)),
                    paste0("5.",(momento) - round(momento, -2)),paste0("6.",(momento) - round(momento, -2)),
                    paste0("7.",(momento) - round(momento, -2)),paste0("8.",(momento) - round(momento, -2)),
                    paste0("9.",(momento) - round(momento, -2)),paste0("10.",(momento) - round(momento, -2)),
                    paste0("11.",(momento) - round(momento, -2)),paste0("12.",(momento) - round(momento, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento,".xlsx"))
  
}


for (momento1 in w1){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento1,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$GA != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 19  #nhor changes 
  for (j in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+j),] #the add changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,6)]
    
    
    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento1-1) - round(momento1-1, -2)),paste0("2.",(momento1-1) - round(momento1-1, -2)),
                    paste0("3.",(momento1-1) - round(momento1-1, -2)),paste0("4.",(momento1-1) - round(momento1-1, -2)),
                    paste0("5.",(momento1-1) - round(momento1-1, -2)),paste0("6.",(momento1-1) - round(momento1-1, -2)),
                    paste0("7.",(momento1-1) - round(momento1-1, -2)),paste0("8.",(momento1-1) - round(momento1-1, -2)),
                    paste0("9.",(momento1-1) - round(momento1-1, -2)),paste0("10.",(momento1-1) - round(momento1-1, -2)),
                    paste0("11.",(momento1-1) - round(momento1-1, -2)),paste0("12.",(momento1-1) - round(momento1-1, -2)),
                    paste0("1.",(momento1) - round(momento1, -2)),paste0("2.",(momento1) - round(momento1, -2)),
                    paste0("3.",(momento1) - round(momento1, -2)),paste0("4.",(momento1) - round(momento1, -2)),
                    paste0("5.",(momento1) - round(momento1, -2)),paste0("6.",(momento1) - round(momento1, -2)),
                    paste0("7.",(momento1) - round(momento1, -2)),paste0("8.",(momento1) - round(momento1, -2)),
                    paste0("9.",(momento1) - round(momento1, -2)),paste0("10.",(momento1) - round(momento1, -2)),
                    paste0("11.",(momento1) - round(momento1, -2)),paste0("12.",(momento1) - round(momento1, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento1,"conGA",".xlsx"))
  
}




w <- c(0809,0810,0811,0812,0813,0814,0815,0816,0817,0818,0819,0820,0821)
w1 <- c(0817,0818,0819,0820,0821)


for (momento in w){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_vf_",momento,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$H3 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,3,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 20   #changes
  for (i in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+i),]  #addition changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,3,6)]
    
    
    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento-1) - round(momento-1, -2)),paste0("2.",(momento-1) - round(momento-1, -2)),
                    paste0("3.",(momento-1) - round(momento-1, -2)),paste0("4.",(momento-1) - round(momento-1, -2)),
                    paste0("5.",(momento-1) - round(momento-1, -2)),paste0("6.",(momento-1) - round(momento-1, -2)),
                    paste0("7.",(momento-1) - round(momento-1, -2)),paste0("8.",(momento-1) - round(momento-1, -2)),
                    paste0("9.",(momento-1) - round(momento-1, -2)),paste0("10.",(momento-1) - round(momento-1, -2)),
                    paste0("11.",(momento-1) - round(momento-1, -2)),paste0("12.",(momento-1) - round(momento-1, -2)),
                    paste0("1.",(momento) - round(momento, -2)),paste0("2.",(momento) - round(momento, -2)),
                    paste0("3.",(momento) - round(momento, -2)),paste0("4.",(momento) - round(momento, -2)),
                    paste0("5.",(momento) - round(momento, -2)),paste0("6.",(momento) - round(momento, -2)),
                    paste0("7.",(momento) - round(momento, -2)),paste0("8.",(momento) - round(momento, -2)),
                    paste0("9.",(momento) - round(momento, -2)),paste0("10.",(momento) - round(momento, -2)),
                    paste0("11.",(momento) - round(momento, -2)),paste0("12.",(momento) - round(momento, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento,".xlsx"))
  
}


for (momento1 in w1){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento1,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$GA != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 20  #nhor changes 
  for (j in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+j),] #the add changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,6)]

    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento1-1) - round(momento1-1, -2)),paste0("2.",(momento1-1) - round(momento1-1, -2)),
                    paste0("3.",(momento1-1) - round(momento1-1, -2)),paste0("4.",(momento1-1) - round(momento1-1, -2)),
                    paste0("5.",(momento1-1) - round(momento1-1, -2)),paste0("6.",(momento1-1) - round(momento1-1, -2)),
                    paste0("7.",(momento1-1) - round(momento1-1, -2)),paste0("8.",(momento1-1) - round(momento1-1, -2)),
                    paste0("9.",(momento1-1) - round(momento1-1, -2)),paste0("10.",(momento1-1) - round(momento1-1, -2)),
                    paste0("11.",(momento1-1) - round(momento1-1, -2)),paste0("12.",(momento1-1) - round(momento1-1, -2)),
                    paste0("1.",(momento1) - round(momento1, -2)),paste0("2.",(momento1) - round(momento1, -2)),
                    paste0("3.",(momento1) - round(momento1, -2)),paste0("4.",(momento1) - round(momento1, -2)),
                    paste0("5.",(momento1) - round(momento1, -2)),paste0("6.",(momento1) - round(momento1, -2)),
                    paste0("7.",(momento1) - round(momento1, -2)),paste0("8.",(momento1) - round(momento1, -2)),
                    paste0("9.",(momento1) - round(momento1, -2)),paste0("10.",(momento1) - round(momento1, -2)),
                    paste0("11.",(momento1) - round(momento1, -2)),paste0("12.",(momento1) - round(momento1, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento1,"conGA",".xlsx"))
  
}



w <- c(0909,0910,0911,0912,0913,0914,0915,0916,0917,0918,0919,0920,0921)
w1 <- c(0917,0918,0919,0920,0921)


for (momento in w){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$H3 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,3,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 21   #changes
  for (i in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+i),]  #addition changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,3,6)]

    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento-1) - round(momento-1, -2)),paste0("2.",(momento-1) - round(momento-1, -2)),
                    paste0("3.",(momento-1) - round(momento-1, -2)),paste0("4.",(momento-1) - round(momento-1, -2)),
                    paste0("5.",(momento-1) - round(momento-1, -2)),paste0("6.",(momento-1) - round(momento-1, -2)),
                    paste0("7.",(momento-1) - round(momento-1, -2)),paste0("8.",(momento-1) - round(momento-1, -2)),
                    paste0("9.",(momento-1) - round(momento-1, -2)),paste0("10.",(momento-1) - round(momento-1, -2)),
                    paste0("11.",(momento-1) - round(momento-1, -2)),paste0("12.",(momento-1) - round(momento-1, -2)),
                    paste0("1.",(momento) - round(momento, -2)),paste0("2.",(momento) - round(momento, -2)),
                    paste0("3.",(momento) - round(momento, -2)),paste0("4.",(momento) - round(momento, -2)),
                    paste0("5.",(momento) - round(momento, -2)),paste0("6.",(momento) - round(momento, -2)),
                    paste0("7.",(momento) - round(momento, -2)),paste0("8.",(momento) - round(momento, -2)),
                    paste0("9.",(momento) - round(momento, -2)),paste0("10.",(momento) - round(momento, -2)),
                    paste0("11.",(momento) - round(momento, -2)),paste0("12.",(momento) - round(momento, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento,".xlsx"))
  
}


for (momento1 in w1){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento1,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$GA != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 21  #nhor changes 
  for (j in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+j),] #the add changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,6)]
    
    
    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento1-1) - round(momento1-1, -2)),paste0("2.",(momento1-1) - round(momento1-1, -2)),
                    paste0("3.",(momento1-1) - round(momento1-1, -2)),paste0("4.",(momento1-1) - round(momento1-1, -2)),
                    paste0("5.",(momento1-1) - round(momento1-1, -2)),paste0("6.",(momento1-1) - round(momento1-1, -2)),
                    paste0("7.",(momento1-1) - round(momento1-1, -2)),paste0("8.",(momento1-1) - round(momento1-1, -2)),
                    paste0("9.",(momento1-1) - round(momento1-1, -2)),paste0("10.",(momento1-1) - round(momento1-1, -2)),
                    paste0("11.",(momento1-1) - round(momento1-1, -2)),paste0("12.",(momento1-1) - round(momento1-1, -2)),
                    paste0("1.",(momento1) - round(momento1, -2)),paste0("2.",(momento1) - round(momento1, -2)),
                    paste0("3.",(momento1) - round(momento1, -2)),paste0("4.",(momento1) - round(momento1, -2)),
                    paste0("5.",(momento1) - round(momento1, -2)),paste0("6.",(momento1) - round(momento1, -2)),
                    paste0("7.",(momento1) - round(momento1, -2)),paste0("8.",(momento1) - round(momento1, -2)),
                    paste0("9.",(momento1) - round(momento1, -2)),paste0("10.",(momento1) - round(momento1, -2)),
                    paste0("11.",(momento1) - round(momento1, -2)),paste0("12.",(momento1) - round(momento1, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento1,"conGA",".xlsx"))
  
}



w <- c(1009,1010,1011,1012,1013,1014,1015,1016,1017,1018,1019,1020,1021)
w1 <- c(1017,1018,1019,1020,1021)


for (momento in w){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$H3 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,3,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 22   #changes
  for (i in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+i),]  #addition changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,3,6)]
    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento-1) - round(momento-1, -2)),paste0("2.",(momento-1) - round(momento-1, -2)),
                    paste0("3.",(momento-1) - round(momento-1, -2)),paste0("4.",(momento-1) - round(momento-1, -2)),
                    paste0("5.",(momento-1) - round(momento-1, -2)),paste0("6.",(momento-1) - round(momento-1, -2)),
                    paste0("7.",(momento-1) - round(momento-1, -2)),paste0("8.",(momento-1) - round(momento-1, -2)),
                    paste0("9.",(momento-1) - round(momento-1, -2)),paste0("10.",(momento-1) - round(momento-1, -2)),
                    paste0("11.",(momento-1) - round(momento-1, -2)),paste0("12.",(momento-1) - round(momento-1, -2)),
                    paste0("1.",(momento) - round(momento, -2)),paste0("2.",(momento) - round(momento, -2)),
                    paste0("3.",(momento) - round(momento, -2)),paste0("4.",(momento) - round(momento, -2)),
                    paste0("5.",(momento) - round(momento, -2)),paste0("6.",(momento) - round(momento, -2)),
                    paste0("7.",(momento) - round(momento, -2)),paste0("8.",(momento) - round(momento, -2)),
                    paste0("9.",(momento) - round(momento, -2)),paste0("10.",(momento) - round(momento, -2)),
                    paste0("11.",(momento) - round(momento, -2)),paste0("12.",(momento) - round(momento, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento,".xlsx"))
  
}



for (momento1 in w1){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento1,".xlsx"),sheet=1,col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$GA != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,6)]
  df$Hom <- df$Hom*10000
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 22  #nhor changes 
  for (j in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+j),] #the add changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,6)]

    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento1-1) - round(momento1-1, -2)),paste0("2.",(momento1-1) - round(momento1-1, -2)),
                    paste0("3.",(momento1-1) - round(momento1-1, -2)),paste0("4.",(momento1-1) - round(momento1-1, -2)),
                    paste0("5.",(momento1-1) - round(momento1-1, -2)),paste0("6.",(momento1-1) - round(momento1-1, -2)),
                    paste0("7.",(momento1-1) - round(momento1-1, -2)),paste0("8.",(momento1-1) - round(momento1-1, -2)),
                    paste0("9.",(momento1-1) - round(momento1-1, -2)),paste0("10.",(momento1-1) - round(momento1-1, -2)),
                    paste0("11.",(momento1-1) - round(momento1-1, -2)),paste0("12.",(momento1-1) - round(momento1-1, -2)),
                    paste0("1.",(momento1) - round(momento1, -2)),paste0("2.",(momento1) - round(momento1, -2)),
                    paste0("3.",(momento1) - round(momento1, -2)),paste0("4.",(momento1) - round(momento1, -2)),
                    paste0("5.",(momento1) - round(momento1, -2)),paste0("6.",(momento1) - round(momento1, -2)),
                    paste0("7.",(momento1) - round(momento1, -2)),paste0("8.",(momento1) - round(momento1, -2)),
                    paste0("9.",(momento1) - round(momento1, -2)),paste0("10.",(momento1) - round(momento1, -2)),
                    paste0("11.",(momento1) - round(momento1, -2)),paste0("12.",(momento1) - round(momento1, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento1,"conGA",".xlsx"))
  
}




w <- c(1109,1110,1111,1112,1113,1114,1115,1116,1117,1118,1119,1120,1121)
w1 <- c(1117,1118,1119,1120,1121)


for (momento in w){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet="Sheet1",col_names = TRUE) 
  
  data0 <- data0[,c("...1","Quarterly","Monthly HomFi_PopEst_sa","Homicide_Guns_Archive_sa", "MO_Homic&(G_or_F)" ,"Ciudades", "GT_Homicides_sa_det","BCs")]
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$H3 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,3,6)]
  
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 23   #changes
  for (i in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+i),]  #addition changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,3,6)]
    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento-1) - round(momento-1, -2)),paste0("2.",(momento-1) - round(momento-1, -2)),
                    paste0("3.",(momento-1) - round(momento-1, -2)),paste0("4.",(momento-1) - round(momento-1, -2)),
                    paste0("5.",(momento-1) - round(momento-1, -2)),paste0("6.",(momento-1) - round(momento-1, -2)),
                    paste0("7.",(momento-1) - round(momento-1, -2)),paste0("8.",(momento-1) - round(momento-1, -2)),
                    paste0("9.",(momento-1) - round(momento-1, -2)),paste0("10.",(momento-1) - round(momento-1, -2)),
                    paste0("11.",(momento-1) - round(momento-1, -2)),paste0("12.",(momento-1) - round(momento-1, -2)),
                    paste0("1.",(momento) - round(momento, -2)),paste0("2.",(momento) - round(momento, -2)),
                    paste0("3.",(momento) - round(momento, -2)),paste0("4.",(momento) - round(momento, -2)),
                    paste0("5.",(momento) - round(momento, -2)),paste0("6.",(momento) - round(momento, -2)),
                    paste0("7.",(momento) - round(momento, -2)),paste0("8.",(momento) - round(momento, -2)),
                    paste0("9.",(momento) - round(momento, -2)),paste0("10.",(momento) - round(momento, -2)),
                    paste0("11.",(momento) - round(momento, -2)),paste0("12.",(momento) - round(momento, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento,".xlsx"))
  
}



for (momento1 in w1){
  
  data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento1,".xlsx"),sheet="Sheet1",col_names = TRUE) 
  
  data0 <- data0[,c("...1","Quarterly","Monthly HomFi_PopEst_sa","Homicide_Guns_Archive_sa", "MO_Homic&(G_or_F)" ,"Ciudades", "GT_Homicides_sa_det","BCs")]
  colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
  data <- data0[min(which(data0$GA != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,2:8])
  
  colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
  #df <-df[,-3]
  df <-df[,-c(1,6)]
  
  VARselect(df, lag.max = 3,type='const')
  
  vartsDyn<- lineVar(df, lag = 3)
  
  nhor <- 23  #nhor changes 
  for (j in 1:nhor){
    
    df_pred <- tail(df,3)
    
    fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
    df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+j),] #the add changes
    df_new$Hom <- fore_v2[1]
    df_new <- data.frame(df_new[,2:8])
    df_new <-df_new[,-c(1,6)]
    
    
    
    df <- rbind(df,df_new)
    
  }
  
  
  var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
  
  fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
  
  results_date <- c(paste0("1.",(momento1-1) - round(momento1-1, -2)),paste0("2.",(momento1-1) - round(momento1-1, -2)),
                    paste0("3.",(momento1-1) - round(momento1-1, -2)),paste0("4.",(momento1-1) - round(momento1-1, -2)),
                    paste0("5.",(momento1-1) - round(momento1-1, -2)),paste0("6.",(momento1-1) - round(momento1-1, -2)),
                    paste0("7.",(momento1-1) - round(momento1-1, -2)),paste0("8.",(momento1-1) - round(momento1-1, -2)),
                    paste0("9.",(momento1-1) - round(momento1-1, -2)),paste0("10.",(momento1-1) - round(momento1-1, -2)),
                    paste0("11.",(momento1-1) - round(momento1-1, -2)),paste0("12.",(momento1-1) - round(momento1-1, -2)),
                    paste0("1.",(momento1) - round(momento1, -2)),paste0("2.",(momento1) - round(momento1, -2)),
                    paste0("3.",(momento1) - round(momento1, -2)),paste0("4.",(momento1) - round(momento1, -2)),
                    paste0("5.",(momento1) - round(momento1, -2)),paste0("6.",(momento1) - round(momento1, -2)),
                    paste0("7.",(momento1) - round(momento1, -2)),paste0("8.",(momento1) - round(momento1, -2)),
                    paste0("9.",(momento1) - round(momento1, -2)),paste0("10.",(momento1) - round(momento1, -2)),
                    paste0("11.",(momento1) - round(momento1, -2)),paste0("12.",(momento1) - round(momento1, -2)))
  
  library(xlsx)
  write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento1,"conGA",".xlsx"))
  
}

