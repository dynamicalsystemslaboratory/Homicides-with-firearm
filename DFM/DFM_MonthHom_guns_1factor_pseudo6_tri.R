# Code to estimate DFM as in the paper
#"On the use of dynamic factor modeling to predict homicides
#with firearm in the United States"

# November, 2022

#############################################################
remove(list=ls())   # Remove global environment
cat("\f")           # Clear the screen
graphics.off()      # Close the current graphical device
set.seed(1)
library("readxl")
require("writexl")
library(ks)
library(matrixStats)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#############################################################
#   Adjust the following to control specification desired   #                           #
#############################################################
va<-1    # va is the vare of rnd number to fill unobseved data
vf<-1    # var of factor error 
n<-6    # total number of indicators 
qf<-2    # lag length of factor and idios of GDP 
q1<-2    # lag length of all idios. of monthly indicators 
pphi<-max(c(qf,q1)); # pphi is the max lag length 
nk<-pphi+1      # nk is the first observation for which the likelihood will be evaluated 
pnk<-(qf+1)+(q1+1)*n    # pnk is the dimension of the state space (B)                            
outp<-0         # outp=0 to skip outputs 
graphs<-1       # graphs=0 to skip graphs 
je<-1           # je=0 to skip hessian calculations 
Ktr<-2
ks<-0
hs<-0

#############################################################
#                      LOAD DATA                            #
#############################################################

w <- c(1207,0108,0208,0308,0408,0508,0608,0708,0808,0908,1008,1108,
       1208,0109,0209,0309,0409,0509,0609,0709,0809,0909,1009,1109,
       1209,0110,0210,0310,0410,0510,0610,0710,0810,0910,1010,1110,
      1210, 0111,0211,0311,0411,0511,0611,0711,0811,0911,1011,1111,
       1211,0112,0212,0312,0412,0512,0612,0712,0812,0912,1012,1112,
       1212,0113,0213,0313,0413,0513,0613,0713,0813,0913,0915,1013,1113,
       1213,0214,0314,0414,0514,0614,0714,0814,0914,1014,1114,
      1214,0115,0215,0315,0415,0515,0615,0715,0815,
      1015,1115,1215,0116,0216,0316,0416,0516,0616,0716,0816,0916,1016,1116,1216,
      1216,0117,0217,0317,0417,0517,0617,071,0718,0817,0917,1017,1117,
      0418,0518,0618,0818,0918,1018,1218,0119,0219,0319,
      0419,0519,0619,0719,0819,0919,1019,1119,
      1219,0120,0220,0320,0420,0520,0620,0720,0820,0920,1020,1120,
      1220,0121,0221,0321,0421,0521,0621,0721,0821,0921,1021,1121,
      1217,01180,02180,03180,1118) #100 iterations factr 1e12

conv <- c()

for (momento in w){

indica0<-read_xlsx(paste0("datos_Monthly_Homicides_",momento,".xlsx"),sheet=1,col_names = TRUE) 


fecha<-as.matrix(indica0[14:(nrow(indica0)),1])
indica<-as.matrix(indica0[14:(nrow(indica0)),2:10])
indica <- indica[,-c(6,7,8)]
gg1<-subset(indica[,2], indica[,2]!=99999)
me1<-mean2(gg1)
std1<-sd(gg1)
vname1<-c("Quarterly","Homicide_HomFi_PopEst_sa","Guns_Archive","MOHom","Ciudades","Bcs_lag7")



#############################################################
#                    SOME PROCEDURES                        #
#                          HERE                             #
#############################################################
standard<-function(datos){
  datos2<-datos
  for(j in 1:ncol(datos2)){
    dataj<-subset(datos2[,j], datos2[,j]!=99999)
    datajm<-mean2(dataj)
    datajst<-sd(dataj)
    for (i in 1:nrow(datos)){
      if (datos2[i,j] != 99999){
        datos2[i,j]=(datos[i,j]-datajm)/datajst}
    }
  }
  return(datos2)
}

relleno<-function(datos,va){
  for(j in 1:ncol(datos)){
    dataj<-datos[,j]
    nm<-sum(dataj==99999)
    index<-which(dataj==99999)
    dataj[index]<-rnorm(nm,mean=0,sd=va)
    datos[,j]<-dataj
  }
  return(datos)
}

relleno2<-function(datos,va){
  for(j in 1:ncol(datos)){
    dataj<-datos[,j]
    nm<-sum(dataj==99999)
    index<-which(dataj==99999)
    dataj[index]<-1
    datos[,j]<-dataj
  }
  return(datos)
}

trans<-function(vari2){
  cy2<-n+qf+n*q1        
  rv<-length(vari2)
  partrans<-c(vari2[1:cy2],(vari2[(cy2+1):rv]^2))
  return(partrans)
}

ofn<-function(th2,Yv){
  #Filter<-matrix(0,captst,pnk)
  th<-trans(th2)      
  rv<-length(th)
  RQHF<-matrices(th)
  R<-RQHF[[1]]
  Q<-RQHF[[2]]
  H<-RQHF[[3]]
  F<-RQHF[[4]]
  
  beta00<-matrix(0,pnk,1)
  P00<-diag(pnk)
  
  like<-matrix(0,captst,1)
  fun<-0
  for(it in 1:captst){           # kalman filter iterations
    indexit<-index[it,]
    Hit<-matrix(rep(indexit,each=ncol(H)),ncol=ncol(H),byrow=TRUE)*H  # Element-wise multiplication
    Rit<-diag(n)
    Rit<-diag(c(matrix(1,n,1)-indexit),n)
    
    beta10<-F%*%beta00                 # Prediction equations
    P10<-F%*%P00%*%t(F)+Q
    n10<-Yv[it,]-(Hit%*%beta10)        # forecast error 
    F10<-Hit%*%P10%*%t(Hit)+Rit
    
    dF<-det(F10)
    # if (dF<1e-5){
    #   print(dF)
    #   fun<-1000000
    #   break
    # }
    
    like[it]<--0.5*(log(2*pi*det(F10))+t(n10)%*%solve(F10,tol=1e-27)%*%n10)
    K<-P10%*%t(Hit)%*%solve(F10)       # Kalman gain      
    beta11<-beta10+K%*%n10             # Updating equations 
    #Filter[it,]<<-t(beta11)
    Filter[it,]<-t(beta11)
    P11<-P10-K%*%Hit%*%P10
    
    Filter2 <<-Filter
    
    #if (Ktr==2){
    #  Pmat[it,]<<-vech(P11[1:pq,1:pq])
    #}
    beta00<-beta11
    P00<-P11
    fun<-fun-like[it]
  }
  
  #Filter <<- Filter
  K2 <<- K
  Hit2 <<- Hit
  F2 <<- F
  beta112 <<-beta11
  #fun<--colSums(like)
  #print(fun)
  cat("\f")
  print(th)
  print(fun)
  return(fun)
}

matrices<-function(z){
  Rs<-(va^2)*matrix(1,n,1)
  Qs<-matrix(0,pnk,pnk)
  Qs[1,1]<-vf
  cy2<-n+qf+n*q1
  cy3<-qf+1
  for(j in 1:n){
    Qs[(cy3+(j-1)*(q1+1)+1),(cy3+(j-1)*(q1+1)+1)]<-z[(cy2+j)]
  }
  nece<-cbind(1,matrix(0,1,q1))
  Hs<-cbind(z[1:n],matrix(0,n,(qf)),kronecker(diag(n), nece))
  Hs[1,qf] <- z[1]; Hs[1,qf+1]<-z[1];
  
  cy2<-n;
  F1<-cbind(t(z[(n+1):(n+qf)]),matrix(0,1,(pnk-qf)))
  F2<-cbind(diag(qf-1),matrix(0,qf-1,pnk-qf+1))
  F2b<-cbind(0,diag(qf-1),matrix(0,qf-1,pnk-qf))
  cy2<-cy2+qf
  nece<-matrix(0,(q1+1)*n,(q1+1)*n)
  ma<-matrix(0,q1+1,q1+1)
  ma[(2:q1),(1:q1-1)]<-diag(q1-1)
  ma[(3:(q1+1)),(2:q1)]<-diag(q1-1)
  for(j in 1:n){
    ma[1,(1:q1)]<-z[(cy2+(j-1)*q1+1):(cy2+j*q1)]
    nece[((j-1)*(q1+1)+1):(j*(q1+1)),((j-1)*(q1+1)+1):(j*(q1+1))]<-ma
  }
  F3=cbind(matrix(0,(q1+1)*n,qf+1),nece)
  Fs=rbind(F1,F2,F2b,F3)  
  return(list(Rs,Qs,Hs,Fs))
}


#############################################################
#             Fill unobserved and standardize               #            
#############################################################
indica<-standard(indica)
indica2<-relleno2(indica,va)
y<-indica2
capt<-nrow(y)
captst<-capt-pphi            # captst is the effective sample size @
Yv<-y[nk:capt,]
mat1<-indica[nk:capt,]
mat<-(mat1!=99999)
index<-replace(mat,mat<0,0)

#############################################################
#                 Initial parameters' values                #
#############################################################

#6 variables_1221
startval <- c(0.0745,  0.511,  0.2198,  0.1774,  0.2223,  0.1086,  0.5672,
             0.411,  1.609, -0.865, 0.4373,  0.558,  0.215, -0.059 ,
             0.262,  0.056,  0.185,  0.2498,  0.59879224,  0.24639772,  0.15331963,
             0.20608082,  0.14663668,  0.72284051,  0.65885320,  0.50094384)

nth<-length(startval)

#############################################################
#         I will need the following matrices                #
#############################################################
Filter<-matrix(0,captst,pnk)  # Filtered inferences
pq<-qf+q1
pnk2<-(pq^2+pq)/2;
Pmat<-matrix(0,captst,pnk2)
  
#############################################################
#               Use numerical optimizer                     #
#############################################################
options=list(maxit = 100 
            #,reltol=1e-5
            #abstol=1e-8
            #,trace=TRUE
            #,ndeps=1e-3
            ,factr=1e12 #multiplies by 1e-15 to get tolerance
            #,pgtol=1e-10
            #trace=10
            )
#, control=options
#res <- optim(par=startval,fn=ofn,Yv,gr="BFGS",hessian = FALSE,control=options)

res <- optim(par=startval,fn=ofn,Yv,gr=NULL,method="L-BFGS-B",hessian = FALSE,control=options)

conv[which(w==momento)] <- res$convergence

theta<-res$par
th<-trans(theta)
th
gdpm <- res$par[2]*(Filter2[,1])+(Filter2[,((qf+1) + 4)])
stdgdp <- std1
megdp <- me1
gdpm <- gdpm*stdgdp + megdp
common1 <- (res$par[2]*(Filter2[,1]))*stdgdp
#common2 <- (res$par[1+n]*Filter2[,3])*sd(indica[3:length(indica[,1]),1])
idio <- (Filter2[,((qf+1) + 4)])*stdgdp
#idio <- Filter2[,(nf*qf + 1)]*sd(indica[3:length(indica[,1]),1])

#media <- mean(subset(indica[3:length(indica[,1]),1],indica[3:length(indica[,1]),1]!=99999))
media <- megdp
# plot(gdpm,ylab="value",type="l",ylim=c(-4e-06,4e-05))
# 
# lines(common1,type="l",col=2)
# #lines(common2,type="l",col=3)
# lines(idio,type="l",col=3)
# lines(megdp,type="l",col=4)
# legend("topleft", legend=c("value", "common1","idiosyncratic"),
#        col=c("black", "red","green"), lty=1:2, cex=0.8)
# 
# 
# #
# 
# 
# fit2 <- lm(common1 ~ gdpm)
# ajuste <- summary(fit2)
# ajuste$r.squared
# 
# 
# #############################################################
# #                   Standard deviations                     #
# #############################################################
#  library(rootSolve)
#  x <- res$par
#  if (je!=0){
#  options=list(fnscale=1
#              ,parscale=rep.int(1, nth)
#              ,ndeps=rep.int(1e-3, nth)
#  )
#  h<-optimHess(x,ofn,Yv,gr=NULL,control = options)
#  hi<-solve(h);
#  stdor<-diag(hi)^.5;
#  gr<-gradient(trans,x,centered = FALSE, pert = 1e-8)
#  Hfin<-gr%*%hi%*%t(gr)
#  std=abs(diag(Hfin))^.5
#  }
# 
# 
# #and the p-values
#  pval <- x/std
#  paste0("los pvalores son",(1-pnorm(pval[1:n]))*2)
# # round(pval,2)
# # round(theta,2)


results <- data.frame(fecha[3:capt],gdpm,common1,idio,rep(media,length(fecha[3:capt])))

#write_xlsx(results,"Results_MonthHom_guns_1factor_peque?o")
library(xlsx)
write.xlsx(results, file=paste0("Results_MonthHom_guns_1factor_7series",momento,".xlsx"), sheetName="Series")
write.xlsx(theta, file=paste0("Results_MonthHom_guns_1factor_7series",momento,".xlsx"), sheetName="Parameters", append=TRUE)

#plot(momento,res$convergence,xlim=c(momento-1,momento+1))

}

