# Homicides-with-firearm

This readme file will help you replicate the results in the paper titled "On the use of dynamic factor modeling to predict homicides with firearm in the United States"

1) The extracted time series and the same series after their respective seasonal adjustment and detrending, are located in the file "time_series_original_&_sa_det-xlsx". The latter can be also obtained with the "deseason.R" file.

2) A dataset as in December 2021 can be found in the file "data_Monthly_homicides_1221.xlsx".  The file to get the parameters for DFM, their standard deviations and p-values, together with the residuals analysis for the model with this whole sample can be found in the file "DFM_MonthHom_guns_1factor.R". Also the plot of the factor and reconstructed monthly homicides with firearm series can be obtained with the same code. 

The code is set so that an out of sample exercise can be performed when the datasets are provided. A second database as in november 2021 can be found in the file "data_Monthly_Homicides_1121.xlsx" so the output with these two datasets can be obtained. The folder "DFM" reproduce the out-of-sample exercise with all the pseudo real time dataframes. 

3) The files "Forecasts_OUT_back_tri.xlsx", "Forecasts_OUT_now_tri.xlsx", and "DataForPTTest_tri.xlsx" summarize the forecasts that are passed to get the HLN test and the PT test. Those test can be done with the HLN.R", which uses those forecasts.

4) A folder for each of the alternative models can be found, storing each one the corresponding code file for the estimation. 
