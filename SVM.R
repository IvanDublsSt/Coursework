library(data.table)
library(openxlsx)
library("readxl")
library(stringi)
library(stringr)
library(ggplot2)
library(htmlwidgets)
library(plotly)
library(hrbrthemes)
library(dplyr)
library(patchwork) 
library(tidyr)
library(foreach)
library(doParallel)
library(parallel)
setwd("F:/Иван/Data")
Sys.setenv(LANG = "en")







znorm_vector <- function(vector){
  vecmean <- mean(vector, na.rm = T)
  vecstdev <- sd(vector, na.rm = T)
  normalized_vector <- (vector-vecmean)/vecstdev
  return(normalized_vector)
}

znorm_table <- function(dtable){
  charnames <- colnames(dtable)[sapply(dtable, is.character)]
  numericnames <- colnames(dtable)[!sapply(dtable, is.character)]
  dtable_numeric <- dtable[, ..numericnames]
  dtable_character <- dtable[, ..charnames]
  dtable_normalized <- list()
  m<-1
  for(i in dtable_numeric){
    dtable_normalized[[m]] <-znorm_vector(i)
    m <- m+1
  }
  dtable_normalized <- do.call(cbind, dtable_normalized)
  dtable_normalized <- as.data.table(dtable_normalized)
  colnames(dtable_normalized) <- colnames(dtable_numeric)
  dtable_final <- cbind(dtable_character, dtable_normalized)
  return(dtable_final)
}

library(e1071)

modelpred <- predict(modelsvm, smpl_test)
modelsvm$performances
mean((smpl_test[!is.na(NAs_pnl)]$YTM_ind_main -  modelpred)^2)


CreateAverage <- function(Table){
  smpl_dt_norm_av_strings <- Table[, .SD[1],.SDcols = c("Currency", "Exch_name", "Indicative_yield_type"),  by = c("ISIN", "Year", "Month")] 
  smpl_dt_norm_av <- Table[, lapply(.SD, function(x){mean(x, na.rm = T)}), by = c("ISIN", "Year", "Month"), .SDcols = !c("Currency", "Exch_name", "Indicative_yield_type")]
  smpl_dt_norm_av <- cbind(smpl_dt_norm_av, smpl_dt_norm_av_strings)
  return(smpl_dt_norm_av)
}

CreateTestTrain <- function(Table, Share, Variable){
  listofvars <- c("G_spread", "G_spread_interpolated","YTM_ind_main", "ISIN", "Date_trading")
  listofvars <- listofvars[listofvars!=Variable]
  Table[, ..listofvars := NULL]
  lng <- length(Table$Year)
  nums <- sample(1:lng, Share*lng, replace = F)
  smpl_train <- smpl_dt_norm[nums]
  smpl_test <- smpl_dt_norm[-nums]
  return(list(smpl_train, smpl_test))
}

CleanData <- function(Table, Variable, Average = F){
  Table <- Table[State_bank == 0]
  Table[, c("LaggedYTM","LaggedYTM2", "LaggedYTM3") := .(shift(get(Variable), 1L, fill = NA, type = "lag"),
                                                     shift(get(Variable), 2L, fill = NA, type = "lag"),
                                                     shift(get(Variable), 3L, fill = NA, type = "lag"))]
  Table[, "Indicative_yield_type" := as.factor(Indicative_yield_type)]
  Table[, "Currency" := as.factor(Currency)]
  Table[, "Exch_name" := as.factor(Exch_name)]
  Table[is.na(Coupon), "Coupon" := 0]
  Table[is.na(Days_to_call), "Days_to_call" := 0]
  Table[is.na(Moscow), "Moscow" := 0]
  Table[is.na(LaggedYTM), "LaggedYTM" := mean(LaggedYTM, na.rm = T), by = c("ISIN", "Month", "Year")]
  Table[is.na(LaggedYTM2), "LaggedYTM2" := mean(LaggedYTM2, na.rm = T), by = c("ISIN", "Month", "Year")]
  Table[is.na(LaggedYTM3), "LaggedYTM3" := mean(LaggedYTM3, na.rm = T), by = c("ISIN", "Month", "Year")]
  Table <- Table[!is.na(NAs_pnl)]
  Table <- Table[!is.na(LaggedYTM)]
  listofvars <- c("G_spread", "G_spread_interpolated","YTM_ind_main",  "Date_trading", "State_bank", "REGN")
  listofvars <- listofvars[listofvars!=Variable]
  Table[, (listofvars) := NULL]
  if (Average == F){
    Table[, "ISIN" := NULL]
  }
  if (Average == T){
    smpl_dt_norm_av_strings <- Table[, .SD[1],.SDcols = c("Currency", "Exch_name", "Indicative_yield_type"),  by = c("ISIN", "Year", "Month")] 
    smpl_dt_norm_av_strings <- smpl_dt_norm_av_strings[, (c("ISIN", "Year", "Month")) := NULL]
    smpl_dt_norm_av <- Table[, lapply(.SD, function(x){mean(x, na.rm = T)}), by = c("ISIN", "Year", "Month"), .SDcols = !c("Currency", "Exch_name", "Indicative_yield_type")]
    Table <- cbind(smpl_dt_norm_av, smpl_dt_norm_av_strings)
    Table[, "ISIN" := NULL]
  }
  Table <- as.data.table(Table[, lapply(.SD, function(x){ifelse((is.infinite(x)|is.nan(x)), 0, x)})])
  return(Table)
}

EstimateModelLong <- function(Table, TableName, Variable,  Kernel = NULL){
  if (is.null(Kernel)){
    modelsvm <- tune.svm(get(Variable)~.,data = Table, gamma = 10^(-6:-3), cost = c(0.1,1,10^(1:3)))
  }
  else{
    modelsvm <- tune.svm(get(Variable)~.,data = Table, gamma = 10^(-6:-3), cost = c(0.1,1,10^(1:3)), kernel = Kernel)
  }
  bestGamma <- modelsvm$best.parameters[[1]]
  bestC <- modelsvm$best.parameters[[2]]
  bestPerformance <- modelsvm$best.performance
  ParameterString <- paste(TableName, Kernel, as.character(bestGamma), as.character(bestC))
  results <- list(Say = ParameterString,
                  Res = bestPerformance,
                  ResNorm = (bestPerformance^(1/2))/mean(Table[, get(Variable)][[1]]),
                  Table = modelsvm$performances)
  return(results)
}

EstimateModelShort <- function(Table, TableName, Variable,  Kernel = NULL, bestGamma = NULL, bestC=NULL){

  
  if (is.null(Kernel)){
    if (is.null(bestGamma)){
      modelsvm <- svm(get(Variable)~.,data = Table, cross = 5)
      }
    else{
      modelsvm <- svm(get(Variable)~.,data = Table, gamma = bestGamma, cost = bestC, cross = 5)
      }
  }
  else{
    if (is.null(bestGamma)){
      modelsvm <- svm(get(Variable)~.,data = Table, kernel = Kernel, cross = 5)}
    else{
      modelsvm <- svm(get(Variable)~.,data = Table, gamma = bestGamma, cost = bestC, kernel = Kernel, cross = 5)
    }
  }

  bestPerformance <- modelsvm$tot.MSE
  ParameterString <- paste(TableName, Kernel, as.character(bestGamma), as.character(bestC))
  results <- list(Say = ParameterString,
                  Res = bestPerformance,
                  ResNorm = (bestPerformance^(1/2))/mean(Table[, get(Variable)][[1]], na.rm = T),
                  ResInt = (bestPerformance^(1/2))/sd(Table[, get(Variable)][[1]], na.rm = T),
                  Table = modelsvm$performances)
  
}
modelsvm <- svm(get("G_spread_interpolated")~.,data = CleanData(smpl_dt_norm, "G_spread_interpolated", Average = F), cross = 10, gamma = 0.001, cost = 100)

modelsvm$tot.MSE

#fast way average


start <- Sys.time()
smpl_dt <- fread("F:/Иван/Data/Tables/Cb1_I_.csv")
smpl_dt_norm <- cbind(znorm_table(smpl_dt[, !c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")]), smpl_dt[, c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")])
dt <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = F)
start <- Sys.time()
results <- svm(get("G_spread_interpolated")~.,data = dt, gamma = 0.0001, cost = 100)
#results <- EstimateModelLong(dt, "Aa1_V_.csv", "G_spread_interpolated", Kernel = "radial")
Sys.time() - start
mean(((predict(results, dt) - dt$G_spread_interpolated)^(2)))
#loop 
library(foreach)
library(doParallel)
library(parallel)

alltables_nosepyears <- list.files("F:/Иван/Data/Tables/")
alltables_nosepyears <-alltables_nosepyears[!str_detect(alltables_nosepyears, "20")]
alltables_nosepyears <-alltables_nosepyears[!str_detect(alltables_nosepyears, "a")]
alltables_nosepyears <-alltables_nosepyears[!str_detect(alltables_nosepyears, "c1")]
alltables_nosepyears <-alltables_nosepyears[!str_detect(alltables_nosepyears, "_II_")]
alltables_nosepyears <-alltables_nosepyears[!str_detect(alltables_nosepyears, "_IV_")]


cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)


#loop for SVM model with averaged values, all tables except the sep years, default kernel, YTM_ind_main

defaultkernellistYTM <- foreach(i = alltables_nosepyears, .packages = c("data.table", "stringr", "e1071"))%dopar%{
  try({smpl_dt <- fread(paste("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/Tables/", i, sep = ""))
  smpl_dt_norm <- cbind(znorm_table(smpl_dt[, !c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")]), smpl_dt[, c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")])
  dt <- CleanData(smpl_dt_norm, "YTM_ind_main", Average = T)
  results <- EstimateModelLong(dt, i, "YTM_ind_main")
  results})
}

saveRDS(defaultkernellistYTM, file = "my_data1.rds")
#loop for SVM model with averaged values, all tables except the sep years, default kernel, G_spread_interpolated


alltables_savethebees <- alltables_nosepyears[str_detect(alltables_noyears, "B")]
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

start <- Sys.time()
defaultkernellistGspreadinter<- foreach(i = alltables_nosepyears, .packages = c("data.table", "stringr", "e1071"))%dopar%{
  try({
  saveRDS("check", str_replace(i, "\\.csv", "\\.rds"))
  smpl_dt <- fread(paste("F:/Иван/Data/Tables/", i, sep = ""))
  smpl_dt_norm <- cbind(znorm_table(smpl_dt[, !c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")]), smpl_dt[, c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")])
  dt <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = T)
  results <- EstimateModelLong(dt, i, "G_spread_interpolated")
  saveRDS(results, file = paste("F:/Иван/Data/SVM_raw/defaultkernellistGspreadinter", str_remove(i, "\\.csv"), ".rds"))
  results})
}
saveRDS(defaultkernellistGspreadinter, file = "F:/Иван/Data/SVM_raw/defaultkernellistGspreadinter.rds")
print(Sys.time() - start)
stopCluster(cl)


#loop for SVM model with averaged values, all tables except the sep years, default kernel, G_spread_interpolated


cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

start <- Sys.time()
nonaverageddefaultkernellistGspreadinter<- foreach(i = alltables_savethebees, .packages = c("data.table", "stringr", "e1071"))%dopar%{
  try({
    saveRDS("check",paste("D:/Data_coursework/Data/Checks/", str_replace(i, "\\.csv", "\\.rds"), sep = ""))
    smpl_dt <- fread(paste("D:/Data_coursework/Data/Tables/", i, sep = ""))
    smpl_dt_norm <- cbind(znorm_table(smpl_dt[, !c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")]), smpl_dt[, c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")])
    dt <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = F)
    results <- EstimateModelShort(dt, i, "G_spread_interpolated", bestGamma = 0.0004, bestC = 100)
    saveRDS(results, file = paste("D:/Data_coursework/Data/SVM_raw/nonaverageddefaultkernellistGspreadinter", str_remove(i, "\\.csv"), ".rds"))
    results})
}
saveRDS(nonaverageddefaultkernellistGspreadinter, file = "D:/Data_coursework/Data/SVM_raw/nonaverageddefaultkernellistGspreadinter.rds")
print(Sys.time() - start)
stopCluster(cl)


#SAVE THE BEES loop for SVM model with averaged values, all tables except the sep years, default kernel, G_spread_interpolated


alltables_savethebees <- alltables_nosepyears[str_detect(alltables_nosepyears, "B")]
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

start <- Sys.time()
defaultkernellistGspreadinter<- foreach(i = alltables_savethebees, .packages = c("data.table", "stringr", "e1071"))%dopar%{
  try({
    saveRDS("check",paste("D:/Data_coursework/Data/Checks/", str_replace(i, "\\.csv", "\\.rds"), sep = ""))
    smpl_dt <- fread(paste("D:/Data_coursework/Data/Tables/", i, sep = ""))
    smpl_dt_norm <- cbind(znorm_table(smpl_dt[, !c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")]), smpl_dt[, c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")])
    dt <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = T)
    results <- EstimateModelLong(dt, i, "G_spread_interpolated")
    saveRDS(results, file = paste("D:/Data_coursework/Data/SVM_raw/defaultkernellistGspreadinter", str_remove(i, "\\.csv"), ".rds"))
    results})
}
saveRDS(defaultkernellistGspreadinter, file = "D:/Data_coursework/Data/SVM_raw/defaultkernellistGspreadinter.rds")
print(Sys.time() - start)
stopCluster(cl)


locallist <- list()
m<-1
for (i in list.files("F:/Иван/Data/SVM_raw/")){
  f <- readRDS(paste("F:/Иван/Data/SVM_raw/", i, sep = ""))
  locallist[[m]] <-f$Res
  names(locallist)[m] <- f$Say
  m <- m+1
}
f <- as.data.table(do.call(rbind, locallist))
f[order(V1)]
f[, "Names" := names(locallist)]
f[order(V1)]
f[, "Share" := V1^(1/2)]

localli
st <- list()
m<-1
for (i in list.files("D:/Data_coursework/Data/SVM_raw/")){
  f <- readRDS(paste("D:/Data_coursework/Data/SVM_raw//", i, sep = ""))
  locallist[[m]] <-f$Res
  names(locallist)[m] <- f$Say
  m <- m+1
}
f <- as.data.table(do.call(rbind, locallist))
f[order(V1)]
f[, "Names" := names(locallist)]
f[order(V1)]
f[, "Share" := V1^(1/2)]
