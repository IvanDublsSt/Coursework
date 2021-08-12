
#функции и библиотеки
library(data.table)
library(openxlsx)
library(stringr)
library(e1071)
library(FNN)
library(MLmetrics)
library(foreach)
library(doParallel)
install.packages("randomForest")
library(randomForest)

setwd("C:/Coursework/Data_coursework")
Sys.setenv(LANG = "en")

#возвращает случайно отнормированные годы обратно в исходный вид
mapyear <- function(year, years){
  years <- unique(years)
  yearsreal <- c(2015:2020)
  #print(years <= year)
  #print(length(years[years <= year]))
  return(yearsreal[length(years[years <= year])])
}

#возвращает случайно отнормированные месяцы обратно в исходный вид
mapmonth <- function(month, months){
  months <- unique(months)
  monthsreal <- c(1:12)
  #print(years <= year)
  #print(length(years[years <= year]))
  return(monthsreal[length(months[months <= month])])
}

#возвращает случайно отнормированные регистрационные номера обратно в исходный вид
mapregn <- function(month, months){
  months <- unique(months)
  monthsreal <- c(1:5000)
  #print(years <= year)
  #print(length(years[years <= year]))
  return(monthsreal[length(months[months <= month])])
}
#нормирует один вектор
znorm_vector <- function(vector){
  vecmean <- mean(vector, na.rm = T)
  vecstdev <- sd(vector, na.rm = T)
  normalized_vector <- (vector-vecmean)/vecstdev
  return(normalized_vector)
}
#нормирует таблицу, не трогая не-числовые столбцы
znorm_table <- function(dtable){
  charnames <- colnames(dtable)[!sapply(dtable, is.numeric)]
  numericnames <- colnames(dtable)[sapply(dtable, is.numeric)]
  dtable_numeric <- dtable[, ..numericnames]
  dtable_character <- dtable[, ..charnames]
  dtable_normalized <- as.data.table(apply(dtable_numeric, 2, znorm_vector))
  dtable_final <- cbind(dtable_character, dtable_normalized)
  return(dtable_final)
}
#вычисляет геометрическое среднее
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
#Чистит данные: превращает качественные переменные в факторы, отсутствующие в нули,
#добавляет лаги зависимой переменной, если Average = T - усредняет значения зависимой переменной,
#если State = T - делает это все для государственных банков, иначе - только для частных
g <- data.table("x" = c(1,2,3,4,5,6), "y" = c("a", "a", "a", "b", "b", "b"))
g[, shift(x,1L, fill=NA, type = "lag"), by = y]
CleanData <- function(Table, Variable, Average = F, State = F){
  
  Table <- Table[State_bank == State*1]
  # Table[, c("LaggedYTM","LaggedYTM2", "LaggedYTM3") := .(shift(get(Variable), 1L, fill = NA, type = "lag"),
  #                                                        shift(get(Variable), 2L, fill = NA, type = "lag"),
  #                                                        shift(get(Variable), 3L, fill = NA, type = "lag"))]
  Table[, "Indicative_yield_type" := as.factor(Indicative_yield_type)]
  Table[, "Currency" := as.factor(Currency)]
  Table[, "Exch_name" := as.factor(Exch_name)]
  Table[is.na(Coupon), "Coupon" := 0]
  Table[is.na(Days_to_call), "Days_to_call" := 0]
  Table[is.na(Moscow), "Moscow" := 0]
  # Table[is.na(LaggedYTM), "LaggedYTM" := mean(LaggedYTM, na.rm = T), by = c("ISIN", "Month", "Year")]
  # Table[is.na(LaggedYTM2), "LaggedYTM2" := mean(LaggedYTM2, na.rm = T), by = c("ISIN", "Month", "Year")]
  # Table[is.na(LaggedYTM3), "LaggedYTM3" := mean(LaggedYTM3, na.rm = T), by = c("ISIN", "Month", "Year")]
  Table <- Table[!is.na(NAs_pnl)]
  # Table <- Table[!is.na(LaggedYTM)]
  listofvars <- c("G_spread", "G_spread_interpolated","YTM_ind_main",  "Date_trading", "State_bank")
  listofvars <- listofvars[listofvars!=Variable]
  Table[, "V1":= NULL]
  Table[, "V1.x":= NULL]
  Table[, "V1.y":= NULL]
  Table[, "num":= NULL]
  Table[, (listofvars) := NULL]
  if (Average == F){
    ISINs <- Table$ISIN
    Table[, "ISIN" := NULL]
  }
  if (Average == T){
    smpl_dt_norm_av_strings <- Table[, .SD[1],.SDcols = c("Currency", "Exch_name", "Indicative_yield_type"),  by = c("ISIN", "Year", "Month")] 
    smpl_dt_norm_av_strings <- smpl_dt_norm_av_strings[, (c("ISIN", "Year", "Month")) := NULL]
    smpl_dt_norm_av <- Table[, lapply(.SD, function(x){mean(x, na.rm = T)}), by = c("ISIN", "Year", "Month"), .SDcols = !c("Currency", "Exch_name", "Indicative_yield_type")]
    Table <- cbind(smpl_dt_norm_av, smpl_dt_norm_av_strings)
    Table[, c("LaggedYTM","LaggedYTM2", "LaggedYTM3") := .(shift(get(Variable), 1L, fill = NA, type = "lag"),
                                                           shift(get(Variable), 2L, fill = NA, type = "lag"),
                                                           shift(get(Variable), 3L, fill = NA, type = "lag")), by = "ISIN"]    
    Table[, c("MeanLaggedYTM","MeanLaggedYTM2", "MeanLaggedYTM3") := .(mean(LaggedYTM, na.rm = T),
                                                                    mean(LaggedYTM2, na.rm = T),
                                                                    mean(LaggedYTM3, na.rm = T)), by = c("Year")] 
    Table[is.na(LaggedYTM),"LaggedYTM":=MeanLaggedYTM]
    Table[is.na(LaggedYTM2),"LaggedYTM2":=MeanLaggedYTM2]
    Table[is.na(LaggedYTM3),"LaggedYTM3":=MeanLaggedYTM3]
    Table[, c("MeanLaggedYTM","MeanLaggedYTM2", "MeanLaggedYTM3") := .(NULL, NULL, NULL)]
    ISINs <- Table$ISIN
    Table[, "ISIN" := NULL]

        
  }
  if (Average == "Med"){
    smpl_dt_norm_av_strings <- Table[, .SD[1],.SDcols = c("Currency", "Exch_name", "Indicative_yield_type"),  by = c("ISIN", "Year", "Month")] 
    smpl_dt_norm_av_strings <- smpl_dt_norm_av_strings[, (c("ISIN", "Year", "Month")) := NULL]
    smpl_dt_norm_av <- Table[, lapply(.SD, function(x){median(as.double(x), na.rm = T)}), by = c("ISIN", "Year", "Month"), .SDcols = !c("Currency", "Exch_name", "Indicative_yield_type")]
    Table <- cbind(smpl_dt_norm_av, smpl_dt_norm_av_strings)
    ISINs <- Table$ISIN
    Table[, "ISIN" := NULL]
  }
  
  return(list("Table" = Table, "ISIN" = ISINs))
}
#аналогично предыдущей функции, но фильтрует по системной значимости
CleanDataSystem <- function(Table, Variable, Average = F, System = F){
  
  Table <- Table[State_bank == System*1]
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
  listofvars <- c("G_spread", "G_spread_interpolated","YTM_ind_main",  "Date_trading", "State_bank")
  listofvars <- listofvars[listofvars!=Variable]
  Table[, (listofvars) := NULL]
  if (Average == F){
    ISINs <- Table$ISIN
    Table[, "ISIN" := NULL]
  }
  if (Average == T){
    smpl_dt_norm_av_strings <- Table[, .SD[1],.SDcols = c("Currency", "Exch_name", "Indicative_yield_type"),  by = c("ISIN", "Year", "Month")] 
    smpl_dt_norm_av_strings <- smpl_dt_norm_av_strings[, (c("ISIN", "Year", "Month")) := NULL]
    smpl_dt_norm_av <- Table[, lapply(.SD, function(x){mean(x, na.rm = T)}), by = c("ISIN", "Year", "Month"), .SDcols = !c("Currency", "Exch_name", "Indicative_yield_type")]
    Table <- cbind(smpl_dt_norm_av, smpl_dt_norm_av_strings)
    ISINs <- Table$ISIN
    Table[, "ISIN" := NULL]
  }
  if (Average == "Med"){
    smpl_dt_norm_av_strings <- Table[, .SD[1],.SDcols = c("Currency", "Exch_name", "Indicative_yield_type"),  by = c("ISIN", "Year", "Month")] 
    smpl_dt_norm_av_strings <- smpl_dt_norm_av_strings[, (c("ISIN", "Year", "Month")) := NULL]
    smpl_dt_norm_av <- Table[, lapply(.SD, function(x){median(as.double(x), na.rm = T)}), by = c("ISIN", "Year", "Month"), .SDcols = !c("Currency", "Exch_name", "Indicative_yield_type")]
    Table <- cbind(smpl_dt_norm_av, smpl_dt_norm_av_strings)
    ISINs <- Table$ISIN
    Table[, "ISIN" := NULL]
  }
  
  return(list("Table" = Table, "ISIN" = ISINs))
}

#Обучает модель на разных значениях гиперпараметров, возвращает таблицу с результатами для лучшей модели
EstimateModelLong <- function(Table, TableName, Variable = "G_spread_interpolated",  Kernel = NULL){
  if (is.null(Kernel)){
    modelsvm <- tune.svm(get(Variable)~.,data = Table, gamma = 10^(-6:-3), cost = c(0.1,1,10^(1:3)), epsilon = c(0.01,0.1, 0.2))
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
                  ResNorm = (bestPerformance^(1/2))/mean(Table[, get(Variable)][[1]], na.rm = T),
                  ResInt = (bestPerformance^(1/2))/sd(Table[, get(Variable)][[1]], na.rm=T),
                  Table = modelsvm$performances)
  return(results)
}

#принимает модель и таблицу с данными, возвращает таблицу с прогнозами
MakePrediction <- function(Model, Table, Variable = "G_spread_interpolated", Average = T, ISINs = NULL){
  #Table <- CleanData(Table,Variable,Average,State = T)
  Table[, "ISIN" := NULL]
  preds <- predict(Model, Table)
  #print(Table[, get(Variable)])
  #print(preds)
  errors_vec <- Table[, get(Variable)] - preds
  Var_vec <- Table[, get(Variable)]
  return(data.table(
    "ISIN" = ISINs,
    "Year" = Table$Year,
    "Month" = Table$Month,
    "Predictions" = preds, 
    "Variable" = Var_vec,
    "Errors" = errors_vec
  ))
}

#Загрузка и очистка данных
smpl_dt <- fread("C:/Coursework/Data_coursework/Tables/Cb1_I_.csv")
smpl_dt <- smpl_dt[!str_detect(ISIN, "XS") ]
smpl_dt[,"LaggedYTM" := NULL]
smpl_dt[,"ReturnEquityMonth" := NULL]
smpl_dt[,"ReturnEquityDay" := NULL]
#smpl_dt[Indicative_yield_type!="Put/Call"]
#попробовать выкинуть значения к оферте !!!!


smpl_dt_norm <- cbind(znorm_table(smpl_dt[, !c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")]), smpl_dt[, c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")])
dt <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = T)$Table
dt_isin <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = T)$ISIN

#получаем ненормированные таблицы на всякий случай 
dt_not_norm <- CleanData(smpl_dt, "G_spread_interpolated", Average = T)$Table
dt_not_norm_isin <- CleanData(smpl_dt, "G_spread_interpolated", Average = T)$ISIN

#Получение out-of-sample ошибок на оптимальной модели и очистка данных

dt[, "Iteration" := sample(1:5, .N, replace = T)]
dt_no <- dt[(G_spread_interpolated <= quantile(dt$G_spread_interpolated, 0.9))&(G_spread_interpolated >= quantile(dt$G_spread_interpolated, 0.1))]
dt_isin_no <- dt_isin[(dt$G_spread_interpolated <= quantile(dt$G_spread_interpolated, 0.9))&(dt$G_spread_interpolated >= quantile(dt$G_spread_interpolated, 0.1))]

modelsvm <- svm(G_spread_interpolated~.-Iteration,data = dt_no[Iteration != 1], gamma = 10, cost = 0.01, epsilon = 0.005)
a <- MakePrediction(modelsvm, dt_no[Iteration == 1], ISINs = dt_isin_no[dt_no$Iteration==1])
# for (i in 2:5){
#   start <- Sys.time()
#   modelsvm <- svm(G_spread_interpolated~.-Iteration,data = dt_isin_no[Iteration != i], gamma = 0.001, cost = 100, epsilon = 0.01)
#   anew <- MakePrediction(modelsvm, dt_no[Iteration == i], ISINs = dt_isin_no[dt_no$Iteration==i])
#   a <- rbind(a, anew)
#   Sys.time() - start
# }

for (i in 2:5){
  start <- Sys.time()
  modelsvm <- svm(G_spread_interpolated~.-Iteration,data = dt_no[Iteration != i], gamma = 10, cost = 0.01, epsilon = 0.005)
  anew <- MakePrediction(modelsvm, dt_no[Iteration == i], ISINs = dt_isin_no[dt_no$Iteration == i])
  print(anew)
  a <- rbind(a, anew)
  print(Sys.time() - start)
}
a <- do.call(rbind, a)
a[, "Year" := sapply(Year, mapyear, years = a$Year)]
a[, "Month" := sapply(Month, mapmonth, months = a$Month)]
a[, "Variable" := Variable*100]
a[, "Errors" := Errors*100]
a[, "Predictions" := Predictions*100]
a[, MAE(Predictions, Variable)]


a[, "Year" := sapply(Year, mapyear, years = a$Year)]
a[, "Month" := sapply(Month, mapmonth, months = a$Month)]
#поправить порядки переменных
a[, "Variable" := Variable*100]
a[, "Errors" := Errors*100]
a[, "Predictions" := Predictions*100]

a[, mean(Variable)]
a[, mean(Errors)]
a[, sd(Errors)]
a[, MAE(Variable, Predictions)]
##### оптимальная модель
dt_no <- dt[(G_spread_interpolated <= quantile(dt$G_spread_interpolated, 0.9))&(G_spread_interpolated >= quantile(dt$G_spread_interpolated, 0.1))]
dt_isin_no <- dt_isin[(dt$G_spread_interpolated <= quantile(dt$G_spread_interpolated, 0.9))&(dt$G_spread_interpolated >= quantile(dt$G_spread_interpolated, 0.1))]

localsupera <- foreach(i = 1:5)%do%{
  modelsvm <- randomForest(G_spread_interpolated~.-Iteration,data = dt_no[Iteration != i],keep.forest = T, mtry = 30, ntree=1000)
  anew <- MakePrediction(modelsvm, dt_no[Iteration == i], ISINs = dt_isin_no[dt_no$Iteration==i])
  anew
}
localsupera <- do.call(rbind, localsupera)
localsupera[, "Year" := sapply(Year, mapyear, years = localsupera$Year)]
localsupera[, "Month" := sapply(Month, mapmonth, months = localsupera$Month)]
localsupera[, "Variable" := Variable*100]
localsupera[, "Errors" := Errors*100]
localsupera[, "Predictions" := Predictions*100]
localsupera[, MAE(Predictions, Variable)]


#Поиск утечки данных, убираем по одной 
varlist <- colnames(dt)
tableslist <- list()
varlist <- varlist[varlist != "G_spread_interpolated"]
varlist <- varlist[varlist != "Year"]
varlist <- varlist[varlist != "Month"]
varlist <- varlist[varlist != "Index"]

dt_copy <- copy(dt)
dt_copy[, "Index" := NULL]
for (m in 1:length(varlist)){
i <- varlist[m]
print(i)
dt <- copy(dt_copy)
dt[, (i) := NULL]
print(colnames(dt))
dt[, "Index" := 1:.N]

dt[, "Iteration" := sample(1:5, .N, replace = T)]

modelsvm <- svm(get("G_spread_interpolated")~.-Iteration,data = dt[Iteration != 1], gamma = 0.0001, cost = 100, epsilon = 0.001)
a <- MakePrediction(modelsvm, dt[Iteration == 1], ISINs = dt_isin[dt$Iteration==1])
for (i in 2:5){
  start <- Sys.time()
  modelsvm <- svm(get("G_spread_interpolated")~.-Iteration,data = dt[Iteration != i], gamma = 0.0001, cost = 100, epsilon = 0.001)
  anew <- MakePrediction(modelsvm, dt[Iteration == i], ISINs = dt_isin[dt$Iteration==i])
  a <- rbind(a, anew)
  Sys.time() - start
}
a[, "Year" := sapply(Year, mapyear, years = a$Year)]
a[, "Month" := sapply(Month, mapmonth, months = a$Month)]
#поправить порядки переменных
a[, "Variable" := Variable*100]
a[, "Errors" := Errors*100]
a[, "Predictions" := Predictions*100]

tableslist[[m]] <- a}
means <- do.call(cbind,lapply(tableslist, function(x){x[, mean(Errors)]}))
absmeans <- do.call(cbind,lapply(tableslist, function(x){x[, MAE(Predictions, Variable)]}))
sds <- do.call(cbind,lapply(tableslist, function(x){x[, sd(Errors)]}))
transpose(means)
typeof(means)
leakage_table <- data.table("Variable_dropped" = varlist, "Mean" = means[1,], "MAE" = absmeans[1,], "SD" = sds[1,])
write.xlsx(leakage_table, "LeakageSearchTable.xlsx")

#Поиск утечки данных, добавляем по одной 
varlist <- colnames(dt)
tableslist <- list()
varlist <- varlist[varlist != "G_spread_interpolated"]
varlist <- varlist[varlist != "Year"]
varlist <- varlist[varlist != "Month"]
varlist <- varlist[varlist != "Index"]

dt_copy <- copy(dt)
dt_g <- dt_copy$G_spread_interpolated
dt_copy[, "Index" := NULL]
for (m in 1:length(varlist)){
  dt <- dt_copy[,1:(3+m)]
  dt[, "G_spread_interpolated" := dt_g]

  print(colnames(dt))
  dt[, "Index" := 1:.N]
  
  dt[, "Iteration" := sample(1:5, .N, replace = T)]
  
  modelsvm <- svm(get("G_spread_interpolated")~.-Iteration,data = dt[Iteration != 1], gamma = 0.0001, cost = 100, epsilon = 0.001)
  a <- MakePrediction(modelsvm, dt[Iteration == 1], ISINs = dt_isin[dt$Iteration==1])
  for (i in 2:5){
    start <- Sys.time()
    modelsvm <- svm(get("G_spread_interpolated")~.-Iteration,data = dt[Iteration != i], gamma = 0.0001, cost = 100, epsilon = 0.001)
    anew <- MakePrediction(modelsvm, dt[Iteration == i], ISINs = dt_isin[dt$Iteration==i])
    a <- rbind(a, anew)
    Sys.time() - start
  }
  a[, "Year" := sapply(Year, mapyear, years = a$Year)]
  a[, "Month" := sapply(Month, mapmonth, months = a$Month)]
  #поправить порядки переменных
  a[, "Variable" := Variable*100]
  a[, "Errors" := Errors*100]
  a[, "Predictions" := Predictions*100]
  
  tableslist[[m]] <- a}
means <- do.call(cbind,lapply(tableslist, function(x){x[, mean(Errors)]}))
absmeans <- do.call(cbind,lapply(tableslist, function(x){x[, MAE(Predictions, Variable)]}))
sds <- do.call(cbind,lapply(tableslist, function(x){x[, sd(Errors)]}))
transpose(means)
typeof(means)
leakage_table <- data.table("Variable_dropped" = varlist, "Mean" = means[1,], "MAE" = absmeans[1,], "SD" = sds[1,])
write.xlsx(leakage_table, "LeakageSearchTableBackwards.xlsx")

#а что если вообще все переменные повыкидывать?
modelsvm <- svm(G_spread_interpolated~.,data = dt[Iteration!=4, c(1,29)], gamma = 0.0001, cost = 100, epsilon = 0.001)
preds <- MakePrediction(modelsvm, dt[Iteration==4, c(1,29)])
preds[, MAE(Variable*100, Predictions*100)]
preds[, mean(Errors*100)]
#а если вручную делать предикшны при этом
preds_manual <- predict(modelsvm, dt[Iteration==5])
errors <- dt[(Iteration==5)&(!is.na(LaggedYTM))]$G_spread_interpolated-preds_manual
MAE(preds_manual*100, dt[(Iteration==5)&(!is.na(LaggedYTM))]$G_spread_interpolated*100)
mean(errors*100)

#а что если попытаться линейной моделью?
linmod <- lm(G_spread_interpolated~.-Iteration, dt[Iteration!=4])
preds <- MakePrediction(linmod, dt[Iteration==4])
preds[, MAE(Variable*100, Predictions*100)]
preds[, mean(Errors*100)]
summary(linmod)

#симулированные данные? 
dt_sim <- data.table("y" = rnorm(10000, mean=mean(dt$G_spread_interpolated), sd = sd(dt$G_spread_interpolated)), "x" = rnorm(10000, mean=mean(dt$Year), sd = sd(dt$Year)), "Iteration" = sample(1:5,10000,replace = T))
plot(x=dt_sim$x, y = dt_sim$y)
modelsvm <- svm(y~.,data = dt_sim[Iteration!=4, c(1)], gamma = 0.0001, cost = 100, epsilon = 0.001)
preds_manual <- predict(modelsvm, dt_sim[Iteration==4, c(1)])
errors <- dt_sim[Iteration==4, c(1)]$y-preds_manual
MAE(preds_manual*100, dt_sim[Iteration==4, c(1,2)]$y*100)
mean(errors*100)

#другая функция
install.packages("libsvm")
library(libsvm)
libsvm::svm()

#подбор гиперпараметров SVM####
foreach (i=1:3) %do% {
  foreach(r=c("a", "b", "c"))%do%
            {paste(r, i)}
}
numCores <- detectCores()
registerDoParallel(numCores-2)
CrossValidateSVM <- function(gamma, cost, epsilon){
  

dt[, "Iteration" := sample(1:5, .N, replace = T)]
localsupera <- foreach(i = 1:5)%do%{
  modelsvm <- svm(G_spread_interpolated~.-Iteration,data = dt[Iteration != i], gamma = gamma, cost = cost, epsilon = epsilon)
  anew <- MakePrediction(modelsvm, dt[Iteration == i], ISINs = dt_isin[dt$Iteration==i])
  anew
}
localsupera <- do.call(rbind, localsupera)
# modelsvm <- svm(G_spread_interpolated~.-Iteration,data = dt[Iteration != 1], gamma = gamma, cost = cost, epsilon = epsilon)
# a <- MakePrediction(modelsvm, dt[Iteration == 1], ISINs = dt_isin[dt$Iteration==1])
# for (i in 2:10){
#   start <- Sys.time()
#   modelsvm <- svm(G_spread_interpolated~.-Iteration,data = dt[Iteration != i], gamma = gamma, cost = cost, epsilon = epsilon)
#   anew <- MakePrediction(modelsvm, dt[Iteration == i], ISINs = dt_isin[dt$Iteration==i])
#   a <- rbind(a, anew)
#   Sys.time() - start
# }
localsupera[, "Year" := sapply(Year, mapyear, years = localsupera$Year)]
localsupera[, "Month" := sapply(Month, mapmonth, months = localsupera$Month)]
#поправить порядки переменных
localsupera[, "Variable" := Variable*100]
localsupera[, "Errors" := Errors*100]
localsupera[, "Predictions" := Predictions*100]
return(localsupera)}

start<-Sys.time()
a<-CrossValidateSVM(gamma = 0.001, 100, 0.01)
Sys.time() - start
a[, mean(Errors)]
a[, mean(Variable)]
a[, MAE(Variable, Predictions)]

gammas <- c(0.0001, 0.001, 0.01)
costs <- c(10, 100, 1000)
epsilons <- c(0.01, 0.005, 0.001)

supertable <- foreach(cost=costs)%do%{
  foreach(gamma = gammas)%do%{
    foreach(epsilon = epsilons)%do%{
      list(CrossValidateSVM(gamma = gamma, epsilon = epsilon, cost=cost), paste(cost,gamma,epsilon))
    }
  }
}

saveRDS(supertable, "HyperparametersRawSVM.rds")
supertable2 <-unlist(supertable, recursive = F)
supertable3 <-unlist(supertable2, recursive = F)
supertable_final <- lapply(supertable3, function(x){x[[1]]})
names(supertable_final) <- lapply(supertable3, function(x){x[[2]]})
saveRDS(supertable_final, "HyperparametersSVM.rds")

SVM_MAEs <- lapply(supertable_final, function(x){x[, MAE(Predictions,Variable)]})
SVM_MAEs <- unlist(SVM_MAEs, recursive = F)
SVM_MAEs[order(SVM_MAEs)]
saveRDS(SVM_MAEs[order(SVM_MAEs)], "HyperparametersMAESVM.rds")
read
best_svm_results <- supertable_final$`10 0.01 0.005`
best_svm_results_no01 <- best_svm_results[(Variable <= quantile(best_svm_results$Variable, 0.9))&(Variable >= quantile(best_svm_results$Variable, 0.1))]
best_svm_results_no01[, MAE(Predictions,Variable)]


dt[, "Iteration" := sample(1:5, .N, replace = T)]
dt_no <- dt[(G_spread_interpolated <= quantile(dt$G_spread_interpolated, 0.9))&(G_spread_interpolated >= quantile(dt$G_spread_interpolated, 0.1))]
dt_isin_no <- dt_isin[(dt$G_spread_interpolated <= quantile(dt$G_spread_interpolated, 0.9))&(dt$G_spread_interpolated >= quantile(dt$G_spread_interpolated, 0.1))]

modelsvm <- svm(G_spread_interpolated~.-Iteration,data = dt_no[Iteration != 1], gamma = 10, cost = 0.01, epsilon = 0.005)
a <- MakePrediction(modelsvm, dt_no[Iteration == 1], ISINs = dt_isin_no[dt_no$Iteration == 1])
for (i in 2:5){
  start <- Sys.time()
  modelsvm <- svm(G_spread_interpolated~.-Iteration,data = dt_no[Iteration != i], gamma = 10, cost = 0.01, epsilon = 0.005)
  anew <- MakePrediction(modelsvm, dt_no[Iteration == i], ISINs = dt_isin_no[dt_no$Iteration == i])
  print(anew)
  a <- rbind(a, anew)
  print(Sys.time() - start)
}
a[, "Year" := sapply(Year, mapyear, years = a$Year)]
a[, "Month" := sapply(Month, mapmonth, months = a$Month)]
a[, "Variable" := Variable*100]
a[, "Errors" := Errors*100]
a[, "Predictions" := Predictions*100]
a[, MAE(Predictions, Variable)]
#подбор гиперпараметров RF####
foreach (i=1:3) %do% {
  foreach(r=c("a", "b", "c"))%do%
    {paste(r, i)}
}
numCores <- detectCores()
registerDoParallel(numCores-2)
dt[, "Iteration" := sample(1:5, .N, replace = T)]
a <- randomForest(G_spread_interpolated~.-Iteration,data = dt[Iteration != 1][1:500], xtest = dt[Iteration == 1,-c("Iteration", "G_spread_interpolated")][1:600], keep.forest = T)
mean(a$test$predicted)
MakePrediction(a, dt[Iteration == 1,][1:600])

CrossValidateRF <- function(numvariables){
  
  
  dt[, "Iteration" := sample(1:5, .N, replace = T)]
  localsupera <- foreach(i = 1:5)%do%{
    modelsvm <- randomForest(G_spread_interpolated~.-Iteration,data = dt[Iteration != i],keep.forest = T, mtry = numvariables, ntree=1000)
    anew <- MakePrediction(modelsvm, dt[Iteration == i], ISINs = dt_isin[dt$Iteration==i])
    anew
  }
  localsupera <- do.call(rbind, localsupera)
  # modelsvm <- svm(G_spread_interpolated~.-Iteration,data = dt[Iteration != 1], gamma = gamma, cost = cost, epsilon = epsilon)
  # a <- MakePrediction(modelsvm, dt[Iteration == 1], ISINs = dt_isin[dt$Iteration==1])
  # for (i in 2:10){
  #   start <- Sys.time()
  #   modelsvm <- svm(G_spread_interpolated~.-Iteration,data = dt[Iteration != i], gamma = gamma, cost = cost, epsilon = epsilon)
  #   anew <- MakePrediction(modelsvm, dt[Iteration == i], ISINs = dt_isin[dt$Iteration==i])
  #   a <- rbind(a, anew)
  #   Sys.time() - start
  # }
  localsupera[, "Year" := sapply(Year, mapyear, years = localsupera$Year)]
  localsupera[, "Month" := sapply(Month, mapmonth, months = localsupera$Month)]
  #поправить порядки переменных
  localsupera[, "Variable" := Variable*100]
  localsupera[, "Errors" := Errors*100]
  localsupera[, "Predictions" := Predictions*100]
  return(localsupera)}

start<-Sys.time()
a<-CrossValidateRF(numvariables = 10)
Sys.time() - start
a[, mean(Errors)]
a[, mean(Variable)]
a[, MAE(Variable, Predictions)]



supertableRF <- foreach(numvariables=c(5, 10, 15, 20, 30))%do%{
      list(CrossValidateRF(numvariables=numvariables), numvariables)
}
saveRDS(supertableRF, "HyperparametersRawSVM.rds")

supertableRF <- readRDS("HyperparametersRawSVM.rds")
supertableRF_final <- lapply(supertableRF, function(x){x[[1]]})
names(supertableRF_final) <- lapply(supertableRF, function(x){x[[2]]})
saveRDS(supertableRF_final, "HyperparametersRF.rds")

RF_MAEs <- lapply(supertableRF_final, function(x){x[, MAE(Predictions,Variable)]})
RF_MAEs <- unlist(RF_MAEs, recursive = F)
RF_MAEs[order(RF_MAEs)]
saveRDS(RF_MAEs[order(RF_MAEs)], "HyperparametersMAESVM.rds")


dt_no <- dt[(G_spread_interpolated <= quantile(dt$G_spread_interpolated, 0.9))&(G_spread_interpolated >= quantile(dt$G_spread_interpolated, 0.1))]
dt_isin_no <- dt_isin[(dt$G_spread_interpolated <= quantile(dt$G_spread_interpolated, 0.9))&(dt$G_spread_interpolated >= quantile(dt$G_spread_interpolated, 0.1))]

localsupera <- foreach(i = 1:5)%do%{
  modelsvm <- randomForest(G_spread_interpolated~.-Iteration,data = dt_no[Iteration != i],keep.forest = T, mtry = 30, ntree=1000)
  anew <- MakePrediction(modelsvm, dt_no[Iteration == i], ISINs = dt_isin_no[dt_no$Iteration==i])
  anew
}
localsupera <- do.call(rbind, localsupera)
localsupera[, "Year" := sapply(Year, mapyear, years = localsupera$Year)]
localsupera[, "Month" := sapply(Month, mapmonth, months = localsupera$Month)]
localsupera[, "Variable" := Variable*100]
localsupera[, "Errors" := Errors*100]
localsupera[, "Predictions" := Predictions*100]
localsupera[, MAE(Predictions, Variable)]

#knn
dt_for_knn <- copy(dt)
dt_for_knn[, "Currency" := NULL]
dt_for_knn[, "Exch_name" := NULL]
dt_for_knn[, "Indicative_yield_type" := NULL]
dt_for_knn[, "Index" := NULL]



dt_for_knn_test <- dt_for_knn[Iteration == 10]
dt_for_knn_train <- dt_for_knn[Iteration != 10]

dt_for_knn_train <- dt_for_knn_train[, -"Iteration"]
dt_for_knn_test <- dt_for_knn_test[, -"Iteration"]
dt_for_knn_test_g <- dt_for_knn_test$G_spread_interpolated
dt_for_knn_test <- dt_for_knn_test[, -"G_spread_interpolated"]
dt_for_knn_train_g <- dt_for_knn_train$G_spread_interpolated
dt_for_knn_train <- dt_for_knn_train[, -"G_spread_interpolated"]
dt_for_knn_train


knnmod <- knn.reg(train = dt_for_knn_train,test = dt_for_knn_test, y = dt_for_knn_train_g, k = 8 )
MAE(as.numeric(dt_for_knn_test_g),knnmod$pred)
typeof(knnmod$pred)
#приклеить таблицу с ненормированными использованными переменными 
dt_with_isin <- cbind(dt_not_norm, dt_not_norm_isin)
dt_with_isin[, "ISIN" := dt_not_norm_isin]
dt_with_isin[, "dt_not_norm_isin" := NULL]

dt_with_isin[, "Year" := sapply(Year, mapyear, years = dt_with_isin$Year)]
dt_with_isin[, "Month" := sapply(Month, mapmonth, months = dt_with_isin$Month)]

a_added <- merge(a, dt_with_isin, by = c("ISIN", "Month", "Year"))

#сохранить результат
saveRDS(localsupera, "ResidualsTableOutOfSampleCheck.rds")

#получаем лучшую модель, обученную на всех данных по частным банкам
dt[,"Iteration":=NULL]
modelsvm <- randomForest(G_spread_interpolated~.-Iteration,data = dt_no,keep.forest = T, mtry = 30, ntree=1000)
saveRDS(modelsvm, "BestModelCheck.rds")
#получение дифференциалов на этой модели
modelsvm <- readRDS("BestModelCheck.rds")
dt_state <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = T, State = T)$Table
dt_state[, "Iteration":=1]
dt_state_isin <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = T, State = T)$ISIN
dt_state_no <- dt_state[(G_spread_interpolated <= quantile(dt$G_spread_interpolated, 0.9))&(G_spread_interpolated >= quantile(dt$G_spread_interpolated, 0.1))]
dt_state_isin_no <- dt_state_isin[(dt_state$G_spread_interpolated <= quantile(dt$G_spread_interpolated, 0.9))&(dt_state$G_spread_interpolated >= quantile(dt$G_spread_interpolated, 0.1))]
dt_state_no[, "ISIN" := dt_state_isin_no]
##несколько манипуляций с форматом данных, без которых не обучалась модель
#проверить списки факторов !!!
dt_state_no <- subset(dt_state_no, Currency %in% levels(dt_no$Currency))
dt_state_no$Currency <- droplevels(dt_state_no$Currency)
dt_state_no <- subset(dt_state_no,Exch_name %in% levels(dt_no$Exch_name))
dt_state_no$Exch_name <- droplevels(dt_state_no$Exch_name)
levels(dt_state_no$Exch_name) <- levels(dt_no$Exch_name)
levels(dt_state_no$Currency) <- levels(dt_no$Currency)
dt_state_isin_no <- dt_state_no$ISIN
##получаем прогноз для государственных банков на той же модели
m <- MakePrediction(modelsvm, dt_state_no, ISINs = dt_state_isin_no)
m[, "Year" := sapply(Year, mapyear, years = m$Year)]
m[, "Month" := sapply(Month, mapmonth, months = m$Month)]
m[, "Errors" := -Errors]
m[, "Variable" := Variable*100]
m[, "Errors" := Errors*100]
m[, "Predictions" := Predictions*100]
m[, MAE(Predictions, Variable)]
saveRDS(m, "DifferentialsTableCheck.rds")
m[, mean(Errors)]
#Загрузка и очистка данных
library(moments)
m <- readRDS("DifferentialsTableCheck.rds")
a <- readRDS("ResidualsTableOutOfSampleCheck.rds")
m[, lapply(.SD, mean)]
a[, lapply(.SD, mean)]

m[, lapply(.SD, sd)]
a[, lapply(.SD, sd)]

skewness(m$Variable)
skewness(m$Predictions)
skewness(a$Variable)

m[, lapply(.SD, median)]
a[, lapply(.SD, median)]

m[, lapply(.SD, function(x){sum(x < 0)/sum(x!=10000000)})]
a[, lapply(.SD, function(x){sum(x < 0)/sum(x!=10000000)})]

quantiles_high <- quantile(m$Variable, probs = seq(1, 0.8, -0.01))
quantiles_low <- quantile(m$Variable, probs = seq(0, 0.2, 0.01))
means <- c()
for (i in 1:21){
  means <- c(means, m[(Variable <= quantiles_high[i])&(Variable >= quantiles_low[i]), mean(Variable)])
}
plot(seq(1, 0.6, -0.02),means, xlab = "share of observations remaining", ylab = "sample mean")

m[Year !=2020, lapply(.SD, mean)]
m[Year == 2020, lapply(.SD, mean)]

m[Year !=2020, lapply(.SD, sd)]
m[Year == 2020, lapply(.SD, sd)]

skewness(m[Year !=2020]$Variable)
skewness(m[Year !=2020]$Predictions)
skewness(m[Year ==2020]$Variable)
skewness(m[Year ==2020]$Predictions)

m[Year !=2020, lapply(.SD, median)]
m[Year ==2020, lapply(.SD, median)]

m[Year !=2020, lapply(.SD, function(x){sum(x < 0)/sum(x!=10000000)})]
m[Year ==2020, lapply(.SD, function(x){sum(x < 0)/sum(x!=10000000)})]
