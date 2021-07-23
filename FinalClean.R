
#функции и библиотеки
library(data.table)
library(openxlsx)
library(stringr)
library(e1071)

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
CleanData <- function(Table, Variable, Average = F, State = F){
  
  Table <- Table[State_bank == State*1]
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
smpl_dt_norm <- cbind(znorm_table(smpl_dt[, !c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")]), smpl_dt[, c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")])
dt <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = T)$Table
dt_isin <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = T)$ISIN
#Получение out-of-sample ошибок на оптимальной модели и очистка данных
dt[, "Iteration" := sample(1:20, .N, replace = T)]

modelsvm <- svm(get("G_spread_interpolated")~.-Iteration,data = dt[Iteration != 1], gamma = 0.0001, cost = 100, epsilon = 0.001)
a <- MakePrediction(modelsvm, dt[Iteration == 1], ISINs = dt_isin[dt$Iteration==1])
for (i in 2:20){
  start <- Sys.time()
  modelsvm <- svm(get("G_spread_interpolated")~.-Iteration,data = dt[Iteration != i], gamma = 0.0001, cost = 100, epsilon = 0.001)
  anew <- MakePrediction(modelsvm, dt[Iteration == i], ISINs = dt_isin[dt$Iteration==i])
  a <- rbind(a, anew)
  Sys.time() - start
}
a[, "Year" := sapply(Year, mapyear, years = a$Year)]
a[, "Month" := sapply(Month, mapmonth, months = a$Month)]
saveRDS(a, "ResidualsTableOutOfSampleCheck.rds")

#получаем лучшую модель, обученную на всех данных по частным банкам
dt[,"Iteration":=NULL]
modelsvm <- svm(get("G_spread_interpolated")~.,data = dt, gamma = 0.0001, cost = 100, epsilon = 0.001)
saveRDS(modelsvm, "BestModelCheck.rds")
#получение дифференциалов на этой модели
modelsvm <- readRDS("BestModelCheck.rds")
dt_state <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = T, State = T)$Table
dt_state_isin <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = T, State = T)$ISIN
dt_state[, "ISIN" := dt_state_isin]
##несколько манипуляций с форматом данных, без которых не обучалась модель
dt_state <- subset(dt_state, Currency %in% levels(dt$Currency))
dt_state$Currency <- droplevels(dt_state$Currency)
dt_state <- subset(dt_state,Exch_name %in% levels(dt$Exch_name))
dt_state$Exch_name <- droplevels(dt_state$Exch_name)
levels(dt_state$Exch_name) <- levels(dt$Exch_name)
levels(dt_state$Currency) <- levels(dt$Currency)
dt_state_isin <- dt_state$ISIN
##получаем прогноз для государственных банков на той же модели
m <- MakePrediction(modelsvm, dt_state, ISINs = dt_state_isin)
m[, "Year" := sapply(Year, mapyear, years = m$Year)]
m[, "Month" := sapply(Month, mapmonth, months = m$Month)]
m[, "Errors" := -Errors]
saveRDS(m, "DifferentialsTableCheck.rds")
mean(m$Errors)
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
