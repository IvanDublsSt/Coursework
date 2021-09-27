library(data.table)
library(MASS)
library(rms)
library(plyr)
library(rlang)

setwd("C:/Coursework/Data_coursework")
Sys.setenv(LANG = "en")

#upload the table and make some higher-order aggregation of factors (rather arbitrary)
TheTable <- read.csv("BanksTableWithNormatives.csv")
View(TheTable)
TheTable <- as.data.table(TheTable)
TheTable[, "order" := as.factor(order)]
TheTable[, ':='(Ctb_net = Credits_to_banks-Credits_to_banks_minus,
                           Securities_net = Securities- Securities_minus,
                           Ctc_net = Corporate_credits - Corporate_credits_minus,
                           Ctp_net = Retail_credits - Retail_credits_minus,
                           Reserves_net = Reserves_for_delayed_loans - Reserves_for_delayed_loans_minus,
                           Mincap_net = Main_capital - Main_capital_minus,
                           Ctb_pnl_net = Credits_to_banks_pnl - Credits_to_banks_pnl_minus,
                           Ctp_pnl_net = Retail_credits_pnl-Retail_credits_pnl_minus,
                           Debt_pnl_net = Debt_pnl - Debt_pnl_minus,
                           SecuritiesIssued_net = Securities_issued - Securities_issued_minus,
                           Fee_net = Fee_income - Fee_expenditure + Fee_expenditure_minus,
                           Rfc = Realized_fc - Realized_fc_pnlold,
                           Drv_net = Derivatives_etc - Derivatives_etc_minus,
                           Otherexp_net = Other_expenditures - Other_expenditures_minus,
                           Otherinc_net = Other_income - Other_income_minus,
                           Ibt_net = Income_before_taxes - Income_before_taxes_minus,
                           Ret_net = Retained_earnings - Retained_earnings_minus)]
TheTable[, ':='(TotalAssets = Cash_and_equivalents + NOSTRO - NOSTRO_minus + Ctb_net + Securities_net+
                             Ctc_net + Ctp_net + Reserves_net + Other_assets-Other_assets_minus)]
TheTable[, ':='(BankCapital = Mincap_net + Ret_net + Retained_earnings_this_year - 
                             Retained_earnings_this_year_minus + Next_periods - Next_periods_minus)]
TheTable[, ':='(
  CS = BankCapital/TotalAssets,
  TotalDeposits = LORO + Interbank_loans + Corporate_time_deposits +
    Retail_time_deposits,
  e_f = (Corporate_time_deposits/BankCapital))]

TheTable[, ':='(
  NonF = (TotalDeposits-Ctb_net)/TotalDeposits,
  Pl5 = (Ctb_net-Interbank_loans)/TotalDeposits,
  T_R = (Reserves_for_delayed_loans- Reserves_for_delayed_loans_minus)/BankCapital)]



#separate a table, provided in "dataset" variable, into train and test parts
TestTrain <- function(dataset, TrainShare = 0.8){
  number_of_units <- length(dataset[[1]])
  train_length <- round(TrainShare*number_of_units)
  test_length <- round((1 - TrainShare)*number_of_units)-1
  sample_numbers <- sample(1:number_of_units, number_of_units)
  train_sample_numbers <- sample_numbers[1:train_length]
  test_sample_numbers <- sample_numbers[(train_length+1):number_of_units]
  train_sample <- dataset[train_sample_numbers]
  test_sample <- dataset[test_sample_numbers]
  return(list(train = train_sample,
              test = test_sample))
}

#generate a table with principal components from the one provided in "BanksWithRatingsTable_Train";
#if "CreatePredictionSet" = True and "BanksWithRatingsTable_Predict" is provided with a table of factors for
#the observations for which the variable to be predicted is not observed, then the function also outputs a table of
#principal components for those observations, based on the same loadings as those of the train set

#VarList contains a list of variables used 
VarList <- quote(list(order, Year, Month, Cash_and_equivalents, Ctb_net, Securities_net, Ctc_net, 
Ctp_net, Reserves_net, Mincap_net, Ctb_pnl_net, Ctp_pnl_net,
Debt_pnl_net, SecuritiesIssued_net, Fee_net, Rfc, Drv_net,
Otherexp_net, Otherinc_net, Interbank_loans,
Bonds, Equity, NetIncome, ROE, NetInterbank, InterbankShare,
Liabilities, GeneralLeverage, SecuritiesLeverage, TotalAssets,
BankCapital, CS, TotalDeposits, e_f, T_R, N1.0, N1.1, N1.2, N2, 
N3, N4, Ar1.0, Ar1.1, Ar1.2, Ar3.0, Ar2.0, Ar4.0, Arisk0, Kins, Kras, Kf, PR0, PR1, PR2))


GeneratePCTable <- function(BanksWithRatingsTable_Train, CreatePredictionSet = F, BanksWithRatingsTable_Predict = NULL, ListOfVariables = VarList){
  
  myvarsdataframe <- copy(BanksWithRatingsTable_Train)
  
  #select needed variables
  myvarsdataframe <- myvarsdataframe[, eval(ListOfVariables)]
  
  #remove NAs from selected columns; unfortunately, there is no algorithm implemented yet which would select these columns automatically
  myvarsdataframe <- myvarsdataframe[!is.na(Ar1.0)]
  
  #separate non-numeric columns
  myvarsdataframeorder <- myvarsdataframe$order
  myvarsdataframeyear <- myvarsdataframe$Year
  myvarsdataframemonth <- myvarsdataframe$Month
  
  #convert to numeric all the columns, except for the dependent variable and date
  myvarsdataframe <- (as.data.table(myvarsdataframe))[, lapply(.SD, as.numeric), .SDcols = setdiff(colnames(myvarsdataframe), c("order", "Year", "Month"))]
  
  #get a table of principal components for the train set
  PCTable <- prcomp(na.omit(myvarsdataframe[, -c("order", "Month", "Year")]), center = T, scale. = T)
  
  #(get PC loadings, which were generated for the training set)
  PC_loadings <- PCTable$rotation
  
  PCTable <- PCTable$x
  PCTable <- as.data.table(PCTable)
  PCTable[, ':=' ("order" = myvarsdataframeorder,
                  "Year" = myvarsdataframeyear,
                  "Month" = myvarsdataframemonth)]
  
  #the following part will be run if you choose to generate also the PC set, for which predictions are expected
  #which is generated by applying the factor loadings obtained for the training set to the values in the new one
  if (CreatePredictionSet == T){
    
    #repeat the procedure of data preparation for prediction set 
    myvarsdataframe_new <- copy(BanksWithRatingsTable_Predict)
    myvarsdataframe_new <- myvarsdataframe_new[, eval(ListOfVariables)]
    myvarsdataframe_new_order <- myvarsdataframe_new$order
    myvarsdataframe_new_year <- myvarsdataframe$Year
    myvarsdataframe_new_month <- myvarsdataframe$month
    myvarsdataframe_new <- (as.data.table(myvarsdataframe_new))[, lapply(.SD, as.numeric), .SDcols = setdiff(colnames(myvarsdataframe_new), c("order", "Year", "Month"))]
    
    #these removals are again rather empirical, unfortunately
    myvarsdataframe_new <-myvarsdataframe_new[!is.na(ROE)]
    myvarsdataframe_new <-myvarsdataframe_new[!is.infinite(ROE)]
    myvarsdataframe_new <- myvarsdataframe_new[!is.na(Ar1.0)]
    
    #remove the dependent variable, year and month from dataset
    myvarsdataframe_new[, ':=' ("order" = NULL,
                                "Year" = NULL,
                                "Month" = NULL)]
    
    #scale the data
    sclddata <- scale(as.matrix(myvarsdataframe_new[, -c("Year", "Month")]))
    
    #create the new PC table
    PC_new <- sclddata %*% PC_loadings
    PC_new <- as.data.table(PC_new)
    PC_new[, ":="(Year = myvarsdataframe_new_year,
                  Month = myvarsdataframe_new_month)]

    #return the results
    return(list("PCTable" = PCTable, "PCTablePredict" = PC_new, "orderPredict" = myvarsdataframe_new_order))
  }
  else {
    return(list("PCTable" = PCTable, "PCTablePredict" = NULL, "orderPredict" = NULL))
  }
}

#the PC table is restricted to N desired components + order/Year/Month variables
GenerateRestrictedPCTable <- function(PCTable, PCNumber, order = T, Year = T, Month = T){
  
  #generate a list of names of principal components columns to be taken, then add
  #(or not add) dependent variable, month and year to this list
  #depending on the user choice
  ColumnNames <- paste("PC", 1:PCNumber, sep = "")
  if (order==T){
    ColumnNames <- c(ColumnNames, "order")
  }
  if (Year==T){
    ColumnNames <- c(ColumnNames, "Year")
  }
  if (Month==T){
    ColumnNames <- c(ColumnNames, "Month")
  }
  #restrict the PC table to the columns enlisted in the previous step
  five_PC <- PCTable[,..ColumnNames]
  return(five_PC)
}

#вот эта функция существует по достаточно неудобной причине - функция polr на моем датасете не может самостоятельно найти
#starting values для оптимизации, а я так и не смог разобраться, какой длины должен быть вектор этих values,
#притом, что он явно зависит от количества переменных. Так что я просто перебираю векторы разной длины, пока один не сработает
#Благо, это занимает несколько секунд.
#Если возвращается NULL, то функция не смогла найти длину, которая не вызовет ошибку: поиск причины можно выполнить, 
#обозначив PrintErrors=T, тогда функция будет печатать все ошибки, которые получает
FindStartLength <- function(Table, method = c("logistic"), start = -0.1, to = 0.1, bycoef = 0.2, PrintErrors = F){
  l <- NULL
  for (i in 1:100){
    #пытаемся обучить модель с вектором "start", состоящим из n=1:100 точек
    #если НЕ получаем ошибку, то сохраняем n
    if (PrintErrors == T){
      tryCatch({
        polr(as.factor(order)~., data = Table, Hess = T,method=method, start = seq(from = start, to = to, by = bycoef/i))
        l <- i}, 
        error = function(c)(print(c)))
    }
    else{
      tryCatch({
        polr(as.factor(order)~., data = Table, Hess = T,method=method, start = seq(from = start, to = to, by = bycoef/i))
        l <- i}, 
        error = function(c){""})
    }}
  
  return(l)}

#Model validation. If "CrossValidate = T", "K"-fold validation is performed. Otherwize, the errors are obtained in-sample.

ValidateModel <- function(Table, K = 20, CrossValidate = T, method = c("logistic"), start = -0.1, to = 0.1, bycoef = 0.2){
  
  errors <- c()
  errors_primary_binary <- c()
  errors_plus_binary <- c()
  errors_minus_binary <- c()
  lengths_of_test_set <- c()
  
  if (is.null(FindStartLength(Table))){
    print("Unable to find length of starting values vector")
  }
  
  if (CrossValidate == T){
    for (i in 1:K){
      tt <- TestTrain(Table)
      test <- tt$test
      train <- tt$train
      model <- polr(as.factor(order)~., data = train, Hess = T,method=method, start = seq(from = start, to = to, by = bycoef/FindStartLength(Table)))
      newerror <- mean(abs(as.numeric(test$order) - as.numeric(predict(model, test[, -"order"]))))
      errors <- c(errors, newerror)
      
      test_predictions <- predict(model, test[, -"order"])
      newerror_primary_bin <- sum(as.numeric(test$order) == as.numeric(test_predictions))
      newerror_plus_bin <- sum(as.numeric(test$order) == as.numeric(test_predictions)+1)
      newerror_minus_bin <- sum(as.numeric(test$order) == as.numeric(test_predictions)-1)
      
      errors_primary_binary <- c(errors_primary_binary, newerror_primary_bin)
      errors_plus_binary <- c(errors_plus_binary, newerror_plus_bin)
      errors_minus_binary <- c(errors_minus_binary, newerror_minus_bin)
      
      lengths_of_test_set <- c(lengths_of_test_set, length(as.numeric(test$order)))
    }}
  
  else{
    
    model <- polr(as.factor(order)~., data = Table, Hess = T,method=method, start = seq(from = start, to = to, by = bycoef/FindStartLength(Table)))
    test_predictions <- predict(model, test[, -"order"])
    
    newerror <- mean(abs(as.numeric(Table$order) - as.numeric(test_predictions)))
    errors <- c(errors, newerror)
    newerror_primary_bin <- sum(as.numeric(Table$order) == as.numeric(test_predictions))
    newerror_plus_bin <- sum(as.numeric(Table$order) == as.numeric(test_predictions)+1)
    newerror_minus_bin <- sum(as.numeric(Table$order) == as.numeric(test_predictions)-1)
    
    errors_primary_binary <- c(errors_primary_binary, newerror_primary_bin)
    errors_plus_binary <- c(errors_plus_binary, newerror_plus_bin)
    errors_minus_binary <- c(errors_minus_binary, newerror_minus_bin)
    
    lengths_of_test_set <- c(lengths_of_test_set, length(as.numeric(test$order)))
  }
  
  return(data.table("Iteration" = 1:length(errors),"AverageErrors" = errors, "NumberOfTrueGuesses" = errors_primary_binary/lengths_of_test_set, 
                    "NumberOfTrueGuessesPlusOne" = errors_plus_binary/lengths_of_test_set, "NumberOfTrueGuessesMinusOne" = errors_minus_binary/lengths_of_test_set, 
                    "NumberOfApproximateGuesses" = errors_minus_binary/lengths_of_test_set + errors_plus_binary/lengths_of_test_set + errors_primary_binary/lengths_of_test_set))
}

#Give this function training "Table", "TableNew" with the observations for which new predictions are desired,
#and it will return the model applied, vector of predicted ratings, and "TableNew" with appended predictions
GetPrediction <- function(Table, TableNew, method = c("logistic"), start = -0.1, to = 0.1, bycoef = 0.2){
  best_model <- polr(as.factor(order)~., data = Table, Hess = T,method=method, start = seq(from = start, to = to, by = bycoef/FindStartLength(Table)))
  predicted_ratings <- predict(best_model, TableNew)
  TableNew[, "PredictedRating" := predicted_ratings]
  return(list("model" = best_model, "PredictedRatings" = predicted_ratings, "FactorsAndRatings" = TableNew))
}

#run algorithm as an example ####
PCTable <- GeneratePCTable(BanksWithRatingsTable_Train = TheTable[(Year>=2016)&(Month>=4)&(Status!="absent")],
                           CreatePredictionSet = T,
                           BanksWithRatingsTable_Predict = TheTable[(Year>=2016)&(Month>=4)&(Status=="absent")])

ten_PC <- GenerateRestrictedPCTable(PCTable$PCTable, 20)

OutOfSampleErrors <- ValidateModel(ten_PC[, -c("Month")], K = 20)

#ужс
OutOfSampleErrors[, mean(AverageErrors)]

FinalPredictions <- GetPrediction(ten_PC, PCTable$PCTablePredict)
