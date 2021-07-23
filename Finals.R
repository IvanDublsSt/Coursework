library(data.table)
library(openxlsx)
library(stringr)
library(e1071)

setwd("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework")
Sys.setenv(LANG = "en")

mapyear <- function(year, years){
  years <- unique(years)
  yearsreal <- c(2015:2020)
  #print(years <= year)
  #print(length(years[years <= year]))
  return(yearsreal[length(years[years <= year])])
}

mapyear(c(-5,-5,-2,-3))

mapmonth <- function(month, months){
  months <- unique(months)
  monthsreal <- c(1:12)
  #print(years <= year)
  #print(length(years[years <= year]))
  return(monthsreal[length(months[months <= month])])
}
znorm_vector <- function(vector){
  vecmean <- mean(vector, na.rm = T)
  vecstdev <- sd(vector, na.rm = T)
  normalized_vector <- (vector-vecmean)/vecstdev
  return(normalized_vector)
}

znorm_table <- function(dtable){
  charnames <- colnames(dtable)[!sapply(dtable, is.numeric)]
  numericnames <- colnames(dtable)[sapply(dtable, is.numeric)]
  dtable_numeric <- dtable[, ..numericnames]
  dtable_character <- dtable[, ..charnames]
  dtable_normalized <- as.data.table(apply(dtable_numeric, 2, znorm_vector))
  dtable_final <- cbind(dtable_character, dtable_normalized)
  return(dtable_final)
}
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
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

MakePredictionState <- function(Model, Table, Variable = "G_spread_interpolated", Average = T, ISINs = NULL){
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

interpolater_borders <- function(x){
  x_init <- x
  x_init <- data.table("x" = x_init)
  x_init[, "x_prev" := shift(x)]
  x_init[, "diff_prev" := x_prev - x]
  diff_prev <- mean(x_init$diff_prev, na.rm = T)
  if (is.na(x[1])){
    num_of_nas <- 0
    for(i in x){
      if(is.na(i)){num_of_nas <- num_of_nas + 1}
      else{break}
    }
    replacements <- x[num_of_nas+1] + diff_prev
    if (num_of_nas !=1){
    for(i in 1:(num_of_nas-1)){
      replacements <- c(replacements, replacements[length(replacements)] + diff_prev)
    }}
    replacements <- rev(replacements)
    x[1:num_of_nas] <- replacements
  }

  x<-rev(x)
  if (is.na(x[1])){
    num_of_nas <- 0
    for(i in x){
      if(is.na(i)){num_of_nas <- num_of_nas + 1}
      else{break}
    }
    replacements <- x[num_of_nas+1] - diff_prev
    if (num_of_nas != 1){
      for(i in 1:(num_of_nas-1)){
        replacements <- c(replacements, replacements[length(replacements)] - diff_prev)
      }}
    replacements <- rev(replacements)
    x[1:num_of_nas] <- replacements
  }
 
  return(rev(x))
}

summary.plm.full <- function (object, vcov = NULL, ...) 
{
  vcov_arg <- vcov
  
  #add plm::: for plm functions so they are calllex correctly
  model <- plm:::describe(object, "model")
  effect <- plm:::describe(object, "effect")
  random.method <- plm:::describe(object, "random.method")
  object$r.squared <- c(rsq = r.squared(object), 
                        adjrsq = r.squared(object, dfcor = TRUE),
                        # add the two new r squared terms here
                        rsq_overall = r.squared(object, model = "pooled"),
                        rsq_btw = r.squared(update(object, effect = "individual", model = "between")))
  
  use.norm.chisq <- FALSE
  if (model == "random") 
    use.norm.chisq <- TRUE
  if (length(formula(object))[2] >= 2) 
    use.norm.chisq <- TRUE
  if (model == "ht") 
    use.norm.chisq <- TRUE
  object$fstatistic <- pwaldtest(object, test = ifelse(use.norm.chisq, 
                                                       "Chisq", "F"), vcov = vcov_arg)
  if (!is.null(vcov_arg)) {
    if (is.matrix(vcov_arg)) 
      rvcov <- vcov_arg
    if (is.function(vcov_arg)) 
      rvcov <- vcov_arg(object)
    std.err <- sqrt(diag(rvcov))
  }
  else {
    std.err <- sqrt(diag(stats::vcov(object)))
  }
  b <- coefficients(object)
  z <- b/std.err
  p <- if (use.norm.chisq) {
    2 * pnorm(abs(z), lower.tail = FALSE)
  }
  else {
    2 * pt(abs(z), df = object$df.residual, lower.tail = FALSE)
  }
  object$coefficients <- cbind(b, std.err, z, p)
  colnames(object$coefficients) <- if (use.norm.chisq) {
    c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
  }
  else {
    c("Estimate", "Std. Error", "t-value", "Pr(>|t|)")
  }
  if (!is.null(vcov_arg)) {
    object$rvcov <- rvcov
    rvcov.name <- paste0(deparse(substitute(vcov)))
    attr(object$rvcov, which = "rvcov.name") <- rvcov.name
  }
  object$df <- c(length(b), object$df.residual, length(object$aliased))
  class(object) <- c("summary.plm.full", "plm", "panelmodel")
  object
}

print.summary.plm.full <- function (x, digits = max(3, getOption("digits") - 2), width = getOption("width"), 
                                    subset = NULL, ...) 
{
  formula <- formula(x)
  has.instruments <- (length(formula)[2] >= 2)
  effect <- plm:::describe(x, "effect")
  model <- plm:::describe(x, "model")
  if (model != "pooling") {
    cat(paste(plm:::effect.plm.list[effect], " ", sep = ""))
  }
  cat(paste(plm:::model.plm.list[model], " Model", sep = ""))
  if (model == "random") {
    ercomp <- describe(x, "random.method")
    cat(paste(" \n   (", random.method.list[ercomp], "'s transformation)\n", 
              sep = ""))
  }
  else {
    cat("\n")
  }
  if (has.instruments) {
    cat("Instrumental variable estimation\n")
    if (model != "within") {
      ivar <- plm:::describe(x, "inst.method")
      cat(paste0("   (", plm:::inst.method.list[ivar], "'s transformation)\n"))
    }
  }
  if (!is.null(x$rvcov)) {
    cat("\nNote: Coefficient variance-covariance matrix supplied: ", 
        attr(x$rvcov, which = "rvcov.name"), "\n", sep = "")
  }
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  pdim <- pdim(x)
  print(pdim)
  if (model %in% c("fd", "between")) {
    cat(paste0("Observations used in estimation: ", nobs(x), 
               "\n"))
  }
  if (model == "random") {
    cat("\nEffects:\n")
    print(x$ercomp)
  }
  cat("\nResiduals:\n")
  df <- x$df
  rdf <- df[2L]
  if (rdf > 5L) {
    save.digits <- unlist(options(digits = digits))
    on.exit(options(digits = save.digits))
    print(plm:::sumres(x))
  }
  else if (rdf > 0L) 
    print(residuals(x), digits = digits)
  if (rdf == 0L) {
    cat("ALL", x$df[1L], "residuals are 0: no residual degrees of freedom!")
    cat("\n")
  }
  if (any(x$aliased, na.rm = TRUE)) {
    naliased <- sum(x$aliased, na.rm = TRUE)
    cat("\nCoefficients: (", naliased, " dropped because of singularities)\n", 
        sep = "")
  }
  else cat("\nCoefficients:\n")
  if (is.null(subset)) 
    printCoefmat(coef(x), digits = digits)
  else printCoefmat(coef(x)[subset, , drop = FALSE], digits = digits)
  cat("\n")
  cat(paste("Total Sum of Squares:    ", signif(plm:::tss.plm(x), digits), 
            "\n", sep = ""))
  cat(paste("Residual Sum of Squares: ", signif(deviance(x), 
                                                digits), "\n", sep = ""))
  cat(paste("R-Squared:      ", signif(x$r.squared[1], digits), 
            "\n", sep = ""))
  cat(paste("Adj. R-Squared: ", signif(x$r.squared[2], digits), 
            "\n", sep = ""))
  # add the new r squared terms here
  cat(paste("Overall R-Squared:      ", signif(x$r.squared[3], digits), 
            "\n", sep = ""))
  cat(paste("Between R-Squared:      ", signif(x$r.squared[4], digits), 
            "\n", sep = ""))
  fstat <- x$fstatistic
  if (names(fstat$statistic) == "F") {
    cat(paste("F-statistic: ", signif(fstat$statistic), " on ", 
              fstat$parameter["df1"], " and ", fstat$parameter["df2"], 
              " DF, p-value: ", format.pval(fstat$p.value, digits = digits), 
              "\n", sep = ""))
  }
  else {
    cat(paste("Chisq: ", signif(fstat$statistic), " on ", 
              fstat$parameter, " DF, p-value: ", format.pval(fstat$p.value, 
                                                             digits = digits), "\n", sep = ""))
  }
  invisible(x)
}

#robustness####
#residuals####
#download
smpl_dt <- fread("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/Tables/Cd1_I_.csv")
#transform the date
smpl_dt[(format(as.Date(Date_trading,format="%Y-%m-%d"), format = "%d") >=15)&(Month!=12), "Month":=Month+1]
#normalize
smpl_dt_norm <- cbind(znorm_table(smpl_dt[, !c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")]), smpl_dt[, c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")])
#outliers
smpl_dt_norm <- smpl_dt_norm[znorm_vector(G_spread_interpolated) <=3, ]
smpl_dt_norm <- smpl_dt_norm[znorm_vector(G_spread_interpolated) >=3, ]
#subsample
dt <- copy(smpl_dt_norm)
dt <- dt[order(Date_trading, Year, Month)]
dt[, "V1" := 1:.N]
dt[, "TrueYear" := ordered(Year, levels = unique(Year), labels = c(2015:2020))]
dt[, "TrueMonth" := ordered(Month, levels = unique(Month), labels = c(1:12))]
dt_test <- copy(dt)
years <- unique(dt$Year)
months<- unique(dt$Month)

for (i in 2015:2020){
  for (s in 1:12){
    num_start <- min(dt[(TrueYear == i)&(TrueMonth==s)]$V1)
    num_end <-   max(dt[(TrueYear == i)&(TrueMonth==s)]$V1)
    smpl <- sample(x = (num_start:num_end),round((num_end-num_start)/2), replace = F)
    dt <- dt[!(V1 %in% smpl)]
    dt_test <- dt_test[(V1 %in% smpl)|(!((TrueYear == i)&(TrueMonth==s)))]
  }
}
dt[, "TrueYear" := NULL]
dt[, "TrueMonth" := NULL]
dt_test[, "TrueYear" := NULL]
dt_test[, "TrueMonth" := NULL]
#get columns needed 
dt <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = "Med")$Table
dt_isin <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = "Med")$ISIN

#train the model
#modelsvm <- svm(get("G_spread_interpolated")~.,data = dt, gamma = 0.0001, cost = 100, epsilon = 0.001, kernel = "poly")
#get out of sample resids
dt[, "Iteration" := sample(1:20, .N, replace = T)]
start <- Sys.time()
modelsvm <- svm(get("G_spread_interpolated")~.-Iteration,data = dt[Iteration != 1], gamma = 0.0001, cost = 100, epsilon = 0.001)
a <- MakePredictionState(modelsvm, dt[Iteration == 1], ISINs = dt_isin[dt$Iteration==1])
Sys.time() - start

for (i in 2:20){
  start <- Sys.time()
  modelsvm <- svm(get("G_spread_interpolated")~.-Iteration,data = dt[Iteration != i], gamma = 0.0001, cost = 100, epsilon = 0.001)
  anew <- MakePredictionState(modelsvm, dt[Iteration == i], ISINs = dt_isin[dt$Iteration==i])
  a <- rbind(a, anew)
  Sys.time() - start
}
a[, "Year" := sapply(Year, mapyear, years = a$Year)]
a[, "Month" := sapply(Month, mapmonth, months = a$Month)]
mean(a$Errors^2)^(1/2)
saveRDS(a, "ResidualsTableRobustOutOfSample.rds")
#get test samples
dt_test_isin <- CleanData(dt_test, "G_spread_interpolated", Average = "Med")$ISIN
dt_test <- CleanData(dt_test, "G_spread_interpolated", Average = "Med")$Table

modelsvm <- readRDS("ModelRobustness.rds")
#get residuals
m <- MakePredictionState(modelsvm, dt_test, "G_spread_interpolated", T, dt_test_isin)
m[, "Year" := sapply(Year, mapyear, years = m$Year)]
m[, "Month" := sapply(Month, mapmonth, months = m$Month)]
mm <- mean((predict(modelsvm, dt_test) - dt_test$G_spread_interpolated)^2)
m[, mean(Errors^2)]
saveRDS(m, "ResidualsTableRobustness.rds")
saveRDS(modelsvm, "ModelRobustness.rds")

t.test(sample(seq(100000,2000000,1), 1000000), sample(-seq(100000,2000000,1), 1000000), alternative = "greater", mu = 0)$p.value

#differentials####

#download
smpl_dt <- fread("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/Tables/Cd1_I_.csv")
#transform the date
smpl_dt[(format(as.Date(Date_trading,format="%Y-%m-%d"), format = "%d") >=15)&(Month!=12), "Month":=Month+1]
#normalize
smpl_dt_norm <- cbind(znorm_table(smpl_dt[, !c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")]), smpl_dt[, c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")])
#get columns needed 
dt_state <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = "Med", State = T)$Table
dt_state_isin <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = "Med", State = T)$ISIN
#get differentials
dt_state[, "ISIN" := dt_state_isin]
dt_state <- subset(dt_state, Currency %in% levels(dt$Currency))
dt_state$Currency <- droplevels(dt_state$Currency)
dt_state <- subset(dt_state,Exch_name %in% levels(dt$Exch_name))
dt_state$Exch_name <- droplevels(dt_state$Exch_name)
levels(dt_state$Exch_name) <- levels(dt$Exch_name)
levels(dt_state$Currency) <- levels(dt$Currency)
dt_state_isin <- dt_state$ISIN
m_state <- MakePredictionState(modelsvm, dt_state, "G_spread_interpolated", T, dt_state_isin)
m_state[, "Year" := sapply(Year, mapyear, years = m_state$Year)]
m_state[, "Month" := sapply(Month, mapmonth, months = m_state$Month)]
saveRDS(m_state, "DifferentialsRobustness.rds")
#test the residuals robustness ####
Residuals <- readRDS("ResidualsTableRobustness.rds")
years_t_tests <- c()
names(years_t_tests) <- NULL
for (i in 2015:2020){
  Residuals_T_2015 <- Residuals[Year == i, c("Month", "Errors")]
  Residuals_T_2015[, "Errors" := -Errors]
  m <- t.test(Residuals_T_2015$Errors, alternative = "greater", mu = 0)
  newnames <- c(names(years_t_tests), as.character(i))
  years_t_tests <- c(years_t_tests, m$p.value)
  names(years_t_tests) <- newnames
}

write.xlsx(years_t_tests, "ttests_years_resids_robust.xlsx")
months_t_tests <- list()

for (i in 2015:2020){
  this_year_t_tests <- c()
  for (s in 1:12){
    Residuals_T_2015 <- Residuals[(Year == i)&(Month == s), c("Month", "Errors")]
    Residuals_T_2015[, "Errors" := -Errors]
    m <- t.test(Residuals_T_2015$Errors, alternative = "greater", mu = 0)
    this_year_t_tests <- c(this_year_t_tests, m$p.value)
  }
  months_t_tests[[i-2014]] <- this_year_t_tests
}
months_t_tests <- do.call(cbind, months_t_tests)
write.xlsx(months_t_tests, "ttests_months_resids_robust.xlsx")

mean(Residuals[(Year == 2015), c("Errors")]$Errors, na.rm = T)
(var(Residuals[(Year == 2015), c("Errors")]$Errors, na.rm = T))^(1/2)


Residuals_T <- Residuals[, c("Month", "Errors")]
Residuals_T[, "Errors" := -Errors]
m <- t.test(Residuals_T$Errors, alternative = "greater", mu = 0)
m$p.value


#t-tests differentials ####
#Differentials

Differentials <- readRDS("DifferentialsRobustness.rds")

years_t_tests <- c()
names(years_t_tests) <- NULL
for (i in 2015:2020){
  Differentials_T_2015 <- Differentials[Year == i, c("Month", "Errors")]
  Differentials_T_2015[, "Errors" := -Errors]
  m <- t.test(Differentials_T_2015$Errors, alternative = "greater", mu = 0)
  newnames <- c(names(years_t_tests), as.character(i))
  years_t_tests <- c(years_t_tests, m$p.value)
  names(years_t_tests) <- newnames
}

write.xlsx(years_t_tests, "ttests_years_robust_diff.xlsx")
months_t_tests <- list()

for (i in 2015:2020){
  this_year_t_tests <- c()
  for (s in 1:12){
    Differentials_T_2015 <- Differentials[(Year == i)&(Month == s), c("Month", "Errors")]
    Differentials_T_2015[, "Errors" := -Errors]
    m <- t.test(Differentials_T_2015$Errors, alternative = "greater", mu = 0)
    this_year_t_tests <- c(this_year_t_tests, m$p.value)
  }
  months_t_tests[[i-2014]] <- this_year_t_tests
}
months_t_tests <- do.call(cbind, months_t_tests)
write.xlsx(months_t_tests, "ttests_months_robust_diff.xlsx")

Differentials_T <- Differentials[, c("Month", "Errors")]
Differentials_T[, "Errors" := -Errors]
m <- t.test(Differentials_T$Errors, alternative = "greater", mu = 0)
m$p.value

#t-test both####

Differentials <- readRDS("DifferentialsRobustness.rds")

years_t_tests <- c()
names(years_t_tests) <- NULL
for (i in 2015:2020){
  Differentials_T_2015 <- Differentials[Year == i, c("Month", "Errors")]
  Differentials_T_2015[, "Errors" := -Errors]
  Residuals_T_2015 <- Residuals[Year == i, c("Month", "Errors")]
  Residuals_T_2015[, "Errors" := -Errors]
  m <- t.test(Differentials_T_2015$Errors,y = Residuals_T_2015$Errors, alternative = "less", mu = 0)
  newnames <- c(names(years_t_tests), as.character(i))
  years_t_tests <- c(years_t_tests, m$p.value)
  names(years_t_tests) <- newnames
}

write.xlsx(years_t_tests, "ttests_years_robust_twofolds.xlsx")
months_t_tests <- list()

for (i in 2015:2020){
  this_year_t_tests <- c()
  for (s in 1:12){
    Differentials_T_2015 <- Differentials[(Year == i)&(Month == s), c("Month", "Errors")]
    Differentials_T_2015[, "Errors" := -Errors]
    Residuals_T_2015 <- Residuals[(Year == i)&(Month == s), c("Month", "Errors")]
    Residuals_T_2015[, "Errors" := -Errors]
    m <- t.test(Differentials_T_2015$Errors,y = Residuals_T_2015$Errors, alternative = "greater", mu = 0)
    this_year_t_tests <- c(this_year_t_tests, m$p.value)
  }
  months_t_tests[[i-2014]] <- this_year_t_tests
}
months_t_tests <- do.call(cbind, months_t_tests)
write.xlsx(months_t_tests, "ttests_months_robust_twofolds.xlsx")

Differentials_T <- Differentials[, c("Month", "Errors")]
Differentials_T[, "Errors" := -Errors]
Residuals_T[, "Errors" := -Errors]
m <- t.test(Differentials_T$Errors, Residuals$Errors, alternative = "less", mu = 0)
m$p.value


#Graphs robust####
library(ggplot2)
library(hrbrthemes)
breaks <- c("2015 1", "2016 1", "2017 1", "2018 1", "2019 1", "2020 1")
Residuals_mean <- Residuals[, mean(Errors), by = c("Year", "Month")]
png("TimeSeriesPrivateRobust.png", width = 700, height = 400)
ggplot(Residuals_mean, aes(y=V1, x=paste(Year, Month))) + 
  geom_bar(stat = "identity") + scale_x_discrete(breaks = breaks) + theme_ipsum()+
  ylab("Error") + xlab("Date")
dev.off()

Diffs_mean <- Differentials[, mean(-Errors), by = c("Year", "Month")]
png("TimeSeriesStateRobust.png", width = 700, height = 400)
ggplot(Diffs_mean, aes(y=V1, x=paste(Year, Month))) + 
  geom_bar(stat = "identity") + scale_x_discrete(breaks = breaks) + theme_ipsum()+
  ylab("Error") + xlab("Date")
dev.off()

Residuals_stdev <- Residuals[, sd(Errors, na.rm = T), by = c("Year", "Month")]
png("TimeSeriesPrivateRobustSdev.png", width = 700, height = 400)
ggplot(Residuals_stdev, aes(y=V1, x=paste(Year, Month))) + 
  geom_bar(stat = "identity") + scale_x_discrete(breaks = breaks) + theme_ipsum()+
  ylab("Error") + xlab("Date")
dev.off()
#Granger robust####
Differentials <- Differentials[, c("Year", "Month", "Errors")]
Differentials <- Differentials[, mean(Errors, na.rm = T), by = c("Year", "Month")]
alldates <- Differentials[, c("Year", "Month")]

GDP <- fread("Russia_GDP_monthly.csv")
GDP <- GDP[-1]
GDP[, "Date" := as.Date(as.character(V1), format = "%d.%m.%Y")]
GDP[, "Year" := year(Date)]
GDP[, "Month" := month(Date)]
GDP[, "GDPGrowth" := str_remove(V3, "%")]
GDP[, "GDPGrowth" := str_replace(GDPGrowth, ",", ".")]
GDP <- GDP[, mean(as.numeric(GDPGrowth), na.rm = T), by = c("Year", "Month")]
GDP <- merge(GDP, alldates, by = c("Year", "Month"), all = T)
GDP <- GDP[Year < 2021]
GDP <- zoo(GDP)
GDP <- na.approx(GDP)
GDP <- as.data.table(GDP)
GDP[(Year == 2015)&(Month == 3), "V1" := (99.599 - 100.804)/100.804]
GDP[(Year == 2015)&(Month == 1), "V1" := (99.923 - 100.798)/100.798]
GDP[(Year == 2015)&(Month == 2), "V1" := (99.754 - 100.801)/100.801]



#VarStep: GDP robust####

Differentials
#Test for unit roots
adf.test(Differentials$V1)$p.value
adf.test(diff(Differentials$V1))$p.value
kpss.test(Differentials$V1)$p.value
kpss.test(diff(Differentials$V1,1))$p.value


adf.test(GDP$V1)$p.value
adf.test(diff(GDP$V1,1))$p.value
kpss.test(GDP$V1)$p.value
kpss.test(diff(GDP$V1,1))$p.value

# Since first order differencing eliminates the unit root, the maximum order of integration
# is concluded to be I(1).
Vars <- merge(Differentials, GDP, by = c("Year", "Month"))
colnames(Vars) <- c("Year", "Month", "Differentials", "Variable")
Vars[, "Differentials" := Differentials - shift(Differentials)]
Vars[, "Variable" := Variable - shift(Variable)]
Vars <- Vars[-1]
#Set up VAR-Model
#select lag order // either 2 or 6
m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICGDP.xlsx")

#VAR Model, lag=2
V.1<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.1)$serial$p.value
arch.test(V.1, 2,multivariate.only = T)

#VAR-Model, lag=6
V.2<-VAR(Vars[,3:4],p=1,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

#VAR-Model, lag=7
V.3<-VAR(Vars[,3:4],p=7,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 


# Wald-test for the first 6 lags
# The test can be directly done with the VAR model, however using the correct
# variables is a little more tricky

#VAR-Model, lag=7 (additional lag, though not tested)
VFinal<-VAR(Vars[,3:4],p=2,type="both")
VFinal$varresult
summary(VFinal)

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

install.packages("tsDyn")
library(tsDyn)
m <- VECM(Vars[, c(3,4)],1, estim = "ML")
summary(m)
#Wald-test (H0: Robusta does not Granger-cause Arabica)
wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2))
# Could not be rejected (X2=8.6; p=0.2)

#Wald.test (H0: Arabica does not Granger-cause Robusta)
wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1))


VFinal<-VAR(Vars[,3:4],p=3,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))

#robust but old model####
#residuals old####
#download
smpl_dt <- fread("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/Tables/Cd1_I_.csv")
#transform the date
smpl_dt[(format(as.Date(Date_trading,format="%Y-%m-%d"), format = "%d") >=15)&(Month!=12), "Month":=Month+1]
#normalize
smpl_dt_norm <- cbind(znorm_table(smpl_dt[, !c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")]), smpl_dt[, c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")])
#outliers
smpl_dt_norm <- smpl_dt_norm[znorm_vector(G_spread_interpolated) <=3, ]
smpl_dt_norm <- smpl_dt_norm[znorm_vector(G_spread_interpolated) >=3, ]
#subsample
dt <- copy(smpl_dt_norm)
dt <- dt[order(Date_trading, Year, Month)]
dt[, "V1" := 1:.N]
dt[, "TrueYear" := ordered(Year, levels = unique(Year), labels = c(2015:2020))]
dt[, "TrueMonth" := ordered(Month, levels = unique(Month), labels = c(1:12))]
dt_test <- copy(dt)
years <- unique(dt$Year)
months<- unique(dt$Month)

for (i in 2015:2020){
  for (s in 1:12){
    num_start <- min(dt[(TrueYear == i)&(TrueMonth==s)]$V1)
    num_end <-   max(dt[(TrueYear == i)&(TrueMonth==s)]$V1)
    smpl <- sample(x = (num_start:num_end),round((num_end-num_start)/2), replace = F)
    dt <- dt[!(V1 %in% smpl)]
    dt_test <- dt_test[(V1 %in% smpl)|(!((TrueYear == i)&(TrueMonth==s)))]
  }
}
dt[, "TrueYear" := NULL]
dt[, "TrueMonth" := NULL]
dt_test[, "TrueYear" := NULL]
dt_test[, "TrueMonth" := NULL]
#get columns needed 
dt <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = "Med")$Table
dt_isin <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = "Med")$ISIN

#train the model
start <- Sys.time()
modelsvm <- tune.svm(get("G_spread_interpolated")~.,data = dt, cross = 5, gamma = 0.0001, cost = 100, epsilon = 0.001)
Sys.time() - start
saveRDS(modelsvm, "ModelRobustOld.rds")
initial_model <- readRDS("BestModel.rds")
#get test samples
dt_test_isin <- CleanData(dt_test, "G_spread_interpolated", Average = "Med")$ISIN
dt_test <- CleanData(dt_test, "G_spread_interpolated", Average = "Med")$Table
#cleaning for prediction
dt_test[, "ISIN" := dt_test_isin]
dt_test <- subset(dt_test, Currency %in% levels(dt$Currency))
dt_test$Currency <- droplevels(dt_test$Currency)
dt_test <- subset(dt_test,Exch_name %in% levels(dt$Exch_name))
dt_test$Exch_name <- droplevels(dt_test$Exch_name)
levels(dt_test$Exch_name) <- levels(dt$Exch_name)
levels(dt_test$Currency) <- levels(dt$Currency)
dt_test_isin <- dt_test$ISIN
#get residuals
m <- MakePredictionState(modelsvm$best.model, dt_test, "G_spread_interpolated", T, dt_test_isin)
m[, "Year" := sapply(Year, mapyear, years = m$Year)]
m[, "Month" := sapply(Month, mapmonth, months = m$Month)]
mm <- mean((predict(modelsvm, dt_test) - dt_test$G_spread_interpolated)^2)
m[, mean(Errors^2)]
saveRDS(m, "ResidualsTableRobustnessOld.rds")
saveRDS(modelsvm, "ModelRobustnessOld.rds")


#differentials old####

#download
smpl_dt <- fread("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/Tables/Cd1_I_.csv")
#transform the date
smpl_dt[(format(as.Date(Date_trading,format="%Y-%m-%d"), format = "%d") >=15)&(Month!=12), "Month":=Month+1]
#normalize
smpl_dt_norm <- cbind(znorm_table(smpl_dt[, !c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")]), smpl_dt[, c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")])
#get columns needed 
dt_state <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = "Med", State = T)$Table
dt_state_isin <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = "Med", State = T)$ISIN
#get differentials
dt_state[, "ISIN" := dt_state_isin]
dt_state <- subset(dt_state, Currency %in% levels(dt$Currency))
dt_state$Currency <- droplevels(dt_state$Currency)
dt_state <- subset(dt_state,Exch_name %in% levels(dt$Exch_name))
dt_state$Exch_name <- droplevels(dt_state$Exch_name)
levels(dt_state$Exch_name) <- levels(dt$Exch_name)
levels(dt_state$Currency) <- levels(dt$Currency)
dt_state_isin <- dt_state$ISIN
m_state <- MakePredictionState(modelsvm$best.model, dt_state, "G_spread_interpolated", T, dt_state_isin)
m_state[, "Year" := sapply(Year, mapyear, years = m_state$Year)]
m_state[, "Month" := sapply(Month, mapmonth, months = m_state$Month)]
saveRDS(m_state, "DifferentialsRobustnessOld.rds")


#VarStep: GDP robust old####

Differentials <- readRDS("DifferentialsRobustnessOld.rds")
Differentials <- Differentials[, c("Year", "Month", "Errors")]
Differentials <- Differentials[, mean(Errors, na.rm = T), by = c("Year", "Month")]
alldates <- Differentials[, c("Year", "Month")]

#Test for unit roots
adf.test(Differentials$V1)$p.value
adf.test(diff(Differentials$V1))$p.value
kpss.test(Differentials$V1)$p.value
kpss.test(diff(Differentials$V1,1))$p.value


adf.test(GDP$V1)$p.value
adf.test(diff(GDP$V1,1))$p.value
kpss.test(GDP$V1)$p.value
kpss.test(diff(GDP$V1,1))$p.value

# Since first order differencing eliminates the unit root, the maximum order of integration
# is concluded to be I(1).
Vars <- merge(Differentials, GDP, by = c("Year", "Month"))
colnames(Vars) <- c("Year", "Month", "Differentials", "Variable")
Vars[, "Differentials" := Differentials - shift(Differentials)]
Vars[, "Variable" := Variable - shift(Variable)]
Vars <- Vars[-1]
#Set up VAR-Model
#select lag order // either 2 or 6
m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICGDP.xlsx")

#VAR Model, lag=2
V.1<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.1)$serial$p.value
arch.test(V.1, 2,multivariate.only = T)

#VAR-Model, lag=6
V.2<-VAR(Vars[,3:4],p=1,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

#VAR-Model, lag=7
V.3<-VAR(Vars[,3:4],p=7,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 


# Wald-test for the first 6 lags
# The test can be directly done with the VAR model, however using the correct
# variables is a little more tricky

#VAR-Model, lag=7 (additional lag, though not tested)
VFinal<-VAR(Vars[,3:4],p=2,type="both")
VFinal$varresult
summary(VFinal)

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

install.packages("tsDyn")
library(tsDyn)
m <- VECM(Vars[, c(3,4)],1, estim = "ML")
summary(m)
#Wald-test (H0: Robusta does not Granger-cause Arabica)
wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2))
# Could not be rejected (X2=8.6; p=0.2)

#Wald.test (H0: Arabica does not Granger-cause Robusta)
wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1))


VFinal<-VAR(Vars[,3:4],p=3,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))

(exp(0.04*1)-0.9)/(1.1-0.9)
(0*0.7040539 + 3*(1-0.7040539))*exp(-0.04)
(1*0.7040539 + 0*(1-0.7040539))*exp(-0.04)
log(0.85/3)
log(0.676)
as.Date(m)
(exp(0.39)*20 - 18)/4

(exp(0.39)-0.9)/0.2
smpl_dt <- fread("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/Tables/Cb1_I_.csv")
smpl_dt_norm <- cbind(znorm_table(smpl_dt[, !c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")]), smpl_dt[, c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank")])
dt <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = T)$Table
dt_isin <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = T)$ISIN

dt[, "Iteration" := sample(1:20, .N, replace = T)]


#taking out of sample residuals 12.06.2021#####
start <- Sys.time()
modelsvm <- svm(get("G_spread_interpolated")~.-Iteration,data = dt[Iteration != 1], gamma = 0.0001, cost = 100, epsilon = 0.001)
a <- MakePredictionState(modelsvm, dt[Iteration == 1], ISINs = dt_isin[dt$Iteration==1])
Sys.time() - start

for (i in 2:20){
  start <- Sys.time()
  modelsvm <- svm(get("G_spread_interpolated")~.-Iteration,data = dt[Iteration != i], gamma = 0.0001, cost = 100, epsilon = 0.001)
  anew <- MakePredictionState(modelsvm, dt[Iteration == i], ISINs = dt_isin[dt$Iteration==i])
  a <- rbind(a, anew)
  Sys.time() - start
}

a[, "Year" := sapply(Year, mapyear, years = a$Year)]
a[, "Month" := sapply(Month, mapmonth, months = a$Month)]
#
mean(a$Errors)
saveRDS(a, "ResidualsTableOutOfSample.rds")


#This is the only correct model and I do not know why 
start <- Sys.time()
#modelsvm <- tune.svm(get("G_spread_interpolated")~.,data = dt, cross = 5, gamma = 0.0001, cost = 100, epsilon = 0.001)
Sys.time() - start
#here it ends

#modelsvmp <- tune.svm(G_spread_interpolated~.,data =  dt, gamma = 0.0001, cost = 100, epsilon = 0.001)
modelsvmp$performances
modelsvm$performances
modelsvm$train.ind
modelsvmp$train.ind
modelsvmp$nparcomb
modelsvm$best.model
modelsvmp$best.model

start <- Sys.time()
#modelsvmq <- svm( G_spread_interpolated~., dt, gamma = 0.0001, cost = 100, epsilon = 0.001)
Sys.time() - start
model


colnames(dt)
colnames(dt_state)

#the real work is here #####
modelsvm <- readRDS("BestModel.rds")
dt_state <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = T, State = T)$Table
dt_state_isin <- CleanData(smpl_dt_norm, "G_spread_interpolated", Average = T, State = T)$ISIN
dt_state[, "ISIN" := dt_state_isin]
dt_state <- subset(dt_state, Currency %in% levels(dt$Currency))
dt_state$Currency <- droplevels(dt_state$Currency)
dt_state <- subset(dt_state,Exch_name %in% levels(dt$Exch_name))
dt_state$Exch_name <- droplevels(dt_state$Exch_name)
levels(dt_state$Exch_name) <- levels(dt$Exch_name)
levels(dt_state$Currency) <- levels(dt$Currency)
dt_state_isin <- dt_state$ISIN
modelsvm$best.model$residuals
saveRDS(modelsvm, "BestModel.rds")
modelsvm <- readRDS("BestModel.rds")
#modelsvm <- tune.svm(get("G_spread_interpolated")~.,data = dt, cross = 5, gamma = 0.0001, cost = 100, epsilon = 0.001)
modelsvm$best.model$tot.MSE
mean((predict(modelsvm$best.model, dt) - dt$G_spread_interpolated))
m <- MakePredictionState(modelsvm$best.model, dt_state, ISINs = dt_state_isin)
m[, "Year" := sapply(Year, mapyear, years = m$Year)]
m[, "Month" := sapply(Month, mapmonth, months = m$Month)]

a <- MakePredictionState(modelsvm$best.model, dt, ISINs = dt_isin)
a[, "Year" := sapply(Year, mapyear, years = a$Year)]
a[, "Month" := sapply(Month, mapmonth, months = a$Month)]
saveRDS(m, "DifferentialsTable.rds")
saveRDS(a, "ResidualsTable.rds")
m <- readRDS("ResidualsTableOutOfSample.rds")
m[, "Year" := sapply(Year, mapyear, years = m$Year)]
m[, "Month" := sapply(Month, mapmonth, months = m$Month)]
mean(m$Errors^2)
#linear regression####
library(AER)
library(plm)
library(stargazer)
smpl_dt[, "Date" := paste(Year, Month)]
smpl_dt <- smpl_dt[Exch_name != "Венская ФБ"]
model_linear <- lm(G_spread_interpolated~.,
                    data = dt)
linear_predict <- predict(model_linear, dt)
mean((linear_predict - dt$G_spread_interpolated)^2)
smpl_dt[, "Iteration" := sample(1:20, .N, replace = T)]
dt[, "Iteration" := sample(1:20, .N, replace = T)]

start <- Sys.time()
modelsvm <- lm(G_spread_interpolated~.,
               data = dt[Iteration!=1])
a <- MakePredictionState(modelsvm, dt[Iteration == 1], ISINs = dt_isin[dt$Iteration==1])
Sys.time() - start

for (i in 2:20){
  start <- Sys.time()
  modelsvm <- lm(G_spread_interpolated~.,
                 data = dt[Iteration!=i])
  anew <- MakePredictionState(modelsvm, dt[Iteration == i], ISINs = dt_isin[dt$Iteration==i])
  a <- rbind(a, anew)
  Sys.time() - start
}
mean(a$Errors^2)
#explore! ####
library(ggplot2)
library(hrbrthemes)
mean((m$Errors))
mean((predict(modelsvm$best.model, dt_state)-dt_state$G_spread_interpolated))

m[order(Year, Month), mean(Errors), by = c("Year", "Month")]
plot()
p <- ggplot(m, aes(x=Errors)) + 
  geom_histogram()
p
m[, paste(Year, Month)]
m_averages <- m[, mean(-Errors), by = c("Year", "Month")]
breaks <- c("2015 1", "2016 1", "2017 1", "2018 1", "2019 1", "2020 1")
png("TimeSeriesState.png", width = 700, height = 400)
ggplot(m_averages, aes(y=V1, x=paste(Year, Month))) + 
  geom_bar(stat = "identity") + scale_x_discrete(breaks = breaks) + theme_ipsum()+
  ylab("Error") + xlab("Date")
dev.off()
  

dt[, "Prediction" := predict(modelsvm$best.model, dt)]
dt[, "Error" := Prediction - G_spread_interpolated]

a <- readRDS("ResidualsTableOutOfSample.rds")
a[, "Year" := sapply(Year, mapyear, years = a$Year)]
a[, "Month" := sapply(Month, mapmonth, months = a$Month)]
mean(a$Errors^2)
dt_averaged_for_graph <- a[, mean(-Errors), by = c("Year", "Month")]
dt_averaged_for_graph[, "Month" := as.numeric(Month)]
dt_averaged_for_graph[, "zoodate" := as.yearmon(paste(Year, Month), "%Y %m")]
setorderv(dt_averaged_for_graph, c("Year", "Month"))
Sys.setlocale("LC_ALL","English")
png("TimeSeriesPrivate.png", width = 700, height = 400)
ggplot(dt_averaged_for_graph, aes(y=V1, x=zoodate)) + 
  geom_bar(stat = "identity")  + theme_ipsum()+
  ylab("Error") + xlab("Date")
dev.off()
library(zoo)
dt_median_for_graph <- a[, median(-Errors*10000), by = c("Year", "Month")]
dt_median_for_graph[, "Month" := as.numeric(Month)]
dt_median_for_graph[, "zoodate" := as.yearmon(paste(Year, Month), "%Y %m")]
setorderv(dt_median_for_graph, c("Year", "Month"))
Sys.setlocale("LC_ALL","English")
png("TimeSeriesPrivateMedian.png", width = 700, height = 400)
ggplot(dt_median_for_graph, aes(y=V1, x=zoodate)) + 
  geom_bar(stat = "identity")  + theme_ipsum()+
  ylab("Error") + xlab("Date")
dev.off()

mean(m$Errors)
png("TimeSeriesPrivateRobust.png", width = 700, height = 400)
ggplot(dt_averaged_for_graph, aes(y=V1, x=zoodate)) + 
  geom_bar(stat = "identity")  + theme_ipsum()+
  ylab("Error") + xlab("Date")
dev.off()
a <- readRDS("DifferentialsTable.rds")
mean(a$Errors)
dt_averaged_for_graph <- a[, mean(-Errors), by = c("Year", "Month")]
dt_averaged_for_graph[, "Month" := as.numeric(Month)]
dt_averaged_for_graph[, "zoodate" := as.yearmon(paste(Year, Month), "%Y %m")]
Sys.setlocale("LC_ALL","English")
png("TimeSeriesState.png", width = 700, height = 400)
ggplot(dt_averaged_for_graph, aes(y=V1, x=zoodate)) + 
  geom_bar(stat = "identity")  + theme_ipsum()+
  ylab("Error") + xlab("Date")
dev.off()

dt_median_for_graph <- a[, median(-Errors*10000), by = c("Year", "Month")]
dt_median_for_graph[, "Month" := as.numeric(Month)]
dt_median_for_graph[, "zoodate" := as.yearmon(paste(Year, Month), "%Y %m")]
Sys.setlocale("LC_ALL","English")
png("TimeSeriesStateMedianFirstHalf.png", width = 700, height = 400)
ggplot(dt_median_for_graph[Year <=2018], aes(y=V1, x=zoodate)) + 
  geom_bar(stat = "identity")  + theme_ipsum()+
  ylab("Error") + xlab("Date")
dev.off()
png("TimeSeriesStateMedian.png", width = 700, height = 400)
ggplot(dt_median_for_graph, aes(y=V1, x=zoodate)) + 
  geom_bar(stat = "identity")  + theme_ipsum()+
  ylab("Error") + xlab("Date")
dev.off()

dt_median_for_graph
#confidence intervals
f<- -a[(Year == 2020)&(Month == 2)]$Errors*10000
CI_t <- function (x, ci = 0.95)
{
  `%>%` <- magrittr::`%>%`
  Margin_Error <- qt(ci + (1 - ci)/2, df = length(x) - 1) * sd(x)/sqrt(length(x))
  df_out <- data.frame( sample_size=length(x), Mean=mean(x), sd=sd(x),
                        Margin_Error=Margin_Error,
                        'CI lower limit'=(mean(x) - Margin_Error),
                        'CI Upper limit'=(mean(x) + Margin_Error)) %>%
    tidyr::pivot_longer(names_to = "Measurements", values_to ="values", 1:6 )
  return(df_out)
}

ConfIntMy <- function(f){
r <- CI_t(f)
lower <- r$values[5]
upper <- r$values[6]
meanmy <-  r$values[2]
return(list("lower" = lower, "upper" = upper, "mean" = meanmy))
}
a[, "Lower" := ConfIntMy(-Errors*10000)$lower, by = c("Year", "Month")]
a[, "Upper" := ConfIntMy(-Errors*10000)$upper, by = c("Year", "Month")]
a[, "Average" := ConfIntMy(-Errors*10000)$mean, by = c("Year", "Month")]
dt_confint <- a[, c("Year", "Month", "Lower", "Upper", "Average")]
dt_confint <- unique(dt_confint, by = c("Year", "Month"))
Sys.setlocale("LC_ALL","English")
dt_confint[, "zoodate" := as.yearmon(paste(Year, Month), "%Y %m")]

png("TimeSeriesStateConfInt.png", width = 700, height = 400)
ggplot(dt_confint) +
  geom_line( aes(x=zoodate, y=Average), color="black", size=1, alpha=0.9, linetype=1)+
  geom_line( aes(x=zoodate, y=Lower), color="blue", size=1, alpha=0.9, linetype=2)+
  geom_line( aes(x=zoodate, y=Upper), color="red", size=1, alpha=0.9, linetype=2)+
  geom_hline(yintercept = 0)+
  theme_ipsum()+
  xlab("Date")+
  ylab("Differential")
dev.off()
png("TimeSeriesStateConfIntSmall.png", width = 700, height = 400)
ggplot(dt_confint[(Year <=2018)&(Year >=2016)]) +
  geom_line( aes(x=zoodate, y=Average), color="black", size=1, alpha=0.9, linetype=1)+
  geom_line( aes(x=zoodate, y=Lower), color="blue", size=1, alpha=0.9, linetype=2)+
  geom_line( aes(x=zoodate, y=Upper), color="red", size=1, alpha=0.9, linetype=2)+
  geom_hline(yintercept = 0)+
  theme_ipsum()+
  xlab("Date")+
  ylab("Differential")
dev.off()

a <- readRDS("DifferentialsRobustness.rds")
dt_averaged_for_graph <- a[, mean(-Errors), by = c("Year", "Month")]
dt_averaged_for_graph[, "Month" := as.numeric(Month)]
dt_averaged_for_graph[, "zoodate" := as.yearmon(paste(Year, Month), "%Y %m")]
Sys.setlocale("LC_ALL","English")
png("TimeSeriesStateRobust.png", width = 700, height = 400)
ggplot(dt_averaged_for_graph, aes(y=V1, x=zoodate)) + 
  geom_bar(stat = "identity")  + theme_ipsum()+
  ylab("Error") + xlab("Date")
dev.off()
for (i in 2015:2020){
  i <- 2020
  png(paste("TimeSeriesState", i, ".png"), width = 700, height = 400)
  ggplot(m_averages[Year == i], aes(y=V1, x=paste(Month))) + ylim(0, max(m_averages$V1))+
    geom_bar(stat = "identity") + scale_x_discrete(breaks = breaks)+ theme_ipsum()+
    ylab("Error") + xlab("Date")
  dev.off()
  
}

m
png(paste("ResidualsModel.png"), width = 700, height = 400)

ggplot(data.table("Residuals" = modelsvm$best.model$residuals), aes(x=log(Residuals))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
dev.off()

png(paste("ErrorsDist.png"), width = 700, height = 400)

ggplot(m, aes(x=-log(Errors))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
dev.off()



m[, mean(-Errors), by = "Year"]
m[, "Year" := sapply(m$Year, mapyear)]
m[, "Month" := sapply(m$Month, mapmonth)]
saveRDS(m, "finalDifferential.rds")

#study the time series ###
Differentials <- readRDS("DifferentialsTable.rds")
Residuals <- readRDS("ResidualsTable.rds")


library(fdatest)
library(dplyr)
library(tidyr)
library(zoo)

#Differentials
Shares_of_NAs <- matrix(NA,1,12)
p_values <- list()
Differentials_FDA_2015 <- Differentials[Year == 2015, c("ISIN", "Year", "Month", "Errors")]
Differentials_FDA_2015[, "Errors" := -Errors]
Differentials_FDA_2015[, "Period":=paste(Year, Month, sep = "_")]
Differentials_FDA_2015[, c("Year", "Month") := NULL]
Differentials_FDA_2015 <- spread(Differentials_FDA_2015,key = "Period", value = "Errors" )
Differentials_FDA_2015 <- Differentials_FDA_2015[, -"ISIN"]
Share_NAs <- transpose(Differentials_FDA_2015[, lapply(.SD, function(x){sum(is.na(x))/length(x)})])$V1
Shares_of_NAs <- rbind(Shares_of_NAs, Share_NAs)
Differentials_FDA_2015 <- transpose(Differentials_FDA_2015)
Differentials_FDA_2015[, "Index" := 1:.N]
l <- length(Differentials_FDA_2015)
Differentials_FDA_2015 <- zoo(Differentials_FDA_2015)
index(Differentials_FDA_2015) <- Differentials_FDA_2015[,l]
Differentials_FDA_2015 <- na.approx(Differentials_FDA_2015)
Differentials_FDA_2015 <- as.data.table(Differentials_FDA_2015)
Differentials_FDA_2015 <- Differentials_FDA_2015[, lapply(.SD, interpolater_borders)]
Differentials_FDA_2015 <- as.data.frame(Differentials_FDA_2015)
Differentials_FDA_2015 <- Differentials_FDA_2015[ , apply(Differentials_FDA_2015, 2, function(x) !any(is.na(x)))]
Differentials_FDA_2015$Index <- NULL
Differentials_FDA_2015 <- transpose(Differentials_FDA_2015)
Differentials_FDA_2015 <- as.data.table(Differentials_FDA_2015)
fdatest_diff_alt_2015 <- ITP1bspline(Differentials_FDA_2015, mu = 0)
p_values[ifelse(length(p_values) + 1 > 1,length(p_values) + 1,1)] <- fdatest_diff_alt_2015$corrected.pval
fdatest_diff_alt_2015$corrected.pval
plot.ITP1(fdatest_diff_alt_2015)
ITPimage(fdatest_diff_alt_2015)

Differentials_FDA_2016 <- Differentials[Year == 2016, c("ISIN", "Year", "Month", "Errors")]
Differentials_FDA_2016[, "Errors" := -Errors]
Differentials_FDA_2016[, "Period":=paste(Year, Month, sep = "_")]
Differentials_FDA_2016[, c("Year", "Month") := NULL]
Differentials_FDA_2016 <- spread(Differentials_FDA_2016,key = "Period", value = "Errors" )
Differentials_FDA_2016 <- Differentials_FDA_2016[, -"ISIN"]
Share_NAs <- transpose(Differentials_FDA_2016[, lapply(.SD, function(x){sum(is.na(x))/length(x)})])$V1
Shares_of_NAs <- rbind(Shares_of_NAs, Share_NAs)
Differentials_FDA_2016 <- transpose(Differentials_FDA_2016)
Differentials_FDA_2016[, "Index" := 1:.N]
l <- length(Differentials_FDA_2016)
Differentials_FDA_2016 <- zoo(Differentials_FDA_2016)
index(Differentials_FDA_2016) <- Differentials_FDA_2016[,l]
Differentials_FDA_2016 <- na.approx(Differentials_FDA_2016)
Differentials_FDA_2016 <- as.data.table(Differentials_FDA_2016)
Differentials_FDA_2016 <- Differentials_FDA_2016[, lapply(.SD, interpolater_borders)]
Differentials_FDA_2016 <- as.data.frame(Differentials_FDA_2016)
Differentials_FDA_2016 <- Differentials_FDA_2016[ , apply(Differentials_FDA_2016, 2, function(x) !any(is.na(x)))]
Differentials_FDA_2016$Index <- NULL
Differentials_FDA_2016 <- transpose(Differentials_FDA_2016)
Differentials_FDA_2016 <- as.data.table(Differentials_FDA_2016)
#check the fuck
#Differentials_FDA_2016[, "Index" := 1:.N]
#Differentials_FDA_2016[V12 < -0.1,]
#Differentials_FDA_2016[41]

fdatest_diff_alt_2016 <- ITP1bspline(Differentials_FDA_2016, mu = 0)
p_values[ifelse(length(p_values) + 1 > 1,length(p_values) + 1,1)] <- fdatest_diff_alt_2016$corrected.pval

Differentials_FDA_2017 <- Differentials[Year == 2017, c("ISIN", "Year", "Month", "Errors")]
Differentials_FDA_2017[, "Errors" := -Errors]
Differentials_FDA_2017[, "Period":=paste(Year, Month, sep = "_")]
Differentials_FDA_2017[, c("Year", "Month") := NULL]
Differentials_FDA_2017 <- spread(Differentials_FDA_2017,key = "Period", value = "Errors" )
Differentials_FDA_2017 <- Differentials_FDA_2017[, -"ISIN"]
Share_NAs <- transpose(Differentials_FDA_2017[, lapply(.SD, function(x){sum(is.na(x))/length(x)})])$V1
Shares_of_NAs <- rbind(Shares_of_NAs, Share_NAs)
Differentials_FDA_2017 <- transpose(Differentials_FDA_2017)
Differentials_FDA_2017[, "Index" := 1:.N]
l <- length(Differentials_FDA_2017)
Differentials_FDA_2017 <- zoo(Differentials_FDA_2017)
index(Differentials_FDA_2017) <- Differentials_FDA_2017[,l]
Differentials_FDA_2017 <- na.approx(Differentials_FDA_2017)
Differentials_FDA_2017 <- as.data.table(Differentials_FDA_2017)
Differentials_FDA_2017 <- Differentials_FDA_2017[, lapply(.SD, interpolater_borders)]
#
Differentials_FDA_2017 <- as.data.frame(Differentials_FDA_2017)
Differentials_FDA_2017 <- Differentials_FDA_2017[ , apply(Differentials_FDA_2017, 2, function(x) !any(is.na(x)))]
Differentials_FDA_2017$Index <- NULL
Differentials_FDA_2017 <- transpose(Differentials_FDA_2017)
Differentials_FDA_2017 <- as.data.table(Differentials_FDA_2017)
fdatest_diff_alt_2017 <- ITP1bspline(Differentials_FDA_2017, mu = 0)
p_values[ifelse(length(p_values) + 1 > 1,length(p_values) + 1,1)] <- fdatest_diff_alt_2017$corrected.pval

Differentials_FDA_2018 <- Differentials[Year == 2018, c("ISIN", "Year", "Month", "Errors")]
Differentials_FDA_2018[, "Errors" := -Errors]
Differentials_FDA_2018[, "Period":=paste(Year, Month, sep = "_")]
Differentials_FDA_2018[, c("Year", "Month") := NULL]
Differentials_FDA_2018 <- spread(Differentials_FDA_2018,key = "Period", value = "Errors" )
Differentials_FDA_2018 <- Differentials_FDA_2018[, -"ISIN"]
Share_NAs <- transpose(Differentials_FDA_2018[, lapply(.SD, function(x){sum(is.na(x))/length(x)})])$V1
Shares_of_NAs <- rbind(Shares_of_NAs, Share_NAs)
Differentials_FDA_2018 <- transpose(Differentials_FDA_2018)
Differentials_FDA_2018[, "Index" := 1:.N]
l <- length(Differentials_FDA_2018)
Differentials_FDA_2018 <- zoo(Differentials_FDA_2018)
index(Differentials_FDA_2018) <- Differentials_FDA_2018[,l]
Differentials_FDA_2018 <- na.approx(Differentials_FDA_2018)
Differentials_FDA_2018 <- as.data.table(Differentials_FDA_2018)
Differentials_FDA_2018 <- Differentials_FDA_2018[, lapply(.SD, interpolater_borders)]
Differentials_FDA_2018 <- as.data.frame(Differentials_FDA_2018)
Differentials_FDA_2018 <- Differentials_FDA_2018[ , apply(Differentials_FDA_2018, 2, function(x) !any(is.na(x)))]
Differentials_FDA_2018$Index <- NULL
Differentials_FDA_2018 <- transpose(Differentials_FDA_2018)
Differentials_FDA_2018 <- as.data.table(Differentials_FDA_2018)
fdatest_diff_alt_2018 <- ITP1bspline(Differentials_FDA_2018, mu = 0)
p_values[ifelse(length(p_values) + 1 > 1,length(p_values) + 1,1)] <- fdatest_diff_alt_2018$corrected.pval

Differentials_FDA_2019 <- Differentials[Year == 2019, c("ISIN", "Year", "Month", "Errors")]
Differentials_FDA_2019[, "Errors" := -Errors]
Differentials_FDA_2019[, "Period":=paste(Year, Month, sep = "_")]
Differentials_FDA_2019[, c("Year", "Month") := NULL]
Differentials_FDA_2019 <- spread(Differentials_FDA_2019,key = "Period", value = "Errors" )
Differentials_FDA_2019 <- Differentials_FDA_2019[, -"ISIN"]
Share_NAs <- transpose(Differentials_FDA_2019[, lapply(.SD, function(x){sum(is.na(x))/length(x)})])$V1
Shares_of_NAs <- rbind(Shares_of_NAs, Share_NAs)
Differentials_FDA_2019 <- transpose(Differentials_FDA_2019)
Differentials_FDA_2019[, "Index" := 1:.N]
l <- length(Differentials_FDA_2019)
Differentials_FDA_2019 <- zoo(Differentials_FDA_2019)
index(Differentials_FDA_2019) <- Differentials_FDA_2019[,l]
Differentials_FDA_2019 <- na.approx(Differentials_FDA_2019)
Differentials_FDA_2019 <- as.data.table(Differentials_FDA_2019)
Differentials_FDA_2019 <- Differentials_FDA_2019[, lapply(.SD, interpolater_borders)]
Differentials_FDA_2019 <- as.data.frame(Differentials_FDA_2019)
Differentials_FDA_2019 <- Differentials_FDA_2019[ , apply(Differentials_FDA_2019, 2, function(x) !any(is.na(x)))]
Differentials_FDA_2019$Index <- NULL
Differentials_FDA_2019 <- transpose(Differentials_FDA_2019)
Differentials_FDA_2019 <- as.data.table(Differentials_FDA_2019)
fdatest_diff_alt_2019 <- ITP1bspline(Differentials_FDA_2019, mu = 0)
p_values[ifelse(length(p_values) + 1 > 1,length(p_values) + 1,1)] <- fdatest_diff_alt_2019$corrected.pval

Differentials_FDA_2020 <- Differentials[Year == 2020, c("ISIN", "Year", "Month", "Errors")]
Differentials_FDA_2020[, "Errors" := -Errors]
Differentials_FDA_2020[, "Period":=paste(Year, Month, sep = "_")]
Differentials_FDA_2020[, c("Year", "Month") := NULL]
Differentials_FDA_2020 <- spread(Differentials_FDA_2020,key = "Period", value = "Errors" )
Differentials_FDA_2020 <- Differentials_FDA_2020[, -"ISIN"]
Share_NAs <- transpose(Differentials_FDA_2020[, lapply(.SD, function(x){sum(is.na(x))/length(x)})])$V1
Shares_of_NAs <- rbind(Shares_of_NAs, Share_NAs)
Differentials_FDA_2020 <- transpose(Differentials_FDA_2020)
Differentials_FDA_2020[, "Index" := 1:.N]
l <- length(Differentials_FDA_2020)
Differentials_FDA_2020 <- zoo(Differentials_FDA_2020)
index(Differentials_FDA_2020) <- Differentials_FDA_2020[,l]
Differentials_FDA_2020 <- na.approx(Differentials_FDA_2020)
Differentials_FDA_2020 <- as.data.table(Differentials_FDA_2020)
Differentials_FDA_2020 <- Differentials_FDA_2020[, lapply(.SD, interpolater_borders)]
Differentials_FDA_2020 <- as.data.frame(Differentials_FDA_2020)
Differentials_FDA_2020 <- Differentials_FDA_2020[ , apply(Differentials_FDA_2020, 2, function(x) !any(is.na(x)))]
Differentials_FDA_2020$Index <- NULL
Differentials_FDA_2020 <- transpose(Differentials_FDA_2020)
Differentials_FDA_2020 <- as.data.table(Differentials_FDA_2020)
fdatest_diff_alt_2020 <- ITP1bspline(Differentials_FDA_2020, mu = 0)
p_values[ifelse(length(p_values) + 1 > 1,length(p_values) + 1,1)] <- fdatest_diff_alt_2020$corrected.pval

saveRDS(list(fdatest_diff_alt_2015, fdatest_diff_alt_2016, fdatest_diff_alt_2017,fdatest_diff_alt_2018,fdatest_diff_alt_2019,Shares_of_NAs),"Tests_and_NAs.rds")
saveRDS(list(Share_NAs, fdatest_diff_alt_2020), "file2020.rds")
ITPimage(fdatest_diff_alt_2017)


Differentials_FDA_2016 <- Differentials[Year == 2016, c("ISIN", "Year", "Month", "Errors")]
Differentials_FDA_2016[, "Errors" := -Errors]
Differentials_FDA_2016[, "Period":=paste(Year, Month, sep = "_")]
Differentials_FDA_2016[, c("Year", "Month") := NULL]
Differentials_FDA_2016 <- spread(Differentials_FDA_2016,key = "Period", value = "Errors" )
Differentials_FDA_2016 <- Differentials_FDA_2016[, -"ISIN"]
Share_NAs <- transpose(Differentials_FDA_2016[, lapply(.SD, function(x){sum(is.na(x))/length(x)})])$V1
Shares_of_NAs <- rbind(Shares_of_NAs, Share_NAs)
Differentials_FDA_2016 <- transpose(Differentials_FDA_2016)
Differentials_FDA_2016[, "Index" := 1:.N]
l <- length(Differentials_FDA_2016)
Differentials_FDA_2016 <- zoo(Differentials_FDA_2016)
index(Differentials_FDA_2016) <- Differentials_FDA_2016[,l]
Differentials_FDA_2016 <- na.approx(Differentials_FDA_2016)
Differentials_FDA_2016 <- as.data.table(Differentials_FDA_2016)
Differentials_FDA_2016 <- Differentials_FDA_2016[, lapply(.SD, interpolater_borders)]
Differentials_FDA_2016 <- as.data.frame(Differentials_FDA_2016)
Differentials_FDA_2016 <- Differentials_FDA_2016[ , apply(Differentials_FDA_2016, 2, function(x) !any(is.na(x)))]
Differentials_FDA_2016$Index <- NULL
Differentials_FDA_2016 <- transpose(Differentials_FDA_2016)
Differentials_FDA_2016 <- as.data.table(Differentials_FDA_2016)
Differentials_FDA_2016 <- Differentials_FDA_2016[order(V12)][2:57]
fdatest_diff_alt_2016 <- ITP1bspline(Differentials_FDA_2016, mu = 0)
p_values[ifelse(length(p_values) + 1 > 1,length(p_values) + 1,1)] <- fdatest_diff_alt_2016$corrected.pval
ITPimage(fdatest_diff_alt_2016)

Differentials_FDA_2017 <- Differentials[Year == 2017, c("ISIN", "Year", "Month", "Errors")]
Differentials_FDA_2017[, "Errors" := -Errors]
Differentials_FDA_2017[, "Period":=paste(Year, Month, sep = "_")]
Differentials_FDA_2017[, c("Year", "Month") := NULL]
Differentials_FDA_2017 <- spread(Differentials_FDA_2017,key = "Period", value = "Errors" )
Differentials_FDA_2017 <- Differentials_FDA_2017[, -"ISIN"]
Share_NAs <- transpose(Differentials_FDA_2017[, lapply(.SD, function(x){sum(is.na(x))/length(x)})])$V1
Shares_of_NAs <- rbind(Shares_of_NAs, Share_NAs)
Differentials_FDA_2017 <- transpose(Differentials_FDA_2017)
Differentials_FDA_2017[, "Index" := 1:.N]
l <- length(Differentials_FDA_2017)
Differentials_FDA_2017 <- zoo(Differentials_FDA_2017)
index(Differentials_FDA_2017) <- Differentials_FDA_2017[,l]
Differentials_FDA_2017 <- na.approx(Differentials_FDA_2017)
Differentials_FDA_2017 <- as.data.table(Differentials_FDA_2017)
Differentials_FDA_2017 <- Differentials_FDA_2017[, lapply(.SD, interpolater_borders)]
Differentials_FDA_2017 <- as.data.frame(Differentials_FDA_2017)
Differentials_FDA_2017 <- Differentials_FDA_2017[ , apply(Differentials_FDA_2017, 2, function(x) !any(is.na(x)))]
Differentials_FDA_2017$Index <- NULL
Differentials_FDA_2017 <- transpose(Differentials_FDA_2017)
Differentials_FDA_2017 <- as.data.table(Differentials_FDA_2017)
Differentials_FDA_2017 <- Differentials_FDA_2017[order(V12)][2:50]
fdatest_diff_alt_2017 <- ITP1bspline(Differentials_FDA_2017, mu = 0)
p_values[ifelse(length(p_values) + 1 > 1,length(p_values) + 1,1)] <- fdatest_diff_alt_2017$corrected.pval
ITPimage(fdatest_diff_alt_2017)

rbind(fdatest_diff_alt_2016$corrected.pval,fdatest_diff_alt_2017$corrected.pval)

x <- seq(1,30,1)
y <- 15*x + rnorm(30,0-15*x,20)
functions <- matrix(y,1)
for (i in 1:19){
  y <- ifelse(x<15,15*x + rnorm(30,0-15*x,20), 15*x + rnorm(30,0,20))
  functions <- rbind(functions, y)
}

functions[,1] <- x[1:length(functions[,1])]
m <- ITP1bspline(functions, mu = 0)
ITPimage(m)
m$corrected.pval
#fdatest_diff <- ITP1fourier(Differentials_FDA_2016[, -"Index"], mu = 0)
fdatest_diff_alt <- ITP1bspline(Differentials_FDA_2016[, -"Index"], mu = 0)
#fdatest_diff$corrected.pval
fdatest_diff_alt$corrected.pval
ITPimage(fdatest_diff_alt, alpha = 0.01)
fdatest_diff_alt$coeff

summary(fdatest_diff_alt)
png("ITPFourier2016.png")
plot.ITP1(fdatest_diff_alt, ylim=c(-1,1), alpha1 = 0.01)
dev.off()




Differentials_FDA <- Differentials[, c("ISIN", "Year", "Month", "Errors")]
Differentials_FDA[, "Period":=paste(Year, Month, sep = "_")]
Differentials_FDA[, c("Year", "Month") := NULL]
Differentials_FDA <- spread(Differentials_FDA,key = "Period", value = "Errors" )
Differentials_FDA <- Differentials_FDA[, -"ISIN"]
Share_NAs <- transpose(Differentials_FDA[, lapply(.SD, function(x){sum(is.na(x))/length(x)})])$V1
Differentials_FDA <- transpose(Differentials_FDA)
Differentials_FDA[, "Index" := 1:.N]
l <- length(Differentials_FDA)
Differentials_FDA <- zoo(Differentials_FDA)
index(Differentials_FDA) <- Differentials_FDA[,l]
Differentials_FDA <- na.approx(Differentials_FDA)
Differentials_FDA <- as.data.table(Differentials_FDA)
Differentials_FDA <- Differentials_FDA[, lapply(.SD, interpolater_borders)]
Differentials_FDA <- as.data.frame(Differentials_FDA)
Differentials_FDA <- Differentials_FDA[ , apply(Differentials_FDA, 2, function(x) !any(is.na(x)))]
Differentials_FDA <- as.data.table(Differentials_FDA)

#fdatest_diff <- ITP1fourier(Differentials_FDA[, -"Index"], mu = 0)
fdatest_diff_alt <- ITP1bspline(Differentials_FDA[, -"Index"], mu = 0)
#fdatest_diff$corrected.pval
fdatest_diff_alt$corrected.pval

png("ITPFourier2016.png")
plot.ITP1(fdatest_diff_alt, ylim=c(0,0.5), alpha1 = 0.01, par = 1)
dev.off()


#import the tests and nas shares and try to get images
tests <- readRDS("Tests_and_NAs.rds")
tests2020 <- readRDS("file2020.rds")
NAs_shares <- tests[[6]]
NAs_shares <- as.data.table(NAs_shares)
NAs_shares <- NAs_shares[!is.na(V1)]
NAs_shares[, "Year" := (1:.N)+2014]


fdatest_diff_alt_2015 <- tests[[1]]
fdatest_diff_alt_2016 <- tests[[2]]
fdatest_diff_alt_2017 <- tests[[3]]
fdatest_diff_alt_2018 <- tests[[4]]
fdatest_diff_alt_2019 <- tests[[5]]
fdatest_diff_alt_2020 <- tests2020[[2]]

fdatest_diff_alt_2016$corrected.pval

plot.ITP1(fdatest_diff_alt_2016, alpha2 = 0.01)
ITPimage(fdatest_diff_alt_2016)
fdatest_diff_alt_2020$corrected.pval

pvalues <- do.call(cbind, list(fdatest_diff_alt_2015$corrected.pval,
        fdatest_diff_alt_2016$corrected.pval,
        fdatest_diff_alt_2017$corrected.pval,
        fdatest_diff_alt_2018$corrected.pval,
        fdatest_diff_alt_2019$corrected.pval,
        fdatest_diff_alt_2020$corrected.pval))
colnames(pvalues) <- c(2015:2020)
pvalues <- as.data.table(pvalues)

NAs_shares[, "Year" := NULL]
NAs_shares <- transpose(NAs_shares)
colnames(NAs_shares) <- as.character(c(2015:2020))


write.xlsx(pvalues,"Pvalues_fda_no_adjustments.xlsx")
write.xlsx(NAs_shares,"NAs_shares_fda.xlsx")


#Residuals
Residuals <- readRDS("ResidualsTable.rds")


Shares_of_NAs <- matrix(NA,1,12)
p_values <- list()
Residuals_FDA_2015 <- Residuals[Year == 2015, c("ISIN", "Year", "Month", "Errors")]
Residuals_FDA_2015[, "Errors" := -Errors]
Residuals_FDA_2015[, "Period":=paste(Year, Month, sep = "_")]
Residuals_FDA_2015[, c("Year", "Month") := NULL]
Residuals_FDA_2015 <- spread(Residuals_FDA_2015,key = "Period", value = "Errors" )
Residuals_FDA_2015 <- Residuals_FDA_2015[, -"ISIN"]
Share_NAs <- transpose(Residuals_FDA_2015[, lapply(.SD, function(x){sum(is.na(x))/length(x)})])$V1
Shares_of_NAs <- rbind(Shares_of_NAs, Share_NAs)
Residuals_FDA_2015 <- transpose(Residuals_FDA_2015)
Residuals_FDA_2015[, "Index" := 1:.N]
l <- length(Residuals_FDA_2015)
Residuals_FDA_2015 <- zoo(Residuals_FDA_2015)
index(Residuals_FDA_2015) <- Residuals_FDA_2015[,l]
Residuals_FDA_2015 <- na.approx(Residuals_FDA_2015)
Residuals_FDA_2015 <- as.data.table(Residuals_FDA_2015)
Residuals_FDA_2015 <- Residuals_FDA_2015[, lapply(.SD, interpolater_borders)]
Residuals_FDA_2015 <- as.data.frame(Residuals_FDA_2015)
Residuals_FDA_2015 <- Residuals_FDA_2015[ , apply(Residuals_FDA_2015, 2, function(x) !any(is.na(x)))]
Residuals_FDA_2015$Index <- NULL
Residuals_FDA_2015 <- transpose(Residuals_FDA_2015)
Residuals_FDA_2015 <- as.data.table(Residuals_FDA_2015)
fdatest_resid_alt_2015 <- ITP1bspline(Residuals_FDA_2015, mu = 0)
p_values[ifelse(length(p_values) + 1 > 1,length(p_values) + 1,1)] <- fdatest_resid_alt_2015$corrected.pval
fdatest_resid_alt_2015$corrected.pval
plot.ITP1(fdatest_resid_alt_2015)
ITPimage(fdatest_resid_alt_2015)

Residuals_FDA_2016 <- Residuals[Year == 2016, c("ISIN", "Year", "Month", "Errors")]
Residuals_FDA_2016[, "Errors" := -Errors]
Residuals_FDA_2016[, "Period":=paste(Year, Month, sep = "_")]
Residuals_FDA_2016[, c("Year", "Month") := NULL]
Residuals_FDA_2016 <- spread(Residuals_FDA_2016,key = "Period", value = "Errors" )
Residuals_FDA_2016 <- Residuals_FDA_2016[, -"ISIN"]
Share_NAs <- transpose(Residuals_FDA_2016[, lapply(.SD, function(x){sum(is.na(x))/length(x)})])$V1
Shares_of_NAs <- rbind(Shares_of_NAs, Share_NAs)
Residuals_FDA_2016 <- transpose(Residuals_FDA_2016)
Residuals_FDA_2016[, "Index" := 1:.N]
l <- length(Residuals_FDA_2016)
Residuals_FDA_2016 <- zoo(Residuals_FDA_2016)
index(Residuals_FDA_2016) <- Residuals_FDA_2016[,l]
Residuals_FDA_2016 <- na.approx(Residuals_FDA_2016)
Residuals_FDA_2016 <- as.data.table(Residuals_FDA_2016)
Residuals_FDA_2016 <- Residuals_FDA_2016[, lapply(.SD, interpolater_borders)]
Residuals_FDA_2016 <- as.data.frame(Residuals_FDA_2016)
Residuals_FDA_2016 <- Residuals_FDA_2016[ , apply(Residuals_FDA_2016, 2, function(x) !any(is.na(x)))]
Residuals_FDA_2016$Index <- NULL
Residuals_FDA_2016 <- transpose(Residuals_FDA_2016)
Residuals_FDA_2016 <- as.data.table(Residuals_FDA_2016)
Residuals_FDA_2016[V12 < -0.05]
fdatest_resid_alt_2016 <- ITP1bspline(Residuals_FDA_2016, mu = 0)
p_values[ifelse(length(p_values) + 1 > 1,length(p_values) + 1,1)] <- fdatest_resid_alt_2016$corrected.pval

Residuals_FDA_2017 <- Residuals[Year == 2017, c("ISIN", "Year", "Month", "Errors")]
Residuals_FDA_2017[, "Errors" := -Errors]
Residuals_FDA_2017[, "Period":=paste(Year, Month, sep = "_")]
Residuals_FDA_2017[, c("Year", "Month") := NULL]
Residuals_FDA_2017 <- spread(Residuals_FDA_2017,key = "Period", value = "Errors" )
Residuals_FDA_2017 <- Residuals_FDA_2017[, -"ISIN"]
Share_NAs <- transpose(Residuals_FDA_2017[, lapply(.SD, function(x){sum(is.na(x))/length(x)})])$V1
Shares_of_NAs <- rbind(Shares_of_NAs, Share_NAs)
Residuals_FDA_2017 <- transpose(Residuals_FDA_2017)
Residuals_FDA_2017[, "Index" := 1:.N]
l <- length(Residuals_FDA_2017)
Residuals_FDA_2017 <- zoo(Residuals_FDA_2017)
index(Residuals_FDA_2017) <- Residuals_FDA_2017[,l]
Residuals_FDA_2017 <- na.approx(Residuals_FDA_2017)
Residuals_FDA_2017 <- as.data.table(Residuals_FDA_2017)
Residuals_FDA_2017 <- Residuals_FDA_2017[, lapply(.SD, interpolater_borders)]
Residuals_FDA_2017 <- as.data.frame(Residuals_FDA_2017)
Residuals_FDA_2017 <- Residuals_FDA_2017[ , apply(Residuals_FDA_2017, 2, function(x) !any(is.na(x)))]
Residuals_FDA_2017$Index <- NULL
Residuals_FDA_2017 <- transpose(Residuals_FDA_2017)
Residuals_FDA_2017 <- as.data.table(Residuals_FDA_2017)
fdatest_resid_alt_2017 <- ITP1bspline(Residuals_FDA_2017, mu = 0)
p_values[ifelse(length(p_values) + 1 > 1,length(p_values) + 1,1)] <- fdatest_resid_alt_2017$corrected.pval


Residuals_FDA_2018 <- Residuals[Year == 2018, c("ISIN", "Year", "Month", "Errors")]
Residuals_FDA_2018[, "Errors" := -Errors]
Residuals_FDA_2018[, "Period":=paste(Year, Month, sep = "_")]
Residuals_FDA_2018[, c("Year", "Month") := NULL]
Residuals_FDA_2018 <- spread(Residuals_FDA_2018,key = "Period", value = "Errors" )
Residuals_FDA_2018 <- Residuals_FDA_2018[, -"ISIN"]
Share_NAs <- transpose(Residuals_FDA_2018[, lapply(.SD, function(x){sum(is.na(x))/length(x)})])$V1
Shares_of_NAs <- rbind(Shares_of_NAs, Share_NAs)
Residuals_FDA_2018 <- transpose(Residuals_FDA_2018)
Residuals_FDA_2018[, "Index" := 1:.N]
l <- length(Residuals_FDA_2018)
Residuals_FDA_2018 <- zoo(Residuals_FDA_2018)
index(Residuals_FDA_2018) <- Residuals_FDA_2018[,l]
Residuals_FDA_2018 <- na.approx(Residuals_FDA_2018)
Residuals_FDA_2018 <- as.data.table(Residuals_FDA_2018)
Residuals_FDA_2018 <- Residuals_FDA_2018[, lapply(.SD, interpolater_borders)]
Residuals_FDA_2018 <- as.data.frame(Residuals_FDA_2018)
Residuals_FDA_2018 <- Residuals_FDA_2018[ , apply(Residuals_FDA_2018, 2, function(x) !any(is.na(x)))]
Residuals_FDA_2018$Index <- NULL
Residuals_FDA_2018 <- transpose(Residuals_FDA_2018)
Residuals_FDA_2018 <- as.data.table(Residuals_FDA_2018)
fdatest_resid_alt_2018 <- ITP1bspline(Residuals_FDA_2018, mu = 0)
p_values[ifelse(length(p_values) + 1 > 1,length(p_values) + 1,1)] <- fdatest_resid_alt_2018$corrected.pval

Residuals_FDA_2019 <- Residuals[Year == 2019, c("ISIN", "Year", "Month", "Errors")]
Residuals_FDA_2019[, "Errors" := -Errors]
Residuals_FDA_2019[, "Period":=paste(Year, Month, sep = "_")]
Residuals_FDA_2019[, c("Year", "Month") := NULL]
Residuals_FDA_2019 <- spread(Residuals_FDA_2019,key = "Period", value = "Errors" )
Residuals_FDA_2019 <- Residuals_FDA_2019[, -"ISIN"]
Share_NAs <- transpose(Residuals_FDA_2019[, lapply(.SD, function(x){sum(is.na(x))/length(x)})])$V1
Shares_of_NAs <- rbind(Shares_of_NAs, Share_NAs)
Residuals_FDA_2019 <- transpose(Residuals_FDA_2019)
Residuals_FDA_2019[, "Index" := 1:.N]
l <- length(Residuals_FDA_2019)
Residuals_FDA_2019 <- zoo(Residuals_FDA_2019)
index(Residuals_FDA_2019) <- Residuals_FDA_2019[,l]
Residuals_FDA_2019 <- na.approx(Residuals_FDA_2019)
Residuals_FDA_2019 <- as.data.table(Residuals_FDA_2019)
Residuals_FDA_2019 <- Residuals_FDA_2019[, lapply(.SD, interpolater_borders)]
Residuals_FDA_2019 <- as.data.frame(Residuals_FDA_2019)
Residuals_FDA_2019 <- Residuals_FDA_2019[ , apply(Residuals_FDA_2019, 2, function(x) !any(is.na(x)))]
Residuals_FDA_2019$Index <- NULL
Residuals_FDA_2019 <- transpose(Residuals_FDA_2019)
Residuals_FDA_2019 <- as.data.table(Residuals_FDA_2019)
fdatest_resid_alt_2019 <- ITP1bspline(Residuals_FDA_2019, mu = 0)
p_values[ifelse(length(p_values) + 1 > 1,length(p_values) + 1,1)] <- fdatest_resid_alt_2019$corrected.pval

Residuals_FDA_2020 <- Residuals[Year == 2020, c("ISIN", "Year", "Month", "Errors")]
Residuals_FDA_2020[, "Errors" := -Errors]
Residuals_FDA_2020[, "Period":=paste(Year, Month, sep = "_")]
Residuals_FDA_2020[, c("Year", "Month") := NULL]
Residuals_FDA_2020 <- spread(Residuals_FDA_2020,key = "Period", value = "Errors" )
Residuals_FDA_2020 <- Residuals_FDA_2020[, -"ISIN"]
Share_NAs <- transpose(Residuals_FDA_2020[, lapply(.SD, function(x){sum(is.na(x))/length(x)})])$V1
Shares_of_NAs <- rbind(Shares_of_NAs, Share_NAs)
Residuals_FDA_2020 <- transpose(Residuals_FDA_2020)
Residuals_FDA_2020[, "Index" := 1:.N]
l <- length(Residuals_FDA_2020)
Residuals_FDA_2020 <- zoo(Residuals_FDA_2020)
index(Residuals_FDA_2020) <- Residuals_FDA_2020[,l]
Residuals_FDA_2020 <- na.approx(Residuals_FDA_2020)
Residuals_FDA_2020 <- as.data.table(Residuals_FDA_2020)
Residuals_FDA_2020 <- Residuals_FDA_2020[, lapply(.SD, interpolater_borders)]
Residuals_FDA_2020 <- as.data.frame(Residuals_FDA_2020)
Residuals_FDA_2020 <- Residuals_FDA_2020[ , apply(Residuals_FDA_2020, 2, function(x) !any(is.na(x)))]
Residuals_FDA_2020$Index <- NULL
Residuals_FDA_2020 <- transpose(Residuals_FDA_2020)
Residuals_FDA_2020 <- as.data.table(Residuals_FDA_2020)
fdatest_resid_alt_2020 <- ITP1bspline(Residuals_FDA_2020, mu = 0)
p_values[ifelse(length(p_values) + 1 > 1,length(p_values) + 1,1)] <- fdatest_resid_alt_2020$corrected.pval

saveRDS(list(fdatest_resid_alt_2015, fdatest_resid_alt_2016, fdatest_resid_alt_2017,fdatest_resid_alt_2018,fdatest_resid_alt_2019,Shares_of_NAs),"Tests_and_NAs_resids.rds")
saveRDS(list(Share_NAs, fdatest_resid_alt_2020), "file2020_resids.rds")
ITPimage(fdatest_resid_alt_2017)

#import the tests and nas shares and try to get images, residuals
tests <- readRDS("Tests_and_NAs_resids.rds")
tests2020 <- readRDS("file2020_resids.rds")
NAs_shares <- tests[[6]]
NAs_shares <- as.data.table(NAs_shares)
NAs_shares <- NAs_shares[!is.na(V1)]
NAs_shares[, "Year" := (1:.N)+2014]


fdatest_resid_alt_2015 <- tests[[1]]
fdatest_resid_alt_2016 <- tests[[2]]
fdatest_resid_alt_2017 <- tests[[3]]
fdatest_resid_alt_2018 <- tests[[4]]
fdatest_resid_alt_2019 <- tests[[5]]
fdatest_resid_alt_2020 <- tests2020[[2]]

fdatest_resid_alt_2016$corrected.pval

plot.ITP1(fdatest_resid_alt_2016, alpha2 = 0.01)
ITPimage(fdatest_resid_alt_2016)
fdatest_resid_alt_2020$corrected.pval

pvalues <- do.call(cbind, list(fdatest_resid_alt_2015$corrected.pval,
                               fdatest_resid_alt_2016$corrected.pval,
                               fdatest_resid_alt_2017$corrected.pval,
                               fdatest_resid_alt_2018$corrected.pval,
                               fdatest_resid_alt_2019$corrected.pval,
                               fdatest_resid_alt_2020$corrected.pval))
colnames(pvalues) <- c(2015:2020)
pvalues <- as.data.table(pvalues)

NAs_shares[, "Year" := NULL]
NAs_shares <- transpose(NAs_shares)
colnames(NAs_shares) <- as.character(c(2015:2020))


write.xlsx(pvalues,"Pvalues_fda_no_adjustments_resids.xlsx")
write.xlsx(NAs_shares,"NAs_shares_fda_resids.xlsx")

#t-tests ####

#Differentials
Differentials <- readRDS("DifferentialsTable.rds")
Differentials

years_t_tests <- c()
names(years_t_tests) <- NULL
for (i in 2015:2020){
  Differentials_T_2015 <- Differentials[Year == i, c("Month", "Errors")]
  Differentials_T_2015[, "Errors" := -Errors]
  m <- t.test(Differentials_T_2015$Errors, alternative = "greater", mu = 0)
  newnames <- c(names(years_t_tests), as.character(i))
  years_t_tests <- c(years_t_tests, m$p.value)
  names(years_t_tests) <- newnames
}

write.xlsx(years_t_tests, "ttests_years.xlsx")
months_t_tests <- list()

for (i in 2015:2020){
  this_year_t_tests <- c()
  for (s in 1:12){
  Differentials_T_2015 <- Differentials[(Year == i)&(Month == s), c("Month", "Errors")]
  Differentials_T_2015[, "Errors" := -Errors]
  m <- t.test(Differentials_T_2015$Errors, alternative = "greater", mu = 0)
  this_year_t_tests <- c(this_year_t_tests, m$p.value)
  }
  months_t_tests[[i-2014]] <- this_year_t_tests
}
months_t_tests <- do.call(cbind, months_t_tests)
write.xlsx(months_t_tests, "ttests_months.xlsx")

Differentials_T <- Differentials[, c("Month", "Errors")]
Differentials_T[, "Errors" := -Errors]
m <- t.test(Differentials_T$Errors, alternative = "greater", mu = 0)
m$p.value
#many tests
shares_of_acceptances <- c()
for (j in seq(0,0.5,0.01)){
months_t_tests <- c()
for (i in 2015:2020){
  this_year_t_tests <- c()
  for (s in 1:12){
    Differentials_T_2015 <- Differentials[(Year == i)&(Month == s), c("Month", "Errors")]
    Differentials_T_2015[, "Errors" := -Errors]
    m <- t.test(Differentials_T_2015$Errors, alternative = "two.sided", mu = j)
  this_year_t_tests <- c(this_year_t_tests, m$p.value)
    # m <- mean(Differentials_T_2015$Errors)
    # this_year_t_tests <- c(this_year_t_tests, m)
  }
  months_t_tests <- c(months_t_tests, this_year_t_tests)
}
shares_of_acceptances<- c(shares_of_acceptances, length(months_t_tests[months_t_tests >0.05])/length(months_t_tests))
}
#many-many tests
months_t_tests <- list()
months_p_values <- list()
for (n in 2015:2020){
  this_year_t_tests <- c()
  this_year_ps <- c()
  for (l in 1:12){
    this_month_ps <- c()
    for (j in seq(0,0.5,0.01)){
      
    Differentials_T_2015 <- Differentials[(Year == n)&(Month == l), c("Month", "Errors")]
    Differentials_T_2015[, "Errors" := -Errors]
    m <- t.test(Differentials_T_2015$Errors, alternative = "two.sided", mu = j)
    this_month_ps <- c(this_month_ps, m$p.value)
    }
    this_year_t_tests <- c(this_year_t_tests, which.max(this_month_ps)*0.01)
    this_year_ps <- c(this_year_ps, max(this_month_ps))
    
  }
  months_p_values[[n-2014]] <- this_year_ps
  
  months_t_tests[[n-2014]] <- this_year_t_tests
}
do.call(cbind,months_p_values)
write.xlsx(do.call(cbind,months_t_tests), "OptimalMeanValues.xlsx")
#Residuals

Residuals

years_t_tests <- c()
names(years_t_tests) <- NULL
for (i in 2015:2020){
  Residuals_T_2015 <- Residuals[Year == i, c("Month", "Errors")]
  Residuals_T_2015[, "Errors" := -Errors]
  m <- t.test(Residuals_T_2015$Errors, alternative = "greater", mu = 0)
  newnames <- c(names(years_t_tests), as.character(i))
  years_t_tests <- c(years_t_tests, m$p.value)
  names(years_t_tests) <- newnames
}

write.xlsx(years_t_tests, "ttests_years_resids.xlsx")
months_t_tests <- list()

for (i in 2015:2020){
  this_year_t_tests <- c()
  for (s in 1:12){
    Residuals_T_2015 <- Residuals[(Year == i)&(Month == s), c("Month", "Errors")]
    Residuals_T_2015[, "Errors" := -Errors]
    m <- t.test(Residuals_T_2015$Errors, alternative = "greater", mu = 0)
    this_year_t_tests <- c(this_year_t_tests, m$p.value)
  }
  months_t_tests[[i-2014]] <- this_year_t_tests
}
months_t_tests <- do.call(cbind, months_t_tests)
write.xlsx(months_t_tests, "ttests_months_resids.xlsx")

Residuals_T <- Residuals[, c("Month", "Errors")]
Residuals_T[, "Errors" := -Errors]
m <- t.test(Residuals_T$Errors, alternative = "greater", mu = 0)
m$p.value

#issue level to find out the source of outliers#####
suspicious_issues_1 <- Residuals[(Year==2015)&(Month==4)]
plot(suspicious_issues_1$Errors[order(suspicious_issues_1$Errors)])
mean(suspicious_issues_1$Errors)
suspicious_issues_1[order(suspicious_issues_1$Errors)]
suspicious_issues_1[abs(Errors)<0.001, mean(Errors)]
suspicious_issues_1_errors <- 
#causality tests ####

library(fUnitRoots)
library(urca)
library(vars)
library(aod)
library(zoo)
library(tseries)
#the variables download
Differentials <- readRDS("DifferentialsTable.rds")

Currency <- fread("Currency.csv")
Oil <- fread("Oil_Prices.csv")
GDP <- fread("Russia_GDP_monthly.csv")

Banks <- fread("big_spread.csv", colClasses = "numeric")
Retained_earnings <- c(10801,70302,70701, 70702, 70703, 70704, 70705,70715,70801)
for (i in 1:length(Retained_earnings)){
  try({m <- Retained_earnings[i]
  print(m)
  Banks[, "Retained_earnings":= Retained_earnings + get(as.character(m))]})
}
Banks <- Banks[, c("Year", "Month", "REGN", "Retained_earnings")]

InterestTable <- list()
for (i in 2015:2020){
  newtable <- fread(paste("данные по ставочкам/данные по ставочкам/cbr", as.character(i), ".csv", sep = ""))
  InterestTable[[i-2014]] <- newtable 
}
InterestTable <- do.call(rbind, InterestTable)
colnames(InterestTable) <- c("Date", "0.25", "0.5", "0.75", "1", "2", "3", "5", "7", "10", "15", "20", "30")
InterestTable <- InterestTable[-1]
InterestTable[, Date := date <- as.Date(as.character(Date), format = "%d.%m.%Y")]
InterestRate1 <- InterestTable[, c("Date", "1")]
InterestRate10 <- InterestTable[, c("Date", "10")]

MOEX <- fread("moex_data.csv", encoding = "UTF-8")
MOEX[, "TRADEYEAR" := year(TRADEDATE)]
MOEX[, c("HIGH", "LOW", "CAPITALIZATION") := .(as.numeric(str_replace(HIGH, ",", "\\.")), as.numeric(str_replace(LOW, ",", "\\.")), as.numeric(str_replace(CAPITALIZATION, ",", "\\.")))]
MOEX[, "PrevPrice" := shift((HIGH/2 + LOW/2), 1L, NA, "lead")]
MOEX[, "ReturnEquityDay" := ((HIGH/2 + LOW/2) - PrevPrice)/PrevPrice]
MOEX[, "PrevPriceMonth" := shift((HIGH/2 + LOW/2), 30, NA, "lead")]
MOEX[, "ReturnEquityMonth" := ((HIGH/2 + LOW/2) - PrevPriceMonth)/PrevPriceMonth]

YTMs <- fread("Big_medium_rare_table_bonds_UTF.csv", encoding = "UTF-8")
YTMs <- YTMs[ Indicative_yield_type!="Current"]
YTMs <- YTMs[ Exch_name !="Cbonds Estimation"]
YTMs[, "Year" := year(Date_trading)]
YTMs[, "Month" := month(Date_trading)]
YTMs <- YTMs[, c("YTM_ind_main", "Year", "Month")]

#the variables preparation
Differentials <- Differentials[, c("Year", "Month", "Errors")]
Differentials <- Differentials[, mean(Errors, na.rm = T), by = c("Year", "Month")]
alldates <- Differentials[, c("Year", "Month")]

YTMs <- YTMs[, mean(YTM_ind_main, na.rm = T), by = c("Month", "Year")]

MOEX[, "Year" := year(TRADEDATE)]
MOEX[, "Month" := month(TRADEDATE)]
MOEXDay <- MOEX[, c("ReturnEquityDay", "Year", "Month")]
MOEXMonth <- MOEX[, c("ReturnEquityMonth", "Year", "Month")]
MOEXDay <- MOEXDay[, mean(ReturnEquityDay, na.rm = T), by = c("Year", "Month")]
MOEXMonth <- MOEXMonth[, mean(ReturnEquityMonth, na.rm = T), by = c("Year", "Month")]
MOEXMonth <- MOEXMonth[Year <2021]

InterestRate1[, "Year" := year(Date)]
InterestRate1[, "Month" := month(Date)]
InterestRate10[, "Year" := year(Date)]
InterestRate10[, "Month" := month(Date)]
InterestRate1 <- InterestRate1[, mean(as.numeric(str_replace(`1`, ",", "\\.")), na.rm = T), by = c("Year", "Month")]
InterestRate1 <- InterestRate1[!is.na(Year)]
InterestRate10 <- InterestRate10[, mean(as.numeric(str_replace(`10`, ",", "\\.")), na.rm = T), by = c("Year", "Month")]
InterestRate10 <- InterestRate10[!is.na(Year)]



Oil[, "Date" := as.Date(as.character(Date), format = "%m/%d/%Y")]
Oil[, "Year" := year(Date)]
Oil[, "Month" := month(Date)]
Oil <- Oil[Year >= 2015]
Oil <- Oil[, mean((High+Low)/2), by = c("Month", "Year")]
Oil <- Oil[Year<2021]

Currency
Currency[, "data" := as.Date(as.character(data), format = "%d.%m.%Y")]
Currency[, "Year" := year(data)]
Currency[, "Month" := month(data)]
Currency <- Currency[, mean(curs), by = c("Month", "Year")]
Currency <- Currency[Year<2021]

GDP <- fread("Russia_GDP_monthly.csv")
GDP <- GDP[-1]
GDP[, "Date" := as.Date(as.character(V1), format = "%d.%m.%Y")]
GDP[, "Year" := year(Date)]
GDP[, "Month" := month(Date)]
GDP[, "GDPGrowth" := str_remove(V3, "%")]
GDP[, "GDPGrowth" := str_replace(GDPGrowth, ",", ".")]
GDP <- GDP[, mean(as.numeric(GDPGrowth), na.rm = T), by = c("Year", "Month")]
GDP <- merge(GDP, alldates, by = c("Year", "Month"), all = T)
GDP <- GDP[Year < 2021]
GDP <- zoo(GDP)
GDP <- na.approx(GDP)
GDP <- as.data.table(GDP)
GDP[(Year == 2015)&(Month == 3), "V1" := (99.599 - 100.804)/100.804]
GDP[(Year == 2015)&(Month == 1), "V1" := (99.923 - 100.798)/100.798]
GDP[(Year == 2015)&(Month == 2), "V1" := (99.754 - 100.801)/100.801]

BanksNumber <- Banks[, unique(REGN), by = c("Year", "Month")][, .N,by = c("Year", "Month")]
BanksNumber <- BanksNumber[, "prev" := shift(N, 1L)]
BanksNumber[(Year == 2015)&(Month == 1), "prev" := 825]
BanksNumber[, "V1" := N - prev]

BanksEarnings <- Banks[, sum(Retained_earnings), by = c("Year", "Month")]
BanksEarnings <- BanksEarnings[, "prev" := shift(V1, 1L)]
BanksEarnings[(Year == 2015)&(Month == 1), "prev" := 53670382622]
BanksEarnings[, "V1" := (V1-prev)/prev]


#tests ####


#VarStep: GDP####

Differentials
#Test for unit roots
adf.test(Differentials$V1)$p.value
adf.test(diff(Differentials$V1))$p.value
kpss.test(Differentials$V1)$p.value
kpss.test(diff(Differentials$V1,1))$p.value


adf.test(GDP$V1)$p.value
adf.test(diff(GDP$V1,1))$p.value
kpss.test(GDP$V1)$p.value
kpss.test(diff(GDP$V1,1))$p.value

# Since first order differencing eliminates the unit root, the maximum order of integration
# is concluded to be I(1).
Vars <- merge(Differentials, GDP, by = c("Year", "Month"))
colnames(Vars) <- c("Year", "Month", "Differentials", "Variable")
mean(Vars$Variable, na.rm=T)
IQR(Vars$Variable, na.rm=T)
#Set up VAR-Model
#select lag order // either 2 or 6
m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICGDP.xlsx")

#VAR Model, lag=2
V.1<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.1)$serial$p.value
arch.test(V.1, 2,multivariate.only = T)

#VAR-Model, lag=6
V.2<-VAR(Vars[,3:4],p=5,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

#VAR-Model, lag=7
V.3<-VAR(Vars[,3:4],p=7,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 


# Wald-test for the first 6 lags
# The test can be directly done with the VAR model, however using the correct
# variables is a little more tricky

#VAR-Model, lag=7 (additional lag, though not tested)
VFinal<-VAR(Vars[,3:4],p=6,type="both")
VFinal$varresult
summary(VFinal)

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

install.packages("tsDyn")
library(tsDyn)
m <- VECM(Vars[, c(3,4)],1, estim = "ML")
summary(m)
#Wald-test (H0: Robusta does not Granger-cause Arabica)
wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2))
# Could not be rejected (X2=8.6; p=0.2)

#Wald.test (H0: Arabica does not Granger-cause Robusta)
wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1))


VFinal<-VAR(Vars[,3:4],p=7,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4,6,8,10))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3,5,7,9))

#VarStep: MOEXDay####

Differentials
#Test for unit roots
adf.test(Differentials$V1)$p.value
adf.test(diff(Differentials$V1))$p.value
kpss.test(Differentials$V1)$p.value
kpss.test(diff(Differentials$V1,1))$p.value


adf.test(MOEXDay$V1)$p.value
adf.test(diff(MOEXDay$V1,1))$p.value
kpss.test(MOEXDay$V1)$p.value
kpss.test(diff(MOEXDay$V1,1))$p.value

# Since first order differencing eliminates the unit root, the maximum order of integration
# is concluded to be I(1).
Vars <- merge(Differentials, MOEXDay, by = c("Year", "Month"))
colnames(Vars) <- c("Year", "Month", "Differentials", "Variable")

#Set up VAR-Model
#select lag order // either 2 or 6
m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICMOEXDay.xlsx")

#VAR Model, lag=2
V.1<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.1)$serial$p.value

#VAR-Model, lag=3
V.2<-VAR(Vars[,3:4],p=3,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

#VAR-Model, lag=7
V.3<-VAR(Vars[,3:4],p=7,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 


# Wald-test for the first 6 lags
# The test can be directly done with the VAR model, however using the correct
# variables is a little more tricky

#VAR-Model, lag=7 (additional lag, though not tested)
VFinal<-VAR(Vars[,3:4],p=3,type="both")
VFinal$varresult
summary(VFinal)

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

#Wald-test (H0: Robusta does not Granger-cause Arabica)
wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))
# Could not be rejected (X2=8.6; p=0.2)
-5*10^(-2)
#Wald.test (H0: Arabica does not Granger-cause Robusta)
wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))


VFinal<-VAR(Vars[,3:4],p=4,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4,6))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3,5))


#VarStep: MOEXMonth####

Differentials
#Test for unit roots
adf.test(Differentials$V1)$p.value
adf.test(diff(Differentials$V1))$p.value
kpss.test(Differentials$V1)$p.value
kpss.test(diff(Differentials$V1,1))$p.value

MOEXMonth <- MOEXMonth[!is.na(V1)]
Differentials_short <- Differentials[1:71]

adf.test(MOEXMonth$V1)$p.value
adf.test(diff(MOEXMonth$V1,1))$p.value
kpss.test(MOEXMonth$V1)$p.value
kpss.test(diff(MOEXMonth$V1,1))$p.value

# Since first order differencing eliminates the unit root, the maximum order of integration
# is concluded to be I(1).
Vars <- merge(Differentials_short, MOEXMonth, by = c("Year", "Month"))
colnames(Vars) <- c("Year", "Month", "Differentials_short", "Variable")

#Set up VAR-Model
#select lag order // either 2 or 6
m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICMOEXMonth.xlsx")

#VAR Model, lag=2
V.1<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.1)$serial$p.value

#VAR-Model, lag=3
V.2<-VAR(Vars[,3:4],p=3,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

#VAR-Model, lag=7
V.3<-VAR(Vars[,3:4],p=7,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 


# Wald-test for the first 6 lags
# The test can be directly done with the VAR model, however using the correct
# variables is a little more tricky

#VAR-Model, lag=7 (additional lag, though not tested)
VFinal<-VAR(Vars[,3:4],p=3,type="both")
VFinal$varresult
summary(VFinal)

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

#Wald-test (H0: Robusta does not Granger-cause Arabica)
wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))
# Could not be rejected (X2=8.6; p=0.2)

#Wald.test (H0: Arabica does not Granger-cause Robusta)
wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))

VFinal<-VAR(Vars[,3:4],p=4,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4,6))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3,5))


#VarStep: InterestRate1####

Differentials
#Test for unit roots
adf.test(Differentials$V1)$p.value
adf.test(diff(Differentials$V1))$p.value
kpss.test(Differentials$V1)$p.value
kpss.test(diff(Differentials$V1,1))$p.value


adf.test(InterestRate1$V1)$p.value
adf.test(diff(InterestRate1$V1,1))$p.value
kpss.test(InterestRate1$V1)$p.value
kpss.test(diff(InterestRate1$V1,1))$p.value

# Since first order differencing eliminates the unit root, the maximum order of integration
# is concluded to be I(1).
Vars <- merge(Differentials, InterestRate1, by = c("Year", "Month"))
colnames(Vars) <- c("Year", "Month", "Differentials", "Variable")
mean(Vars$Variable, na.rm=T)
IQR(Vars$Variable, na.rm=T)

#Set up VAR-Model
#select lag order // either 2 or 6
m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICInterestRate1.xlsx")

#VAR Model, lag=2
V.1<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.1)$serial$p.value
arch.test(V.1)
#VAR-Model, lag=5
V.2<-VAR(Vars[,3:4],p=5,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

#VAR-Model, lag=7
V.3<-VAR(Vars[,3:4],p=7,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 


# Wald-test for the first 6 lags
# The test can be directly done with the VAR model, however using the correct
# variables is a little more tricky

#VAR-Model, lag=7 (additional lag, though not tested)
VFinal<-VAR(Vars[,3:4],p=3,type="both")
VFinal$varresult
summary(VFinal)
m <- VECM(Vars[, c(3,4)],1, estim = "ML")
summary(m)
jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

#Wald-test (H0: Robusta does not Granger-cause Arabica)
wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))
# Could not be rejected (X2=8.6; p=0.2)

#Wald.test (H0: Arabica does not Granger-cause Robusta)
wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))


VFinal<-VAR(Vars[,3:4],p=4,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4,6))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3,5))

#VarStep: InterestRate10####

Differentials
#Test for unit roots
adf.test(Differentials$V1)$p.value
adf.test(diff(Differentials$V1))$p.value
kpss.test(Differentials$V1)$p.value
kpss.test(diff(Differentials$V1,1))$p.value


adf.test(InterestRate10$V1)$p.value
adf.test(diff(InterestRate10$V1,1))$p.value
kpss.test(InterestRate10$V1)$p.value
kpss.test(diff(InterestRate10$V1,1))$p.value

# Since first order differencing eliminates the unit root, the maximum order of integration
# is concluded to be I(1).
Vars <- merge(Differentials, InterestRate10, by = c("Year", "Month"))
colnames(Vars) <- c("Year", "Month", "Differentials", "Variable")
IQR(Vars$Variable, na.rm=T)

#Set up VAR-Model
#select lag order // either 2 or 6
m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICInterestRate10.xlsx")
mean(Vars$Variable)
#VAR Model, lag=2
V.1<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.1)$serial$p.value
arch.test(V.1)
#VAR-Model, lag=4
V.2<-VAR(Vars[,3:4],p=4,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

#VAR-Model, lag=9
V.3<-VAR(Vars[,3:4],p=9,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 


# Wald-test for the first 6 lags
# The test can be directly done with the VAR model, however using the correct
# variables is a little more tricky

#VAR-Model, lag=7 (additional lag, though not tested)
VFinal<-VAR(Vars[,3:4],p=3,type="both")
VFinal$varresult
summary(VFinal)

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)
m <- VECM(Vars[, c(3,4)],1, estim = "ML")
summary(m)
#Wald-test (H0: Robusta does not Granger-cause Arabica)
wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))
# Could not be rejected (X2=8.6; p=0.2)

#Wald.test (H0: Arabica does not Granger-cause Robusta)
wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))

VFinal<-VAR(Vars[,3:4],p=4,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4,6))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3,5))

#VarStep: Oil####

adf.test(Oil$V1)$p.value
adf.test(diff(Oil$V1,1))$p.value
kpss.test(Oil$V1)$p.value
kpss.test(diff(Oil$V1,1))$p.value

Vars <- merge(Differentials, Oil, by = c("Year", "Month"))
colnames(Vars) <- c("Year", "Month", "Differentials", "Variable")
mean(Vars$Variable, na.rm=T)
IQR(Vars$Variable, na.rm=T)

m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICOil.xlsx")

V.1<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.1)$serial$p.value
arch.test(V.1, lags.single = 2)

V.2<-VAR(Vars[,3:4],p=4,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

V.3<-VAR(Vars[,3:4],p=9,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 

VFinal<-VAR(Vars[,3:4],p=3,type="both")
VFinal$varresult
summary(VFinal)

m <- VECM(Vars[, c(3,4)],1, estim = "ML")
summary(m)

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))

VFinal<-VAR(Vars[,3:4],p=4,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4,6))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3,5))

#VarStep: Currency####

adf.test(Currency$V1)$p.value
adf.test(diff(Currency$V1,1))$p.value
kpss.test(Currency$V1)$p.value
kpss.test(diff(Currency$V1,2))$p.value

Vars <- merge(Differentials, Currency, by = c("Year", "Month"))
colnames(Vars) <- c("Year", "Month", "Differentials", "Variable")

m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICCurrency.xlsx")

V.1<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.1)$serial$p.value

V.2<-VAR(Vars[,3:4],p=3,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

V.3<-VAR(Vars[,3:4],p=6,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 

VFinal<-VAR(Vars[,3:4],p=4,type="both")

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4,6))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3,5))

VFinal<-VAR(Vars[,3:4],p=5,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4,6,8))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3,5,7))

#VarStep: YTMs####

adf.test(YTMs$V1)$p.value
adf.test(diff(YTMs$V1,1))$p.value
kpss.test(YTMs$V1)$p.value
kpss.test(diff(YTMs$V1,2))$p.value

Vars <- merge(Differentials, YTMs, by = c("Year", "Month"))
colnames(Vars) <- c("Year", "Month", "Differentials", "Variable")

m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICYTMs.xlsx")

V.1<-VAR(Vars[,3:4],p=1,type="both")
serial.test(V.1)$serial$p.value

V.2<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

V.3<-VAR(Vars[,3:4],p=4,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 

VFinal<-VAR(Vars[,3:4],p=2,type="both")

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1))

VFinal<-VAR(Vars[,3:4],p=3,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))


#VarStep: BanksNumber####

adf.test(BanksNumber$V1)$p.value
adf.test(diff(BanksNumber$V1,1))$p.value
kpss.test(BanksNumber$V1)$p.value
kpss.test(diff(BanksNumber$V1,2))$p.value
BanksNumber <- BanksNumber[, c("Year", "Month", "V1")]

Vars <- merge(Differentials, BanksNumber[,c("Year", "Month", "V1")], by = c("Year", "Month"))
colnames(Vars) <- c("Year", "Month", "Differentials", "Variable")

m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICBanksNumber.xlsx")

V.1<-VAR(Vars[,3:4],p=1,type="both")
serial.test(V.1)$serial$p.value

V.2<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

V.3<-VAR(Vars[,3:4],p=4,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 

VFinal<-VAR(Vars[,3:4],p=3,type="both")

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))

VFinal<-VAR(Vars[,3:4],p=4,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4,6))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3,5))

#VarStep: BanksEarnings####

adf.test(BanksEarnings$V1)$p.value
adf.test(diff(BanksEarnings$V1,1))$p.value
kpss.test(BanksEarnings$V1)$p.value
kpss.test(diff(BanksEarnings$V1,2))$p.value

Vars <- merge(Differentials, BanksEarnings[,c("Year", "Month", "V1")], by = c("Year", "Month"))
colnames(Vars) <- c("Year", "Month", "Differentials", "Variable")

m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICBanksEarnings.xlsx")

V.1<-VAR(Vars[,3:4],p=1,type="both")
serial.test(V.1)$serial$p.value

V.2<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

V.3<-VAR(Vars[,3:4],p=4,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 

VFinal<-VAR(Vars[,3:4],p=5,type="both")

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

VFinal<-VAR(Vars[,3:4],p=5,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4,6,8))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3,5,7))

VFinal<-VAR(Vars[,3:4],p=6,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4,6,8,10))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3,5,7,9))


#VarStep: GDPOil####

Differentials
#Test for unit roots
adf.test(Differentials$V1)$p.value
adf.test(diff(Differentials$V1))$p.value
kpss.test(Differentials$V1)$p.value
kpss.test(diff(Differentials$V1,1))$p.value


adf.test(GDP$V1)$p.value
adf.test(diff(GDP$V1,1))$p.value
kpss.test(GDP$V1)$p.value
kpss.test(diff(GDP$V1,1))$p.value

# Since first order differencing eliminates the unit root, the maximum order of integration
# is concluded to be I(1).
Vars <- merge(Differentials, GDP, by = c("Year", "Month"))
Vars <- merge(Vars, Oil, by = c("Year", "Month"))
colnames(Vars) <- c("Year", "Month", "Differentials", "GDP", "Oil")

#Set up VAR-Model
#select lag order // either 2 or 6
m <- VARselect(Vars[,3:5],lag=10,type="both")
m
write.xlsx(m$criteria, "AICGDPOil.xlsx")

#VAR Model, lag=2
V.1<-VAR(Vars[,3:5],p=2,type="both")
serial.test(V.1)$serial$p.value

#VAR-Model, lag=7
V.3<-VAR(Vars[,3:5],p=5,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 


# Wald-test for the first 6 lags
# The test can be directly done with the VAR model, however using the correct
# variables is a little more tricky

#VAR-Model, lag=7 (additional lag, though not tested)
VFinal<-VAR(Vars[,3:5],p=6,type="both")
VFinal$varresult

#oil>diff
wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(3,6,9,12,15))
#GDP>diff
wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,5,8,11,14))
#oil>GDP
wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms=c(3,6,9,12,15))
#diff>GDP
wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms=c(1,4,7,10,13))
#GDP>oil
wald.test(b=coef(VFinal$varresult[[3]]), Sigma=vcov(VFinal$varresult[[3]]), Terms=c(2,5,8,11,14))
#diff>oil
wald.test(b=coef(VFinal$varresult[[3]]), Sigma=vcov(VFinal$varresult[[3]]), Terms=c(1,4,7,10,13))


VFinal<-VAR(Vars[,3:5],p=2,type="both")
VFinal$varresult

#oil>diff
wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(3))
#GDP>diff
wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2))
#oil>GDP
wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms=c(3))
#diff>GDP
wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms=c(1))
#GDP>oil
wald.test(b=coef(VFinal$varresult[[3]]), Sigma=vcov(VFinal$varresult[[3]]), Terms=c(2))
#diff>oil
wald.test(b=coef(VFinal$varresult[[3]]), Sigma=vcov(VFinal$varresult[[3]]), Terms=c(1))

#Bankwise Differential ####
Differentials <- readRDS("DifferentialsTable.rds")
AggrBanksTable <- fread("AggregatedBanksTable.csv", colClasses = "numeric")
AggrBanksTable <- AggrBanksTable[, c("REGN","Year","Month","Equity", "ROE", "InterbankShare", "CorporateShare","GeneralLeverage","SecuritiesLeverage")]
SampleTable <- CleanData(smpl_dt, "G_spread_interpolated", Average = T, State = T)$Table
SampleTable[, "ISIN" := CleanData(smpl_dt, "G_spread_interpolated", Average = T, State = T)$ISIN]
BondsChars <- SampleTable[, c("REGN","ISIN", "Year", "Month", "Currency", "Callable", "Days_to_maturity", "BA_spread", "Coupon")]
MergedBanksBondsChars <- merge(BondsChars, AggrBanksTable, by = c("REGN", "Year", "Month"), all.x = T)
#Create categories table####
CategoriesTable <- MergedBanksBondsChars[, c("REGN", "Year", "Month", "ISIN")]
CategoriesTable[, "CurrencyCategory" := ifelse(MergedBanksBondsChars$Currency == "RUB",0,ifelse((MergedBanksBondsChars$Currency == "USD")|(MergedBanksBondsChars$Currency == "EUR"),1,2))]
CategoriesTable[, "CallabilityCategory" := ifelse(MergedBanksBondsChars$Callable>0.5,1,0)]
CategoriesTable[, "MaturityCategory" := ifelse(MergedBanksBondsChars$Days_to_maturity <= quantile(MergedBanksBondsChars$Days_to_maturity, probs = c(0.33,0.67))[1],0,ifelse(MergedBanksBondsChars$Days_to_maturity <= quantile(MergedBanksBondsChars$Days_to_maturity, probs = c(0.33,0.67))[2],1,2))]
CategoriesTable[, "LiquidityCategory" := ifelse(MergedBanksBondsChars$BA_spread <= quantile(MergedBanksBondsChars$BA_spread, probs = c(0.33,0.67))[1],0,ifelse(MergedBanksBondsChars$BA_spread <= quantile(MergedBanksBondsChars$BA_spread, probs = c(0.33,0.67))[2],1,2))]
CategoriesTable[, "CouponCategory" := ifelse(MergedBanksBondsChars$Coupon <= quantile(MergedBanksBondsChars$Coupon, probs = c(0.33,0.67))[1],0,ifelse(MergedBanksBondsChars$Coupon <= quantile(MergedBanksBondsChars$Coupon, probs = c(0.33,0.67))[2],1,2))]
CategoriesTable[, "SizeCategory" := ifelse(MergedBanksBondsChars$Equity <= quantile(MergedBanksBondsChars$Equity, probs = c(0.33,0.67))[1],0,ifelse(MergedBanksBondsChars$Equity <= quantile(MergedBanksBondsChars$Equity, probs = c(0.33,0.67))[2],1,2))]
CategoriesTable[, "EfficiencyCategory" := ifelse(MergedBanksBondsChars$ROE <= quantile(MergedBanksBondsChars$ROE, probs = c(0.33,0.67), na.rm=T)[1],0,ifelse(MergedBanksBondsChars$ROE <= quantile(MergedBanksBondsChars$ROE, probs = c(0.33,0.67), na.rm=T)[2],1,2))]
CategoriesTable[, "InterbankShareCategory" := ifelse(MergedBanksBondsChars$InterbankShare <= quantile(MergedBanksBondsChars$InterbankShare, probs = c(0.33,0.67), na.rm=T)[1],0,ifelse(MergedBanksBondsChars$InterbankShare <= quantile(MergedBanksBondsChars$InterbankShare, probs = c(0.33,0.67), na.rm=T)[2],1,2))]
CategoriesTable[, "CorporateShareCategory" := ifelse(MergedBanksBondsChars$CorporateShare <= quantile(MergedBanksBondsChars$CorporateShare, probs = c(0.33,0.67), na.rm=T)[1],0,ifelse(MergedBanksBondsChars$CorporateShare <= quantile(MergedBanksBondsChars$CorporateShare, probs = c(0.33,0.67), na.rm=T)[2],1,2))]
CategoriesTable[, "GLeverageCategory" := ifelse(MergedBanksBondsChars$GeneralLeverage <= quantile(MergedBanksBondsChars$GeneralLeverage, probs = c(0.33,0.67), na.rm=T)[1],0,ifelse(MergedBanksBondsChars$GeneralLeverage <= quantile(MergedBanksBondsChars$GeneralLeverage, probs = c(0.33,0.67), na.rm=T)[2],1,2))]
CategoriesTable[, "SLeverageCategory" := ifelse(MergedBanksBondsChars$SecuritiesLeverage <= quantile(MergedBanksBondsChars$Securities, probs = c(0.33,0.67), na.rm=T)[1],0,ifelse(MergedBanksBondsChars$SecuritiesLeverage <= quantile(MergedBanksBondsChars$SecuritiesLeverage, probs = c(0.33,0.67), na.rm=T)[2],1,2))]
CategoriesTable[, "REGN" := NULL]
CategoriesTable <- merge(CategoriesTable, Differentials, by = c("ISIN", "Year", "Month"), all.y = T)
CategoriesTable[, "Predictions" := NULL]
CategoriesTable[, "YTMCategory" := ifelse(CategoriesTable$Variable <= quantile(CategoriesTable$Variable, probs = c(0.33,0.67), na.rm=T)[1],0,ifelse(CategoriesTable$Variable <= quantile(CategoriesTable$Variable, probs = c(0.33,0.67), na.rm=T)[2],1,2))]
CategoriesTable[, "Variable" := NULL]

CategoriesTable[(SizeCategory == 2)&(InterbankShareCategory == 0)]
#Create categories graph#####
library(ggplot2)
library(tidyr)
CategoriesTableStrange <- gather(CategoriesTable, key = "Metrics", value = "Group", -c((1:3),15))
CategoriesTableStrange <- as.data.table(CategoriesTableStrange)
CategoriesTableStrange[, "Errors" := -Errors]
CategoriesTableStrange_mean <- CategoriesTableStrange[, mean(Errors), by = c("Metrics", "Group")]
CategoriesTableStrange_mean <- CategoriesTableStrange_mean[!is.na(Group)]

png("CategoriesBar.png", width = 700, height = 400)
ggplot(CategoriesTableStrange_mean, aes(fill=as.factor(Group), y=V1, x=Metrics)) + 
  geom_bar(position="dodge", stat="identity") + theme_ipsum() + ylab("Average Differential") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +scale_fill_discrete(name = "Sector")
dev.off()

#regression for differentials#####
library(AER)
library(plm)
library(stargazer)

MergedTotalTable <- merge(MergedBanksBondsChars, Differentials, by = c("ISIN", "Year", "Month"), all.y = T)
MergedTotalTable[, "Errors" := -Errors]
MergedTotalTable[, "Callable" := ifelse(Callable>0.5,1,0)]
MergedTotalTable[,"Date" :=  as.yearmon(paste(Year, Month), "%Y %m")]
MergedTotalTable[is.infinite(ROE), "ROE" :=1]
MergedTotalTable <- MergedTotalTable[Equity>0]
MergedTotalTable <- MergedTotalTable[InterbankShare>=0]
MergedTotalTable <- MergedTotalTable[SecuritiesLeverage>=0]
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
m <- coeftest(fatal_fe_mod)
var(MergedTotalTable$Errors)^(1/2)
as.data.table(m[])
write.xlsx(as.data.table(m[]), "RegressionCategories.xlsx")


MergedTotalTable
MergedTotalTable_norm <- znorm_table(MergedTotalTable)
fatal_fe_mod_log <- plm(Errors ~Currency + Callable + Days_to_maturity +
                          BA_spread + Coupon + Equity + ROE + InterbankShare +
                          CorporateShare + GeneralLeverage + SecuritiesLeverage, 
                    data = MergedTotalTable_norm,
                    index = c("ISIN", "Date"), 
                    model = "within")
coeftest(fatal_fe_mod_log)
m <- coeftest(fatal_fe_mod_log)
write.xlsx(as.data.table(m[]), "RegressionCategoriesLog.xlsx")

fatal_fe_mod_log <- lm(Errors ~Currency + Callable + Days_to_maturity +
                          BA_spread + Coupon + Equity + ROE + InterbankShare +
                          CorporateShare + GeneralLeverage + SecuritiesLeverage, 
                        data = MergedTotalTable_norm)
coeftest(fatal_fe_mod_log)
m <-anova(fatal_fe_mod_log)
var_explained <- 100*m$`Sum Sq`/sum(m$`Sum Sq`)



#share of explained variance
#total
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#currency
fatal_fe_mod <- plm(Errors ~ Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#callable
fatal_fe_mod <- plm(Errors ~ Currency + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#maturity
fatal_fe_mod <- plm(Errors ~ Currency + Callable + 
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#spread
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#Coupon
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#equity
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon  + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#roe
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity)  + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#InterbankShare
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + 
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#CorporateShare
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#GeneralLeverage
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#SecuritiesLeverage
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
anova()
#regression for residuals ####
#Bankwise Differential ####
Residuals <- readRDS("ResidualsTableOutOfSample.rds")
Residuals[, "Year" := sapply(Year, mapyear, years = Residuals$Year)]
Residuals[, "Month" := sapply(Month, mapmonth, months = Residuals$Month)]
AggrBanksTable <- fread("AggregatedBanksTable.csv", colClasses = "numeric")
AggrBanksTable <- AggrBanksTable[, c("REGN","Year","Month","Equity", "ROE", "InterbankShare", "CorporateShare","GeneralLeverage","SecuritiesLeverage")]
SampleTable <- CleanData(smpl_dt, "G_spread_interpolated", Average = T, State = F)$Table
SampleTable[, "ISIN" := CleanData(smpl_dt, "G_spread_interpolated", Average = T, State = F)$ISIN]
BondsChars <- SampleTable[, c("REGN","ISIN", "Year", "Month", "Currency", "Callable", "Days_to_maturity", "BA_spread", "Coupon")]
MergedBanksBondsChars <- merge(BondsChars, AggrBanksTable, by = c("REGN", "Year", "Month"), all.x = T)

#Create categories table
CategoriesTable <- MergedBanksBondsChars[, c("REGN", "Year", "Month", "ISIN")]
CategoriesTable[, "CurrencyCategory" := ifelse(MergedBanksBondsChars$Currency == "RUB",0,ifelse((MergedBanksBondsChars$Currency == "USD")|(MergedBanksBondsChars$Currency == "EUR"),1,2))]
CategoriesTable[, "CallabilityCategory" := ifelse(MergedBanksBondsChars$Callable>0.5,1,0)]
CategoriesTable[, "MaturityCategory" := ifelse(MergedBanksBondsChars$Days_to_maturity <= quantile(MergedBanksBondsChars$Days_to_maturity, probs = c(0.33,0.67))[1],0,ifelse(MergedBanksBondsChars$Days_to_maturity <= quantile(MergedBanksBondsChars$Days_to_maturity, probs = c(0.33,0.67))[2],1,2))]
CategoriesTable[, "LiquidityCategory" := ifelse(MergedBanksBondsChars$BA_spread <= quantile(MergedBanksBondsChars$BA_spread, probs = c(0.33,0.67))[1],0,ifelse(MergedBanksBondsChars$BA_spread <= quantile(MergedBanksBondsChars$BA_spread, probs = c(0.33,0.67))[2],1,2))]
CategoriesTable[, "CouponCategory" := ifelse(MergedBanksBondsChars$Coupon <= quantile(MergedBanksBondsChars$Coupon, probs = c(0.33,0.67))[1],0,ifelse(MergedBanksBondsChars$Coupon <= quantile(MergedBanksBondsChars$Coupon, probs = c(0.33,0.67))[2],1,2))]
CategoriesTable[, "SizeCategory" := ifelse(MergedBanksBondsChars$Equity <= quantile(MergedBanksBondsChars$Equity, probs = c(0.33,0.67), na.rm = T)[1],0,ifelse(MergedBanksBondsChars$Equity <= quantile(MergedBanksBondsChars$Equity, probs = c(0.33,0.67), na.rm = T)[2],1,2))]
CategoriesTable[, "EfficiencyCategory" := ifelse(MergedBanksBondsChars$ROE <= quantile(MergedBanksBondsChars$ROE, probs = c(0.33,0.67), na.rm=T)[1],0,ifelse(MergedBanksBondsChars$ROE <= quantile(MergedBanksBondsChars$ROE, probs = c(0.33,0.67), na.rm=T)[2],1,2))]
CategoriesTable[, "InterbankShareCategory" := ifelse(MergedBanksBondsChars$InterbankShare <= quantile(MergedBanksBondsChars$InterbankShare, probs = c(0.33,0.67), na.rm=T)[1],0,ifelse(MergedBanksBondsChars$InterbankShare <= quantile(MergedBanksBondsChars$InterbankShare, probs = c(0.33,0.67), na.rm=T)[2],1,2))]
CategoriesTable[, "CorporateShareCategory" := ifelse(MergedBanksBondsChars$CorporateShare <= quantile(MergedBanksBondsChars$CorporateShare, probs = c(0.33,0.67), na.rm=T)[1],0,ifelse(MergedBanksBondsChars$CorporateShare <= quantile(MergedBanksBondsChars$CorporateShare, probs = c(0.33,0.67), na.rm=T)[2],1,2))]
CategoriesTable[, "GLeverageCategory" := ifelse(MergedBanksBondsChars$GeneralLeverage <= quantile(MergedBanksBondsChars$GeneralLeverage, probs = c(0.33,0.67), na.rm=T)[1],0,ifelse(MergedBanksBondsChars$GeneralLeverage <= quantile(MergedBanksBondsChars$GeneralLeverage, probs = c(0.33,0.67), na.rm=T)[2],1,2))]
CategoriesTable[, "SLeverageCategory" := ifelse(MergedBanksBondsChars$SecuritiesLeverage <= quantile(MergedBanksBondsChars$Securities, probs = c(0.33,0.67), na.rm=T)[1],0,ifelse(MergedBanksBondsChars$SecuritiesLeverage <= quantile(MergedBanksBondsChars$SecuritiesLeverage, probs = c(0.33,0.67), na.rm=T)[2],1,2))]
CategoriesTable[, "REGN" := NULL]
CategoriesTable <- merge(CategoriesTable, Residuals, by = c("ISIN", "Year", "Month"), all.y = T)
CategoriesTable[, "Predictions" := NULL]
CategoriesTable[, "YTMCategory" := ifelse(CategoriesTable$Variable <= quantile(CategoriesTable$Variable, probs = c(0.33,0.67), na.rm=T)[1],0,ifelse(CategoriesTable$Variable <= quantile(CategoriesTable$Variable, probs = c(0.33,0.67), na.rm=T)[2],1,2))]
CategoriesTable[, "Variable" := NULL]

CategoriesTable[(SizeCategory == 2)&(InterbankShareCategory == 0)]
library(AER)
library(plm)
library(stargazer)
Residuals <- readRDS("ResidualsTableOutOfSample.rds")


MergedTotalTable <- merge(MergedBanksBondsChars, Residuals, by = c("ISIN", "Year", "Month"), all.y = T)
MergedTotalTable[, "Errors" := -Errors]
MergedTotalTable[, "Callable" := ifelse(Callable>0.5,1,0)]
MergedTotalTable[,"Date" :=  as.yearmon(paste(Year, Month), "%Y %m")]
MergedTotalTable[is.infinite(ROE), "ROE" :=1]
MergedTotalTable <- MergedTotalTable[Equity>0]
MergedTotalTable <- MergedTotalTable[InterbankShare>=0]
MergedTotalTable <- MergedTotalTable[SecuritiesLeverage>=0]
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
m <- coeftest(fatal_fe_mod)

as.data.table(m[])
write.xlsx(as.data.table(m[]), "RegressionCategoriesResiduals.xlsx")

MergedTotalTable_norm <- znorm_table(MergedTotalTable)
fatal_fe_mod_log <- plm(Errors ~Currency + Callable + Days_to_maturity +
                          BA_spread + Coupon + Equity + ROE + InterbankShare +
                          CorporateShare + GeneralLeverage + SecuritiesLeverage, 
                        data = MergedTotalTable_norm,
                        index = c("ISIN", "Date"), 
                        model = "within")
coeftest(fatal_fe_mod_log)
m <- coeftest(fatal_fe_mod_log)

write.xlsx(as.data.table(m[]), "RegressionCategoriesLogResiduals.xlsx")

#share of explained variance
#total
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#currency
fatal_fe_mod <- plm(Errors ~ Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#callable
fatal_fe_mod <- plm(Errors ~ Currency + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#maturity
fatal_fe_mod <- plm(Errors ~ Currency + Callable + 
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#spread
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#Coupon
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#equity
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon  + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#roe
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity)  + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#InterbankShare
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + 
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#CorporateShare
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                       log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#GeneralLeverage
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)
#SecuritiesLeverage
fatal_fe_mod <- plm(Errors ~ Currency + Callable + Days_to_maturity +
                      BA_spread + Coupon + log(Equity) + ROE + log(InterbankShare) +
                      log(CorporateShare) + log(GeneralLeverage) + log(SecuritiesLeverage), 
                    data = MergedTotalTable,
                    index = c("ISIN", "Date"), 
                    model = "within")
summary.plm.full(fatal_fe_mod)

#Equity analysis####
Differentials <- readRDS("DifferentialsTable.rds")
SBER <- fread("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/Data_shares/SBER_150101_201231.csv", encoding = "UTF-8", colClasses = "numeric")
colnames(SBER) <- c("Name", "Per", "Date","Time", "Price", "Volume")
SBER <- SBER[, c("Date", "Price", "Volume")]
SBER[, "Date" := as.Date(as.character(Date), format = "%Y%m%d")]
SBER[, "Month" := month(Date)]
SBER[, "Year" := year(Date)]
SBERVol <- SBER[, mean(Volume), by = c("Year", "Month")]
SBERPrice <- SBER[, mean(Price), by = c("Year", "Month")]

USBN <- fread("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/Data_shares/USBN_150101_201231.csv", encoding = "UTF-8")
colnames(USBN) <- c("Name", "Per", "Date","Time", "Price", "Volume")
USBN <- USBN[, c("Date", "Price", "Volume")]
USBN[, "Date" := as.Date(as.character(Date), format = "%Y%m%d")]
USBN[, "Month" := month(Date)]
USBN[, "Year" := year(Date)]
USBNVol <- USBN[, mean(Volume), by = c("Year", "Month")]
USBNPrice <- USBN[, mean(Price), by = c("Year", "Month")]

VTBR <- fread("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/Data_shares/VTBR_150101_201231.csv", encoding = "UTF-8", colClasses = "numeric")
colnames(VTBR) <- c("Name", "Per", "Date","Time", "Price", "Volume")
VTBR <- VTBR[, c("Date", "Price", "Volume")]
VTBR[, "Date" := as.Date(as.character(Date), format = "%Y%m%d")]
VTBR[, "Month" := month(Date)]
VTBR[, "Year" := year(Date)]
VTBRVol <- VTBR[, mean(Volume), by = c("Year", "Month")]
VTBRPrice <- VTBR[, mean(Price), by = c("Year", "Month")]

ROSB <- fread("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/Data_shares/ROSB_150101_201231.csv", encoding = "UTF-8")
colnames(ROSB) <- c("Name", "Per", "Date","Time", "Price", "Volume")
ROSB <- ROSB[, c("Date", "Price", "Volume")]
ROSB[, "Date" := as.Date(as.character(Date), format = "%Y%m%d")]
ROSB[, "Month" := month(Date)]
ROSB[, "Year" := year(Date)]
ROSBVol <- ROSB[, mean(Volume), by = c("Year", "Month")]
ROSBPrice <- ROSB[, mean(Price), by = c("Year", "Month")]

BigRawTableBonds <- fread("Big_medium_rare_table_bonds_UTF.csv", encoding = "UTF-8")
BigRawTableBonds <- BigRawTableBonds[, c("ISIN", "cregnum")]
BigRawTableBonds[, "cregnum" := str_remove(cregnum, "\\s")]
BigRawTableBonds[ ISIN == "RU000A0ZZ5A8"]
SBERISIN <- BigRawTableBonds[cregnum == 1481]
SBERISIN <- unique(SBERISIN, by = "ISIN")
SBERBonds <- merge(SBERISIN, Differentials, by = "ISIN")
SBERBonds[,c("cregnum", "Predictions", "Variable") := NULL]

VTBRISIN <- BigRawTableBonds[cregnum == 1000]
VTBRISIN <- unique(VTBRISIN, by = "ISIN")
VTBRBonds <- merge(VTBRISIN, Differentials, by = "ISIN")
VTBRBonds[,c("cregnum", "Predictions", "Variable") := NULL]

SBERBonds <- SBERBonds[, mean(Errors), by = c("Year", "Month")]
SBERPrice <- merge(SBERBonds,SBERPrice, by = c("Year", "Month") )
colnames(SBERPrice) <- c("Year", "Month", "Differential", "Price")
SBERVol <- merge(SBERBonds,SBERVol, by = c("Year", "Month") )
colnames(SBERVol) <- c("Year", "Month", "Differential", "Volume")

VTBRBonds <- VTBRBonds[, mean(Errors), by = c("Year", "Month")]
VTBRPrice <- merge(VTBRBonds,VTBRPrice, by = c("Year", "Month") )
colnames(VTBRPrice) <- c("Year", "Month", "Differential", "Price")
VTBRVol <- merge(VTBRBonds,VTBRVol, by = c("Year", "Month") )
colnames(VTBRVol) <- c("Year", "Month", "Differential", "Volume")

library(ggplot2)
library(hrbrthemes)
breaks <- c("2015 1", "2016 1", "2017 1", "2018 1", "2019 1", "2020 1")

png("VTBRVol.png", width = 700, height = 400)
ggplot(VTBRVol, aes(y=Volume, x=paste(Year, Month))) + 
  geom_bar(stat = "identity") + scale_x_discrete(breaks = breaks) + theme_ipsum()+
  ylab("Volume VTBR") + xlab("Date")
dev.off()

png("VTBRPrice.png", width = 700, height = 400)
ggplot(VTBRPrice, aes(y=Price, x=paste(Year, Month))) + 
  geom_bar(stat = "identity") + scale_x_discrete(breaks = breaks) + theme_ipsum()+
  ylab("Price VTBR") + xlab("Date")
dev.off()

png("SBERVol.png", width = 700, height = 400)
ggplot(SBERVol, aes(y=Volume, x=paste(Year, Month))) + 
  geom_bar(stat = "identity") + scale_x_discrete(breaks = breaks) + theme_ipsum()+
  ylab("Volume SBER") + xlab("Date")
dev.off()

png("SBERPrice.png", width = 700, height = 400)
ggplot(SBERPrice, aes(y=Price, x=paste(Year, Month))) + 
  geom_bar(stat = "identity") + scale_x_discrete(breaks = breaks) + theme_ipsum()+
  ylab("Price SBER") + xlab("Date")
dev.off()


#test SBERPrice####
SBERPrice[, "V1" := Price]
adf.test(SBERPrice$V1)$p.value
adf.test(diff(SBERPrice$V1,1))$p.value
kpss.test(SBERPrice$V1)$p.value
kpss.test(diff(SBERPrice$V1,2))$p.value

Vars <- SBERPrice[, c("Year", "Month", "Differential", "V1")]
colnames(Vars) <- c("Year", "Month", "Differential", "Variable")

m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICSBERPrice.xlsx")

V.1<-VAR(Vars[,3:4],p=1,type="both")
serial.test(V.1)$serial$p.value

V.2<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

V.3<-VAR(Vars[,3:4],p=4,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 

VFinal<-VAR(Vars[,3:4],p=2,type="both")

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

VFinal<-VAR(Vars[,3:4],p=5,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1))

VFinal<-VAR(Vars[,3:4],p=3,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))
#test SBERPrice####
SBERVol[, "V1" := Volume]
adf.test(SBERVol$V1)$p.value
adf.test(diff(SBERVol$V1,1))$p.value
kpss.test(SBERVol$V1)$p.value
kpss.test(diff(SBERVol$V1,2))$p.value

Vars <- SBERVol[, c("Year", "Month", "Differential", "V1")]
colnames(Vars) <- c("Year", "Month", "Differential", "Variable")

m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICSBERPrice.xlsx")

normalized_Vars <- copy(Vars)
normalized_Vars[, "Differential" := znorm_vector(Differential)]
normalized_Vars[, "Variable" := znorm_vector(Variable)]
V.1<-VAR(normalized_Vars[,3:4],p=1,type="both")
serial.test(V.1)$serial$p.value

V.2<-VAR(normalized_Vars[,3:4],p=2,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

V.3<-VAR(Vars[,3:4],p=4,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 

VFinal<-VAR(normalized_Vars[,3:4],p=2,type="both")

jotest=ca.jo(normalized_Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

VFinal<-VAR(Vars[,3:4],p=5,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1))

VFinal<-VAR(Vars[,3:4],p=3,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))

#test VTBRPrice####
VTBRVol[, "V1" := Volume]
adf.test(VTBRVol$V1)$p.value
adf.test(diff(VTBRVol$V1,1))$p.value
kpss.test(VTBRVol$V1)$p.value
kpss.test(diff(VTBRVol$V1,2))$p.value

Vars <- VTBRVol[, c("Year", "Month", "Differential", "V1")]
colnames(Vars) <- c("Year", "Month", "Differential", "Variable")

m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICVTBRPrice.xlsx")

normalized_Vars <- copy(Vars)
normalized_Vars[, "Differential" := znorm_vector(Differential)]
normalized_Vars[, "Variable" := znorm_vector(Variable)]
V.1<-VAR(normalized_Vars[,3:4],p=1,type="both")
serial.test(V.1)$serial$p.value

V.2<-VAR(normalized_Vars[,3:4],p=2,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

V.3<-VAR(Vars[,3:4],p=4,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 

VFinal<-VAR(normalized_Vars[,3:4],p=2,type="both")

jotest=ca.jo(normalized_Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

VFinal<-VAR(Vars[,3:4],p=5,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1))

VFinal<-VAR(Vars[,3:4],p=3,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))



#test VTBRPrice####
VTBRPrice[, "V1" := Price]
adf.test(VTBRPrice$V1)$p.value
adf.test(diff(VTBRPrice$V1,1))$p.value
kpss.test(VTBRPrice$V1)$p.value
kpss.test(diff(VTBRPrice$V1,2))$p.value

Vars <- VTBRPrice[, c("Year", "Month", "Differential", "V1")]
colnames(Vars) <- c("Year", "Month", "Differential", "Variable")

m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICVTBRPrice.xlsx")

V.1<-VAR(Vars[,3:4],p=1,type="both")
serial.test(V.1)$serial$p.value

V.2<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

V.3<-VAR(Vars[,3:4],p=4,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 

VFinal<-VAR(Vars[,3:4],p=2,type="both")

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

VFinal<-VAR(Vars[,3:4],p=5,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1))

VFinal<-VAR(Vars[,3:4],p=3,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))

#test IndexPrice####
Differentials <- readRDS("DifferentialsTable.rds")
SBERTotal <- merge(SBERPrice, SBERVol, by = c("Year", "Month"))
colnames(SBERTotal)[3:4] <- c("SBERPrice", "SBERVolume")
VTBRTotal <- merge(VTBRPrice, VTBRVol, by = c("Year", "Month"))
colnames(VTBRTotal)[3:4] <- c("VTBRPrice", "VTBRVolume")
ROSBTotal <- merge(ROSBPrice, ROSBVol, by = c("Year", "Month"))
colnames(ROSBTotal)[3:4] <- c("ROSBPrice", "ROSBVolume")
IndexTotal <- merge(SBERTotal,VTBRTotal, by = c("Year", "Month"))
IndexTotal <- merge(IndexTotal,ROSBTotal, by = c("Year", "Month"))
IndexTotal[, "TotalVolume" := SBERVolume+VTBRVolume+ROSBVolume]
IndexTotal[, "SBERWeight" := SBERVolume/TotalVolume]
IndexTotal[, "VTBRWeight" := VTBRVolume/TotalVolume]
IndexTotal[, "ROSBWeight" := ROSBVolume/TotalVolume]
IndexTotal[, "IndexPrice" := (SBERWeight*SBERPrice)+(VTBRWeight*VTBRPrice)+(ROSBWeight*ROSBPrice)]
IndexTotal <- merge(IndexTotal[, c("Year", "Month", "IndexPrice")], Differentials[, c("Year", "Month", "Errors")], by = c("Year", "Month"))
IndexTotal[, "Errors" := -Errors]
IndexTotal <- IndexTotal[, .(Differential = mean(Errors), IndexPrice = mean(IndexPrice)), by = c("Year", "Month")]
#test IndexPrice2####
IndexTotal[, "V1" := IndexPrice]
adf.test(IndexTotal$V1)$p.value
adf.test(diff(IndexTotal$V1,1))$p.value
kpss.test(IndexTotal$V1)$p.value
kpss.test(diff(IndexTotal$V1,2))$p.value

Vars <- IndexTotal[, c("Year", "Month", "Differential", "V1")]
colnames(Vars) <- c("Year", "Month", "Differential", "Variable")

m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICIndexTotal.xlsx")

V.1<-VAR(Vars[,3:4],p=1,type="both")
serial.test(V.1)$serial$p.value

V.2<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

V.3<-VAR(Vars[,3:4],p=4,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 

VFinal<-VAR(Vars[,3:4],p=2,type="both")

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

VFinal<-VAR(Vars[,3:4],p=5,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1))

VFinal<-VAR(Vars[,3:4],p=3,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))

#test SBERPrice2####
SBERPrice <- merge(SBERPrice, Differentials[, c("Year", "Month", "Errors")],by = c("Year", "Month") )
SBERPrice[, "Differential" := -Errors]
SBERPrice <- SBERPrice[, .(Differential = mean(Differential), V1 = mean(V1)), by = c("Year", "Month")]
adf.test(SBERPrice$V1)$p.value
adf.test(diff(SBERPrice$V1,1))$p.value
kpss.test(SBERPrice$V1)$p.value
kpss.test(diff(SBERPrice$V1,2))$p.value

Vars <- SBERPrice[, c("Year", "Month", "Differential", "V1")]
colnames(Vars) <- c("Year", "Month", "Differential", "Variable")

m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICSBERPrice2.xlsx")

V.1<-VAR(Vars[,3:4],p=1,type="both")
serial.test(V.1)$serial$p.value

V.2<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

V.3<-VAR(Vars[,3:4],p=4,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 

VFinal<-VAR(Vars[,3:4],p=2,type="both")

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

VFinal<-VAR(Vars[,3:4],p=5,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1))

VFinal<-VAR(Vars[,3:4],p=3,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))

#test VTBRPrice2####
VTBRPrice <- merge(VTBRPrice, Differentials[, c("Year", "Month", "Errors")],by = c("Year", "Month") )
VTBRPrice[, "Differential" := -Errors]
VTBRPrice <- VTBRPrice[, .(Differential = mean(Differential), V1 = mean(V1)), by = c("Year", "Month")]
adf.test(VTBRPrice$V1)$p.value
adf.test(diff(VTBRPrice$V1,1))$p.value
kpss.test(VTBRPrice$V1)$p.value
kpss.test(diff(VTBRPrice$V1,2))$p.value

Vars <- VTBRPrice[, c("Year", "Month", "Differential", "V1")]
colnames(Vars) <- c("Year", "Month", "Differential", "Variable")

m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICVTBRPrice2.xlsx")

V.1<-VAR(Vars[,3:4],p=1,type="both")
serial.test(V.1)$serial$p.value

V.2<-VAR(Vars[,3:4],p=2,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

V.3<-VAR(Vars[,3:4],p=4,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 

VFinal<-VAR(Vars[,3:4],p=2,type="both")

jotest=ca.jo(Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

VFinal<-VAR(Vars[,3:4],p=5,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1))

VFinal<-VAR(Vars[,3:4],p=3,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))

#test VTBRVol2####
VTBRVol <- merge(VTBRVol, Differentials[, c("Year", "Month", "Errors")],by = c("Year", "Month") )
VTBRVol[, "Differential" := -Errors]
VTBRVol <- VTBRVol[, .(Differential = mean(Differential), V1 = mean(V1)), by = c("Year", "Month")]
adf.test(VTBRVol$V1)$p.value
adf.test(diff(VTBRVol$V1,1))$p.value
kpss.test(VTBRVol$V1)$p.value
kpss.test(diff(VTBRVol$V1,2))$p.value

Vars <- VTBRVol[, c("Year", "Month", "Differential", "V1")]
colnames(Vars) <- c("Year", "Month", "Differential", "Variable")

m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICVTBRVol2.xlsx")
normalized_Vars <- copy(Vars)
normalized_Vars[, "Differential" := znorm_vector(Differential)]
normalized_Vars[, "Variable" := znorm_vector(Variable)]
V.1<-VAR(normalized_Vars[,3:4],p=1,type="both")
serial.test(V.1)$serial$p.value

V.2<-VAR(normalized_Vars[,3:4],p=2,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

V.3<-VAR(Vars[,3:4],p=4,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 

VFinal<-VAR(normalized_Vars[,3:4],p=2,type="both")

jotest=ca.jo(normalized_Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

VFinal<-VAR(Vars[,3:4],p=5,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1))

VFinal<-VAR(Vars[,3:4],p=3,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))


#test SBERVol2####
SBERVol <- merge(SBERVol, Differentials[, c("Year", "Month", "Errors")],by = c("Year", "Month") )
SBERVol[, "Differential" := -Errors]
SBERVol <- SBERVol[, .(Differential = mean(Differential), V1 = mean(V1)), by = c("Year", "Month")]
adf.test(SBERVol$V1)$p.value
adf.test(diff(SBERVol$V1,1))$p.value
kpss.test(SBERVol$V1)$p.value
kpss.test(diff(SBERVol$V1,2))$p.value

Vars <- SBERVol[, c("Year", "Month", "Differential", "V1")]
colnames(Vars) <- c("Year", "Month", "Differential", "Variable")

m <- VARselect(Vars[,3:4],lag=10,type="both")
m
write.xlsx(m$criteria, "AICSBERVol2.xlsx")
normalized_Vars <- copy(Vars)
normalized_Vars[, "Differential" := znorm_vector(Differential)]
normalized_Vars[, "Variable" := znorm_vector(Variable)]
V.1<-VAR(normalized_Vars[,3:4],p=1,type="both")
serial.test(V.1)$serial$p.value

V.2<-VAR(normalized_Vars[,3:4],p=2,type="both")
serial.test(V.2)$serial$p.value #Stability analysis 

V.3<-VAR(Vars[,3:4],p=4,type="both")
serial.test(V.3)$serial$p.value #Stability analysis 

VFinal<-VAR(normalized_Vars[,3:4],p=2,type="both")

jotest=ca.jo(normalized_Vars[, 3:4], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

VFinal<-VAR(Vars[,3:4],p=5,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1))

VFinal<-VAR(Vars[,3:4],p=3,type="both")

wald.test(b=coef(VFinal$varresult[[1]]), Sigma=vcov(VFinal$varresult[[1]]), Terms=c(2,4))

wald.test(b=coef(VFinal$varresult[[2]]), Sigma=vcov(VFinal$varresult[[2]]), Terms= c(1,3))

#SBER separation####
SBERDiff <- Differentials[ISIN %in% SBERBonds$ISIN]
SBERDiff <- SBERDiff[, mean(-Errors), by = c("Year", "Month")]
NoSBERDiff <- Differentials[!(ISIN %in% SBERBonds$ISIN)]
#NoSBERDiff <- NoSBERDiff[, mean(-Errors), by = c("Year", "Month")]
mean(-SBERDiff$Errors)

years_t_tests <- c()
names(years_t_tests) <- NULL
for (i in 2015:2020){
  Differentials_T_2015 <- NoSBERDiff[Year == i, c("Month", "Errors")]
  Differentials_T_2015[, "Errors" := -Errors]
  m <- t.test(Differentials_T_2015$Errors, alternative = "greater", mu = 0)
  newnames <- c(names(years_t_tests), as.character(i))
  years_t_tests <- c(years_t_tests, m$p.value)
  names(years_t_tests) <- newnames
}

write.xlsx(years_t_tests, "ttests_years_nosber.xlsx")
months_t_tests <- list()
NoSBERDiff[(Year == 2015)&(Month == 1)]
for (i in 2015:2020){
  this_year_t_tests <- c()
  for (s in 1:12){
    Differentials_T_2015 <- NoSBERDiff[(Year == i)&(Month == s), c("Month", "Errors")]
    Differentials_T_2015[, "Errors" := -Errors]
    m <- t.test(Differentials_T_2015$Errors, alternative = "greater", mu = 0)
    this_year_t_tests <- c(this_year_t_tests, m$p.value)
  }
  months_t_tests[[i-2014]] <- this_year_t_tests
}
months_t_tests <- do.call(cbind, months_t_tests)
write.xlsx(months_t_tests, "ttests_months_nosber.xlsx")

Differentials_T <- Differentials[, c("Month", "Errors")]
Differentials_T[, "Errors" := -Errors]
m <- t.test(Differentials_T$Errors, alternative = "greater", mu = 0)
m$p.value



#robustness systemical ####
#residuals####
#download
smpl_dt <- fread("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/Tables/Cd1_I_.csv")
smpl_dt[REGN == 1, "System" := 1]
smpl_dt[REGN == 354, "System" := 1]
smpl_dt[REGN == 963, "System" := 1]
smpl_dt[REGN == 1000, "System" := 1]
smpl_dt[REGN == 1326, "System" := 1]
smpl_dt[REGN == 1481, "System" := 1]
smpl_dt[REGN == 1978, "System" := 1]
smpl_dt[REGN == 2209, "System" := 1]
smpl_dt[REGN == 2272, "System" := 1]
smpl_dt[REGN == 3251, "System" := 1]
smpl_dt[REGN == 3292, "System" := 1]
smpl_dt[REGN == 3349, "System" := 1]
smpl_dt[is.na(System), "System" := 0]
smpl_dt <- smpl_dt[REGN != 1481]
smpl_dt_big <- copy(smpl_dt)
#transform the date
smpl_dt[(format(as.Date(Date_trading,format="%Y-%m-%d"), format = "%d") >=15)&(Month!=12), "Month":=Month+1]
#normalize
smpl_dt_norm <- cbind(znorm_table(smpl_dt[, !c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank", "System")]), smpl_dt[, c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank", "System")])
#outliers
smpl_dt_norm <- smpl_dt_norm[znorm_vector(G_spread_interpolated) <=3, ]
smpl_dt_norm <- smpl_dt_norm[znorm_vector(G_spread_interpolated) >=-3, ]
smpl_dt_norm[System==1]
#get columns needed 
dt <- CleanDataSystem(smpl_dt_norm, "G_spread_interpolated", Average = T, System = F)$Table
dt[, "System" := NULL]
dt_isin <- CleanDataSystem(smpl_dt_norm, "G_spread_interpolated", Average = T, System = F)$ISIN

#train the model
start <- Sys.time()
modelsvm <- tune.svm(get("G_spread_interpolated")~.,data = dt, cross = 5, gamma = 0.0001, cost = 100, epsilon = 0.001)
Sys.time() - start

#get residuals
m <- MakePredictionState(modelsvm$best.model, dt, "G_spread_interpolated", T, dt_isin)
m[, "Year" := sapply(Year, mapyear, years = m$Year)]
m[, "Month" := sapply(Month, mapmonth, months = m$Month)]
mm <- mean((predict(modelsvm, dt_test) - dt_test$G_spread_interpolated)^2)
m[, mean(Errors^2)]
saveRDS(m, "ResidualsTableRobustnessSystem.rds")
saveRDS(modelsvm, "ModelRobustnessSystem.rds")

t.test(sample(seq(100000,2000000,1), 1000000), sample(-seq(100000,2000000,1), 1000000), alternative = "greater", mu = 0)$p.value

#differentials####

#download
smpl_dt <- smpl_dt_big
smpl_dt[REGN == 1, "System" := 1]
smpl_dt[REGN == 354, "System" := 1]
smpl_dt[REGN == 963, "System" := 1]
smpl_dt[REGN == 1000, "System" := 1]
smpl_dt[REGN == 1326, "System" := 1]
smpl_dt[REGN == 1481, "System" := 1]
smpl_dt[REGN == 1978, "System" := 1]
smpl_dt[REGN == 2209, "System" := 1]
smpl_dt[REGN == 2272, "System" := 1]
smpl_dt[REGN == 3251, "System" := 1]
smpl_dt[REGN == 3292, "System" := 1]
smpl_dt[REGN == 3349, "System" := 1]
smpl_dt[is.na(System), "System" := 0]
smpl_dt <- smpl_dt[REGN != 1481]
#transform the date
smpl_dt[(format(as.Date(Date_trading,format="%Y-%m-%d"), format = "%d") >=15)&(Month!=12), "Month":=Month+1]
#normalize
smpl_dt_norm <- cbind(znorm_table(smpl_dt[, !c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank", "System")]), smpl_dt[, c("G_spread", "G_spread_interpolated", "YTM_ind_main", "Moscow", "State_bank", "System")])
#get columns needed 
dt_system <- CleanDataSystem(smpl_dt_norm, "G_spread_interpolated", Average = T, System = T)$Table
dt_system_isin <- CleanDataSystem(smpl_dt_norm, "G_spread_interpolated", Average = T, System = T)$ISIN
#get differentials
dt_system[, "ISIN" := dt_system_isin]
dt_system <- subset(dt_system, Currency %in% levels(dt$Currency))
dt_system$Currency <- droplevels(dt_system$Currency)
dt_system <- subset(dt_system,Exch_name %in% levels(dt$Exch_name))
dt_system$Exch_name <- droplevels(dt_system$Exch_name)
levels(dt_system$Exch_name) <- levels(dt$Exch_name)
levels(dt_system$Currency) <- levels(dt$Currency)
dt_system_isin <- dt_system$ISIN
m_system <- MakePredictionState(modelsvm$best.model, dt_system, "G_spread_interpolated", T, dt_system_isin)
m_system[, "Year" := sapply(Year, mapyear, years = m_system$Year)]
m_system[, "Month" := sapply(Month, mapmonth, months = m_system$Month)]
saveRDS(m_system, "DifferentialsRobustnessSystem.rds")
#test the residuals robustness ####
Residuals <- readRDS("ResidualsTableRobustnessSystem.rds")
years_t_tests <- c()
names(years_t_tests) <- NULL
for (i in 2015:2020){
  Residuals_T_2015 <- Residuals[Year == i, c("Month", "Errors")]
  Residuals_T_2015[, "Errors" := -Errors]
  m <- t.test(Residuals_T_2015$Errors, alternative = "greater", mu = 0)
  newnames <- c(names(years_t_tests), as.character(i))
  years_t_tests <- c(years_t_tests, m$p.value)
  names(years_t_tests) <- newnames
}

write.xlsx(years_t_tests, "ttests_years_resids_robust_system.xlsx")
months_t_tests <- list()

for (i in 2015:2020){
  this_year_t_tests <- c()
  for (s in 1:12){
    Residuals_T_2015 <- Residuals[(Year == i)&(Month == s), c("Month", "Errors")]
    Residuals_T_2015[, "Errors" := -Errors]
    m <- t.test(Residuals_T_2015$Errors, alternative = "greater", mu = 0)
    this_year_t_tests <- c(this_year_t_tests, m$p.value)
  }
  months_t_tests[[i-2014]] <- this_year_t_tests
}
months_t_tests <- do.call(cbind, months_t_tests)
write.xlsx(months_t_tests, "ttests_months_resids_robust_system.xlsx")

mean(Residuals[(Year == 2015), c("Errors")]$Errors, na.rm = T)
(var(Residuals[(Year == 2015), c("Errors")]$Errors, na.rm = T))^(1/2)


Residuals_T <- Residuals[, c("Month", "Errors")]
Residuals_T[, "Errors" := -Errors]
m <- t.test(Residuals_T$Errors, alternative = "greater", mu = 0)
m$p.value


#t-tests differentials ####
#Differentials

Differentials <- readRDS("DifferentialsRobustnessSystem.rds")

years_t_tests <- c()
names(years_t_tests) <- NULL
for (i in 2015:2020){
  Differentials_T_2015 <- Differentials[Year == i, c("Month", "Errors")]
  Differentials_T_2015[, "Errors" := -Errors]
  m <- t.test(Differentials_T_2015$Errors, alternative = "greater", mu = 0)
  newnames <- c(names(years_t_tests), as.character(i))
  years_t_tests <- c(years_t_tests, m$p.value)
  names(years_t_tests) <- newnames
}

write.xlsx(years_t_tests, "ttests_years_robust_diff_system.xlsx")
months_t_tests <- list()

for (i in 2015:2020){
  this_year_t_tests <- c()
  for (s in 1:12){
    Differentials_T_2015 <- Differentials[(Year == i)&(Month == s), c("Month", "Errors")]
    Differentials_T_2015[, "Errors" := -Errors]
    m <- t.test(Differentials_T_2015$Errors, alternative = "greater", mu = 0)
    this_year_t_tests <- c(this_year_t_tests, m$p.value)
  }
  months_t_tests[[i-2014]] <- this_year_t_tests
}
months_t_tests <- do.call(cbind, months_t_tests)
write.xlsx(months_t_tests, "ttests_months_robust_diff_system.xlsx")

Differentials_T <- Differentials[, c("Month", "Errors")]
Differentials_T[, "Errors" := -Errors]
m <- t.test(Differentials_T$Errors, alternative = "greater", mu = 0)
m$p.value

#t-test both####

Differentials <- readRDS("DifferentialsRobustness.rds")

years_t_tests <- c()
names(years_t_tests) <- NULL
for (i in 2015:2020){
  Differentials_T_2015 <- Differentials[Year == i, c("Month", "Errors")]
  Differentials_T_2015[, "Errors" := -Errors]
  Residuals_T_2015 <- Residuals[Year == i, c("Month", "Errors")]
  Residuals_T_2015[, "Errors" := -Errors]
  m <- t.test(Differentials_T_2015$Errors,y = Residuals_T_2015$Errors, alternative = "less", mu = 0)
  newnames <- c(names(years_t_tests), as.character(i))
  years_t_tests <- c(years_t_tests, m$p.value)
  names(years_t_tests) <- newnames
}

write.xlsx(years_t_tests, "ttests_years_robust_twofolds.xlsx")
months_t_tests <- list()

for (i in 2015:2020){
  this_year_t_tests <- c()
  for (s in 1:12){
    Differentials_T_2015 <- Differentials[(Year == i)&(Month == s), c("Month", "Errors")]
    Differentials_T_2015[, "Errors" := -Errors]
    Residuals_T_2015 <- Residuals[(Year == i)&(Month == s), c("Month", "Errors")]
    Residuals_T_2015[, "Errors" := -Errors]
    m <- t.test(Differentials_T_2015$Errors,y = Residuals_T_2015$Errors, alternative = "greater", mu = 0)
    this_year_t_tests <- c(this_year_t_tests, m$p.value)
  }
  months_t_tests[[i-2014]] <- this_year_t_tests
}
months_t_tests <- do.call(cbind, months_t_tests)
write.xlsx(months_t_tests, "ttests_months_robust_twofolds.xlsx")

Differentials_T <- Differentials[, c("Month", "Errors")]
Differentials_T[, "Errors" := -Errors]
Residuals_T[, "Errors" := -Errors]
m <- t.test(Differentials_T$Errors, Residuals$Errors, alternative = "less", mu = 0)