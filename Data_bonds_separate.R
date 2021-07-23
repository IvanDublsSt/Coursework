goodbye
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
setwd("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework")
Sys.setenv(LANG = "en")

#read the table
BigRawTableBonds <- fread("Big_medium_rare_table_bonds_UTF_check.csv", encoding = "UTF-8")

#add previous price 
BigRawTableBonds[, "LaggedYTM" := shift(YTM_ind_main, 1L, NA,"lag"), by = "ISIN"]

#add equity index
equity_data <- fread("moex_data.csv", encoding = "UTF-8")
equity_data[, "TRADEYEAR" := year(TRADEDATE)]
equity_data[, c("HIGH", "LOW", "CAPITALIZATION") := .(as.numeric(str_replace(HIGH, ",", "\\.")), as.numeric(str_replace(LOW, ",", "\\.")), as.numeric(str_replace(CAPITALIZATION, ",", "\\.")))]
equity_data[, "PrevPrice" := shift((HIGH/2 + LOW/2), 1L, NA, "lead")]
equity_data[, "ReturnEquityDay" := ((HIGH/2 + LOW/2) - PrevPrice)/PrevPrice]
equity_data[, "PrevPriceMonth" := shift((HIGH/2 + LOW/2), 30, NA, "lead")]
equity_data[, "ReturnEquityMonth" := ((HIGH/2 + LOW/2) - PrevPriceMonth)/PrevPriceMonth]

#add Moscow
BigRawTableBonds[, "Moscow" := ifelse(str_detect(strcuraddr, "Москва"), 1, 0)]

#add year and month
BigRawTableBonds[, "Year" := year(Date_trading)]
BigRawTableBonds[, "Month" := month(Date_trading)]

#extract needed variables and marge
localtable <- equity_data[, c("ReturnEquityDay", "ReturnEquityMonth", "CAPITALIZATION", "TRADEDATE")]
BigRawTableBonds[, "Date_char" := unlist(lapply(str_split(BigRawTableBonds$Date_trading, " "), function(x){x[[1]][1]}))]
BigRawTableBonds <- merge(BigRawTableBonds, localtable, by.x = "Date_char", by.y = "TRADEDATE", all.x = T)

BigRawTableBonds[, c("ReturnEquityDay", "ReturnEquityMonth", "Capitalization", "BA_spread")  := .(ifelse(is.na(ReturnEquityDay), mean(ReturnEquityDay, na.rm = T), ReturnEquityDay),
                                                                                    ifelse(is.na(ReturnEquityMonth), mean(ReturnEquityMonth, na.rm = T), ReturnEquityMonth),
                                                                                    ifelse(is.na(CAPITALIZATION), mean(CAPITALIZATION, na.rm = T), CAPITALIZATION),
                                                                                    ifelse(is.na(BA_spread), mean(BA_spread, na.rm = T), BA_spread)), 
                 by = "Year"]

#prepared table bonds
BigTableBonds <- BigRawTableBonds[,c("Date_trading","Year","Month","ISIN","Currency","Exch_name","BA_spread","Coupon","YTM_ind_main","Indicative_yield_type",
                                      "G_spread","G_spread_interpolated","State_bank","Days_to_maturity","Days_to_call","Callable","cregnum",
                                      "G_spread_interpolated","LaggedYTM","Moscow","ReturnEquityDay","ReturnEquityMonth","Capitalization")]
BigTableBonds <- BigTableBonds[ Indicative_yield_type!="Current"]
BigTableBonds <- BigTableBonds[ Exch_name !="Cbonds Estimation"]
BigTableBonds[, "Informativeness" := .N/(365*6), by = ISIN]

#separate years datasets
FifteenTableBonds <- BigTableBonds[Year == "2015"]
SixteenTableBonds <- BigTableBonds[Year == "2016"]
SeventeenTableBonds <- BigTableBonds[Year == "2017"]
EighteenTableBonds <- BigTableBonds[Year == "2018"]
NineteenTableBonds <- BigTableBonds[Year == "2019"]
TwentyTableBonds <- BigTableBonds[Year == "2020"]
#filtered datasets
names <- c("I", "II", "III", "IV", "V")
m<-0
for (i in c(0,0.1,0.2,0.3,0.4)){
  m <- m+1
  localtable <- BigTableBonds[Informativeness <= quantile(Informativeness, 1-i)] 
  write.csv(localtable, paste("2/1_", names[m], "_raw.csv", sep = ""))
}
year <- 2014 
for (t in list(FifteenTableBonds,SixteenTableBonds,SeventeenTableBonds,EighteenTableBonds,NineteenTableBonds,TwentyTableBonds)){
  m<-0
  year <- year + 1
  for (i in c(0,0.1,0.2,0.3,0.4)){
    m <- m+1

    localtable <- t[Informativeness <= quantile(Informativeness, 1-i)] 
    write.csv(localtable, paste("2", "/2_", names[m], "_", as.character(year), "_raw.csv", sep = ""))
  }
}

View(BigRawTableBonds[is.na(cregnum)])


#merge bonds and banks


library(foreach)
library(doParallel)
cl <- makeCluster(detectCores() - 1, outfile = "")
registerDoParallel(cl, cores = detectCores() - 1)



setwd("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework")
allbanks <- list.files(path = "C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/A")
allbonds <- list.files(path = "C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/2")



localbanks <- fread(paste("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/A/", allbanks[1], sep = ""), colClasses = "numeric")
ucn <- unique(colnames(localbanks))
localbanks <- localbanks[, ..ucn]
localbonds <- fread(paste("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/2/", allbonds[1], sep = ""))
localbonds <- localbonds[!is.na(cregnum)]
ucn <- unique(colnames(localbonds))
localbonds <- localbonds[, ..ucn]
localbonds[, "cregnum" := str_remove(cregnum, "\\s")]
localbonds[, "cregnum" := as.numeric(cregnum)]
localbonds <- localbonds[!is.na(cregnum)]
allbonds[29:length(allbonds)]
localbanks[, "REGN" := as.numeric(REGN)]
localall <- merge(localbanks, localbonds, by.x = c("REGN", "Year", "Month"), by.y = c("cregnum", "Year", "Month"), all.y = T)
write.csv(localall, paste("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/Tables/",str_remove(allbanks[1], "raw.csv"), str_remove(allbonds[1], "raw.csv"), ".csv", sep = ""))
allbanks <- allbanks[2:length(allbanks)]

for (i in allbanks){
  if(i=="Aeraw.csv"){
    foreach(j = allbonds[29:length(allbonds)], .packages = c("data.table", "stringr")) %dopar%{
      localbanks <- fread(paste("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/A/", i, sep = ""), colClasses = "numeric")
      ucn <- unique(colnames(localbanks))
      localbanks <- localbanks[, ..ucn]
      localbonds <- fread(paste("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/2/", j, sep = ""))
      localbonds <- localbonds[!is.na(cregnum)]
      ucn <- unique(colnames(localbonds))
      localbonds <- localbonds[, ..ucn]
      localbonds[, "cregnum" := str_remove(cregnum, "\\s")]
      localbonds[, "cregnum" := as.numeric(cregnum)]
      localbonds <- localbonds[!is.na(cregnum)]
      localbanks[, "REGN" := as.numeric(REGN)]
      localall <- merge(localbanks, localbonds, by.x = c("REGN", "Year", "Month"), by.y = c("cregnum", "Year", "Month"), all.y = T)
      write.csv(localall, paste("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/Tables/",str_remove(i, "raw.csv"), str_remove(j, "raw.csv"), ".csv", sep = ""))
      print("")
      print(i)
      print(j)
      print("")
    }
  }
  else{
  foreach(j = allbonds, .packages = c("data.table", "stringr")) %dopar%{
    start <- Sys.time()
    localbanks <- fread(paste("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/A/", i, sep = ""), colClasses = "numeric")
    ucn <- unique(colnames(localbanks))
    localbanks <- localbanks[, ..ucn]
    localbonds <- fread(paste("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/2/", j, sep = ""))
    localbonds <- localbonds[!is.na(cregnum)]
    ucn <- unique(colnames(localbonds))
    localbonds <- localbonds[, ..ucn]
    localbonds[, "cregnum" := str_remove(cregnum, "\\s")]
    localbonds[, "cregnum" := as.numeric(cregnum)]
    localbonds <- localbonds[!is.na(cregnum)]
    localbanks[, "REGN" := as.numeric(REGN)]
    localall <- merge(localbanks, localbonds, by.x = c("REGN", "Year", "Month"), by.y = c("cregnum", "Year", "Month"), all.y = T)
    write.csv(localall, paste("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/Tables/",str_remove(i, "raw.csv"), str_remove(j, "raw.csv"), ".csv", sep = ""))
    print("")
    print(i)
    print(j)
    print(Sys.time() - start)
    print("")
  }}
}
detectCores()

start <- Sys.time()
foreach(i = c(1,2,3))%dopar%{
  i^2
}
print(Sys.time() - start)



#normalization
install.packages("dprep")
library(dprep)
znorm_vector <- function(vector){
  vecmean <- mean(vector, na.rm = T)
  vecstdev <- sd(vector, na.rm = T)
  normalized_vector <- (vector-vecmean)/vecstdev
  return(normalized_vector)
}

dtable <- data.table("a" = c(1,2,NA), "b" = c(2,5,9), c = "f")
is.character(dtable)
znorm_table <- function(dtable){
  charnames <- colnames(dtable)[sapply(dtable, is.character)]
  numericnames <- colnames(dtable)[!sapply(dtable, is.character)]
  dtable_numeric <- dtable[, ..numericnames]
  dtable_character <- dtable[, ..charnames]
  dtable_normalized <- as.data.table(apply(dtable_numeric, 2, znorm_vector))
  dtable_final <- cbind(dtable_character, dtable_normalized)
  return(dtable_final)
}
znorm_table(dtable)

smpl_dt <- fread("C:/Users/Fride/OneDrive/Рабочий стол/Coursework/Data_coursework/Tables/Aa1_V_.csv")
smpl_dt_norm <- znorm_table(smpl_dt)
write.csv(smpl_dt_norm, "Aa1_V_norm.csv")




