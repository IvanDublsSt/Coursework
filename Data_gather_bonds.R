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
setwd("C:/Coursework/Data_coursework")
Sys.setenv(LANG = "en")
#####Big table ####
#download the table. 
BigRawTableBonds <- fread("Big_raw_table_bonds.csv", encoding = "UTF-8")
#change names to latin short versions 
names <- colnames(BigRawTableBonds)
names_latin <- stri_trans_general(names, "russian-latin/bgn")
ms <- "4 chernen'kih chertenka chertili chernymi chernilami chertezh"
names_latin_short <- sub("^(\\S*\\s+\\S+\\s+\\S+).*", "\\1", names_latin)
colnames(BigRawTableBonds) <- names_latin_short
colnames(BigRawTableBonds)[23:27]<- c("YTM_ind", "YTM_bid", "YTM_ask", "YTM_last", "YTM_close")
colnames(BigRawTableBonds)[31:35]<- c("YTM_off_ind", "YTM_off_bid", "YTM_off_ask", "YTM_off_last", "YTM_off_close")
colnames(BigRawTableBonds)[36:40]<- c("YTM_curr_ind", "YTM_curr_bid", "YTM_curr_ask", "YTM_curr_last", "YTM_curr_close")
colnames(BigRawTableBonds)[41]<- c("YTM_ind_main")
new_names <- c("Number","Issue",	"Currency",	"Date_trading",	"Date_final",	"Exch_name",	"Bid_price",	"Ask_price",	"BA_spread",
  "Av_price",	"Opening_price",	"Min_price",	"Max_price",	"Last_price",	"Av_weighted_price",	"Closing_price",
  "Indicative_price",	"Indicative_price_type",	"Turnover",	"Number_of_trades",	"Volume",	"Coupon",	"YTM_ind",
  "YTM_bid", "YTM_ask", "YTM_last", "YTM_close","Coupon_accum",	"Duration",	"Option_date","YTM_off_ind", "YTM_off_bid",
  "YTM_off_ask", "YTM_off_last", "YTM_off_close",	"YTM_curr_ind", "YTM_curr_bid", "YTM_curr_ask", "YTM_curr_last", "YTM_curr_close","YTM_ind_main",	
  "Indicative_yield_type",	"Duration_option",	"Trade_regime",	"ISIN",	"G_spread",	"T_spread")
colnames(BigRawTableBonds) <- new_names
BigRawTableBonds <- BigRawTableBonds[Issue != "Ã≈“ ŒÃ¡¿Õ , 01"]

#Throw out the rows with no price (indikativnaya tsena)
BigRawTableBonds <- BigRawTableBonds[!is.na(YTM_ind_main)] 

#add starting dates
table_dates <- BigRawTableBonds[, c("Date_trading",	"Date_final", "Issue")]
table_dates <- table_dates[order(Date_trading)]
table_dates <- table_dates[, head(.SD, 1), by = "Issue"]
colnames(table_dates)[2] <- "Date_first_trading" 
table_dates <- table_dates[, c("Issue", "Date_first_trading")]
BigRawTableBonds <- merge(BigRawTableBonds,table_dates, all.x = T, allow.cartesian = T, by = "Issue")

#add state banks list
state_banks_table <- fread("state_banks_list2.csv")
colnames(state_banks_table) <- c("Number", "Bank", "REGN", "Type", "Date_reg", "Type_own", "Type_owner", "Owner", "Int", "Assets", "Place")
state_banks_table <- as.data.table(unique(state_banks_table, by = "Bank"))

for (i in 1:length(state_banks_table)){
  bank_name <- state_banks_table$Bank[i]
  BigRawTableBonds[str_detect(tolower(Issue), tolower(bank_name)), "State_bank_name" := bank_name]
}
BigRawTableBonds[is.na(State_bank_name), "State_bank_name" := "private" ]
BigRawTableBonds[, "State_bank" := ifelse(State_bank_name == "private", 0, 1) ]
BigRawTableBonds[State_bank == 0]

#add issuer 
str_get <- function(x, string){
  return(str_split(x, string)[[1]][1])
}
str_get(c("1,2", "3,4"), ",")
sapply(c("1,2", "3,4"), str_get,  string = ",")
BigRawTableBonds[, "Issuer" := sapply(Issue, str_get, ",")   ]
length(unique(BigRawTableBonds$Issuer))
length(BigRawTableBonds)

#create days to maturity
BigRawTableBonds[, "Days_to_maturity" := as.Date(as.character(Date_final), format = "%Y-%m-%d") - as.Date(as.character(Date_trading), format = "%Y-%m-%d")]
BigRawTableBonds[, "Total_days_to_maturity" := as.Date(as.character(Date_final), format = "%Y-%m-%d") - as.Date(as.character(Date_first_trading), format = "%Y-%m-%d")]
BigRawTableBonds[, "Days_to_call" := as.Date(as.character(Option_date), format = "%Y-%m-%d") - as.Date(as.character(Date_first_trading), format = "%Y-%m-%d")]

#create callability dummy
BigRawTableBonds[, "Callable" := ifelse(is.na(Days_to_call), 0, 1)]
length(unique(BigRawTableBonds$Issue))
length(unique(BigRawTableBonds[Callable == 1]$Issue))
length(unique(BigRawTableBonds[Callable == 1]$Issuer))
length(unique(BigRawTableBonds[State_bank == 1]$Issue))
length((BigRawTableBonds[State_bank == 1]$Issuer))

#add regnums
regnums <- fread("regnums.csv")
unique(BigRawTableBonds$Issuer)
BigRawTableBonds[, "Issuerx" := Issuer]
BigRawTableBonds[Issuer == "ING Wholesale Banking Russia", "Issuerx" := "»Õ√ ¡¿Õ  (≈¬–¿«»ﬂ)"]
BigRawTableBonds[Issuer == "¿ ¡ œÂÂÒ‚ÂÚ (œ¿Œ)", "Issuerx" := "œÂÂÒ‚ÂÚ"]
BigRawTableBonds[Issuer == "¿ »¡ Œ¡–¿«Œ¬¿Õ»≈ (¿Œ)", "Issuerx" := "Œ·‡ÁÓ‚‡ÌËÂ"]
BigRawTableBonds[Issuer == "¿‚‡Ì„‡‰ ¿ ¡", "Issuerx" := "¿‚‡Ì„‡‰"]
BigRawTableBonds[Issuer == "¡‡ÌÍ Õ‘  (¿Œ)", "Issuerx" := "Õ‘ "]
BigRawTableBonds[Issuer == " ¡  ÓÎ¸ˆÓ ”‡Î‡", "Issuerx" := " ÓÎ¸ˆÓ ”‡Î‡"]
BigRawTableBonds[Issuer == " ¡ Ã»¿ (¿Œ)", "Issuerx" := "Ã»¿"]
BigRawTableBonds[Issuer == " ¡ –ÂÌÂÒÒ‡ÌÒ  Â‰ËÚ", "Issuerx" := "–ÂÌÂÒÒ‡ÌÒ  Â‰ËÚ"]
BigRawTableBonds[Issuer == " ¡ —ÓÎË‰‡ÌÓÒÚ¸", "Issuerx" := "—ÓÎË‰‡ÌÓÒÚ¸"]
BigRawTableBonds[Issuer == " ¡ ÷ÂÌÚ-ËÌ‚ÂÒÚ", "Issuerx" := "÷ÂÌÚ-ËÌ‚ÂÒÚ"]
BigRawTableBonds[Issuer == "ÃÂÊ‰ÛÌ‡Ó‰Ì˚È ËÌ‚ÂÒÚËˆËÓÌÌ˚È ·‡ÌÍ (Ã»¡)", "Issuerx" := "ÃÂÊ‰ÛÌ‡Ó‰Ì˚È ËÌ‚ÂÒÚËˆËÓÌÌ˚È ·‡ÌÍ"]
BigRawTableBonds[Issuer == "—‚ˇÁÌÓÈ ¡‡ÌÍ (¿Œ)", "Issuerx" := "—‚ˇÁÌÓÈ ¡‡ÌÍ"]
BigRawTableBonds[Issuer == "¿  ¡¿–— ¡¿Õ ", "Issuerx" := "¿Í ¡‡Ò"]
BigRawTableBonds[Issuer == "¡‡ÌÍ ¡‡ÎÚËÍ‡", "Issuerx" := "¡‡ÎÚËÍ‡"]
BigRawTableBonds[Issuer == "¡‡ÌÍ ƒÂÊ‡‚‡", "Issuerx" := "ƒÂÊ‡‚‡"]
BigRawTableBonds[Issuer == "¡‡ÌÍ Õ‡ˆËÓÌ‡Î¸Ì˚È ÒÚ‡Ì‰‡Ú", "Issuerx" := "Õ‡ˆËÓÌ‡Î¸Ì˚È ÒÚ‡Ì‰‡Ú"]
BigRawTableBonds[Issuer == "¡‡ÌÍ œÂÚÓÍÓÏÏÂˆ", "Issuerx" := "œÂÚÓÍÓÏÏÂˆ"]
BigRawTableBonds[Issuer == "¡‡ÌÍ –ÛÒÒÍËÈ ÒÚ‡Ì‰‡Ú", "Issuerx" := "–ÛÒÒÍËÈ ÒÚ‡Ì‰‡Ú"]
BigRawTableBonds[Issuer == "¡‡ÌÍ —‡ÌÍÚ-œÂÚÂ·Û„", "Issuerx" := '¡¿Õ  "—¿Õ “-œ≈“≈–¡”–√"']
BigRawTableBonds[Issuer == "¡‡ÌÍ —Ó˛Á", "Issuerx" := "—Ó˛Á"]
BigRawTableBonds[Issuer == "¡‡ÌÍ ”‡ÎÒË·", "Issuerx" := "”‡ÎÒË·"]
BigRawTableBonds[Issuer == "¡‡ÌÍ ‘  ŒÚÍ˚ÚËÂ", "Issuerx" := "‘  ŒÚÍ˚ÚËÂ"]
BigRawTableBonds[Issuer == "¬¡––", "Issuerx" := "¬—≈–Œ——»…— »… ¡¿Õ  –¿«¬»“»ﬂ –≈√»ŒÕŒ¬"]
BigRawTableBonds[Issuer == "¬ÓÒÚÓ˜Ì˚È ›ÍÒÔÂÒÒ ¡‡ÌÍ", "Issuerx" := "¬ÓÒÚÓ˜Ì˚È"]
BigRawTableBonds[Issuer == "–√— ¡‡ÌÍ", "Issuerx" := "–Œ—√Œ——“–¿’ ¡¿Õ "]
BigRawTableBonds[Issuer == "–Õ ¡ ¡‡ÌÍ", "Issuerx" := "–Œ——»…— »… Õ¿÷»ŒÕ¿À‹Õ€…  ŒÃÃ≈–◊≈— »… ¡¿Õ "]
BigRawTableBonds[Issuer == "–ÛÒÒÎ‡‚·‡ÌÍ", "Issuerx" := "–”—— »… —À¿¬ﬂÕ— »… ¡¿Õ "]
BigRawTableBonds[Issuer == "—¡ ¡‡ÌÍ", "Issuerx" := "—”ƒŒ—“–Œ»“≈À‹Õ€… ¡¿Õ "]
BigRawTableBonds[Issuer == "—‚ˇÁ¸-¡‡ÌÍ", "Issuerx" := "œ–ŒÃ—¬ﬂ«‹¡¿Õ "]
BigRawTableBonds[Issuer == "—Ãœ ¡‡ÌÍ", "Issuerx" := "—≈¬≈–Õ€… ÃŒ–— Œ… œ”“‹"]
BigRawTableBonds[Issuer == "”¡–Ë–", "Issuerx" := "”–¿À‹— »… ¡¿Õ  –≈ ŒÕ—“–” ÷»» » –¿«¬»“»ﬂ"]
BigRawTableBonds[Issuer == "”‡Î ‡ÔËÚ‡Î¡‡ÌÍ", "Issuerx" := "”–¿À‹— »…  ¿œ»“¿À"]
BigRawTableBonds[Issuer == "‘œ¡ ¡‡ÌÍ", "Issuerx" := "‘»Õœ–ŒÃ¡¿Õ "]




regnums[, "Issuerx" := tolower(cnamer)]
BigRawTableBonds[, "Issuerx" := tolower(Issuerx)]
BigRawTableBonds<-merge(BigRawTableBonds, regnums, by = "Issuerx", all.x = T, allow.cartesian = T)
unique(BigRawTableBonds[is.na(cregnum)]$Issuer)
BigRawTableBonds[Issuer == "¡‡ÌÍ —‡ÌÍÚ-œÂÚÂ·Û„", "cregnum" := "436"]
BigRawTableBonds[Issuer == "¡¿Õ  ”–¿À—»¡", "cregnum" := "30"]
BigRawTableBonds[Issuer == "¡»Õ¡¿Õ ", "cregnum" := "323"]
BigRawTableBonds[Issuer == "¡‘¿ ¡‡ÌÍ", "cregnum" := "3038"]
BigRawTableBonds[Issuer == "√ÀŒ¡› —¡¿Õ ", "cregnum" := "1942"]
BigRawTableBonds[Issuer == "ƒÂÎ¸Ú‡ Â‰ËÚ", "cregnum" := "3338"]
BigRawTableBonds[Issuer == " ‡ÈËÌ‚ÂÒÚ·‡ÌÍ", "cregnum" := "3360"]
BigRawTableBonds[Issuer == "ÃÂÊ‰ÛÌ‡Ó‰Ì˚È ËÌ‚ÂÒÚËˆËÓÌÌ˚È ·‡ÌÍ (Ã»¡)", "cregnum" := "204"]
BigRawTableBonds[Issuer == " ¡ Ã»¿ (¿Œ)", "cregnum" := "3344"]
BigRawTableBonds[Issuer == "œÂ‚˚È Œ·˙Â‰ËÌÂÌÌ˚È ¡‡ÌÍ (œÂ‚Ó·‡ÌÍ)", "cregnum" := "3251"]
BigRawTableBonds[Issuer == "¡‡ÌÍ œÂÚÓÍÓÏÏÂˆ", "cregnum" := "1776"]

#add linearly interpolated risk-free
thetable <- list()
for (i in 2015:2020){
  newtable <- fread(paste("‰‡ÌÌ˚Â ÔÓ ÒÚ‡‚Ó˜Í‡Ï/‰‡ÌÌ˚Â ÔÓ ÒÚ‡‚Ó˜Í‡Ï/cbr", as.character(i), ".csv", sep = ""))
  thetable[[i-2014]] <- newtable 
}
thetable <- do.call(rbind, thetable)
colnames(thetable) <- c("Date", "0.25", "0.5", "0.75", "1", "2", "3", "5", "7", "10", "15", "20", "30")
thetable <- thetable[-1]
thetable[, Date := date <- as.Date(as.character(Date), format = "%d.%m.%Y")]
linear_interpolation <- function(T_1, T_2, i_1, i_2, T_x){
  i_x = ((T_x-T_1)/(T_2-T_1))*(i_2-i_1)+i_1
  return( i_x)
}

#inputs
i <- 0.68
date <- "26.07.2016"
date <- as.Date(as.character(date), format = "%d.%m.%Y") 
localtable[Date == localtable2[1]$Date_trading]
date-1
as.Date(s, format = "%Y-%m-%d") %in% localtable$Date
s %in% localtable$Date


linear_inter_big <- function( to_maturity, date){
date <- as.Date(date, format = "%Y-%m-%d")
olddate <- date
if ((date != as.Date("2015-01-02", format = "%Y-%m-%d")) & (date != as.Date("2015-01-01", format = "%Y-%m-%d"))){
while (!(date %in% thetable$Date)){

  date <- date - 1
}}
else {
  while (!(date %in% thetable$Date)){

  date <- date + 1}}
to_maturity <- as.numeric(to_maturity)
to_maturity <- to_maturity/360  
#lower and higher bounds
numbers <- as.numeric(c("0.25", "0.5", "0.75", "1", "2", "3", "5", "7", "10", "15", "20", "30"))
if (to_maturity <= 0.25){
  
  high_i <- as.matrix(thetable[Date == date, "0.25"])[1,1]
  high_i <- as.numeric(str_replace(high_i, ",", "."))
  prev_i <- as.matrix(thetable[Date == date, "0.5"])[1,1]
  prev_i <- as.numeric(str_replace(prev_i, ",", "."))
  incline_i <- (prev_i-high_i)/(0.25)
  dist_i <- 0.25-to_maturity

  return(high_i - dist_i*incline_i)
}
if (to_maturity >= 30){
  low_i <- as.matrix(thetable[Date == date, "30"])[1,1]
  low_i <- as.numeric(str_replace(low_i, ",", "."))
  prev_i <- as.matrix(thetable[Date == date, "20"])[1,1]
  prev_i <- as.numeric(str_replace(prev_i, ",", "."))
  incline_i <- (low_i - prev_i)/(10)
  dist_i <- to_maturity -low_i
  return(low_i + dist_i*incline_i)
}
begin <- numbers[numbers <= to_maturity][length(numbers[numbers < to_maturity])]
end <- numbers[numbers >= to_maturity][1]
begend <- c(as.character(begin), as.character(end))
low_i <- as.matrix(thetable[Date == date, ..begend])[1,1]
high_i <- as.matrix(thetable[Date == date, ..begend])[1,2]
low_i <- as.numeric(str_replace(low_i, ",", "."))
high_i <- as.numeric(str_replace(high_i, ",", "."))

return(linear_interpolation(begin, end, low_i, high_i, to_maturity))
}


linear_inter_big(380, "2015-12-30 00:00:00")
linear_interpolation(0.75, 1, 10.02, 10.25, 1)

mapply(linear_inter_big, to_maturity = m, date = s)

localtable2 <- BigRawTableBonds[, c("Date_trading", "Days_to_maturity", "ISIN")]

localtable2[, "RF_interpolated" := mapply(linear_inter_big, to_maturity = Days_to_maturity, date = Date_trading)/100]
saveRDS(localtable2, "TableRFInterpolationCheck.rds")
localtable2 <- readRDS("TableRFInterpolationCheck.rds")
BigRawTableBonds[, "RF_interpolated" := localtable2$RF_interpolated]
#add G-spread interpolated
BigRawTableBonds[, "G_spread_interpolated" := YTM_ind_main - RF_interpolated]

#save the big table cleaned 
write.csv(BigRawTableBonds, "Big_medium_rare_table_bonds_UTF_check.csv", fileEncoding = "UTF-8" )

#read the big table cleaned
BigRawTableBonds <- fread("Big_medium_rare_table_bonds_UTF_check.csv", encoding = "UTF-8")
BigRawTableBonds["¬›¡" %in% Issuer]
View(BigRawTableBonds[order(G_spread_interpolated)])
mean(BigRawTableBonds$G_spread_interpolated)
sd(BigRawTableBonds$G_spread_interpolated)

#Create the heatmap. Create list with all date-issue combinations #####
issues <- unique(BigRawTableBonds$Issue)
alldates2015 <- seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by="days")
alldates2016 <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by="days")
alldates2017 <- seq(as.Date("2017-01-01"), as.Date("2017-12-31"), by="days")
alldates2018 <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by="days")
alldates2019 <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by="days")
alldates2020 <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="days")
alldates <- list(alldates2015,alldates2016,alldates2017,alldates2018,alldates2019,alldates2020)

bigtable <- data.table()
for (i in 1:length(issues)){
  for (j in 1:6){
    datesvec <- alldates[[j]]
    namevec <- rep(issues[i], length(datesvec))
    newtable <- data.table("Issue" = c(namevec), "Date" = c(datesvec))
    bigtable <- rbind(bigtable, newtable)
  }
}
bigtable[, "Check" := "Hi"]

#Create the heatmap. Merge the list with the big table
BigRawTableBonds[, "Data_torgov" := sub("^(\\S*).*", "\\1", Date_trading)]
BigRawTableBonds[, "Data_torgov" := as.Date(`Data torgov`)]
CheckTableBonds <- BigRawTableBonds[, c("Data_torgov", "Issue", "YTM_ind_main")]
colnames(CheckTableBonds) <- c("Date", "Issue", "YTM")


#smalltable <- CheckTableBonds[Issue == "œÓÏÒ‚ˇÁ¸·‡ÌÍ, ¡Œ-02"]
bigtable_merged <- merge(CheckTableBonds, bigtable, by.x = c("Date", "Issue"), by.y = c("Date", "Issue"), allow.cartesian = T, all = TRUE)
bigtable_merged[, "YTM" := !is.na(YTM)]

#dates
DatesTableBonds <- BigRawTableBonds[, c("Issue", "Date_final", "Date_first_trading")]
colnames(DatesTableBonds) <- c("Issue", "Date_final", "Date_first")
DatesTableBonds <- unique(DatesTableBonds)
bigtable_merged <- merge(bigtable_merged, DatesTableBonds, by = "Issue", all.x = T, allow.cartesian = T)

#in-date
bigtable_merged[,"YTM" := ifelse(YTM == F, 0, 2)]
bigtable_merged[(Date_final < Date )|(Date_first>Date), "YTM":=1]
bigtable_merged[, "YTM" := as.factor(YTM)]
#####Heatmap ####

#Create the heatmap. The graph.

# Heatmap 
graph <- ggplot(bigtable_merged, aes(Date, Issue, fill= YTM)) + 
  geom_tile() + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
ggsave(graph, filename = "test.png", dpi = 300, limitsize = F, width = 60, height = 60)

tiff('test.tiff', units="in", width=70, height=70, res=300, compression = 'lzw')
ggplot(bigtable_merged, aes(Date, Issue, fill= YTM)) + 
  geom_tile() + theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
  scale_fill_manual(values = c("white", "red", "blue"))
dev.off()

#Count NAs
CountNATable <- bigtable_merged[YTM == T]
CountNATable <- CountNATable[, .N, by = Issue]
CountDaysTable <- bigtable_merged[, .N, by = Issue]
CountTable <- merge(CountDaysTable, CountNATable, by = "Issue")
CountTable[, "NonNAShare" := 100*N.y/N.x]
CountTable[, c("Issue", "NonNAShare")]
BigRawTableBonds <- merge(BigRawTableBonds, CountTable, by.y = "Issue", by.x = "Bumaga", all.x = T, allow.cartesian = T)

p <- ggplot(CountTable, aes(x=NonNAShare)) + 
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9) + theme( panel.background = element_blank()) 
p
ggsave(p, filename = "NonNAShareHistogram.png", dpi = 300, limitsize = F, width = 10, height = 5)

#Get the marginal effects of reductions 
percents <- quantile(CountTable$NAShare, seq(0,1,0.01))
means <- c()
numbers_issues <- c()
numbers_obs <- c()
for (i in 1:length(percents)){
  means <- c(means, mean(CountTable[NonNAShare >= percents[i]]$NonNAShare))
  numbers_issues <- c(numbers_issues, length(CountTable[NonNAShare >= percents[i]]$NonNAShare))
  numbers_obs <- c(numbers_obs, length(BigRawTableBonds[NonNAShare >= percents[i]]$NonNAShare))
}

names(percents)
data <- data.frame(percents = seq(1,101,1), means = means, numbers_obs = numbers_obs, numbers_issues = numbers_issues)
p<-ggplot(data, aes(x=percents)) +
  geom_line(aes(y = means),col = "red") +
  ylab("Mean share of prices which are present") +
  xlab("Percentile of the share, below which observations are removed")
p
ggsave(p, filename = "RemoveLine3.png", dpi = 300, limitsize = F, width = 10, height = 5)

ggplot(data, aes(x=percents)) +
  geom_line(aes(y = numbers_issues),col = "blue")+
  ylab("Number of remaining issues") +
  xlab("Percentile of the share, below which observations are removed")

p <- ggplot(data, aes(x=means)) +
  geom_line(aes(y = numbers_obs),col = "green") +
  scale_y_continuous(breaks = round(seq(min(data$numbers_obs), max(data$numbers_obs), by = 50000),1))+
  ylab("Number of remaining observations") +
  xlab("Mean share of prices which are present in the remaining subsample")
ggsave(p, filename = "RemoveLine.png", dpi = 300, limitsize = F, width = 10, height = 5)

p <- ggplot(data, aes(x=means)) +
  geom_line(aes(y = numbers_issues),col = "green") +
  scale_y_continuous(breaks = round(seq(min(data$numbers_issues), max(data$numbers_issues), by = 100),1))+
  ylab("Number of remaining issues") +
  xlab("Mean share of prices which are present in the remaining subsample")
ggsave(p, filename = "RemoveLine2.png", dpi = 300, limitsize = F, width = 10, height = 5)
p

#try sorting the heatmap
bigtable_merged <- merge(bigtable_merged, CountTable, by = "Issue", all.x = T, allow.cartesian = T)
bigtable_merged <- bigtable_merged[order(-NonNAShare)]
tiff('test_ordered.tiff', units="in", width=70, height=70, res=300, compression = 'lzw')
ggplot(bigtable_merged, aes(Date, reorder(Issue, NonNAShare), fill= YTM)) + 
  geom_tile() + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
dev.off()

#add begin and end years
BegEndTable <- fread("Bond_issue_dates.csv")
BegEndTable[, "Issue_date" := as.Date(Issue_date, "%d.%m.%Y")]
BegEndTable2 <- BigRawTableBonds[,c("Date_final", "Issue")]
BegEndTable2 <- unique(BegEndTable2)
BegEndTable <- merge(BegEndTable, BegEndTable2, by = "Issue")
bigtable_merged_years <- merge(bigtable_merged, BegEndTable,  by = "Issue", allow.cartesian = T)

#make heatmaps for separate years

for (i in 2015:2020){
bigtable_merged_years_cur <- bigtable_merged_years[(year(Issue_date) <= i)&( year(`Data pogasheniya`) >= i)]
tiff(paste("test_ordered", as.character(i), ".tiff"), units="in", width=50, height=50, res=300, compression = 'lzw')
ggplot(bigtable_merged_years[(year(Issue_date) <= i)&( year(`Data pogasheniya`) >= i)], aes(Date, reorder(Issue, NonNAShare), fill= YTM)) + 
  geom_tile() + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
dev.off()}

###### NAs ####
#Make descriptive statistics. Calculate NAs in each variable.
nas_count_numbers <- c()
nas_count_names <- c()

m <- 1
for (i in BigRawTableBonds){
  na_count <- length(i[!is.na(i)])
  nas_count_numbers <- c(nas_count_numbers, na_count)
  nas_count_names <- c(nas_count_names, colnames(BigRawTableBonds)[m])
  m<-m+1
}
nas_count_table <- data.table("Variable" = nas_count_names, "Non_NAs_number"= nas_count_numbers, "Non_NAs_share" = nas_count_numbers/551139)

library(forcats)

tiff(paste("NAs_bondtable_ordered", ".tiff", sep = ""), units="in", width=10, height=8, res=300, compression = 'lzw')
nas_count_table %>% 
  arrange(Non_NAs_share) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Variable=factor(Variable, levels=Variable)) %>%
  ggplot( aes(x=Variable, y=Non_NAs_share)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
dev.off()


#### Descriptive stats ###
variables_of_interest_dates <- c("Date_trading",	"Date_final")
variables_of_interest <- c("Issue",	"Currency",	"Date_trading",	"Date_final",	"Exch_name",	"Bid_price",	"Ask_price",	"BA_spread",
                           "Av_price",	"Opening_price",	"Min_price",	"Max_price",	"Last_price",	"Av_weighted_price",	"Closing_price",
                           "Indicative_price",	"Indicative_price_type",	"Turnover",	"Number_of_trades",	"Volume",	"Coupon",	"YTM_ind",
                           "YTM_bid", "YTM_ask", "YTM_last", "YTM_close","Coupon_accum",	"Duration",	"Option_date","YTM_off_ind", "YTM_off_bid",
                           "YTM_off_ask", "YTM_off_last", "YTM_off_close",	"YTM_curr_ind", "YTM_curr_bid", "YTM_curr_ask", "YTM_curr_last", "YTM_curr_close","YTM_ind_main",	
                           "Indicative_yield_type",	"Duration_option",	"Trade_regime",	"ISIN",	"G_spread",	"T_spread")
##Number of issues by dates
#by final date
table_dates <- BigRawTableBonds[, c("Date_trading",	"Date_final", "Issue")]
table_dates <- table_dates[, head(.SD, 1), by = "Issue"]
table_dates <- table_dates[order(Date_final)]
maturity_years <- format(as.Date(table_dates$Date_final), "%Y-%m")
maturity_years <- data.table("full" = unique(table_dates$Date_final))
maturity_years[, "year" := format(as.Date(full), "%Y")]
maturity_years <- maturity_years[, head(.SD, 1), by =  "year"]
maturity_breaks <- format(as.Date(maturity_years$full), "%Y-%m")

tiff(paste("finals", ".tiff", sep = ""), units="in", width=20, height=8, res=300, compression = 'lzw')
ggplot(table_dates, aes(x=format(as.Date(Date_final), "%Y-%m"))) + 
  geom_bar(fill="#f68060")+
  xlab("Date of maturity")+
  scale_x_discrete(breaks = maturity_breaks) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

#by trading date CORRECT .x
table_dates <- BigRawTableBonds[, c("Date_trading",	"Date_final", "Issue")]
table_dates <- table_dates[order(Date_trading)]
table_dates <- table_dates[, head(.SD, 1), by = "Issue"]
table_dates <- table_dates[Date_trading!="2015-01-01 00:00:00"]
maturity_years <- format(as.Date(table_dates$Date_trading), "%Y-%m")
maturity_years <- data.table("full" = unique(table_dates$Date_trading))
maturity_years[, "year" := format(as.Date(full), "%Y")]
maturity_years <- maturity_years[, head(.SD, 1), by =  "year"]
maturity_breaks <- format(as.Date(maturity_years$full), "%Y-%m")
tiff(paste("first_trading", ".tiff", sep = ""), units="in", width=20, height=8, res=300, compression = 'lzw')
ggplot(table_dates, aes(x=format(as.Date(Date_trading), "%Y-%m"))) + 
  geom_bar(fill="#f68060")+
  xlab("First date of trading")+
  scale_x_discrete(breaks = maturity_breaks) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

##mean values 

#mean YTM by month
localtable <- BigRawTableBonds[, c("Date_trading","Issue", "YTM_ind_main", "G_spread", "G_spread_interpolated", "Indicative_yield_type")]
localtable[, "ym" := format(as.Date(Date_trading), "%Y-%m")]

localtable_mean <- localtable[, mean(YTM_ind_main), by = "ym"]

tiff(paste("average_YTM", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
ggplot(localtable_mean, aes(x=ym, y = V1)) + 
  geom_bar(fill="#f68060", stat = "identity")+
  xlab("Year-Month")+
  ylab("Average YTM by indicative price")+
  scale_x_discrete(breaks = maturity_breaks) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

localtable_var <- localtable[, var(YTM_ind_main), by = "ym"]

tiff(paste("variance_YTM", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
ggplot(localtable_var, aes(x=ym, y = V1)) + 
  geom_bar(fill="#f68060", stat = "identity")+
  xlab("Year-Month")+
  ylab("Variance of YTM by indicative price")+
  scale_x_discrete(breaks = maturity_breaks) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


localtable_mean <- localtable[G_spread != "", mean(as.numeric(G_spread)), by = "ym"]
localtable_mean <- localtable_mean[V1 < 10^4]
tiff(paste("average_G", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
ggplot(localtable_mean, aes(x=ym, y = V1)) + 
  geom_bar(fill="#f68060", stat = "identity")+
  xlab("Year-Month")+
  ylab("Average G-spread by indicative price")+
  scale_x_discrete(breaks = maturity_breaks) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

localtable_var <- localtable[G_spread != "", var(as.numeric(G_spread)), by = "ym"]
localtable_var<- localtable_var[V1 < 1*10^20]
tiff(paste("variance_G", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
ggplot(localtable_var, aes(x=ym, y = V1)) + 
  geom_bar(fill="#f68060", stat = "identity")+
  xlab("Year-Month")+
  ylab("Variance of G-spread by indicative price")+
  scale_x_discrete(breaks = maturity_breaks) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


localtable_mean <- localtable[, mean(G_spread_interpolated), by = "ym"]

tiff(paste("average_G_interpolated", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
ggplot(localtable_mean, aes(x=ym, y = V1)) + 
  geom_bar(fill="#f68060", stat = "identity")+
  xlab("Year-Month")+
  ylab("Average G_spread by indicative price")+
  scale_x_discrete(breaks = maturity_breaks) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

localtable_mean <- localtable[Indicative_yield_type != "Current", mean(G_spread_interpolated), by = "ym"]
localtable[Indicative_yield_type != "Current"]
tiff(paste("average_G_interpolated_cut", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
ggplot(localtable_mean, aes(x=ym, y = V1)) + 
  geom_bar(fill="#f68060", stat = "identity")+
  xlab("Year-Month")+
  ylab("Average G_spread by indicative price")+
  scale_x_discrete(breaks = maturity_breaks) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


#mean coupon by month
localtable <- BigRawTableBonds[, c("Date_trading","Issue", "Coupon")]
localtable[, "ym" := format(as.Date(Date_trading), "%Y-%m")]
localtable[ym == "2020-05", mean(as.numeric(Coupon))]
localtable[, mean(Coupon)]
localtable_mean <- localtable[!is.na(Coupon), mean(Coupon), by = "ym"]

tiff(paste("average_coupon", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
ggplot(localtable_mean, aes(x=ym, y = V1)) + 
  geom_bar(fill="#f68060", stat = "identity")+
  xlab("Year-Month")+
  ylab("Average coupon of traded bonds")+
  scale_x_discrete(breaks = maturity_breaks) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

localtable_var <- localtable[, var(YTM_ind_main), by = "ym"]

tiff(paste("variance_coupon", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
ggplot(localtable_var, aes(x=ym, y = V1)) + 
  geom_bar(fill="#f68060", stat = "identity")+
  xlab("Year-Month")+
  ylab("Variance of YTM by indicative price")+
  scale_x_discrete(breaks = maturity_breaks) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()
View(BigRawTableBonds)
issuenames <- BigRawTableBonds$Issue
View(issuenames)

#dynamics for one issue ####
library(hrbrthemes)
BigRawTableBonds[, "Days_per_issue" := .N, by = "Issue"]
localtable<- BigRawTableBonds[(Issue == "ING Wholesale Banking Russia, ¡Œ-05")&(Exch_name != "‘ËÍÒËÌ„ Õ‘¿"), c("Issue", "YTM_ind_main", "Date_trading", "G_spread")]
localtable[, mean(YTM_ind_main)]
localtable[(!is.na(G_spread)&(G_spread != "")), mean(as.numeric(G_spread))]

localtable <- localtable[YTM_ind_main < 1]
localtable[, mean(YTM_ind_main)]

tiff(paste("YTM_ing", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
localtable %>%
  ggplot( aes(x=Date_trading, y=YTM_ind_main)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=0.5) +
  theme_ipsum() +
  ggtitle("YTM for ING Wholesale Banking Russia, ¡Œ-05")
dev.off()


localtable<- BigRawTableBonds[(Issue == "ING Wholesale Banking Russia, ¡Œ-05")&(Exch_name != "‘ËÍÒËÌ„ Õ‘¿")&(Indicative_yield_type != "Current"), c("Issue", "YTM_ind_main", "Date_trading", "G_spread")]
localtable[, mean(YTM_ind_main)]

localtable <- localtable[YTM_ind_main < 0.8]
localtable[, mean(YTM_ind_main)]

tiff(paste("YTM_cut_ing", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
localtable %>%
  ggplot( aes(x=Date_trading, y=YTM_ind_main)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=0.5) +
  theme_ipsum() +
  ggtitle("YTM for ING Wholesale Banking Russia, no 'current' yields, ¡Œ-05")
dev.off()




tiff(paste("Gspread_ing", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
localtable %>%
  ggplot( aes(x=Date_trading, y=G_spread)) +    
  geom_point(shape=21, color="blue", fill="#69b3a2", size=0.5) +
  theme_ipsum() +
  ggtitle("G-spread for ING Wholesale Banking Russia, ¡Œ-05")+ theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank())
dev.off()

localtable<- BigRawTableBonds[(Issue == "¬›¡, ¡Œ-14")&(Exch_name != "‘ËÍÒËÌ„ Õ‘¿"), c("Issue", "YTM_ind_main", "Date_trading", "G_spread")]
localtable[, mean(YTM_ind_main)]
min(localtable$G_spread)
View(localtable)
localtable[(!is.na(G_spread)&(G_spread != "")), mean(as.numeric(G_spread))]


localtable <- localtable[YTM_ind_main < 1]
localtable[, mean(YTM_ind_main)]
tiff(paste("YTM_veb", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
localtable %>%
  ggplot( aes(x=Date_trading, y=YTM_ind_main)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=0.5) +
  theme_ipsum() +
  ggtitle("YTM for ¬›¡, ¡Œ-14")
dev.off()

localtable<- BigRawTableBonds[(Issue == "¬›¡, ¡Œ-14")&(Exch_name != "‘ËÍÒËÌ„ Õ‘¿")&(Indicative_yield_type != "Current"), c("Issue", "YTM_ind_main", "Date_trading", "G_spread")]
localtable[, mean(YTM_ind_main)]

localtable <- localtable[YTM_ind_main < 0.8]
localtable[, mean(YTM_ind_main)]

tiff(paste("YTM_cut_veb", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
localtable %>%
  ggplot( aes(x=Date_trading, y=YTM_ind_main)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=0.5) +
  theme_ipsum() +
  ggtitle("YTM for ING Wholesale Banking Russia, no 'current' yields, ¡Œ-05")
dev.off()
1 - length(BigRawTableBonds[(Indicative_yield_type != "Current")]$V1)/length(BigRawTableBonds$V1)

tiff(paste("Gspread_veb", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
localtable %>%
  ggplot( aes(x=Date_trading, y=G_spread)) +    
  geom_point(shape=21, color="blue", fill="#69b3a2", size=0.5) +
  theme_ipsum() +
  ggtitle("G-spread for ¬›¡, ¡Œ-14")+ theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank())
dev.off()


localtable<- BigRawTableBonds[(Issue == "ING Wholesale Banking Russia, ¡Œ-05")&(Exch_name != "‘ËÍÒËÌ„ Õ‘¿"), c("Issue", "YTM_ind_main", "Date_trading", "G_spread_interpolated")]

tiff(paste("G_spread_interpolated_ing", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
localtable %>%
  ggplot( aes(x=Date_trading, y=G_spread_interpolated)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=0.5) +
  theme_ipsum() +
  ggtitle("G-spread interpolated for ING Wholesale Banking Russia, ¡Œ-05")
dev.off()

localtable<- BigRawTableBonds[(Issue == "ING Wholesale Banking Russia, ¡Œ-05")&(Exch_name != "‘ËÍÒËÌ„ Õ‘¿"), c("Issue", "YTM_ind_main", "Date_trading", "G_spread_interpolated", "Indicative_yield_type")]
localtable <- localtable[Indicative_yield_type != "Current"]
localtable <- localtable[G_spread_interpolated <0.7]

tiff(paste("G_spread_interpolated_cut_ing", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
localtable %>%
  ggplot( aes(x=Date_trading, y=G_spread_interpolated)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=0.5) +
  theme_ipsum() +
  ggtitle("G-spread interpolated for ING Wholesale Banking Russia, ¡Œ-05")
dev.off()
#curve of risk-free for one day 
localtable <- BigRawTableBonds[, c("Date_trading", "RF_interpolated", "Days_to_maturity")]
localtable <- localtable[Date_trading == localtable[10000]$Date_trading]
localtable <- unique(localtable, by = "Days_to_maturity")
numbers <- as.numeric(c("0.25", "0.5", "0.75", "1", "2", "3", "5", "7", "10", "15", "20", "30"))
localtable[, "Source" := as.factor(ifelse(Days_to_maturity/360 %in% numbers, "Input", "Interpolated"))]
p <- ggplot(data = localtable[1:100], aes(x = as.numeric(Days_to_maturity), y = RF_interpolated)) +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=0.5) +
  theme_ipsum() +
  ggtitle("2018-08-07")+
  xlab("Days remaining to maturity") +
  ylab("Interpolated risk-free rate")

p

#histogram with comparizon ####

localtable <- BigRawTableBonds[, c("Date_trading","Issue", "YTM_ind_main", "State_bank")]
localtable[, "ym" := format(as.Date(Date_trading), "%Y-%m")]

State_ytm <- localtable[State_bank==1, mean(YTM_ind_main), by = "ym"]$V1
Private_ytm <- localtable[State_bank==0, mean(YTM_ind_main), by = "ym"]$V1
dates <- localtable[State_bank==0, mean(YTM_ind_main), by = "ym"]$ym
localtable <- data.table("Private_YTM" = Private_ytm, "State_YTM" = State_ytm, "Year-Month" = dates)
localtable <- localtable[order(`Year-Month`)]
localtable <- gather(localtable, "Ownership", "YTM", -`Year-Month`)
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

tiff(paste("YTM_separate", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
ggplot(localtable, aes(fill=Ownership, y=YTM, x=`Year-Month`)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_x_discrete(breaks = maturity_breaks) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()




localtable <- BigRawTableBonds[, c("Date_trading","Issue", "YTM_ind_main", "State_bank", "Indicative_yield_type")]
localtable[, "Year-Month" := format(as.Date(Date_trading), "%Y-%m")]
localtable[, "YTM" := mean(YTM_ind_main), by = c("Year-Month", "Indicative_yield_type")]

tiff(paste("Yield_separate_type", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
ggplot(localtable, aes(fill=Indicative_yield_type, y=YTM/10000, x=`Year-Month`)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_x_discrete(breaks = maturity_breaks) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


#distributions inside dates
localtable <- BigRawTableBonds[, c("Date_trading", "YTM_ind_main", "G_spread_interpolated", "G_spread")]
first <- localtable$Date_trading[340000]
localtable <- localtable[(Date_trading == first)&(YTM_ind_main < 1)]

png(paste("Yield_density_20180222", ".png", sep = ""), units="in", width=15, height=10, res=300)
ggplot(data = localtable, aes(x = YTM_ind_main))+ 
  geom_density(fill="#f68060")+
  xlab("Yield")+
  ylab("Density")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


localtable_big <- BigRawTableBonds[, c("Date_trading", "YTM_ind_main", "G_spread_interpolated", "G_spread")]
uniquedates<-unique(localtable_big$Date_trading)
pvalues_shapiro <- c()
numbers_shapiro <- c()
for (i in 1:length(uniquedates)){
  m <- uniquedates[i]
  localtable <- localtable_big[(Date_trading == m)&(YTM_ind_main < 1)]
  if ((length(localtable$Date_trading) > 3)&(length(localtable$Date_trading) < 5000)){
  newpvalue_shapiro <- shapiro.test(localtable$YTM_ind_main)$p.value
  pvalues_shapiro <- c(pvalues_shapiro,newpvalue_shapiro)
  newnumber_shapiro <- length(localtable$Date_trading)
  numbers_shapiro <- c(numbers_shapiro, newnumber_shapiro)}
}

localtable <- data.table("pvalues" = pvalues_shapiro, "numbers" = numbers_shapiro, "index" = seq(1, length(numbers_shapiro)))
p1 <- ggplot(localtable, aes(x=index, y=pvalues)) +
  geom_line(color="#69b3a2", size=0.5) +
  ggtitle("Temperature: range 1-10") +
  theme_ipsum()
p1
p2 <- ggplot(localtable, aes(x=index, y=numbers)) +
  geom_line(color="grey",size=0.5) +
  ggtitle("Price: range 1-100") +
  theme_ipsum()
p2

localtable_small <- localtable[index <= 1500]
p1 <- ggplot(localtable_small, aes(x=index, y=pvalues)) +
  geom_line(color="#69b3a2", size=0.5) +
  theme_ipsum()+
  theme(axis.text.x = element_blank()) +
png(paste("Shapiro_graph", ".png", sep = ""), units="in", width=15, height=10, res=300)
p1
dev.off()
