library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)
library(bit24)
library(stringr)
library(xtable)
library(hrbrthemes)

setwd("C:/Coursework/Data_coursework")
Sys.setenv(LANG = "en")

BigBanksTable <- fread("big_spread.csv", colClasses = "numeric")
View(BigBanksTable)
unique(BigBanksTable$Year)
localtable <- BigBanksTable[, head(.SD, 1), by = "REGN"]
png(paste("density_nas_pnl", ".png", sep = ""), units="in", width=15, height=10, res = 100)
ggplot(BigBanksTable, aes(x=NAs_pnl, y = )) + 
  geom_density(fill="#f68060")+
  xlab("Density of rows")+
  ylab("Share of non-NA pnl entries in a row")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


locallist <- colnames(BigBanksTable)
locallist <- locallist[str_detect(locallist, "pnl_old")]
localtable_old <- BigBanksTable[, ..locallist]
locallist <- colnames(BigBanksTable)
locallist <- locallist[(str_detect(locallist, "pnl")&(!(str_detect(locallist, "pnl_old"))))]
localtable_new <- BigBanksTable[, ..locallist]

countzero <- function(x){
  zeros <- length(x[x!=0])/length(x)
  return(zeros)
}

zerosshares_old <- apply(localtable_old, 1, countzero)
zerosshares_new <- apply(localtable_new, 1, countzero)
zeroshares <- 1- (zerosshares_new + zerosshares_old)/2
localtable <- data.table("zero" = zeroshares)
tiff(paste("density_zeros_pnl", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
ggplot(localtable, aes(x=zero, y = )) + 
  geom_density(fill="#f68060")+
  xlab("Share of non-zero pnl entries in a row")+
  ylab("Density of rows")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


locallist <- colnames(BigBanksTable)
locallist <- locallist[!str_detect(locallist, "pnl")]
localtable <- BigBanksTable[, ..locallist]

zeroshares <- apply(localtable, 1, countzero)
localtable <- data.table("zero" = zeroshares)
tiff(paste("density_zeros", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
ggplot(localtable, aes(x=zero, y = )) + 
  geom_density(fill="#f68060")+
  xlab("Share of non-zero pnl entries in a row")+
  ylab("Density of rows")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

tiff(paste("density_zeros", ".tiff", sep = ""), units="in", width=15, height=10, res=300, compression = 'lzw')
ggplot(BigBanksTable, aes(x=zero, y = )) + 
  geom_density(fill="#f68060")+
  xlab("Share of non-zero pnl entries in a row")+
  ylab("Density of rows")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

BigBondsTable <- fread("Big_medium_rare_table_bonds.csv")

yield <- BigBondsTable[, c("YTM_ind_main", "cregnum", "Date_trading")]
yield[, "cregnum" := as.character(cregnum)]
locallist <- colnames(BigBanksTable)
locallist <- locallist[!str_detect(locallist, "\\D+")]
locallist <- c(locallist, "REGN")
localtable <- BigBanksTable[, ..locallist]
localtable[, "REGN" := as.character(REGN)]
localtable <- merge(yield, localtable, all.x=T, allow.cartesian = T, by.x = "cregnum", by.y = "REGN")
corrs <- apply(localtable, 0, cor, y = yield)
View(localtable)

localtable <- BigBanksTable[, c("REGN", "40903")]
localtable[, "REGN" := as.character(REGN)]
localtable <- localtable[!is.na(cregnum)]
localtable <- merge(yield, localtable, all.x=T, allow.cartesian = T, by.x = "cregnum", by.y = "REGN")


# PCA #####
#bs
locallist <- colnames(BigBanksTable)
locallist <- locallist[!str_detect(locallist, "\\D+")]
localtable <- BigBanksTable[, ..locallist]
localtable <- localtable[, lapply(.SD, as.numeric)]
ranks <- apply(localtable, 2, function(x){length(unique(x))})
locallist <- locallist[ranks!=1]
localtable <- BigBanksTable[, ..locallist]
localtable <- localtable[, lapply(.SD, as.numeric)]


scaledlocaltable <- prcomp(localtable, scale = T)
pca_bs_stdev <- scaledlocaltable$sdev
pca_bs_stdev <- pca_bs_stdev^2
pca_bs_stdev <- pca_bs_stdev/sum(pca_bs_stdev)
data_bs <- data.table("pca_bs" = pca_bs_stdev, "number" = seq(1, length(pca_bs_stdev)))
png(paste("pca_bs", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_bs %>%
  ggplot( aes(x=number, y=pca_bs)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of a component") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Share of variance captured \nby each subsequent component \nin Balance Sheet, 1-1819")
dev.off()

png(paste("pca_bs_short", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_bs[number <= 20] %>%
  ggplot( aes(x=number, y=pca_bs)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of a component") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Share of variance captured \nby each subsequent component \nin Balance Sheet, 1-20")
dev.off()

data_bs[, "cum_pca_bs" := cumsum(pca_bs)]

data_bs <- data_bs[cum_pca_bs <= 0.9]
data_bs[, cum_pca_bs := round(cum_pca_bs, 1)]
data_bs <- unique(data_bs, by = "cum_pca_bs")
data_bs[, "pca_bs" := NULL]
print(xtable(data_bs, type = "latex"), file = "cum_pca_bs.tex")



png(paste("cum_pca_bs", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_bs %>%
  ggplot( aes(x=number, y=cum_pca_bs)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of components") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Cumulative share of variance captured \nby each subsequent component \nin Balance Sheet, 1-1819")
dev.off()

png(paste("cum_pca_bs_short", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_bs[number <= 20] %>%
  ggplot( aes(x=number, y=cum_pca_bs)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of components") + 
  ylab("Share of variance explained by this number of components")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Cumulative share of variance captured \nby each subsequent component \nin Balance Sheet, 1-20")
dev.off()

#pnl_old

locallist <- colnames(BigBanksTable)
locallist <- locallist[str_detect(locallist, "old")]
localtable <- BigBanksTable[, ..locallist]
localtable <- localtable[, lapply(.SD, as.numeric)]
localtable[ , which(apply(localtable, 2, var) != 0)]

ranks <- apply(localtable, 2, function(x){var(x)})
locallist <- locallist[ranks != 0]

localtable <- BigBanksTable[, ..locallist]
localtable <- localtable[, lapply(.SD, as.numeric)]


scaledlocaltable <- prcomp(localtable, scale = T)
pca_is_stdev <- scaledlocaltable$sdev
pca_is_stdev <- pca_is_stdev^2
pca_is_stdev <- pca_is_stdev/sum(pca_is_stdev)
data_pnlold <- data.table("pca_is" = pca_is_stdev, "number" = seq(1, length(pca_is_stdev)))
png(paste("pca_is", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnlold %>%
  ggplot( aes(x=number, y=pca_is)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of a component") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Share of variance captured \nby each subsequent component \nin Income Statement, 1-606")
dev.off()

png(paste("pca_is_short", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnlold[number <= 20] %>%
  ggplot( aes(x=number, y=pca_is)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of a component") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Share of variance captured \nby each subsequent component \nin Income Statement, 1-20")
dev.off()

data_pnlold[, "cum_pca_is" := cumsum(pca_is)]

data_pnlold <- data_pnlold[cum_pca_is <= 0.9]
data_pnlold[, cum_pca_is := round(cum_pca_is, 1)]
data_pnlold <- unique(data_pnlold, by = "cum_pca_is")
data_pnlold[, "pca_is" := NULL]
print(xtable(data_pnlold, type = "latex"), file = "cum_pca_is.tex")


png(paste("cum_pca_is", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnlold %>%
  ggplot( aes(x=number, y=cum_pca_is)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of components") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Cumulative share of variance captured \n\nby each subsequent component \nin Income Statement, 1-606")
dev.off()

png(paste("cum_pca_is_short", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnlold[number <= 20] %>%
  ggplot( aes(x=number, y=cum_pca_is)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of components") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Cumulative share of variance captured \n\nby each subsequent component \nin Income Statement, 1-20")
dev.off()

# is new 

locallist <- colnames(BigBanksTable)
locallist <- locallist[(str_detect(locallist, "pnl"))&(!str_detect(locallist, "old"))&(!str_detect(locallist, "s"))]
localtable <- BigBanksTable[, ..locallist]
localtable <- localtable[, lapply(.SD, as.numeric)]
localtable[ , which(apply(localtable, 2, var) != 0)]

ranks <- apply(localtable, 2, function(x){var(x)})
locallist <- locallist[ranks != 0]
localtable <- BigBanksTable[, ..locallist]
localtable <- localtable[, lapply(.SD, as.numeric)]

scaledlocaltable <- prcomp(localtable, scale = T)
pca_isnew_stdev <- scaledlocaltable$sdev
pca_isnew_stdev <- pca_isnew_stdev^2
pca_isnew_stdev <- pca_isnew_stdev/sum(pca_isnew_stdev)
data_pnl <- data.table("pca_isnew" = pca_isnew_stdev, "number" = seq(1, length(pca_isnew_stdev)))
png(paste("pca_isnew", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnl %>%
  ggplot( aes(x=number, y=pca_isnew)) +    
  geom_line( color="grey", size = 2) +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of a component") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Share of variance captured \nby each subsequent component \nin Income Statement, 1-1810")
dev.off()

png(paste("pca_isnew_short", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnl[number <= 20] %>%
  ggplot( aes(x=number, y=pca_isnew)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of a component") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Share of variance captured \nby each subsequent component \nin Income Statement, 1-20")
dev.off()

data_pnl[, "cum_pca_isnew" := cumsum(pca_isnew)]
data_pnl <- data_pnl[cum_pca_isnew <= 0.9]
data_pnl[, cum_pca_isnew := round(cum_pca_isnew, 1)]
data_pnl <- unique(data_pnl, by = "cum_pca_isnew")
data_pnl[, "pca_isnew" := NULL]
print(xtable(data_pnl, type = "latex"), file = "cum_pca_isnew.tex")
png(paste("cum_pca_isnew", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnl %>%
  ggplot( aes(x=number, y=cum_pca_isnew)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of components") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Cumulative share of variance captured \nby each subsequent component \nin Income Statement, 1-1810")
dev.off()

png(paste("cum_pca_isnew_short", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnl[number <= 20] %>%
  ggplot( aes(x=number, y=cum_pca_isnew)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of components") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Cumulative share of variance captured \nby each subsequent component \nin Income Statement, 1-20")
dev.off()

# PCA aggr#####

bs_names <- c("Cash_and_equivalents","NOSTRO","NOSTRO_minus","Credits_to_banks","Credits_to_banks_minus","Securities","Securities_minus","Corporate_credits","Corporate_credits_minus","Retail_credits","Retail_credits_minus","Reserves_for_delayed_loans","Reserves_for_delayed_loans_minus","Other_assets","Other_assets_minus","LORO","Interbank_loans","Corporate_current_accounts","Corporate_current_accounts_minus","Retail_current_accounts","Trading_accounts","Corporate_time_deposits","Retail_time_deposits","Bonds","Bills","Bills_minus","Certificates_of_deposit","Other_liabilities","Other_liabilities_minus","Main_capital","Main_capital_minus","Retained_earnings","Retained_earnings_minus","Retained_earnings_this_year","Retained_earnings_this_year_minus","Next_periods","Next_periods_minus","Credit_limits","Contingent_liabilities_minus","Securities_for_credits_given","Unexecuted_debts","Other_offbalance","Other_offbalance_minus")
pnlold_names <- c("NOSTRO_pnlold","Credits_to_banks_pnlold","Corporate_credits_pnlold","Retail_credits_pnlold","Debt_assets_pnlold","Bills","Current_accounts_of_banks_pnlold","Term_deposits_of_banks_pnlold","Term_deposits_of_banks_minus_pnlold","Corporate_current_accounts_pnlold","Retail_current_accounts_pnlold","Corporate_term_deposits_pnlold","Retail_term_deposits_pnlold","Expenditures_on_bonds_issued","Expenditures_on_bills_issued","Certificates_pnlold","Fee_income_pnlold","Fee_expenditures_pnlold","Realized_securities_issued_pnlold","Realized_securities_issued_minus_pnlold","Unrealized_securities_pnlold","Unrealized_securities_pnlold_minus","Realized_fc_pnlold","Unrealized_fc_pnlold","Other_corporates_pnlold","Derivatives_etc_pnlold","Derivatives_etc_minus_pnlold","Other_income_pnlold","Other_expenditures_pnlold","Other_expenditures_minus_pnlold","Changes_reserves","Personnel_etc_pnlold","Organization_etc","Organization_etc_minus","PBT_pnlold","Taxes_pnlold","Taxes_minus_pnlold","Retained_earnings_pnlold")

pnl_names <- c("NOSTRO_pnl","Credits_to_banks_pnl","Credits_to_banks_pnl_minus","Corporate_credits_pnl","Corporate_credits_pnl_minus","Retail_credits_pnl","Retail_credits_pnl_minus","Debt_pnl","Debt_pnl_minus","Bills_pnl","Current_accounts_of_banks_pnl_minus","Term_deposits_of_banks_minus_pnl","Current_accounts_corporate_pnl_minus","Current_accounts_retail_pnl_minus","Term_deposits_corporate_minus_pnl","Term_deposits_corporate_pnl","Term_deposits_retail_minus_pnl","Securities_issued","Securities_issued_minus","Fee_income","Fee_expenditure","Fee_expenditure_minus","Realized_securities","Realized_securities_minus","Unrealized_securities","Unrealized_securities_minus","Realized_fc","Realized_fc_minus","Realized_fc_minus","Unrealized_fc","Unrealized_fc_minus","Other_corporates","Derivatives_etc","Derivatives_etc_minus","Other_income","Other_income_minus","Other_expenditures","Other_expenditures_minus","Reserve_adjustments","Reserve_adjustments_minus","Personnel_etc","Personnel_etc_minus","Organization_etc","Organization_etc_minus","Income_before_taxes","Income_before_taxes_minus","Taxes","Taxes_minus","Unretained_earnings","Unretained_earnings_minus","Other_comprehensive_income","Other_comprehensive_income_minus")

#bs
locallist <- colnames(BigBanksTable)
locallist <- locallist[!str_detect(locallist, "\\D+")]
locallist <- c(locallist, bs_names)
localtable <- BigBanksTable[, ..locallist]
localtable <- localtable[, lapply(.SD, as.numeric)]
ranks <- apply(localtable, 2, function(x){length(unique(x))})
locallist <- locallist[ranks!=1]
localtable <- BigBanksTable[, ..locallist]
localtable <- localtable[, lapply(.SD, as.numeric)]


scaledlocaltable <- prcomp(localtable, scale = T)
pca_bs_stdev <- scaledlocaltable$sdev
pca_bs_stdev <- pca_bs_stdev^2
pca_bs_stdev <- pca_bs_stdev/sum(pca_bs_stdev)
data_bs_agg <- data.table("pca_bs" = pca_bs_stdev, "number" = seq(1, length(pca_bs_stdev)))
png(paste("pca_bs_agg", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_bs_agg %>%
  ggplot( aes(x=number, y=pca_bs)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of a component") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Share of variance captured \nby each subsequent component \nin Balance Sheet with aggregations, 1-1477")
dev.off()

png(paste("pca_bs_short_agg", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_bs_agg[number <= 20] %>%
  ggplot( aes(x=number, y=pca_bs)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of a component") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Share of variance captured \nby each subsequent component \nin Balance Sheet with aggregations, 1-20")
dev.off()

data_bs_agg[, "cum_pca_bs" := cumsum(pca_bs)]

data_bs_agg <- data_bs_agg[cum_pca_bs <= 0.9]
data_bs_agg[, cum_pca_bs := round(cum_pca_bs, 1)]
data_bs_agg <- unique(data_bs_agg, by = "cum_pca_bs")
data_bs_agg[, "pca_bs" := NULL]
print(xtable(data_bs_agg, type = "latex"), file = "cum_pca_bs_agg.tex")



png(paste("cum_pca_bs_agg", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_bs_agg %>%
  ggplot( aes(x=number, y=cum_pca_bs)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of components") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Cumulative share of variance captured \nby each subsequent component \nin Balance Sheet, 1-1477")
dev.off()

png(paste("cum_pca_bs_short_agg", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_bs_agg[number <= 20] %>%
  ggplot( aes(x=number, y=cum_pca_bs)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of components") + 
  ylab("Share of variance explained by this number of components")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Cumulative share of variance captured \nby each subsequent component \nin Balance Sheet, 1-20")
dev.off()

#pnl_old

locallist <- colnames(BigBanksTable)
locallist <- locallist[str_detect(locallist, "old")]
locallist <- c(locallist, pnlold_names)
localtable <- BigBanksTable[, ..locallist]
localtable <- localtable[, lapply(.SD, as.numeric)]
localtable[ , which(apply(localtable, 2, var) != 0)]

ranks <- apply(localtable, 2, function(x){var(x)})
locallist <- locallist[ranks != 0]

localtable <- BigBanksTable[, ..locallist]
localtable <- localtable[, lapply(.SD, as.numeric)]


scaledlocaltable <- prcomp(localtable, scale = T)
pca_is_stdev <- scaledlocaltable$sdev
pca_is_stdev <- pca_is_stdev^2
pca_is_stdev <- pca_is_stdev/sum(pca_is_stdev)
data_pnlold_agg <- data.table("pca_is" = pca_is_stdev, "number" = seq(1, length(pca_is_stdev)))
png(paste("pca_is_agg", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnlold_agg %>%
  ggplot( aes(x=number, y=pca_is)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of a component") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Share of variance captured \nby each subsequent component \nin Income Statement with aggregations, 1-394")
dev.off()

png(paste("pca_is_short_agg", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnlold_agg[number <= 20] %>%
  ggplot( aes(x=number, y=pca_is)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of a component") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Share of variance captured \nby each subsequent component \nin Income Statement with aggregations, 1-20")
dev.off()

data_pnlold_agg[, "cum_pca_is" := cumsum(pca_is)]

data_pnlold_agg <- data_pnlold_agg[cum_pca_is <= 0.9]
data_pnlold_agg[, cum_pca_is := round(cum_pca_is, 1)]
data_pnlold_agg <- unique(data_pnlold_agg, by = "cum_pca_is")
data_pnlold_agg[, "pca_is" := NULL]
print(xtable(data_pnlold_agg, type = "latex"), file = "cum_pca_is_agg.tex")


png(paste("cum_pca_is_agg", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnlold_agg %>%
  ggplot( aes(x=number, y=cum_pca_is)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of components") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Cumulative share of variance captured \nby each subsequent component \nin Income Statement with aggregations, 1-394")
dev.off()

png(paste("cum_pca_is_short_agg", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnlold_agg[number <= 20] %>%
  ggplot( aes(x=number, y=cum_pca_is)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of components") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Cumulative share of variance captured \nby each subsequent component \nin Income Statement with aggregations, 1-20")
dev.off()

# is new 

locallist <- colnames(BigBanksTable)
locallist <- locallist[(str_detect(locallist, "pnl"))&(!str_detect(locallist, "old"))&(!str_detect(locallist, "s"))]
locallist <- c(locallist, pnl_names)

localtable <- BigBanksTable[, ..locallist]
localtable <- localtable[, lapply(.SD, as.numeric)]
localtable[ , which(apply(localtable, 2, var) != 0)]

ranks <- apply(localtable, 2, function(x){var(x)})
locallist <- locallist[ranks != 0]
localtable <- BigBanksTable[, ..locallist]
localtable <- localtable[, lapply(.SD, as.numeric)]

scaledlocaltable <- prcomp(localtable, scale = T)
pca_isnew_stdev <- scaledlocaltable$sdev
pca_isnew_stdev <- pca_isnew_stdev^2
pca_isnew_stdev <- pca_isnew_stdev/sum(pca_isnew_stdev)
data_pnl_agg <- data.table("pca_isnew" = pca_isnew_stdev, "number" = seq(1, length(pca_isnew_stdev)))
png(paste("pca_isnew_agg", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnl_agg %>%
  ggplot( aes(x=number, y=pca_isnew)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of a component") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Share of variance captured \nby each subsequent component \nin Income Statement with aggregations, 1-1858")
dev.off()

png(paste("pca_isnew_short_agg", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnl_agg[number <= 20] %>%
  ggplot( aes(x=number, y=pca_isnew)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of a component") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Share of variance captured \nby each subsequent component \nin Income Statement with aggregations, 1-20")
dev.off()

data_pnl_agg[, "cum_pca_isnew" := cumsum(pca_isnew)]
data_pnl_agg <- data_pnl_agg[cum_pca_isnew <= 0.9]
data_pnl_agg[, cum_pca_isnew := round(cum_pca_isnew, 1)]
data_pnl_agg <- unique(data_pnl_agg, by = "cum_pca_isnew")
data_pnl_agg[, "pca_isnew" := NULL]
print(xtable(data_pnl_agg, type = "latex"), file = "cum_pca_isnew_agg.tex")
png(paste("cum_pca_isnew_agg", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnl_agg %>%
  ggplot( aes(x=number, y=cum_pca_isnew)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of components") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Cumulative share of variance captured \nby each subsequent component \nin Income Statement with aggregations, 1-1858")
dev.off()

png(paste("cum_pca_isnew_short_agg", ".png", sep = ""), units="in", width=15, height=10, res=100)
data_pnl_agg[number <= 20] %>%
  ggplot( aes(x=number, y=cum_pca_isnew)) +    
  geom_line( color="grey") +
  geom_point(shape=21, color="blue", fill="#69b3a2", size=2) +
  theme_ipsum() +
  xlab("Number of components") + 
  ylab("Share of variance explained by this component")+
  theme(axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25), plot.title = element_text(size=30))+
  ggtitle("Cumulative share of variance captured \nby each subsequent component \nin Income Statement with aggregations, 1-20")
dev.off()

#PC filtration #####
localtable

CreateScaledTables <- function(data_raw, agg = F){
  
  
  if (agg == F){
  #create table for pnlnew, not aggregated
    
  locallist <- colnames(data_raw)
  locallist <- locallist[(str_detect(locallist, "pnl"))&(!str_detect(locallist, "old"))&(!str_detect(locallist, "s"))]
  localtable <- data_raw[, ..locallist]
  localtable <- localtable[, lapply(.SD, as.numeric)]
  localtable[ , which(apply(localtable, 2, var) != 0)]
  
  ranks <- apply(localtable, 2, function(x){var(x)})
  locallist <- locallist[ranks != 0]
  localtable <- data_raw[, ..locallist]
  localtable <- localtable[, lapply(.SD, as.numeric)]
  
  scaledlocaltable_pnlnew <- prcomp(localtable, scale = T)
  
  #create table for pnlold, not aggregated
  
  
  locallist <- colnames(data_raw)
  locallist <- locallist[str_detect(locallist, "old")]
  locallist <- c(locallist, pnlold_names)
  localtable <- data_raw[, ..locallist]
  localtable <- localtable[, lapply(.SD, as.numeric)]
  localtable[ , which(apply(localtable, 2, var) != 0)]
  
  ranks <- apply(localtable, 2, function(x){var(x)})
  locallist <- locallist[ranks != 0]
  
  localtable <- data_raw[, ..locallist]
  localtable <- localtable[, lapply(.SD, as.numeric)]
  
  
  scaledlocaltable_pnlold <- prcomp(localtable, scale = T)
  
  #create table for bs, not aggregated
  
  locallist <- colnames(data_raw)
  locallist <- locallist[!str_detect(locallist, "\\D+")]
  localtable <- data_raw[, ..locallist]
  localtable <- localtable[, lapply(.SD, as.numeric)]
  ranks <- apply(localtable, 2, function(x){length(unique(x))})
  locallist <- locallist[ranks!=1]
  localtable <- data_raw[, ..locallist]
  localtable <- localtable[, lapply(.SD, as.numeric)]
  
  
  scaledlocaltable_bs <- prcomp(localtable, scale = T)
  
  return(list(scaledlocaltable_bs, scaledlocaltable_pnlnew, scaledlocaltable_pnlold))
  }
  
  
  if (agg == T){
    #create table for pnl new, aggregated
    locallist <- colnames(data_raw)
    locallist <- locallist[(str_detect(locallist, "pnl"))&(!str_detect(locallist, "old"))&(!str_detect(locallist, "s"))]
    locallist <- c(locallist, pnl_names)
    
    localtable <- data_raw[, ..locallist]
    localtable <- localtable[, lapply(.SD, as.numeric)]
    localtable[ , which(apply(localtable, 2, var) != 0)]
    
    ranks <- apply(localtable, 2, function(x){var(x)})
    locallist <- locallist[ranks != 0]
    localtable <- data_raw[, ..locallist]
    localtable <- localtable[, lapply(.SD, as.numeric)]
    
    scaledlocaltable_pnlnew_aggr <- prcomp(localtable, scale = T)
    #create table for pnl old, aggregated
    
    locallist <- colnames(data_raw)
    locallist <- locallist[str_detect(locallist, "old")]
    locallist <- c(locallist, pnlold_names)
    localtable <- data_raw[, ..locallist]
    localtable <- localtable[, lapply(.SD, as.numeric)]
    localtable[ , which(apply(localtable, 2, var) != 0)]
    
    ranks <- apply(localtable, 2, function(x){var(x)})
    locallist <- locallist[ranks != 0]
    
    localtable <- data_raw[, ..locallist]
    localtable <- localtable[, lapply(.SD, as.numeric)]
    
    
    scaledlocaltable_pnlold_aggr <- prcomp(localtable, scale = T)
    
    
    #create table for bs, aggregated
    
    
    locallist <- colnames(data_raw)
    locallist <- locallist[!str_detect(locallist, "\\D+")]
    locallist <- c(locallist, bs_names)
    localtable <- data_raw[, ..locallist]
    localtable <- localtable[, lapply(.SD, as.numeric)]
    ranks <- apply(localtable, 2, function(x){length(unique(x))})
    locallist <- locallist[ranks!=1]
    localtable <- data_raw[, ..locallist]
    localtable <- localtable[, lapply(.SD, as.numeric)]
    
    
    scaledlocaltable_bs_aggr <- prcomp(localtable, scale = T) 
    return(list(scaledlocaltable_bs_aggr, scaledlocaltable_pnlnew_aggr, scaledlocaltable_pnlold_aggr))
    
  }
}
CreateTableOfComponents <- function(NumOfComps_bs, NumOfComps_pnl, NumOfComps_pnlold, scaledtables, agg = F){
if (agg == T){
  
  
  #make small tables
  localsmalltable_bs_agg <- (scaledtables[[1]]$x)[, 1:NumOfComps_bs]
  localsmalltable_pnlnew_agg <- (scaledtables[[2]]$x)[, 1:NumOfComps_pnl]
  localsmalltable_pnlold_agg <- (scaledtables[[3]]$x)[, 1:NumOfComps_pnlold]

  #convert to data tables 
  
  localsmalltable_pnlnew_agg <- as.data.table(localsmalltable_pnlnew_agg)
  localsmalltable_pnlold_agg <- as.data.table(localsmalltable_pnlold_agg)
  localsmalltable_bs_agg <- as.data.table(localsmalltable_bs_agg)
  #add numeric column
  
  localsmalltable_bs_agg[, "num" := 1:.N]
  localsmalltable_pnlnew_agg[, "num" := 1:.N] 
  localsmalltable_pnlold_agg[, "num" := 1:.N]
  
  #merge
  
  bigtable_agg <- merge(localsmalltable_bs_agg, localsmalltable_pnlnew_agg, by = "num")
  bigtable_agg <- merge(bigtable_agg, localsmalltable_pnlold_agg, by = "num")

  return(bigtable_agg)
  
  }
  
  if (agg == F) {
  
  #make small tables


  localsmalltable_bs <-(scaledtables[[1]]$x)[, 1:NumOfComps_bs]
  localsmalltable_pnlnew <- (scaledtables[[2]]$x)[, 1:NumOfComps_pnl]
  localsmalltable_pnlold <- (scaledtables[[3]]$x)[, 1:NumOfComps_pnlold]
  
  #convert them to data tables
  localsmalltable_pnlnew <- as.data.table(localsmalltable_pnlnew)
  localsmalltable_pnlold <- as.data.table(localsmalltable_pnlold)
  localsmalltable_bs <- as.data.table(localsmalltable_bs)
  
  
  #add numeric column
  localsmalltable_pnlnew[, "num" := 1:.N]
  localsmalltable_pnlold[, "num" := 1:.N] 
  localsmalltable_bs[, "num" := 1:.N]
  
  #merge
  bigtable <- merge(localsmalltable_bs, localsmalltable_pnlnew, by = "num")
  bigtable <- merge(bigtable, localsmalltable_pnlold, by = "num")


  return(bigtable)
  }
  
  
}
data_bs_small <- data_bs[2:6]
data_pnl_small <- data_pnl[2:6]
data_pnlold_small <- data_pnlold[2:6]

data_bs_agg_small <- data_bs_agg[2:6]
data_pnl_agg_small <- data_pnl_agg[2:6]
data_pnlold_agg_small <- data_pnlold_agg[2:6]

data_breaks <- cbind(data_bs_small,data_pnl_small, data_pnlold_small)
data_breaks_agg <- cbind(data_bs_agg_small,data_pnl_agg_small, data_pnlold_agg_small)


dir.create("A")



names <- c("a", "b", "c", "d", "e")
addlist <- c("REGN", "Year", "Month", "NAs_pnl", "NAs_bs", "NAs_begend")
addtable <- BigBanksTable[, ..addlist]
scaledtables <- CreateScaledTables(BigBanksTable, agg = F)
for (i in 1:5){
  breaksline <- data_breaks[i][, c(1,3,5)]
  thistable <- CreateTableOfComponents(breaksline[[1,1]], breaksline[[1,2]], breaksline[[1,3]],scaledtables = scaledtables, agg = F)
  thistable <- cbind(addtable, thistable)
  write.csv(thistable, paste("A", names[i], "raw.csv", sep = ""))
}

names <- c("a", "b", "c", "d", "e")
addlist <- c(c("REGN", "Year", "Month", "NAs_pnl", "NAs_bs", "NAs_begend"), pnl_names, pnlold_names, bs_names)
addtable <- BigBanksTable[, ..addlist]
for (i in 1:5){
  
  breaksline <- data_breaks[4][, c(1,3,5)]
  thistable <- CreateTableOfComponents(breaksline[[1,1]], breaksline[[1,2]], breaksline[[1,3]],scaledtables = scaledtables, agg = T)
  thistable <- cbind(addtable, thistable)
  write.csv(thistable, paste("B", names[i], "raw.csv", sep = ""))
}


names <- c("a", "b", "c", "d", "e")
addlist <- (c("REGN", "Year", "Month", "NAs_pnl", "NAs_bs", "NAs_begend"))
addtable <- BigBanksTable[, ..addlist]
scaledtables <- CreateScaledTables(BigBanksTable, T)
for (i in 1:5){
  
  breaksline <- data_breaks_agg[i][, c(1,3,5)]
  thistable <- CreateTableOfComponents(breaksline[[1,1]],breaksline[[1,2]], breaksline[[1,3]],scaledtables = scaledtables, agg = T)
  thistable <- cbind(addtable, thistable)
  write.csv(thistable, paste("C", names[i], "raw.csv", sep = ""))
}

unique(BigBanksTable$Year)



#make small tables
localsmalltable_bs_agg <- (scaledtables[[1]]$x)[, 1:86]
localsmalltable_pnlnew_agg <- (scaledtables[[2]]$x)[, 1:35]
localsmalltable_pnlold_agg <- (scaledtables[[3]]$x)[, 1:25]

#convert to data tables 

localsmalltable_pnlnew_agg <- as.data.table(localsmalltable_pnlnew_agg)
localsmalltable_pnlold_agg <- as.data.table(localsmalltable_pnlold_agg)
localsmalltable_bs_agg <- as.data.table(localsmalltable_bs_agg)
#add numeric column

localsmalltable_bs_agg[, "num" := 1:.N]
localsmalltable_pnlnew_agg[, "num" := 1:.N] 
localsmalltable_pnlold_agg[, "num" := 1:.N]

#merge

bigtable_agg <- merge(localsmalltable_bs_agg, localsmalltable_pnlnew_agg, by = "num")
bigtable_agg <- merge(bigtable_agg, localsmalltable_pnlold_agg, by = "num")
View(bigtable_agg)





#Entries aggr ####


Cash_and_equivalents <- c(20202, 20203, 20209, 20210, 20999, 20206, 20207, 20208, 20302, 20303, 20305, 20308, 20311, 20312, 20315, 20316, 20317, 20318, 20319, 20320, 20401, 20402, 20403, 30102, 30104, 30106, 30224)
NOSTRO <- c(30114, 30119, 30110, 30118, 30125,30213, 30215, 30221, 30228, 30233, 30235, 30402, 30409, 30413, 30416, 30417, 30418, 30419, 30424, 30425, 30426, 47404, 30128, 30242, 30428, 30608)
NOSTRO_minus <- c(20321, 30126, 30129, 30226, 30243, 30410, 30429, 30607,30609)
Credits_to_banks <- c(31901, 31902, 31903, 31904, 31905, 31906, 31907, 31908, 31909, 32902, 32001, 32002, 32003, 32004, 32010, 32201, 32202, 32203, 32204, 32005, 32006, 32205, 32206, 32007, 32008, 32009, 32207, 32208, 32209, 32101, 32102, 32103, 32104, 32110, 32301, 32302, 32303, 32304, 32105, 32106, 32107, 32305, 32306,
                      32108, 32109, 32307, 32308, 32309, 32402, 32502,32401, 32501, 32027, 32116, 32212, 32312, 32407, 32507)
Credits_to_banks_minus <- c(32015, 32028, 32115, 32117, 32211, 32213, 32311,32313, 32403, 32408, 32505, 32508)
Securities <- c(50104, 50105, 50116, 50205, 50206, 50214, 50305, 50306, 50313, 50401, 50402, 50408,50107, 50208, 
                50308, 50404, 50106, 50207, 50307, 50403, 50108, 50209, 50309, 50405, 50110, 50211, 50311, 50407, 
                50109, 50210, 50310, 50406, 50121,  50221, 50113, 50115, 50118, 50218, 50318, 50418, 50505, 50430,
                50508, 51201, 51202, 51203, 51204, 51205, 51206, 51207, 51208, 51209, 51301, 51302, 51303, 51304, 
                51305, 51306, 51307, 51308, 51309, 51214, 51314, 51339, 51340, 51341, 51342, 51501, 51502, 51503,
                51504, 51505, 51506, 51507, 51508, 51509, 51514, 51213,51313, 51401, 51402, 51403, 51404, 51405, 
                51406, 51407, 51408, 51409, 51513, 51526,51601, 51602, 51603, 51604, 51605, 51606, 51607, 51608, 
                51609, 51701, 51702, 51703, 51704, 51705, 51706, 51707, 51708, 51709, 51517, 51901, 51902, 51903, 
                51904, 51905, 51906, 51907, 51908, 51909, 51801, 51802, 51803, 51804, 51805, 51806, 51807, 51808, 
                51809, 51238,51528,50605, 50606, 50705, 50706, 50709, 50805, 50806, 50607, 50608, 50707, 50708, 
                50807, 50808, 50621, 50670,50721, 50770, 	50611, 50613, 50618, 50718,  50738,60106, 60118 )
Securities_minus <- c(50120, 50220, 50114, 50213, 50219, 50312, 50319, 50427, 50431, 50507, 50509, 51339, 51342, 51232,51527,
                      51210, 51240, 51310, 51410, 51510, 51525, 51529, 51610, 51710, 51810, 51910, 50620, 50671, 50720, 50771,
                      50719, 50809, 50910)
Corporate_credits <- c(45101, 45103, 45109, 45201, 45203, 45209, 45301, 45303, 45309, 47001, 47002, 47101, 47102, 47201, 47202, 
                       45104, 45105, 45204, 45205, 45304, 45305, 47003, 47004, 47103, 47104, 47203, 47204,
                       45106, 45107, 45108, 45206, 45306, 47005, 47105, 47205,
                       45207, 45307, 47006, 47106, 47206, 45208, 45308, 47007, 47107, 47207,
                       45601, 45607, 45608, 47301, 47302, 45602, 45603, 47303, 47304, 45604, 47305,
                       45605, 47306, 40308, 45606, 47307, 44101, 44102, 44103, 44109, 44201, 44202, 44203, 44204, 44210, 44301, 44302, 44303, 44304, 44310, 44401, 44402, 44403, 44404, 44410, 44501, 44503, 44509, 44601, 44603, 44609, 44701, 44703, 44709, 44801, 44803, 44809, 44901, 44903, 44909, 45001, 45003, 45009, 46001, 46002, 46101, 46102, 46201, 46202, 46301, 46302, 46401, 46402, 46501, 46502, 46601, 46602, 46701, 46702, 46801, 46802, 46901, 46902,
                       44104, 44105, 44205, 44206, 44305, 44306, 44405, 44406, 44504, 44505, 44604, 44605, 44704, 44705, 44804, 44805, 44904, 44905, 45004, 45005, 46003, 46004, 46103, 46104, 46203, 46204, 46303, 46304, 46403, 46404, 46503, 46504, 46603, 46604, 46703, 46704, 46803, 46804, 46903, 46904,
                       44106, 44207, 44307, 44407, 44506, 44606, 44706, 44806, 44906, 45006, 46005, 46105, 46205, 46305, 46405, 46505, 46605, 46705, 46805, 46905,
                       44107, 44208, 44308, 44408, 44507, 44607, 44707, 44807, 44907, 45007, 46006, 46106, 46206, 46306, 46406, 46506, 46606, 46706, 46806, 46906,
                       44108, 44209, 44309, 44409, 44508, 44608, 44708, 44808, 44908, 45008, 46007, 46107, 46207, 46307, 46407, 46507, 46607, 46707, 46807, 46907,
                       45811, 45812, 45813, 45911, 45912, 45913,
                       40310, 40311, 45816, 45916, 45801, 45802, 45803, 45804, 45805, 45806, 45807, 45808, 45809, 45810, 45901, 45902, 45903, 45904, 45905, 45906, 45907, 45908, 45909, 45910,
                       45116,  45216, 45316,47312,44216, 44416, 44516, 44616, 44716, 44916, 45016, 46012, 46112, 46412,  46512, 46912, 47012, 47112)
Corporate_credits_minus <- c(45115, 45117, 45215,45217, 45315,45317, 47008, 47108, 47208,
                             45615, 45617, 47308,47313,44115, 44215, 44217, 44315, 44317, 44415, 44417, 44515, 44517, 44615,44617, 44715, 44717, 44815, 44817, 44915, 44917,45015, 45017, 46008, 46108, 46208, 46308, 46408, 46508, 46513, 46608, 46613, 46708, 46808, 46813, 46908, 47013, 47113)

Retail_credits <- c(45401, 45403, 45409, 45404, 45405, 45406, 45407, 45410, 45408, 45814, 45914, 45416,45502, 45508, 45509, 45701, 45707, 45708, 45503, 45504, 45702, 45703,
                    45505, 45704, 45506, 45510, 45705, 45709, 45507, 45706, 45815, 45817, 45915, 45917)
Retail_credits_minus <- c(45415, 45417)

Reserves_for_delayed_loans <- c(45713, 45523,45820,  45920)
Reserves_for_delayed_loans_minus <- c(45515, 45524, 45714, 45715, 45818,45821, 45918, 45921)

Other_assets <- c(60401, 60404, 60406, 60407, 60408, 60409, 60410, 60411, 60412, 60413, 60415, 60701, 60702, 60705, 60804, 60901, 60905, 60906, 61002, 61008, 61009,
                  61010, 61013, 61209, 61210, 61211, 61212, 61213, 61901, 61902, 61903, 61904, 61905, 61906, 61907, 61908, 61911,32802, 47427, 47502, 50406,47803,
                  47901, 61011, 30208, 30210, 30211,  30302,  30304,  30306, 30404, 30406, 30602, 47402, 47408, 47410, 47413, 47415, 47417, 47420,
                  47421, 47423, 47431, 47440, 47443, 47447,  47455, 47456,  47468, 47469, 47701, 47801, 47802, 47807,  47809, 47811, 
                  47813, 47816, 50140,  50264,  50428,  50905, 60201, 
                  60202, 60203, 60204, 60205, 60302, 60304, 60306, 60308, 60310, 60312, 60314, 60315, 60323, 60336, 60337, 60339, 60341, 60343, 60347, 60350, 61014, 61601, 62001, 62101, 62102,
                  60101, 60102, 60103, 60104,  60121, 60221,  47465,47704, 47805,60107,60213, 52601 , 61702, 61703)
Other_assets_minus <- c( 60414 ,  60601 ,  60602 ,  60603 ,  60805 ,  60903 ,  61909 ,  61910 ,  30301 ,  30303 ,   30305 , 47452 ,  47461 ,  47462 , 47808 ,  47810 , 
                         47812 ,   47814 ,  47815 , 50141 ,   50265 ,   50429 ,  60120 , 60220 , 47425 ,  47466 ,  47702 ,   47705 ,  47804 , 
                         47806 ,  47902 ,  50908 ,  60105 ,  60108 ,  60206 ,   60214 ,  60324 ,   60352 ,  60405 ,  60706 ,  61012 ,  61501 ,  61912 ,  62002 ,  62103 
)

LORO <- c(30109, 30116, 	30111, 30117, 30122, 30123)
Interbank_loans <- c(31201, 31202, 31203, 31210, 31213, 31214, 31215, 31216, 31204, 31205, 31217, 31218, 31206,
                     31207, 31212, 31219, 31220, 31221, 31222, 31701, 31704, 31801, 31804,32901, 31301, 31302, 31303, 31304, 31310, 31501, 31502, 31503, 31504,
                     31305, 31306, 31505, 31506, 31307, 31308, 31309, 31507, 31508, 31509, 31702, 31802, 31401, 31402, 31403, 31404, 31410, 31601, 31602, 31603, 31604,
                     31405, 31406, 31605, 31606, 31407, 31408, 31409, 31607, 31608, 31609, 31703, 31803, 20313, 20314, 30214, 30220, 30222, 30223, 30227, 30230, 30231, 30411)
Corporate_current_accounts <- c(40101, 40105, 40106, 40107, 40108,  40110,  40116, 40201, 40202, 40203, 40204, 40205, 40206, 40301, 40302, 40306, 40307, 40312, 
                                40314, 40401, 40402, 40403, 40404, 40406, 40409, 40410, 40501, 40502, 40503, 40504, 40505, 40506, 40601, 40602,
                                40603, 40604, 40606, 41001, 41101, 41201, 41301, 41401, 41501, 41601, 41701, 41801, 41901, 42701, 42801, 42901, 43001,
                                43101, 43201, 43301, 43401, 43501, 43601, 20309,  40701, 40702, 40703, 40704, 40705, 40706, 40802, 40810, 40811, 40819, 40821, 40822, 40825, 
                                40901, 40903, 40905, 40906, 40907, 40909, 40911, 40912, 40915, 42001, 42101, 42108, 42201, 43701, 43801, 43901, 47405,  49999, 20310, 40804, 40805, 40806,
                                40807, 40809, 40812, 40814, 40815, 40818, 40826, 40902, 40910, 40913, 42501, 44001
)
Corporate_current_accounts_minus <- c(40109, 40111, 40313, 40908, 47406)
Retail_current_accounts <- c(40817, 40823, 40824, 40914, 42301, 42309, 40803, 40813, 40820, 42601, 42609)
Trading_accounts <- c(30601, 30606)

Corporate_time_deposits <- c(42002, 42102, 42109, 42202, 43702, 43802, 43902, 42003, 42004, 42103, 42104, 42110, 42111, 42203, 42204, 43703, 43704, 43803, 43804, 43903, 43904,
                             42005, 42105, 42112, 42205, 43705, 43805, 43905, 42006, 42106, 42113, 42206, 43706, 43806, 43906, 42007, 42107, 42114, 42207, 43707, 43807, 43907,
                             42502, 44002, 42503, 42504, 44003, 44004, 42505, 44005, 42506, 44006, 42507, 44007, 41002, 41102, 41202, 41302, 41402, 41502, 41602, 41702, 41802, 
                             41902, 42702, 42802, 42902, 43002, 43102, 43202, 43302, 43402, 43502, 43602, 41003, 41004, 41103, 41104, 41203, 41204, 41303, 41304, 41403, 41404,
                             41503, 41504, 41603, 41604, 41703, 41704, 41803, 41804, 41903, 41904, 42703, 42704, 42803, 42804, 42903, 42904, 43003, 43004, 43103, 43104, 43203,
                             43204, 43303, 43304, 43403, 43404, 43503, 43504, 43603, 43604, 41005, 41105, 41205, 41305, 41405, 41505, 41605, 41705, 41805, 41905, 42705, 42805, 
                             42905, 43005, 43105, 43205, 43305, 43405, 43505, 43605, 	41006, 41106, 41206, 41306, 41406, 41506, 41606, 41706, 41806, 41906, 42706, 42806, 42906,
                             43006, 43106, 43206, 43306, 43406, 43506, 43606, 41007, 41107, 41207, 41307, 41407, 41507, 41607, 41707, 41807, 41907, 42707, 42807, 42907, 43007,
                             43107, 43207, 43307, 43407, 43507, 43607)
Retail_time_deposits <- c(42302, 42310,42303, 42304, 42311, 42312,42305, 42313,42306, 42314,42307, 42315,42602, 42610,42603, 42604, 42611, 42612,42605, 42613,42606, 42614,	42607,
                          42615)


Bonds <- c(52001, 52002, 52003, 52004, 52005, 52006, 52401)
Bills <- c(52301, 52406,52302,52303, 52304,52305,52306,52307)
Bills_minus <- c(52503)
Certificates_of_deposit <- c(52101, 52201, 52403, 52404,52102, 52103, 52202, 52203,52104, 52105, 52106, 52204, 52205, 52206)
Other_liabilities <- c(30218, 30219, 30232, 30236, 30401, 30403, 30405, 30407,
                       30408, 30412, 30414, 30415, 30420, 30421, 30422, 30423, 30603, 30604, 47401, 47403, 47407, 47412, 47414, 47416, 47419, 47422, 47424, 47441, 47442, 47444,
                       47445, 47446, 47448,47453, 47458, 60301, 60303, 60305, 60307, 60309, 60311, 60313, 60320, 60322, 60335, 60338, 60340, 60342, 60344, 60348, 60349, 60806, 61602,
                       32801, 47411, 47426, 47501, 50407, 50408, 52402, 52405, 52407, 52501,47409,47418, 47601, 47602, 47603, 47605, 47606, 47607, 47608, 47609,52602,61701)
Other_liabilities_minus <- c(47450, 47451, 47459, 47460, 47464, 47467, 52502)

Main_capital <- c(10207, 10208,10601, 10602, 10603, 10604,10609,10611, 10612,10614, 10619,10621,10626,10628,10630, 10631, 10632,
                  10634,10701, 10702, 10703, 10704)
Main_capital_minus <- c(10501,10502,10605,10610,10613,10620,10623,10627,10629,10633,10635,11101)

Retained_earnings <- c(10801,70302,70701, 70702, 70703, 70704, 70705,70715,70801)
Retained_earnings_minus <- c(10901,70402,70502,70706,70707,70708,70709,70710,70711,70714,70716,70802,70712)

Retained_earnings_this_year <- c(61306,70301, 70401,70601, 70602, 70603, 70604, 70605,70613,70615)
Retained_earnings_this_year_minus <- c(61406,70501,70606,70607,70608,70609,70610,70611,70614,70616,70612)

Next_periods <- c(61301, 61302, 61303, 61304)
Next_periods_minus <- c(61401,61403)

Credit_limits <- c(91416, 91417)
Contingent_liabilities_minus <- c(91315, 91316, 91317, 91319)
Securities_for_credits_given <- c(91312,91311,91313)
Unexecuted_debts <- c(90904, 90902)
Other_offbalance <- c(91603, 91604)
Other_offbalance_minus <- c(91703,91704,91801,91802,91803)

#new PnL
NOSTRO_pnl <- c(11301, 11302, 11303, 13302)
Credits_to_banks_pnl <- c(11401, 11217, 12217, 12301,
                          13401, 21217, 11402, 11118, 11215,
                          12118, 12215, 12302, 21118, 21215,
                          11403, 11119, 11216, 12119, 12216,
                          12303, 21119, 13119, 21216, 13105, 13111,
                          13118, 13215, 13216, 13403)
Credits_to_banks_pnl_minus <- c(14101, 14102, 14103, 33108, 33208, 35105, 35111, 35118, 35119, 35215, 35216, 35401, 35402, 35403)
Corporate_credits_pnl <- c(11101, 11201, 12101, 12201, 21101, 11102, 11202, 12102, 12202, 21102, 11103, 11203, 12103, 12203, 11104,
                           11204, 12104, 12204, 21204, 11105, 11205, 12105, 12205, 21105, 11106, 11206, 12106, 12206, 21106, 11107,
                           11207, 12107, 12207, 21107, 21210, 11108, 11208, 12108, 12208, 21108, 11109, 11209, 12109, 12209, 21109,
                           11110, 11210, 12110, 12210, 11111, 11211, 12111, 12211, 21111, 11212, 11112, 12112, 12212, 21112, 21212,
                           11113, 11213, 12113, 12213, 21113, 11114, 11218, 12114, 12218, 21114, 21218, 11116, 11214, 12116, 12214,
                           21116, 21214, 11120, 12120, 21110, 21202, 21206, 21209, 21211, 13102, 13106, 13107, 13109, 13110, 13113,
                           13114, 13116, 13206, 13212, 13214)
Corporate_credits_pnl_minus <- c(33113, 33120, 34112, 34204, 34212, 41204, 41205, 41211,14307, 14309, 14311,
                                 14313, 14314, 14315, 14316, 14317, 14318, 14407, 14413, 14414, 14415, 14416,
                                 33101, 33102, 33103, 33106, 33107, 33109, 33110, 33206, 33207, 33209, 33210,
                                 33213, 33218, 35102, 35106, 35107, 35109, 35110, 35112, 35113, 35114, 35116,
                                 35206, 35212, 35214, 35218)

Retail_credits_pnl <- c(11115, 11219, 12115, 12219, 21115, 21219, 11117, 11220, 12117, 12220, 21117, 13115, 13117, 21220)
Retail_credits_pnl_minus <- c(33220,14701,35115,35117,35219)

Debt_pnl <- c(11501, 11601, 11701, 13601, 11502, 11602, 11702, 13501, 13502, 13602, 13701, 13702,11503, 11603, 11703,13603,
              13703,11504, 11604, 11704, 13504, 13604, 13704, 13803, 17504, 17604, 17703, 11507, 11607, 11707, 13507, 13607,
              13707, 17507, 17607,11506, 11606, 11706, 13606, 13706, 17506, 17606, 11505, 11605, 11705, 13505, 13605, 13705,
              11508, 11608, 11708, 13508, 13608, 13708)
Debt_pnl_minus <- c(34303,34403,34503,33603,33703,35603,35703,35504, 35604, 35704, 35803, 42603,33507, 33607, 35507, 35607, 35707, 43503,35606, 35706,14801, 14803, 35505, 35605, 35705, 35804, 42604,33804,35508, 35608, 35708)
Bills_pnl <- c(11801, 11802,11803,11807, 11805, 11806,11804, 13804, 14804,11808)

Current_accounts_of_banks_pnl_minus <- c(31210,31211)
Term_deposits_of_banks_minus_pnl <- c(31101, 31317, 31417, 32317, 36101, 36317,31102, 31315, 31415, 32102, 32315, 36415, 36102, 36105, 36311, 36315,31416, 31316, 31103, 32103, 32316, 32101, 36103, 36316, 36416)
Current_accounts_corporate_pnl_minus <- c(31201, 31202, 31203, 31204, 31205, 31206, 31207, 31208, 31209, 31212, 31213, 36203)
Current_accounts_retail_pnl_minus <- c(31501,31502)
Term_deposits_corporate_minus_pnl <- c(31301, 31401, 32301, 31302, 31402, 31303, 31403, 31304, 31404, 31305,
                                       31405, 31306, 32206, 31406, 31307, 31407, 32207, 31308, 31408, 31309, 
                                       31409, 31310, 31410, 31311, 31411, 32211, 32311, 31312, 31412, 14312, 
                                       32212, 32312, 31313, 31413, 32213, 32313, 31314, 31414, 32214, 32314,
                                       31318, 31418, 32218, 32318, 31104, 31105, 32105, 13112, 32201, 32202, 
                                       32205, 32306, 32307, 36411, 36412, 36418, 32203, 36306, 36307, 36309, 
                                       36312, 36313, 36314, 36318, 36407, 36414)
Term_deposits_corporate_pnl <- c(14412, 24105, 24205, 24305, 24307)

Term_deposits_retail_minus_pnl <- c(31601, 31701, 32401, 32501, 31602, 31702, 32402, 36701, 36601, 36602)
Securities_issued <- c(24605)
Securities_issued_minus <- c(31801, 32601, 36801,31804, 32604, 36804,31802, 31803, 31805, 32605, 36803,31901, 31902, 31903)

Fee_income <- c(27101, 27102, 27103, 27104,27201, 27202, 27203, 27204,27501, 27502, 27503, 27504,27701, 27702, 27704,27401,
                27402, 27403, 27404, 27405, 27406, 27407, 27408, 27409,	27801, 27802, 27803, 27804,27301, 27302, 27303, 27304,
                27601, 27602, 27603, 27604,27901, 27902, 27903, 27904)
Fee_expenditure <- c(47104,47105,47106,47101)
Fee_expenditure_minus <- c(47103, 47102,47108, 47109, 47107, 33502, 33503)

Realized_securities <- c(22101, 22102, 22301, 22302, 22401, 22402,	22104, 22304, 22404, 22503, 23101, 23301 ,22403, 22603,22107,
                         23103, 23303, 22307, 22407, 42107, 22507,22106, 22505,22105, 22305, 22405, 23102, 22504, 23302,22108, 22308, 
                         22408, 23104, 23304,22508)
Realized_securities_minus <- c(42101, 42102, 42201, 42202, 42301, 42302, 42401, 42402, 42203,42103, 42303, 42403,42204, 42304, 42404, 42503, 43101, 43301, 43501, 42104, 42207, 42307, 42407, 43103, 43303,22306, 42206, 42306, 42106, 42406,42205, 42305, 42405, 42504, 43102, 43302, 43502, 42105,42208, 42308, 42408, 43104, 43304, 43504, 22508, 42108)
Unrealized_securities <- c(26101, 26102, 26103, 26104, 26105, 26106, 26201, 26202, 26203, 26204, 26205, 26206,22203)
Unrealized_securities_minus <- c(47201, 47202,47203, 41411, 41412, 41415, 41416, 41417, 41418, 41419, 41511, 41512, 41514, 41515, 41516)

Realized_fc <- c(26101, 26102, 26103, 26104, 26105, 26106, 26201, 26202, 26203, 26204, 26205, 26206)
Realized_fc_minus <- c(46101, 46102, 46103, 46104, 46105, 46106, 46201, 46202, 46203, 46204, 46205, 46206)

Unrealized_fc <- c(	26301, 26302, 26303, 26304, 26305, 26306)
Unrealized_fc_minus <- c(46301, 46302, 46303, 46304, 46305, 46306)

Other_corporates <- c(	23502, 23503, 23504, 23501,23602, 23603, 23601, 23604,23802, 23804, 23903, 23701,23901, 23702, 23904, 23902)

Derivatives_etc <- c(25101, 25102, 25103, 25104, 25105, 25202, 25203, 25204, 25205, 25301, 25302, 25303, 25305, 25401, 25402, 25403, 
                     25404, 25405, 25501, 25502, 25503, 25504, 25505,25201, 25304, 25605, 25607,25601,26401, 26402, 26403, 26404, 26405, 26501, 26502, 26503, 26504, 26505)
Derivatives_etc_minus <- c(46401, 46402, 46403, 46404, 46501, 46502, 46503, 46504,45601,45101, 45102, 45103, 45104, 45105, 45201, 45202, 45203, 45204, 45205, 45301, 45302, 45303, 45305, 45401, 45402, 45403, 45404, 45405, 45501, 45502, 45503, 45504, 45505)

Other_income <- c(28301, 28302, 28303, 28502,28501, 28601, 28701, 28702, 29101, 29102, 29103,28504, 28602, 28704, 29106, 29107,28801,28802, 28803, 28703,29401,
                  29402,29403,29404,29405,29406,28401, 28402, 29407,28503)
Other_income_minus <- c(47501, 47601, 47701, 47702,47503, 47602, 47703, 47704, 47502, 48205)



Other_expenditures <- c(47801,47401,48603,48604,48605,48606,48607,48608,48609,24101, 24102, 24215,
                        24315, 24103, 24216, 24316, 24202, 24206, 24207, 24208, 24209, 24210, 24211,
                        24212, 24213, 24214, 24218, 24306, 24309, 24310, 24311, 24312, 24313, 24314,
                        24318, 14601, 14602, 24401, 24402, 24501, 24502, 16101, 24601, 24604, 24602,
                        24603,21411, 21412, 21413, 21414, 21415, 21416, 21417, 21418, 21419, 21511,
                        21512, 21514, 21515, 21516, 21519, 21603, 24217, 24308, 24702, 24703, 24734,
                        24738, 24904)
Other_expenditures_minus <- c(48601, 48611, 48402,44102, 44103, 44212, 44311, 44312, 44315, 44316, 44317,
                              44401, 44402, 44501, 44502, 44601, 44604, 44605, 47402, 47802, 47803, 33302,
                              33303, 35302, 35303, 33118, 33119, 33215, 33216, 33401, 33402, 34118, 41118,
                              41215, 41216, 41303, 33111, 33112, 33114, 33116, 33211, 33212, 33214, 41105,
                              41106, 41109, 41111, 41112, 41113, 41114, 41116, 41212, 41214, 33115, 33219,
                              34115, 41115, 41219, 33117, 41117, 33501, 33601, 33602, 33701, 34301, 34302,
                              34401, 34402, 34501, 34502, 33504, 33604, 33704, 34304, 34404, 34504, 34307,
                              34407, 34507, 33706, 34306, 34406, 33505, 33605, 33705, 34305, 34405, 34505,
                              33508, 33608, 34308, 34408, 34508, 33803, 34603, 34604,44105, 44209, 44218, 
                              44314, 48610, 41102, 41119, 34114, 34117, 34213, 34506, 41107, 41110, 41218,
                              41301, 44101, 44202, 44207, 44211, 44213, 44214, 44216, 44307, 44313, 44702,
                              44703, 44734, 44738, 44904,48201, 48202, 48203)
Reserve_adjustments <- c(15101, 15102, 15105, 15106, 15107, 15108, 15109, 15110, 15111, 15112, 15113, 15114,
                         15115, 15116, 15117, 15118, 15119, 15201, 15202, 15206, 15207, 15209, 15210, 15211,
                         15212, 15213, 15214, 15215, 15216, 15217, 15218, 15219, 15220, 15302, 15303, 15402,
                         15403, 15501, 15504, 15505, 15507, 15508, 15601, 15602, 15603, 15604, 15605, 15606,
                         15607, 15608, 15703, 15704, 23401, 23402, 23403, 23404, 28201, 28202, 28203, 28204,
                         29301, 29302, 29303, 29304, 15103, 15120, 15506, 28205, 17101, 17105, 17108, 17111,
                         17118, 17119, 17208, 17210, 17211, 17215, 17216, 17217, 17301, 17302, 17303, 17401,
                         17402, 17403, 17102, 17106, 17107, 17109, 17110, 17112, 17113, 17114, 17116, 17201,
                         17202, 17206, 17207, 17209, 17212, 17213, 17214, 17218, 17115, 17117, 17219, 17220,
                         17501, 17502, 17601, 17602, 17503, 17603, 17504, 17604, 17703, 17507, 17607, 17506,
                         17606, 17505, 17605, 17704, 17508, 17608)
Reserve_adjustments_minus <- c( 37101, 37102, 37105, 37106, 37107, 37108, 37109, 37110, 37111, 37112, 37113,
                                37114, 37115, 37116, 37117, 37118, 37119, 37206, 37207, 37209, 37210, 37211,
                                37212, 37213, 37214, 37215, 37216, 37217, 37218, 37219, 37220, 37302, 37303,
                                37402, 37403, 37501, 37502, 37504, 37505, 37507, 37508, 37601, 37602, 37603,
                                37604, 37605, 37606, 37607, 37608, 37703, 37704, 43401, 43402, 43403, 43404,
                                47301, 47302, 47303, 47304, 48501, 48502, 48504, 48505, 37104, 37201, 37202,
                                37204, 37208, 37301, 37708, 48503, 37103, 37120, 37205, 37503, 37506, 47305,
                                38102, 38105, 38108, 38111, 38118, 38119, 38202, 38205, 38211, 38215, 38216,
                                38217, 38301, 38302, 38303, 38401, 38402, 38403, 38101, 38104, 38106, 38107,
                                38109, 38110, 38112, 38113, 38114, 38116, 38201, 38206, 38207, 38209, 38210,
                                38212, 38213, 38214, 38218, 38115, 38117, 38219, 38220, 38501, 38502, 38601,
                                38602, 38503, 38603, 38504, 38604, 38703, 38507, 38607, 38506, 38606, 38505,
                                38605, 38704, 38508, 38608)
Personnel_etc <- c(29201, 29203, 29204, 29205, 29207, 29208,29202,48401)
Personnel_etc_minus <- c(48101, 48102, 48103, 48104, 48105, 48106, 48107, 48108, 48109, 48110, 48111, 48113, 
                         48301, 48302, 48303, 47506,48208, 47505, 47504, 48209,48204, 48206,	48403, 48404)

Organization_etc <- c(48112,48405,48406,48407,48408,48409,48602,48410,48411,48412,48413)
Organization_etc_minus <- c(48414)

Income_before_taxes <- c(01000)
Income_before_taxes_minus <- c(02000)

Taxes <- c(51101, 51202)
Taxes_minus <- c(51201)

Unretained_earnings <- c(61101)
Unretained_earnings_minus <- c(61102)

Other_comprehensive_income <- c(71101, 71102, 71103, 71104, 71201, 71202, 71203, 71204, 71301, 71302, 71304,
                                71305, 71306, 71307, 71308, 71401, 71402, 71403, 71404, 71405, 71406, 71407,
                                71408, 71701, 71702, 71703, 71704, 71804, 71907, 71908, 71501,71901, 71902,
                                71903,71602, 71603,81201)
Other_comprehensive_income_minus <- c(72101, 72102, 72103, 72104, 72201, 72202, 72203, 72204, 72301, 72302,
                                      72304, 72305, 72306, 72307, 72308, 72401, 72402, 72404, 72405, 72406,
                                      72407, 72408,72702, 72703, 72704, 72907, 72908,72101, 72102, 72103, 
                                      72104, 72201, 72202, 72203, 72204, 72301, 72302, 72304, 72305, 72306,
                                      72307, 72308, 72401, 72402, 72404, 72405, 72406, 72407, 72408, 71303,
                                      71801, 72303, 72403, 72701,72501,72901, 72902, 72903,72602, 72603,81202)


#old PnL
NOSTRO_pnlold <- c(11301,11302,11303)
Credits_to_banks_pnlold <- c(11118,11119,11401,11402,11403,11215,11216,11217)
Corporate_credits_pnlold <- c(11111,11112,11113,11114,11115,11116,11105,11106,11107,11108,11109,11110,11101,
                              11102,11103,11104,11201,11202,11203,11204,11205,11206,11207,11208,11209,11210,
                              11211,11212,11213,11214)
Retail_credits_pnlold <- c(11115, 11117)
Debt_assets_pnlold <- c(11501,11502,11503,11504,11506,11505,11508)

Bills <- c(11601,11602,11603,11607,11605,11606,11604,11608)
Current_accounts_of_banks_pnlold <- c(21210,21211)
Term_deposits_of_banks_pnlold <- c(21101,21102,21416)
Term_deposits_of_banks_minus_pnlold <- c(21317,21417,21315,21415,21103,21316,21104)

Corporate_current_accounts_pnlold <- c(21201,21202,21203,21204,21205,21206,21207,21208,21209,21212,21213,21214)
Retail_current_accounts_pnlold <- c(21501,21502)

Corporate_term_deposits_pnlold <- c(as.character(seq(21301, 21314)), as.character(seq(21401, 21414)))
Retail_term_deposits_pnlold <- c(21601, 21602, 21701, 21702)
Expenditures_on_bonds_issued <- c(21801)
Expenditures_on_bills_issued <- c(21804)
Certificates_pnlold <- c(21802, 21803)
Fee_income_pnlold <- c(12101,12102,12301,12402,12403,12404,12405,12406,12401,16201,16202,16203)
Fee_expenditures_pnlold <- c(25202,25203,25204,25205,25201,22201,22202,22203,22206)
Realized_securities_issued_pnlold <- c(13101,13102,13103,23101,23102,23103,13104, 13107,13106,13105,13108)
Realized_securities_issued_minus_pnlold <- c(23104, 23107, 23106, 23105, 23108)
Unrealized_securities_pnlold <- c(15101,24101)
Unrealized_securities_pnlold_minus <- c(23109)
Realized_fc_pnlold <- c(12201, 22101)
Unrealized_fc_pnlold <- c(15102, 24102)

Other_corporates_pnlold <- c(14101,14102,14103,14104,14201,14202,14203,14204,14301,14302,14303,14304,14401,14402,14403,14404)
Derivatives_etc_pnlold <- c(16101,25101,15201, 15202, 15203, 15204,24201,13201,23201,15103,24103)
Derivatives_etc_minus_pnlold <-c(24202, 24203, 24204)
Other_income_pnlold <- c(16301,16302,16303,16304,16306,17101, 17201,	17102, 17202,17103, 17203,17301,17302,17303,17304,17305,17306)
Other_expenditures_pnlold <- c(27306, 26307, 12407, 22204)
Other_expenditures_minus_pnlold <- c(25301,25303,27101,27102,27103,27201,27202,27203,27301,27302,27303,27304,27305,27307,27308)
Changes_reserves <- c(16305, 25302)
Personnel_etc_pnlold <- c(26101,26102,26103,26104,26201,26202,26203,26204,26301,26302,26303,	26306,26304,26305)
Organization_etc <- c(26411,26406)
Organization_etc_minus <- c(26401,26402,26403,26404,26405,26407,26408,26409,26410,26412)
PBT_pnlold <- c(01000,02000)
Taxes_pnlold <- c(28103)
Taxes_minus_pnlold <- c(28101,28102)

Retained_earnings_pnlold <- c(32001,32002,33001,33002)



localtable <- BigBanksTable[1:10]
View(localtable)
pnl_rename <- function(name){
  return(as.character(paste(name, "pnl", sep = "")))
}
pnlold_rename <- function(name){
  return(as.character(paste(name,"pnl_old",sep = "")))
}
bs_rename <- function(name){
  return(as.character(name))
}




filename = "pnlold1.txt"
mystring1 <- noquote(paste("localtable[,", "\"", x, "\"", ":=0]", sep = ""))
mystring2 <- noquote(paste("print(", "\"", x, "\"", ")", sep = ""))
mystring3 <- noquote(paste("for (i in 1:length(",  x, ")){", sep = ""))
mystring4 <- noquote(paste("  m <- ", x, "[i]", sep = ""))
mystring5 <- noquote("  print(m)")
mystring6 <- noquote("  print(localtable[,get(pnlold_rename(m))])")
mystring7 <- noquote(paste("  localtable[, ", "\"", x, "\"", ":= ", x, " + get(pnlold_rename(m))]", sep = ""))
mystring8 <- noquote("}")



write(mystring1, file=filename, append=FALSE, sep='\n')
write(mystring2, file=filename, append=T, sep='\n')
write(mystring3, file=filename, append=T, sep='\n')
write(mystring4, file=filename, append=T, sep='\n')
write(mystring5, file=filename, append=T, sep='\n')
write(mystring6, file=filename, append=T, sep='\n')
write(mystring7, file=filename, append=T, sep='\n')
write(mystring8, file=filename, append=T, sep='\n')



pnlold_names <- c("NOSTRO_pnlold","Credits_to_banks_pnlold","Corporate_credits_pnlold","Retail_credits_pnlold","Debt_assets_pnlold","Bills","Current_accounts_of_banks_pnlold","Term_deposits_of_banks_pnlold","Term_deposits_of_banks_minus_pnlold","Corporate_current_accounts_pnlold","Retail_current_accounts_pnlold","Corporate_term_deposits_pnlold","Retail_term_deposits_pnlold","Expenditures_on_bonds_issued","Expenditures_on_bills_issued","Certificates_pnlold","Fee_income_pnlold","Fee_expenditures_pnlold","Realized_securities_issued_pnlold","Realized_securities_issued_minus_pnlold","Unrealized_securities_pnlold","Unrealized_securities_pnlold_minus","Realized_fc_pnlold","Unrealized_fc_pnlold","Other_corporates_pnlold","Derivatives_etc_pnlold","Derivatives_etc_minus_pnlold","Other_income_pnlold","Other_expenditures_pnlold","Other_expenditures_minus_pnlold","Changes_reserves","Personnel_etc_pnlold","Organization_etc","Organization_etc_minus","PBT_pnlold","Taxes_pnlold","Taxes_minus_pnlold","Retained_earnings_pnlold")
filename = "pnlold.txt"
write("pnlold", file=filename, append=FALSE, sep='\n')
for (x in pnlold_names){
  mystring1 <- noquote(paste("localtable[,", "\"", x, "\"", ":=0]", sep = ""))
  mystring2 <- noquote(paste("print(", "\"", x, "\"", ")", sep = ""))
  mystring3 <- noquote(paste("for (i in 1:length(",  x, ")){", sep = ""))
  mystring4 <- noquote(paste("try({m <- ", x, "[i]", sep = ""))
  mystring5 <- noquote("  print(m)")
  mystring6 <- noquote("  print(localtable[,get(pnlold_rename(m))])")
  mystring7 <- noquote(paste("  localtable[, ", "\"", x, "\"", ":= ", x, " + get(pnlold_rename(m))]})", sep = ""))
  mystring8 <- noquote("}")
  
  
  
  write(mystring1, file=filename, append=T, sep='\n')
  write(mystring2, file=filename, append=T, sep='\n')
  write(mystring3, file=filename, append=T, sep='\n')
  write(mystring4, file=filename, append=T, sep='\n')
  write(mystring5, file=filename, append=T, sep='\n')
  write(mystring6, file=filename, append=T, sep='\n')
  write(mystring7, file=filename, append=T, sep='\n')
  write(mystring8, file=filename, append=T, sep='\n')
  
  

}
pnlold_names <- c("NOSTRO_pnlold","Credits_to_banks_pnlold","Corporate_credits_pnlold","Retail_credits_pnlold","Debt_assets_pnlold","Bills","Current_accounts_of_banks_pnlold","Term_deposits_of_banks_pnlold","Term_deposits_of_banks_minus_pnlold","Corporate_current_accounts_pnlold","Retail_current_accounts_pnlold","Corporate_term_deposits_pnlold","Retail_term_deposits_pnlold","Expenditures_on_bonds_issued","Expenditures_on_bills_issued","Certificates_pnlold","Fee_income_pnlold","Fee_expenditures_pnlold","Realized_securities_issued_pnlold","Realized_securities_issued_minus_pnlold","Unrealized_securities_pnlold","Unrealized_securities_pnlold_minus","Realized_fc_pnlold","Unrealized_fc_pnlold","Other_corporates_pnlold","Derivatives_etc_pnlold","Derivatives_etc_minus_pnlold","Other_income_pnlold","Other_expenditures_pnlold","Other_expenditures_minus_pnlold","Changes_reserves","Personnel_etc_pnlold","Organization_etc","Organization_etc_minus","PBT_pnlold","Taxes_pnlold","Taxes_minus_pnlold","Retained_earnings_pnlold")

pnl_names <- c("NOSTRO_pnl","Credits_to_banks_pnl","Credits_to_banks_pnl_minus","Corporate_credits_pnl","Corporate_credits_pnl_minus","Retail_credits_pnl","Retail_credits_pnl_minus","Debt_pnl","Debt_pnl_minus","Bills_pnl","
                  Current_accounts_of_banks_pnl_minus","Term_deposits_of_banks_minus_pnl","Current_accounts_corporate_pnl_minus","Current_accounts_retail_pnl_minus","Term_deposits_corporate_minus_pnl","Term_deposits_corporate_pnl","Term_deposits_retail_minus_pnl","Securities_issued","Securities_issued_minus","Fee_income","Fee_expenditure","Fee_expenditure_minus","Realized_securities","Realized_securities_minus","Unrealized_securities","Unrealized_securities_minus","Realized_fc","Realized_fc_minus","
                  Realized_fc_minus","Unrealized_fc","Unrealized_fc_minus","Other_corporates","Derivatives_etc","Derivatives_etc_minus","Other_income","Other_income_minus","Other_expenditures","Other_expenditures_minus","Reserve_adjustments","Reserve_adjustments_minus","Personnel_etc","Personnel_etc_minus","Organization_etc","Organization_etc_minus","Income_before_taxes","Income_before_taxes_minus","Taxes","Taxes_minus","Unretained_earnings","Unretained_earnings_minus","Other_comprehensive_income","Other_comprehensive_income_minus")
filename = "pnl.txt"
write("pnl", file=filename, append=FALSE, sep='\n')
for (x in pnl_names){
  mystring1 <- noquote(paste("localtable[,", "\"", x, "\"", ":=0]", sep = ""))
  mystring2 <- noquote(paste("print(", "\"", x, "\"", ")", sep = ""))
  mystring3 <- noquote(paste("for (i in 1:length(",  x, ")){", sep = ""))
  mystring4 <- noquote(paste("try({m <- ", x, "[i]", sep = ""))
  mystring5 <- noquote("  print(m)")
  mystring7 <- noquote(paste("  localtable[, ", "\"", x, "\"", ":= ", x, " + get(pnl_rename(m))]})", sep = ""))
  mystring8 <- noquote("}")
  
  
  
  write(mystring1, file=filename, append=T, sep='\n')
  write(mystring2, file=filename, append=T, sep='\n')
  write(mystring3, file=filename, append=T, sep='\n')
  write(mystring4, file=filename, append=T, sep='\n')
  write(mystring5, file=filename, append=T, sep='\n')
  write(mystring7, file=filename, append=T, sep='\n')
  write(mystring8, file=filename, append=T, sep='\n')
  
  
  
}

bs_names <- c("Cash_and_equivalents","NOSTRO","NOSTRO_minus","Credits_to_banks","Credits_to_banks_minus","Securities","Securities_minus","Corporate_credits","Corporate_credits_minus","Retail_credits","Retail_credits_minus","Reserves_for_delayed_loans","Reserves_for_delayed_loans_minus","Other_assets","Other_assets_minus","LORO","Interbank_loans","Corporate_current_accounts","Corporate_current_accounts_minus","Retail_current_accounts","Trading_accounts","Corporate_time_deposits","Retail_time_deposits","Bonds","Bills","Bills_minus","Certificates_of_deposit","Other_liabilities","Other_liabilities_minus","Main_capital","Main_capital_minus","Retained_earnings","Retained_earnings_minus","Retained_earnings_this_year","Retained_earnings_this_year_minus","Next_periods","Next_periods_minus","Credit_limits","Contingent_liabilities_minus","Securities_for_credits_given","Unexecuted_debts","Other_offbalance","Other_offbalance_minus")
pnlold_names <- c("NOSTRO_pnlold","Credits_to_banks_pnlold","Corporate_credits_pnlold","Retail_credits_pnlold","Debt_assets_pnlold","Bills","Current_accounts_of_banks_pnlold","Term_deposits_of_banks_pnlold","Term_deposits_of_banks_minus_pnlold","Corporate_current_accounts_pnlold","Retail_current_accounts_pnlold","Corporate_term_deposits_pnlold","Retail_term_deposits_pnlold","Expenditures_on_bonds_issued","Expenditures_on_bills_issued","Certificates_pnlold","Fee_income_pnlold","Fee_expenditures_pnlold","Realized_securities_issued_pnlold","Realized_securities_issued_minus_pnlold","Unrealized_securities_pnlold","Unrealized_securities_pnlold_minus","Realized_fc_pnlold","Unrealized_fc_pnlold","Other_corporates_pnlold","Derivatives_etc_pnlold","Derivatives_etc_minus_pnlold","Other_income_pnlold","Other_expenditures_pnlold","Other_expenditures_minus_pnlold","Changes_reserves","Personnel_etc_pnlold","Organization_etc","Organization_etc_minus","PBT_pnlold","Taxes_pnlold","Taxes_minus_pnlold","Retained_earnings_pnlold")

pnl_names <- c("NOSTRO_pnl","Credits_to_banks_pnl","Credits_to_banks_pnl_minus","Corporate_credits_pnl","Corporate_credits_pnl_minus","Retail_credits_pnl","Retail_credits_pnl_minus","Debt_pnl","Debt_pnl_minus","Bills_pnl","Current_accounts_of_banks_pnl_minus","Term_deposits_of_banks_minus_pnl","Current_accounts_corporate_pnl_minus","Current_accounts_retail_pnl_minus","Term_deposits_corporate_minus_pnl","Term_deposits_corporate_pnl","Term_deposits_retail_minus_pnl","Securities_issued","Securities_issued_minus","Fee_income","Fee_expenditure","Fee_expenditure_minus","Realized_securities","Realized_securities_minus","Unrealized_securities","Unrealized_securities_minus","Realized_fc","Realized_fc_minus","Realized_fc_minus","Unrealized_fc","Unrealized_fc_minus","Other_corporates","Derivatives_etc","Derivatives_etc_minus","Other_income","Other_income_minus","Other_expenditures","Other_expenditures_minus","Reserve_adjustments","Reserve_adjustments_minus","Personnel_etc","Personnel_etc_minus","Organization_etc","Organization_etc_minus","Income_before_taxes","Income_before_taxes_minus","Taxes","Taxes_minus","Unretained_earnings","Unretained_earnings_minus","Other_comprehensive_income","Other_comprehensive_income_minus")

filename = "bs.txt"
write("bs", file=filename, append=FALSE, sep='\n')
for (x in bs_names){
  mystring1 <- noquote(paste("localtable[,", "\"", x, "\"", ":=0]", sep = ""))
  mystring2 <- noquote(paste("print(", "\"", x, "\"", ")", sep = ""))
  mystring3 <- noquote(paste("for (i in 1:length(",  x, ")){", sep = ""))
  mystring4 <- noquote(paste("try({m <- ", x, "[i]", sep = ""))
  mystring5 <- noquote("  print(m)")
  mystring7 <- noquote(paste("  localtable[, ", "\"", x, "\"", ":= ", x, " + get(as.character(m))]})", sep = ""))
  mystring8 <- noquote("}")
  
  
  
  write(mystring1, file=filename, append=T, sep='\n')
  write(mystring2, file=filename, append=T, sep='\n')
  write(mystring3, file=filename, append=T, sep='\n')
  write(mystring4, file=filename, append=T, sep='\n')
  write(mystring5, file=filename, append=T, sep='\n')
  write(mystring7, file=filename, append=T, sep='\n')
  write(mystring8, file=filename, append=T, sep='\n')
  
  
  
}

BigBanksTable[,"NOSTRO_pnlold":=0]
print("NOSTRO_pnlold")
for (i in 1:length(NOSTRO_pnlold)){
  try({m <- NOSTRO_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "NOSTRO_pnlold":= NOSTRO_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Credits_to_banks_pnlold":=0]
print("Credits_to_banks_pnlold")
for (i in 1:length(Credits_to_banks_pnlold)){
  try({m <- Credits_to_banks_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Credits_to_banks_pnlold":= Credits_to_banks_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Corporate_credits_pnlold":=0]
print("Corporate_credits_pnlold")
for (i in 1:length(Corporate_credits_pnlold)){
  try({m <- Corporate_credits_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Corporate_credits_pnlold":= Corporate_credits_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Retail_credits_pnlold":=0]
print("Retail_credits_pnlold")
for (i in 1:length(Retail_credits_pnlold)){
  try({m <- Retail_credits_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Retail_credits_pnlold":= Retail_credits_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Debt_assets_pnlold":=0]
print("Debt_assets_pnlold")
for (i in 1:length(Debt_assets_pnlold)){
  try({m <- Debt_assets_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Debt_assets_pnlold":= Debt_assets_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Bills":=0]
print("Bills")
for (i in 1:length(Bills)){
  try({m <- Bills[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Bills":= Bills + get(pnlold_rename(m))]})
}
BigBanksTable[,"Current_accounts_of_banks_pnlold":=0]
print("Current_accounts_of_banks_pnlold")
for (i in 1:length(Current_accounts_of_banks_pnlold)){
  try({m <- Current_accounts_of_banks_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Current_accounts_of_banks_pnlold":= Current_accounts_of_banks_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Term_deposits_of_banks_pnlold":=0]
print("Term_deposits_of_banks_pnlold")
for (i in 1:length(Term_deposits_of_banks_pnlold)){
  try({m <- Term_deposits_of_banks_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Term_deposits_of_banks_pnlold":= Term_deposits_of_banks_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Term_deposits_of_banks_minus_pnlold":=0]
print("Term_deposits_of_banks_minus_pnlold")
for (i in 1:length(Term_deposits_of_banks_minus_pnlold)){
  try({m <- Term_deposits_of_banks_minus_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Term_deposits_of_banks_minus_pnlold":= Term_deposits_of_banks_minus_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Corporate_current_accounts_pnlold":=0]
print("Corporate_current_accounts_pnlold")
for (i in 1:length(Corporate_current_accounts_pnlold)){
  try({m <- Corporate_current_accounts_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Corporate_current_accounts_pnlold":= Corporate_current_accounts_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Retail_current_accounts_pnlold":=0]
print("Retail_current_accounts_pnlold")
for (i in 1:length(Retail_current_accounts_pnlold)){
  try({m <- Retail_current_accounts_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Retail_current_accounts_pnlold":= Retail_current_accounts_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Corporate_term_deposits_pnlold":=0]
print("Corporate_term_deposits_pnlold")
for (i in 1:length(Corporate_term_deposits_pnlold)){
  try({m <- Corporate_term_deposits_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Corporate_term_deposits_pnlold":= Corporate_term_deposits_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Retail_term_deposits_pnlold":=0]
print("Retail_term_deposits_pnlold")
for (i in 1:length(Retail_term_deposits_pnlold)){
  try({m <- Retail_term_deposits_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Retail_term_deposits_pnlold":= Retail_term_deposits_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Expenditures_on_bonds_issued":=0]
print("Expenditures_on_bonds_issued")
for (i in 1:length(Expenditures_on_bonds_issued)){
  try({m <- Expenditures_on_bonds_issued[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Expenditures_on_bonds_issued":= Expenditures_on_bonds_issued + get(pnlold_rename(m))]})
}
BigBanksTable[,"Expenditures_on_bills_issued":=0]
print("Expenditures_on_bills_issued")
for (i in 1:length(Expenditures_on_bills_issued)){
  try({m <- Expenditures_on_bills_issued[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Expenditures_on_bills_issued":= Expenditures_on_bills_issued + get(pnlold_rename(m))]})
}
BigBanksTable[,"Certificates_pnlold":=0]
print("Certificates_pnlold")
for (i in 1:length(Certificates_pnlold)){
  try({m <- Certificates_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Certificates_pnlold":= Certificates_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Fee_income_pnlold":=0]
print("Fee_income_pnlold")
for (i in 1:length(Fee_income_pnlold)){
  try({m <- Fee_income_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Fee_income_pnlold":= Fee_income_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Fee_expenditures_pnlold":=0]
print("Fee_expenditures_pnlold")
for (i in 1:length(Fee_expenditures_pnlold)){
  try({m <- Fee_expenditures_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Fee_expenditures_pnlold":= Fee_expenditures_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Realized_securities_issued_pnlold":=0]
print("Realized_securities_issued_pnlold")
for (i in 1:length(Realized_securities_issued_pnlold)){
  try({m <- Realized_securities_issued_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Realized_securities_issued_pnlold":= Realized_securities_issued_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Realized_securities_issued_minus_pnlold":=0]
print("Realized_securities_issued_minus_pnlold")
for (i in 1:length(Realized_securities_issued_minus_pnlold)){
  try({m <- Realized_securities_issued_minus_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Realized_securities_issued_minus_pnlold":= Realized_securities_issued_minus_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Unrealized_securities_pnlold":=0]
print("Unrealized_securities_pnlold")
for (i in 1:length(Unrealized_securities_pnlold)){
  try({m <- Unrealized_securities_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Unrealized_securities_pnlold":= Unrealized_securities_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Unrealized_securities_pnlold_minus":=0]
print("Unrealized_securities_pnlold_minus")
for (i in 1:length(Unrealized_securities_pnlold_minus)){
  try({m <- Unrealized_securities_pnlold_minus[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Unrealized_securities_pnlold_minus":= Unrealized_securities_pnlold_minus + get(pnlold_rename(m))]})
}
BigBanksTable[,"Realized_fc_pnlold":=0]
print("Realized_fc_pnlold")
for (i in 1:length(Realized_fc_pnlold)){
  try({m <- Realized_fc_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Realized_fc_pnlold":= Realized_fc_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Unrealized_fc_pnlold":=0]
print("Unrealized_fc_pnlold")
for (i in 1:length(Unrealized_fc_pnlold)){
  try({m <- Unrealized_fc_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Unrealized_fc_pnlold":= Unrealized_fc_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Other_corporates_pnlold":=0]
print("Other_corporates_pnlold")
for (i in 1:length(Other_corporates_pnlold)){
  try({m <- Other_corporates_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Other_corporates_pnlold":= Other_corporates_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Derivatives_etc_pnlold":=0]
print("Derivatives_etc_pnlold")
for (i in 1:length(Derivatives_etc_pnlold)){
  try({m <- Derivatives_etc_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Derivatives_etc_pnlold":= Derivatives_etc_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Derivatives_etc_minus_pnlold":=0]
print("Derivatives_etc_minus_pnlold")
for (i in 1:length(Derivatives_etc_minus_pnlold)){
  try({m <- Derivatives_etc_minus_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Derivatives_etc_minus_pnlold":= Derivatives_etc_minus_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Other_income_pnlold":=0]
print("Other_income_pnlold")
for (i in 1:length(Other_income_pnlold)){
  try({m <- Other_income_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Other_income_pnlold":= Other_income_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Other_expenditures_pnlold":=0]
print("Other_expenditures_pnlold")
for (i in 1:length(Other_expenditures_pnlold)){
  try({m <- Other_expenditures_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Other_expenditures_pnlold":= Other_expenditures_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Other_expenditures_minus_pnlold":=0]
print("Other_expenditures_minus_pnlold")
for (i in 1:length(Other_expenditures_minus_pnlold)){
  try({m <- Other_expenditures_minus_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Other_expenditures_minus_pnlold":= Other_expenditures_minus_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Changes_reserves":=0]
print("Changes_reserves")
for (i in 1:length(Changes_reserves)){
  try({m <- Changes_reserves[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Changes_reserves":= Changes_reserves + get(pnlold_rename(m))]})
}
BigBanksTable[,"Personnel_etc_pnlold":=0]
print("Personnel_etc_pnlold")
for (i in 1:length(Personnel_etc_pnlold)){
  try({m <- Personnel_etc_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Personnel_etc_pnlold":= Personnel_etc_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Organization_etc":=0]
print("Organization_etc")
for (i in 1:length(Organization_etc)){
  try({m <- Organization_etc[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Organization_etc":= Organization_etc + get(pnlold_rename(m))]})
}
BigBanksTable[,"Organization_etc_minus":=0]
print("Organization_etc_minus")
for (i in 1:length(Organization_etc_minus)){
  try({m <- Organization_etc_minus[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Organization_etc_minus":= Organization_etc_minus + get(pnlold_rename(m))]})
}
BigBanksTable[,"PBT_pnlold":=0]
print("PBT_pnlold")
BigBanksTable[, "PBT_pnlold":= `01000pnl_old`]
BigBanksTable[, "LBT_pnlold":= `02000pnl_old`]


BigBanksTable[,"Taxes_pnlold":=0]
print("Taxes_pnlold")
for (i in 1:length(Taxes_pnlold)){
  try({m <- Taxes_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Taxes_pnlold":= Taxes_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Taxes_minus_pnlold":=0]
print("Taxes_minus_pnlold")
for (i in 1:length(Taxes_minus_pnlold)){
  try({m <- Taxes_minus_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Taxes_minus_pnlold":= Taxes_minus_pnlold + get(pnlold_rename(m))]})
}
BigBanksTable[,"Retained_earnings_pnlold":=0]
print("Retained_earnings_pnlold")
for (i in 1:length(Retained_earnings_pnlold)){
  try({m <- Retained_earnings_pnlold[i]
  print(m)
  #print(BigBanksTable[,get(pnlold_rename(m))])
  BigBanksTable[, "Retained_earnings_pnlold":= Retained_earnings_pnlold + get(pnlold_rename(m))]})
}

View(BigBanksTable)
BigBanksTable[V1 == 3, "16305pnl_old"]

# PNL #####

BigBanksTable[,"NOSTRO_pnl":=0]
print("NOSTRO_pnl")
for (i in 1:length(NOSTRO_pnl)){
  try({m <- NOSTRO_pnl[i]
  print(m)
  BigBanksTable[, "NOSTRO_pnl":= NOSTRO_pnl + get(pnl_rename(m))]})
}
BigBanksTable[,"Credits_to_banks_pnl":=0]
print("Credits_to_banks_pnl")
for (i in 1:length(Credits_to_banks_pnl)){
  try({m <- Credits_to_banks_pnl[i]
  print(m)
  BigBanksTable[, "Credits_to_banks_pnl":= Credits_to_banks_pnl + get(pnl_rename(m))]})
}
BigBanksTable[,"Credits_to_banks_pnl_minus":=0]
print("Credits_to_banks_pnl_minus")
for (i in 1:length(Credits_to_banks_pnl_minus)){
  try({m <- Credits_to_banks_pnl_minus[i]
  print(m)
  BigBanksTable[, "Credits_to_banks_pnl_minus":= Credits_to_banks_pnl_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Corporate_credits_pnl":=0]
print("Corporate_credits_pnl")
for (i in 1:length(Corporate_credits_pnl)){
  try({m <- Corporate_credits_pnl[i]
  print(m)
  BigBanksTable[, "Corporate_credits_pnl":= Corporate_credits_pnl + get(pnl_rename(m))]})
}
BigBanksTable[,"Corporate_credits_pnl_minus":=0]
print("Corporate_credits_pnl_minus")
for (i in 1:length(Corporate_credits_pnl_minus)){
  try({m <- Corporate_credits_pnl_minus[i]
  print(m)
  BigBanksTable[, "Corporate_credits_pnl_minus":= Corporate_credits_pnl_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Retail_credits_pnl":=0]
print("Retail_credits_pnl")
for (i in 1:length(Retail_credits_pnl)){
  try({m <- Retail_credits_pnl[i]
  print(m)
  BigBanksTable[, "Retail_credits_pnl":= Retail_credits_pnl + get(pnl_rename(m))]})
}
BigBanksTable[,"Retail_credits_pnl_minus":=0]
print("Retail_credits_pnl_minus")
for (i in 1:length(Retail_credits_pnl_minus)){
  try({m <- Retail_credits_pnl_minus[i]
  print(m)
  BigBanksTable[, "Retail_credits_pnl_minus":= Retail_credits_pnl_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Debt_pnl":=0]
print("Debt_pnl")
for (i in 1:length(Debt_pnl)){
  try({m <- Debt_pnl[i]
  print(m)
  BigBanksTable[, "Debt_pnl":= Debt_pnl + get(pnl_rename(m))]})
}
BigBanksTable[,"Debt_pnl_minus":=0]
print("Debt_pnl_minus")
for (i in 1:length(Debt_pnl_minus)){
  try({m <- Debt_pnl_minus[i]
  print(m)
  BigBanksTable[, "Debt_pnl_minus":= Debt_pnl_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Bills_pnl":=0]
print("Bills_pnl")
for (i in 1:length(Bills_pnl)){
  try({m <- Bills_pnl[i]
  print(m)
  BigBanksTable[, "Bills_pnl":= Bills_pnl + get(pnl_rename(m))]})
}
BigBanksTable[,"Current_accounts_of_banks_pnl_minus":=0]
print("Current_accounts_of_banks_pnl_minus")
for (i in 1:length(Current_accounts_of_banks_pnl_minus)){
  try({m <- Current_accounts_of_banks_pnl_minus[i]
  print(m)
  BigBanksTable[, "Current_accounts_of_banks_pnl_minus":= Current_accounts_of_banks_pnl_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Term_deposits_of_banks_minus_pnl":=0]
print("Term_deposits_of_banks_minus_pnl")
for (i in 1:length(Term_deposits_of_banks_minus_pnl)){
  try({m <- Term_deposits_of_banks_minus_pnl[i]
  print(m)
  BigBanksTable[, "Term_deposits_of_banks_minus_pnl":= Term_deposits_of_banks_minus_pnl + get(pnl_rename(m))]})
}
BigBanksTable[,"Current_accounts_corporate_pnl_minus":=0]
print("Current_accounts_corporate_pnl_minus")
for (i in 1:length(Current_accounts_corporate_pnl_minus)){
  try({m <- Current_accounts_corporate_pnl_minus[i]
  print(m)
  BigBanksTable[, "Current_accounts_corporate_pnl_minus":= Current_accounts_corporate_pnl_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Current_accounts_retail_pnl_minus":=0]
print("Current_accounts_retail_pnl_minus")
for (i in 1:length(Current_accounts_retail_pnl_minus)){
  try({m <- Current_accounts_retail_pnl_minus[i]
  print(m)
  BigBanksTable[, "Current_accounts_retail_pnl_minus":= Current_accounts_retail_pnl_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Term_deposits_corporate_minus_pnl":=0]
print("Term_deposits_corporate_minus_pnl")
for (i in 1:length(Term_deposits_corporate_minus_pnl)){
  try({m <- Term_deposits_corporate_minus_pnl[i]
  print(m)
  BigBanksTable[, "Term_deposits_corporate_minus_pnl":= Term_deposits_corporate_minus_pnl + get(pnl_rename(m))]})
}
BigBanksTable[,"Term_deposits_corporate_pnl":=0]
print("Term_deposits_corporate_pnl")
for (i in 1:length(Term_deposits_corporate_pnl)){
  try({m <- Term_deposits_corporate_pnl[i]
  print(m)
  BigBanksTable[, "Term_deposits_corporate_pnl":= Term_deposits_corporate_pnl + get(pnl_rename(m))]})
}
BigBanksTable[,"Term_deposits_retail_minus_pnl":=0]
print("Term_deposits_retail_minus_pnl")
for (i in 1:length(Term_deposits_retail_minus_pnl)){
  try({m <- Term_deposits_retail_minus_pnl[i]
  print(m)
  BigBanksTable[, "Term_deposits_retail_minus_pnl":= Term_deposits_retail_minus_pnl + get(pnl_rename(m))]})
}
BigBanksTable[,"Securities_issued":=0]
print("Securities_issued")
for (i in 1:length(Securities_issued)){
  try({m <- Securities_issued[i]
  print(m)
  BigBanksTable[, "Securities_issued":= Securities_issued + get(pnl_rename(m))]})
}
BigBanksTable[,"Securities_issued_minus":=0]
print("Securities_issued_minus")
for (i in 1:length(Securities_issued_minus)){
  try({m <- Securities_issued_minus[i]
  print(m)
  BigBanksTable[, "Securities_issued_minus":= Securities_issued_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Fee_income":=0]
print("Fee_income")
for (i in 1:length(Fee_income)){
  try({m <- Fee_income[i]
  print(m)
  BigBanksTable[, "Fee_income":= Fee_income + get(pnl_rename(m))]})
}
BigBanksTable[,"Fee_expenditure":=0]
print("Fee_expenditure")
for (i in 1:length(Fee_expenditure)){
  try({m <- Fee_expenditure[i]
  print(m)
  BigBanksTable[, "Fee_expenditure":= Fee_expenditure + get(pnl_rename(m))]})
}
BigBanksTable[,"Fee_expenditure_minus":=0]
print("Fee_expenditure_minus")
for (i in 1:length(Fee_expenditure_minus)){
  try({m <- Fee_expenditure_minus[i]
  print(m)
  BigBanksTable[, "Fee_expenditure_minus":= Fee_expenditure_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Realized_securities":=0]
print("Realized_securities")
for (i in 1:length(Realized_securities)){
  try({m <- Realized_securities[i]
  print(m)
  BigBanksTable[, "Realized_securities":= Realized_securities + get(pnl_rename(m))]})
}
BigBanksTable[,"Realized_securities_minus":=0]
print("Realized_securities_minus")
for (i in 1:length(Realized_securities_minus)){
  try({m <- Realized_securities_minus[i]
  print(m)
  BigBanksTable[, "Realized_securities_minus":= Realized_securities_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Unrealized_securities":=0]
print("Unrealized_securities")
for (i in 1:length(Unrealized_securities)){
  try({m <- Unrealized_securities[i]
  print(m)
  BigBanksTable[, "Unrealized_securities":= Unrealized_securities + get(pnl_rename(m))]})
}
BigBanksTable[,"Unrealized_securities_minus":=0]
print("Unrealized_securities_minus")
for (i in 1:length(Unrealized_securities_minus)){
  try({m <- Unrealized_securities_minus[i]
  print(m)
  BigBanksTable[, "Unrealized_securities_minus":= Unrealized_securities_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Realized_fc":=0]
print("Realized_fc")
for (i in 1:length(Realized_fc)){
  try({m <- Realized_fc[i]
  print(m)
  BigBanksTable[, "Realized_fc":= Realized_fc + get(pnl_rename(m))]})
}
BigBanksTable[,"Realized_fc_minus":=0]
print("Realized_fc_minus")
for (i in 1:length(Realized_fc_minus)){
  try({m <- Realized_fc_minus[i]
  print(m)
  BigBanksTable[, "Realized_fc_minus":= Realized_fc_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Realized_fc_minus":=0]
print("Realized_fc_minus")
for (i in 1:length(
  Realized_fc_minus)){
  try({m <- 
    Realized_fc_minus[i]
  print(m)
  BigBanksTable[, "Realized_fc_minus":= Realized_fc_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Unrealized_fc":=0]
print("Unrealized_fc")
for (i in 1:length(Unrealized_fc)){
  try({m <- Unrealized_fc[i]
  print(m)
  BigBanksTable[, "Unrealized_fc":= Unrealized_fc + get(pnl_rename(m))]})
}
BigBanksTable[,"Unrealized_fc_minus":=0]
print("Unrealized_fc_minus")
for (i in 1:length(Unrealized_fc_minus)){
  try({m <- Unrealized_fc_minus[i]
  print(m)
  BigBanksTable[, "Unrealized_fc_minus":= Unrealized_fc_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Other_corporates":=0]
print("Other_corporates")
for (i in 1:length(Other_corporates)){
  try({m <- Other_corporates[i]
  print(m)
  BigBanksTable[, "Other_corporates":= Other_corporates + get(pnl_rename(m))]})
}
BigBanksTable[,"Derivatives_etc":=0]
print("Derivatives_etc")
for (i in 1:length(Derivatives_etc)){
  try({m <- Derivatives_etc[i]
  print(m)
  BigBanksTable[, "Derivatives_etc":= Derivatives_etc + get(pnl_rename(m))]})
}
BigBanksTable[,"Derivatives_etc_minus":=0]
print("Derivatives_etc_minus")
for (i in 1:length(Derivatives_etc_minus)){
  try({m <- Derivatives_etc_minus[i]
  print(m)
  BigBanksTable[, "Derivatives_etc_minus":= Derivatives_etc_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Other_income":=0]
print("Other_income")
for (i in 1:length(Other_income)){
  try({m <- Other_income[i]
  print(m)
  BigBanksTable[, "Other_income":= Other_income + get(pnl_rename(m))]})
}
BigBanksTable[,"Other_income_minus":=0]
print("Other_income_minus")
for (i in 1:length(Other_income_minus)){
  try({m <- Other_income_minus[i]
  print(m)
  BigBanksTable[, "Other_income_minus":= Other_income_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Other_expenditures":=0]
print("Other_expenditures")
for (i in 1:length(Other_expenditures)){
  try({m <- Other_expenditures[i]
  print(m)
  BigBanksTable[, "Other_expenditures":= Other_expenditures + get(pnl_rename(m))]})
}
BigBanksTable[,"Other_expenditures_minus":=0]
print("Other_expenditures_minus")
for (i in 1:length(Other_expenditures_minus)){
  try({m <- Other_expenditures_minus[i]
  print(m)
  BigBanksTable[, "Other_expenditures_minus":= Other_expenditures_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Reserve_adjustments":=0]
print("Reserve_adjustments")
for (i in 1:length(Reserve_adjustments)){
  try({m <- Reserve_adjustments[i]
  print(m)
  BigBanksTable[, "Reserve_adjustments":= Reserve_adjustments + get(pnl_rename(m))]})
}
BigBanksTable[,"Reserve_adjustments_minus":=0]
print("Reserve_adjustments_minus")
for (i in 1:length(Reserve_adjustments_minus)){
  try({m <- Reserve_adjustments_minus[i]
  print(m)
  BigBanksTable[, "Reserve_adjustments_minus":= Reserve_adjustments_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Personnel_etc":=0]
print("Personnel_etc")
for (i in 1:length(Personnel_etc)){
  try({m <- Personnel_etc[i]
  print(m)
  BigBanksTable[, "Personnel_etc":= Personnel_etc + get(pnl_rename(m))]})
}
BigBanksTable[,"Personnel_etc_minus":=0]
print("Personnel_etc_minus")
for (i in 1:length(Personnel_etc_minus)){
  try({m <- Personnel_etc_minus[i]
  print(m)
  BigBanksTable[, "Personnel_etc_minus":= Personnel_etc_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Organization_etc":=0]
print("Organization_etc")
for (i in 1:length(Organization_etc)){
  try({m <- Organization_etc[i]
  print(m)
  BigBanksTable[, "Organization_etc":= Organization_etc + get(pnl_rename(m))]})
}
BigBanksTable[,"Organization_etc_minus":=0]
print("Organization_etc_minus")
for (i in 1:length(Organization_etc_minus)){
  try({m <- Organization_etc_minus[i]
  print(m)
  BigBanksTable[, "Organization_etc_minus":= Organization_etc_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Income_before_taxes":=0]
print("Income_before_taxes")
for (i in 1:length(Income_before_taxes)){
  try({m <- Income_before_taxes[i]
  print(m)
  BigBanksTable[, "Income_before_taxes":= Income_before_taxes + get(pnl_rename(m))]})
}
BigBanksTable[,"Income_before_taxes_minus":=0]
print("Income_before_taxes_minus")
for (i in 1:length(Income_before_taxes_minus)){
  try({m <- Income_before_taxes_minus[i]
  print(m)
  BigBanksTable[, "Income_before_taxes_minus":= Income_before_taxes_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Taxes":=0]
print("Taxes")
for (i in 1:length(Taxes)){
  try({m <- Taxes[i]
  print(m)
  BigBanksTable[, "Taxes":= Taxes + get(pnl_rename(m))]})
}
BigBanksTable[,"Taxes_minus":=0]
print("Taxes_minus")
for (i in 1:length(Taxes_minus)){
  try({m <- Taxes_minus[i]
  print(m)
  BigBanksTable[, "Taxes_minus":= Taxes_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Unretained_earnings":=0]
print("Unretained_earnings")
for (i in 1:length(Unretained_earnings)){
  try({m <- Unretained_earnings[i]
  print(m)
  BigBanksTable[, "Unretained_earnings":= Unretained_earnings + get(pnl_rename(m))]})
}
BigBanksTable[,"Unretained_earnings_minus":=0]
print("Unretained_earnings_minus")
for (i in 1:length(Unretained_earnings_minus)){
  try({m <- Unretained_earnings_minus[i]
  print(m)
  BigBanksTable[, "Unretained_earnings_minus":= Unretained_earnings_minus + get(pnl_rename(m))]})
}
BigBanksTable[,"Other_comprehensive_income":=0]
print("Other_comprehensive_income")
for (i in 1:length(Other_comprehensive_income)){
  try({m <- Other_comprehensive_income[i]
  print(m)
  BigBanksTable[, "Other_comprehensive_income":= Other_comprehensive_income + get(pnl_rename(m))]})
}
BigBanksTable[,"Other_comprehensive_income_minus":=0]
print("Other_comprehensive_income_minus")
for (i in 1:length(Other_comprehensive_income_minus)){
  try({m <- Other_comprehensive_income_minus[i]
  print(m)
  BigBanksTable[, "Other_comprehensive_income_minus":= Other_comprehensive_income_minus + get(pnl_rename(m))]})
}

#bs #####
BigBanksTable[,"Cash_and_equivalents":=0]
print("Cash_and_equivalents")
for (i in 1:length(Cash_and_equivalents)){
  try({m <- Cash_and_equivalents[i]
  print(m)
  BigBanksTable[, "Cash_and_equivalents":= Cash_and_equivalents + get(as.character(m))]})
}
BigBanksTable[,"NOSTRO":=0]
print("NOSTRO")
for (i in 1:length(NOSTRO)){
  try({m <- NOSTRO[i]
  print(m)
  BigBanksTable[, "NOSTRO":= NOSTRO + get(as.character(m))]})
}
BigBanksTable[,"NOSTRO_minus":=0]
print("NOSTRO_minus")
for (i in 1:length(NOSTRO_minus)){
  try({m <- NOSTRO_minus[i]
  print(m)
  BigBanksTable[, "NOSTRO_minus":= NOSTRO_minus + get(as.character(m))]})
}
BigBanksTable[,"Credits_to_banks":=0]
print("Credits_to_banks")
for (i in 1:length(Credits_to_banks)){
  try({m <- Credits_to_banks[i]
  print(m)
  BigBanksTable[, "Credits_to_banks":= Credits_to_banks + get(as.character(m))]})
}
BigBanksTable[,"Credits_to_banks_minus":=0]
print("Credits_to_banks_minus")
for (i in 1:length(Credits_to_banks_minus)){
  try({m <- Credits_to_banks_minus[i]
  print(m)
  BigBanksTable[, "Credits_to_banks_minus":= Credits_to_banks_minus + get(as.character(m))]})
}
BigBanksTable[,"Securities":=0]
print("Securities")
for (i in 1:length(Securities)){
  try({m <- Securities[i]
  print(m)
  BigBanksTable[, "Securities":= Securities + get(as.character(m))]})
}
BigBanksTable[,"Securities_minus":=0]
print("Securities_minus")
for (i in 1:length(Securities_minus)){
  try({m <- Securities_minus[i]
  print(m)
  BigBanksTable[, "Securities_minus":= Securities_minus + get(as.character(m))]})
}
BigBanksTable[,"Corporate_credits":=0]
print("Corporate_credits")
for (i in 1:length(Corporate_credits)){
  try({m <- Corporate_credits[i]
  print(m)
  BigBanksTable[, "Corporate_credits":= Corporate_credits + get(as.character(m))]})
}
BigBanksTable[,"Corporate_credits_minus":=0]
print("Corporate_credits_minus")
for (i in 1:length(Corporate_credits_minus)){
  try({m <- Corporate_credits_minus[i]
  print(m)
  BigBanksTable[, "Corporate_credits_minus":= Corporate_credits_minus + get(as.character(m))]})
}
BigBanksTable[,"Retail_credits":=0]
print("Retail_credits")
for (i in 1:length(Retail_credits)){
  try({m <- Retail_credits[i]
  print(m)
  BigBanksTable[, "Retail_credits":= Retail_credits + get(as.character(m))]})
}
BigBanksTable[,"Retail_credits_minus":=0]
print("Retail_credits_minus")
for (i in 1:length(Retail_credits_minus)){
  try({m <- Retail_credits_minus[i]
  print(m)
  BigBanksTable[, "Retail_credits_minus":= Retail_credits_minus + get(as.character(m))]})
}
BigBanksTable[,"Reserves_for_delayed_loans":=0]
print("Reserves_for_delayed_loans")
for (i in 1:length(Reserves_for_delayed_loans)){
  try({m <- Reserves_for_delayed_loans[i]
  print(m)
  BigBanksTable[, "Reserves_for_delayed_loans":= Reserves_for_delayed_loans + get(as.character(m))]})
}
BigBanksTable[,"Reserves_for_delayed_loans_minus":=0]
print("Reserves_for_delayed_loans_minus")
for (i in 1:length(Reserves_for_delayed_loans_minus)){
  try({m <- Reserves_for_delayed_loans_minus[i]
  print(m)
  BigBanksTable[, "Reserves_for_delayed_loans_minus":= Reserves_for_delayed_loans_minus + get(as.character(m))]})
}
BigBanksTable[,"Other_assets":=0]
print("Other_assets")
for (i in 1:length(Other_assets)){
  try({m <- Other_assets[i]
  print(m)
  BigBanksTable[, "Other_assets":= Other_assets + get(as.character(m))]})
}
BigBanksTable[,"Other_assets_minus":=0]
print("Other_assets_minus")
for (i in 1:length(Other_assets_minus)){
  try({m <- Other_assets_minus[i]
  print(m)
  BigBanksTable[, "Other_assets_minus":= Other_assets_minus + get(as.character(m))]})
}
BigBanksTable[,"LORO":=0]
print("LORO")
for (i in 1:length(LORO)){
  try({m <- LORO[i]
  print(m)
  BigBanksTable[, "LORO":= LORO + get(as.character(m))]})
}
BigBanksTable[,"Interbank_loans":=0]
print("Interbank_loans")
for (i in 1:length(Interbank_loans)){
  try({m <- Interbank_loans[i]
  print(m)
  BigBanksTable[, "Interbank_loans":= Interbank_loans + get(as.character(m))]})
}
BigBanksTable[,"Corporate_current_accounts":=0]
print("Corporate_current_accounts")
for (i in 1:length(Corporate_current_accounts)){
  try({m <- Corporate_current_accounts[i]
  print(m)
  BigBanksTable[, "Corporate_current_accounts":= Corporate_current_accounts + get(as.character(m))]})
}
BigBanksTable[,"Corporate_current_accounts_minus":=0]
print("Corporate_current_accounts_minus")
for (i in 1:length(Corporate_current_accounts_minus)){
  try({m <- Corporate_current_accounts_minus[i]
  print(m)
  BigBanksTable[, "Corporate_current_accounts_minus":= Corporate_current_accounts_minus + get(as.character(m))]})
}
BigBanksTable[,"Retail_current_accounts":=0]
print("Retail_current_accounts")
for (i in 1:length(Retail_current_accounts)){
  try({m <- Retail_current_accounts[i]
  print(m)
  BigBanksTable[, "Retail_current_accounts":= Retail_current_accounts + get(as.character(m))]})
}
BigBanksTable[,"Trading_accounts":=0]
print("Trading_accounts")
for (i in 1:length(Trading_accounts)){
  try({m <- Trading_accounts[i]
  print(m)
  BigBanksTable[, "Trading_accounts":= Trading_accounts + get(as.character(m))]})
}
BigBanksTable[,"Corporate_time_deposits":=0]
print("Corporate_time_deposits")
for (i in 1:length(Corporate_time_deposits)){
  try({m <- Corporate_time_deposits[i]
  print(m)
  BigBanksTable[, "Corporate_time_deposits":= Corporate_time_deposits + get(as.character(m))]})
}
BigBanksTable[,"Retail_time_deposits":=0]
print("Retail_time_deposits")
for (i in 1:length(Retail_time_deposits)){
  try({m <- Retail_time_deposits[i]
  print(m)
  BigBanksTable[, "Retail_time_deposits":= Retail_time_deposits + get(as.character(m))]})
}
BigBanksTable[,"Bonds":=0]
print("Bonds")
for (i in 1:length(Bonds)){
  try({m <- Bonds[i]
  print(m)
  BigBanksTable[, "Bonds":= Bonds + get(as.character(m))]})
}
BigBanksTable[,"Bills":=0]
print("Bills")
for (i in 1:length(Bills)){
  try({m <- Bills[i]
  print(m)
  BigBanksTable[, "Bills":= Bills + get(as.character(m))]})
}
BigBanksTable[,"Bills_minus":=0]
print("Bills_minus")
for (i in 1:length(Bills_minus)){
  try({m <- Bills_minus[i]
  print(m)
  BigBanksTable[, "Bills_minus":= Bills_minus + get(as.character(m))]})
}
BigBanksTable[,"Certificates_of_deposit":=0]
print("Certificates_of_deposit")
for (i in 1:length(Certificates_of_deposit)){
  try({m <- Certificates_of_deposit[i]
  print(m)
  BigBanksTable[, "Certificates_of_deposit":= Certificates_of_deposit + get(as.character(m))]})
}
BigBanksTable[,"Other_liabilities":=0]
print("Other_liabilities")
for (i in 1:length(Other_liabilities)){
  try({m <- Other_liabilities[i]
  print(m)
  BigBanksTable[, "Other_liabilities":= Other_liabilities + get(as.character(m))]})
}
BigBanksTable[,"Other_liabilities_minus":=0]
print("Other_liabilities_minus")
for (i in 1:length(Other_liabilities_minus)){
  try({m <- Other_liabilities_minus[i]
  print(m)
  BigBanksTable[, "Other_liabilities_minus":= Other_liabilities_minus + get(as.character(m))]})
}
BigBanksTable[,"Main_capital":=0]
print("Main_capital")
for (i in 1:length(Main_capital)){
  try({m <- Main_capital[i]
  print(m)
  BigBanksTable[, "Main_capital":= Main_capital + get(as.character(m))]})
}
BigBanksTable[,"Main_capital_minus":=0]
print("Main_capital_minus")
for (i in 1:length(Main_capital_minus)){
  try({m <- Main_capital_minus[i]
  print(m)
  BigBanksTable[, "Main_capital_minus":= Main_capital_minus + get(as.character(m))]})
}
BigBanksTable[,"Retained_earnings":=0]
print("Retained_earnings")
for (i in 1:length(Retained_earnings)){
  try({m <- Retained_earnings[i]
  print(m)
  BigBanksTable[, "Retained_earnings":= Retained_earnings + get(as.character(m))]})
}
BigBanksTable[,"Retained_earnings_minus":=0]
print("Retained_earnings_minus")
for (i in 1:length(Retained_earnings_minus)){
  try({m <- Retained_earnings_minus[i]
  print(m)
  BigBanksTable[, "Retained_earnings_minus":= Retained_earnings_minus + get(as.character(m))]})
}
BigBanksTable[,"Retained_earnings_this_year":=0]
print("Retained_earnings_this_year")
for (i in 1:length(Retained_earnings_this_year)){
  try({m <- Retained_earnings_this_year[i]
  print(m)
  BigBanksTable[, "Retained_earnings_this_year":= Retained_earnings_this_year + get(as.character(m))]})
}
BigBanksTable[,"Retained_earnings_this_year_minus":=0]
print("Retained_earnings_this_year_minus")
for (i in 1:length(Retained_earnings_this_year_minus)){
  try({m <- Retained_earnings_this_year_minus[i]
  print(m)
  BigBanksTable[, "Retained_earnings_this_year_minus":= Retained_earnings_this_year_minus + get(as.character(m))]})
}
BigBanksTable[,"Next_periods":=0]
print("Next_periods")
for (i in 1:length(Next_periods)){
  try({m <- Next_periods[i]
  print(m)
  BigBanksTable[, "Next_periods":= Next_periods + get(as.character(m))]})
}
BigBanksTable[,"Next_periods_minus":=0]
print("Next_periods_minus")
for (i in 1:length(Next_periods_minus)){
  try({m <- Next_periods_minus[i]
  print(m)
  BigBanksTable[, "Next_periods_minus":= Next_periods_minus + get(as.character(m))]})
}
BigBanksTable[,"Credit_limits":=0]
print("Credit_limits")
for (i in 1:length(Credit_limits)){
  try({m <- Credit_limits[i]
  print(m)
  BigBanksTable[, "Credit_limits":= Credit_limits + get(as.character(m))]})
}
BigBanksTable[,"Contingent_liabilities_minus":=0]
print("Contingent_liabilities_minus")
for (i in 1:length(Contingent_liabilities_minus)){
  try({m <- Contingent_liabilities_minus[i]
  print(m)
  BigBanksTable[, "Contingent_liabilities_minus":= Contingent_liabilities_minus + get(as.character(m))]})
}
BigBanksTable[,"Securities_for_credits_given":=0]
print("Securities_for_credits_given")
for (i in 1:length(Securities_for_credits_given)){
  try({m <- Securities_for_credits_given[i]
  print(m)
  BigBanksTable[, "Securities_for_credits_given":= Securities_for_credits_given + get(as.character(m))]})
}
BigBanksTable[,"Unexecuted_debts":=0]
print("Unexecuted_debts")
for (i in 1:length(Unexecuted_debts)){
  try({m <- Unexecuted_debts[i]
  print(m)
  BigBanksTable[, "Unexecuted_debts":= Unexecuted_debts + get(as.character(m))]})
}
BigBanksTable[,"Other_offbalance":=0]
print("Other_offbalance")
for (i in 1:length(Other_offbalance)){
  try({m <- Other_offbalance[i]
  print(m)
  BigBanksTable[, "Other_offbalance":= Other_offbalance + get(as.character(m))]})
}
BigBanksTable[,"Other_offbalance_minus":=0]
print("Other_offbalance_minus")
for (i in 1:length(Other_offbalance_minus)){
  try({m <- Other_offbalance_minus[i]
  print(m)
  BigBanksTable[, "Other_offbalance_minus":= Other_offbalance_minus + get(as.character(m))]})
}


View(BigBanksTable)

write.csv(BigBanksTable, file = "big_spread_with_aggregation.csv")

#Aggregation for final analysis####

bs_names <- c("Cash_and_equivalents","NOSTRO","NOSTRO_minus","Credits_to_banks","Credits_to_banks_minus","Securities","Securities_minus","Corporate_credits","Corporate_credits_minus","Retail_credits","Retail_credits_minus","Reserves_for_delayed_loans","Reserves_for_delayed_loans_minus","Other_assets","Other_assets_minus","LORO","Interbank_loans","Corporate_current_accounts","Corporate_current_accounts_minus","Retail_current_accounts","Trading_accounts","Corporate_time_deposits","Retail_time_deposits","Bonds","Bills","Bills_minus","Certificates_of_deposit","Other_liabilities","Other_liabilities_minus","Main_capital","Main_capital_minus","Retained_earnings","Retained_earnings_minus","Retained_earnings_this_year","Retained_earnings_this_year_minus","Next_periods","Next_periods_minus","Credit_limits","Contingent_liabilities_minus","Securities_for_credits_given","Unexecuted_debts","Other_offbalance","Other_offbalance_minus")
pnlold_names <- c("LBT_pnlold", "NOSTRO_pnlold","Credits_to_banks_pnlold","Corporate_credits_pnlold","Retail_credits_pnlold","Debt_assets_pnlold","Bills","Current_accounts_of_banks_pnlold","Term_deposits_of_banks_pnlold","Term_deposits_of_banks_minus_pnlold","Corporate_current_accounts_pnlold","Retail_current_accounts_pnlold","Corporate_term_deposits_pnlold","Retail_term_deposits_pnlold","Expenditures_on_bonds_issued","Expenditures_on_bills_issued","Certificates_pnlold","Fee_income_pnlold","Fee_expenditures_pnlold","Realized_securities_issued_pnlold","Realized_securities_issued_minus_pnlold","Unrealized_securities_pnlold","Unrealized_securities_pnlold_minus","Realized_fc_pnlold","Unrealized_fc_pnlold","Other_corporates_pnlold","Derivatives_etc_pnlold","Derivatives_etc_minus_pnlold","Other_income_pnlold","Other_expenditures_pnlold","Other_expenditures_minus_pnlold","Changes_reserves","Personnel_etc_pnlold","Organization_etc","Organization_etc_minus","PBT_pnlold","Taxes_pnlold","Taxes_minus_pnlold","Retained_earnings_pnlold")

pnl_names <- c("NOSTRO_pnl","Credits_to_banks_pnl","Credits_to_banks_pnl_minus","Corporate_credits_pnl","Corporate_credits_pnl_minus","Retail_credits_pnl","Retail_credits_pnl_minus","Debt_pnl","Debt_pnl_minus","Bills_pnl","Current_accounts_of_banks_pnl_minus","Term_deposits_of_banks_minus_pnl","Current_accounts_corporate_pnl_minus","Current_accounts_retail_pnl_minus","Term_deposits_corporate_minus_pnl","Term_deposits_corporate_pnl","Term_deposits_retail_minus_pnl","Securities_issued","Securities_issued_minus","Fee_income","Fee_expenditure","Fee_expenditure_minus","Realized_securities","Realized_securities_minus","Unrealized_securities","Unrealized_securities_minus","Realized_fc","Realized_fc_minus","Realized_fc_minus","Unrealized_fc","Unrealized_fc_minus","Other_corporates","Derivatives_etc","Derivatives_etc_minus","Other_income","Other_income_minus","Other_expenditures","Other_expenditures_minus","Reserve_adjustments","Reserve_adjustments_minus","Personnel_etc","Personnel_etc_minus","Organization_etc","Organization_etc_minus","Income_before_taxes","Income_before_taxes_minus","Taxes","Taxes_minus","Unretained_earnings","Unretained_earnings_minus","Other_comprehensive_income","Other_comprehensive_income_minus")


BigBanksTable <- fread("big_spread_with_aggregation.csv", colClasses = "numeric")
general_names <- c(bs_names, pnlold_names, pnl_names, "Year", "Month", "REGN")
#Equity
AggrBanksTable <- BigBanksTable[, ..general_names]
AggrBanksTable[, "Equity" := Main_capital - Main_capital_minus +Retained_earnings- Retained_earnings_minus + Retained_earnings_this_year - Retained_earnings_this_year_minus + Next_periods - Next_periods_minus]
#ROE
AggrBanksTable[(Year >2016)|((Year==2016)|(Month >4)), "NetIncome" := Income_before_taxes - Income_before_taxes_minus -Taxes+Taxes_minus]
AggrBanksTable[!((Year >2016)|((Year==2016)|(Month >4))), "NetIncome" := PBT_pnlold - LBT_pnlold]
AggrBanksTable[, "ROE" := NetIncome/Equity]
#Interbank
AggrBanksTable[, "NetInterbank" := Interbank_loans + LORO + NOSTRO-NOSTRO_minus + Credits_to_banks - Credits_to_banks_minus]
AggrBanksTable[, "InterbankShare" := NetInterbank/Equity]
#CorporateLoans
AggrBanksTable[, "CorporateShare" := (Corporate_credits- Corporate_credits_minus + Corporate_current_accounts - Corporate_current_accounts_minus + Corporate_time_deposits)/(Retail_credits - Retail_credits_minus + Credits_to_banks - Credits_to_banks_minus + Retail_current_accounts + Retail_time_deposits)]
#General leverage
AggrBanksTable[, "Liabilities" := LORO + Interbank_loans + Corporate_current_accounts -Corporate_current_accounts_minus + Retail_current_accounts + Trading_accounts + Corporate_time_deposits + Retail_time_deposits + Bills - Bills_minus + Certificates_of_deposit + Other_liabilities - Other_liabilities_minus]
AggrBanksTable[, "GeneralLeverage" := Liabilities/Equity]
#MarketLeverage
AggrBanksTable[, "Securities" := Bills - Bills_minus + Certificates_of_deposit + Other_liabilities - Other_liabilities_minus]
AggrBanksTable[, "SecuritiesLeverage" := Securities/Liabilities]


write.csv(AggrBanksTable, "AggregatedBanksTable.csv")

#create the aggregated table with ratings ####
AggrTable <- read.csv("AggregatedBanksTable.csv")
library("xlsx")

ListOfBanks <- read.xlsx("List_of_banks_regnums.xlsx",1, encoding = "UTF-8")
ListOfBanks[ListOfBanks$cregnum == 23]
AggrTableWithNames <- merge(AggrTable, ListOfBanks, by.x = "REGN", by.y = "cregnum", all.x=T)
AggrTableWithNames <- AggrTableWithNames[!is.na(AggrTableWithNames$csname),]

library(XML)
library(xml2)
library(stringr)
RatingTableRawHTML <- read_html("Рейтингуемые лица.html")
RatingTableRawHTMLTable <- xml_find_all(RatingTableRawHTML, xpath = ".//div[@class = 'search-table__wrapper-main']")
RatingTableRawTable <- xml_text(RatingTableRawHTMLTable)
RatingTableRawTable <- str_split(RatingTableRawTable, "\n\t\t\t\t\t\t\t\t\t\t\n                \n              \n            \n              \n                \n\t\t\t\t\t\t\t\t\t\t")
View(RatingTableRawTable[[1]])
SingleStringProcess <- function(string){
RatingStringRaw <- string
RatingStringRaw <- str_replace_all(RatingStringRaw, '\t', "")
RatingStringRaw <- str_replace_all(RatingStringRaw, '\n', "")
RatingStringRaw <- str_split(RatingStringRaw, "  ")
RatingStringRawMask <- str_detect(RatingStringRaw[[1]], ".")
RatingStringRaw <- RatingStringRaw[[1]][RatingStringRawMask]
if (!(T %in% str_detect(RatingStringRaw, "Отозван"))){
RatingStringRawTable <- data.table(name = RatingStringRaw[1], 
                                   rating = RatingStringRaw[2],
                                   Status = RatingStringRaw[3])
}
else{
  RatingStringRawTable <- data.table(name = RatingStringRaw[1], 
                                     rating = "No",
                                     Status = RatingStringRaw[2])
}
return(RatingStringRawTable)}
RatingTableRawTable <- lapply(RatingTableRawTable[[1]], SingleStringProcess)
RatingTableRawTable <- do.call(rbind, RatingTableRawTable)
RatingTableRawTable[, "name":=str_remove(name, "^\\s")]
RatingTableRawTable[, "name":=str_remove(name, "\\s$")]
RatingTableRawTable[, "name":=str_remove(name, '"+')]
RatingTableRawTable[, "name":=str_remove(name, "'+")]
RatingTableRawTable[, "name":=str_remove(name, '«+')]
RatingTableRawTable[, "name":=str_remove(name, '»+')]
RatingTableRawTable[, "name":=str_remove(name, '"+')]
RatingTableRawTable[, "name":=str_remove(name, "'+")]
RatingTableRawTable[, "name":=str_remove(name, '«+')]
RatingTableRawTable[, "name":=str_remove(name, '»+')]
RatingTableRawTable[, "name":=str_remove(name, '"+')]
RatingTableRawTable[, "name":=str_remove(name, "'+")]
RatingTableRawTable[, "name":=str_remove(name, '«+')]
RatingTableRawTable[, "name":=str_remove(name, '»+')]
RatingTableRawTable[, "name":= tolower(name)]
RatingTableRawTable[name == "ао натиксис банк", "name":= "натиксис банк ао"]
RatingTableRawTable[name == "ао банк реалист", "name":= "ао реалист банк"]
RatingTableRawTable[name == "ао экспобанк", "name":= "ооо экспобанк"]
RatingTableRawTable[, "rating":=str_remove(rating, "^\\s")]
RatingTableRawTable[, "rating":=str_remove(rating, "\\s$")]
RatingsTable <- copy(RatingTableRawTable)
# write.xlsx(RatingsTable, "RatingsTable.xlsx")


RatingTableRawTable[name == "АО ЮниКредит Банк"]
View(RatingTableRawTable$name)
View(AggrTableWithRating$csname)
unique(RatingTableRawTable$name)
unique(AggrTableWithRating$csname)
intersect(RatingTableRawTable$name, AggrTableWithRating$csname)
setdiff(RatingTableRawTable$name, AggrTableWithRating$csname)

AggrTableWithRating[csname == "ПАО Сбербанк", c("rating")]
AggrTableWithRating[rating != "absent", .N]
AggrTableWithRating[csname == "банк втб (пао)", .N]

# View(unique(AggrTableWithRating[rating != "absent", c("csname")]))

AggrTableWithNames <- as.data.table(AggrTableWithNames)
AggrTableWithNames[, "csname":=str_remove(csname, "^\\s")]
AggrTableWithNames[, "csname":=str_remove(csname, "\\s$")]
AggrTableWithNames[, "csname":=str_remove(csname, '"+')]
AggrTableWithNames[, "csname":=str_remove(csname, "'+")]
AggrTableWithNames[, "csname":=str_remove(csname, '«+')]
AggrTableWithNames[, "csname":=str_remove(csname, '»+')]
AggrTableWithNames[, "csname":=str_remove(csname, '"+')]
AggrTableWithNames[, "csname":=str_remove(csname, "'+")]
AggrTableWithNames[, "csname":=str_remove(csname, '«+')]
AggrTableWithNames[, "csname":=str_remove(csname, '»+')]
AggrTableWithNames[, "csname":=str_remove(csname, '"+')]
AggrTableWithNames[, "csname":=str_remove(csname, "'+")]
AggrTableWithNames[, "csname":=str_remove(csname, '«+')]
AggrTableWithNames[, "csname":=str_remove(csname, '»+')]
AggrTableWithNames[, "csname":= tolower(csname)]
AggrTableWithNames[is.na(csname)]
AggrTableWithRating <- merge(AggrTableWithNames, RatingsTable, by.x="csname", by.y="name", all.x=T)

AggrTableWithRating <- as.data.table(AggrTableWithRating)
AggrTableWithRating[csname =="банк втб (пао)", "rating":="AAA(RU)"]
OrderingMatrix <- data.table(rating = c("AAA(RU)", "AA+(RU)", "AA(RU)", "AA-(RU)", "A+(RU)",
                                        "A(RU)", 'A-(RU)', 'BBB+(RU)', 'BBB(RU)','BBB-(RU)',
                                        'BB+(RU)', 'BB(RU)', "BB-(RU)", "B+(RU)", "B(RU)",
                                        "B-(RU)", "No"), order = c(1:17))
AggrTableWithRating<-merge(AggrTableWithRating, OrderingMatrix, by = "rating", all.x=T)
AggrTableWithRating <- as.data.table(AggrTableWithRating)
AggrTableWithRating[is.na(rating),"order":=18]
AggrTableWithRating[is.na(rating),"rating":="absent"]
AggrTableWithRating[rating == "absent","Status":="absent"]
View(AggrTableWithRating)

#add normatives
Norm_table <- read.csv("normative_spread.csv")
Norms_table <- read.csv("normative_support_spread.csv")
AggrTableWithRating <- merge(AggrTableWithRating, Norm_table, all.x = T, by = c("REGN", "Year", "Month"))
AggrTableWithRating <- merge(AggrTableWithRating, Norms_table, all.x = T, by = c("REGN", "Year", "Month"))
write.csv("BanksTableWithNormatives.csv", AggrTableWithRating)

View(AggrTableWithRating)
summary(lm(order~Cash_and_equivalents,data= AggrTableWithRating))
length(AggrTableWithRating[[1]])
sort(AggrTableWithRating[, order], decreasing = T)


TestTrain <- function(dataset){
  number_of_units <- length(dataset[[1]])
  train_length <- round(0.8*number_of_units)
  test_length <- round(0.2*number_of_units)-1
  sample_numbers <- sample(1:number_of_units, number_of_units)
  train_sample_numbers <- sample_numbers[1:train_length]
  test_sample_numbers <- sample_numbers[(train_length+1):number_of_units]
  train_sample <- dataset[train_sample_numbers]
  test_sample <- dataset[test_sample_numbers]
  return(list(train = train_sample,
              test = test_sample))
}

varslist <- colnames(AggrTableWithRating[, -c("rating", "csname", "REGN", "X", "Status", "order", "Year", "Month")])

GenerateFormula <- function(listofvars){
  paste("order~", paste(listofvars, collapse = " + "))
}

library(MASS)
AggrTableWithRating[, "order" := as.factor(order)]
AggrTableWithRating[, ':='(Ctb_net = Credits_to_banks-Credits_to_banks_minus,
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
AggrTableWithRating[, ':='(TotalAssets = Cash_and_equivalents + NOSTRO - NOSTRO_minus + Ctb_net + Securities_net+
                             Ctc_net + Ctp_net + Reserves_net + Other_assets-Other_assets_minus)]
AggrTableWithRating[, ':='(BankCapital = Mincap_net + Ret_net + Retained_earnings_this_year - 
                             Retained_earnings_this_year_minus + Next_periods - Next_periods_minus)]
AggrTableWithRating[, ':='(
                           CS = BankCapital/TotalAssets,
                           TotalDeposits = LORO + Interbank_loans + Corporate_time_deposits +
                             Retail_time_deposits,
                           e_f = (Corporate_time_deposits/BankCapital))]

AggrTableWithRating[, ':='(
  NonF = (TotalDeposits-Ctb_net)/TotalDeposits,
  Pl5 = (Ctb_net-Interbank_loans)/TotalDeposits,
  T_R = (Reserves_for_delayed_loans- Reserves_for_delayed_loans_minus)/BankCapital)]
length(AggrTableWithRating$csname)
length(AggrTableWithRating[Status!="absent"]$rating)

library("rms")
mysmalltable <- AggrTableWithRating[(Year>=2016)&(Month>=4)&(Status!= "absent")]
mysmalltable[, "order" := droplevels(order)]
start <- Sys.time()
smpmod <- orm(order~Cash_and_equivalents + Ctb_net + Securities_net + Ctc_net + 
                 Ctp_net, data = mysmalltable, method= "orm.fit")
smpmod
print(Sys.time() - start)
AggrTableWithRating[(Status!="absent"), .N, by = order]

source(orm)
myvarsdataframe <- copy(AggrTableWithRating[(Year>=2016)&(Month>=4)&(Status!="absent")])
myvarsdataframe <- myvarsdataframe[, .(order, Year, Month, Cash_and_equivalents, Ctb_net, Securities_net, Ctc_net, 
                      Ctp_net, Reserves_net, Mincap_net, Ctb_pnl_net, Ctp_pnl_net,
                      Debt_pnl_net, SecuritiesIssued_net, Fee_net, Rfc, Drv_net,
                      Otherexp_net, Otherinc_net, Interbank_loans,
                      Bonds, Equity, NetIncome, ROE, NetInterbank, InterbankShare,
                      Liabilities, GeneralLeverage, SecuritiesLeverage, TotalAssets,
                      BankCapital, CS, TotalDeposits, e_f, T_R, N1.0, N1.1, N1.2, N2, N3, N4, Ar1.0, Ar1.1, Ar1.2, Ar3.0, Ar2.0, Ar4.0, Arisk0, Kins, Kras, Kf, PR0, PR1, PR2)]
myvarsdataframe <- myvarsdataframe[!is.na(Ar1.0)]
myvarsdataframeorder <- myvarsdataframe$order
myvarsdataframeyear <- myvarsdataframe$Year
myvarsdataframemonth <- myvarsdataframe$month
myvarsdataframe <- (as.data.table(myvarsdataframe))[, lapply(.SD, as.numeric), .SDcols = setdiff(colnames(myvarsdataframe), c("order"))]
myvarsdataframe[, "order" := myvarsdataframeorder]
m <- prcomp(na.omit(myvarsdataframe[, -c("order", "Month", "Year")]), center = T, scale. = T)
summary(m)
PC_loadings <- m$rotation

myvarsdataframe_new <- copy(AggrTableWithRating[(Year>=2016)&(Month>=4)&(Status=="absent")])
myvarsdataframe_new <- myvarsdataframe_new[, .(order, Year, Month, Cash_and_equivalents, Ctb_net, Securities_net, Ctc_net, 
                                               Ctp_net, Reserves_net, Mincap_net, Ctb_pnl_net, Ctp_pnl_net,
                                               Debt_pnl_net, SecuritiesIssued_net, Fee_net, Rfc, Drv_net,
                                               Otherexp_net, Otherinc_net, Interbank_loans,
                                               Bonds, Equity, NetIncome, ROE, NetInterbank, InterbankShare,
                                               Liabilities, GeneralLeverage, SecuritiesLeverage, TotalAssets,
                                               BankCapital, CS, TotalDeposits, e_f, T_R, N1.0, N1.1, N1.2, N2,
                                               N3, N4, Ar1.0, Ar1.1, Ar1.2, Ar3.0, Ar2.0, Ar4.0, Arisk0, Kins, Kras, Kf, PR0, PR1, PR2)]
myvarsdataframeorder <- myvarsdataframe_new$order
myvarsdataframe_new <- (as.data.table(myvarsdataframe_new))[, lapply(.SD, as.numeric), .SDcols = setdiff(colnames(myvarsdataframe_new), "order")]
myvarsdataframe_new[, "order" := NULL]
myvarsdataframe_new <-myvarsdataframe_new[!is.na(ROE)]
myvarsdataframe_new <-myvarsdataframe_new[!is.infinite(ROE)]
myvarsdataframe_new <- myvarsdataframe_new[!is.na(Ar1.0)]

scale(myvarsdataframe_new[, ROE])
mean(myvarsdataframe_new[, ROE], na.rm = T)
myvarsdataframe_new[is.infinite(ROE), ROE]

sclddata <- scale(as.matrix(myvarsdataframe_new[, -c("Year", "Month")]))

PC_new <- sclddata %*% PC_loadings
PC_new <- as.data.table(PC_new)
PC_new[, ":="(Year = myvarsdataframe_new$Year,
              Month = myvarsdataframe_new$Month)]

twenty_PC_new <- PC_new[,c(1:20,52,53)]


PC<-as.data.table(m$x)
PC[, "order" := na.omit(myvarsdataframe)$order]
PC[, "Year" := myvarsdataframe$Year]
PC[, "Month" := myvarsdataframe$Month]
twenty_PC <- PC[,c(1:20, 52,53,54)]
fifteen_PC <- PC[,c(1:15, 52,53,54)]
ten_PC <- PC[,c(1:10, 52,53,54)]
i<-c("PC1", "Month")
m <- 5
i <- c(paste("PC", 1:m, sep = ""), "order", "Year", "Month")
five_PC <- PC[,i, with = F]

ten_PC[, "order" := droplevels(order)]
levels(ten_PC$order)
ten_PC[, mean(PC1), by = order]

VarList <- quote(list(order, Year, Month, Cash_and_equivalents, Ctb_net, Securities_net, Ctc_net, 
                      Ctp_net, Reserves_net, Mincap_net, Ctb_pnl_net, Ctp_pnl_net,
                      Debt_pnl_net, SecuritiesIssued_net, Fee_net, Rfc, Drv_net,
                      Otherexp_net, Otherinc_net, Interbank_loans,
                      Bonds, Equity, NetIncome, ROE, NetInterbank, InterbankShare,
                      Liabilities, GeneralLeverage, SecuritiesLeverage, TotalAssets,
                      BankCapital, CS, TotalDeposits, e_f, T_R, N1.0, N1.1, N1.2, N2, N3,
                      N4, Ar1.0, Ar1.1, Ar1.2, Ar3.0, Ar2.0, Ar4.0, Arisk0, Kins, Kras, Kf, PR0, PR1, PR2))
NAVarList <- quote(Ar1.0)

GeneratePCTable <- function(BanksWithRatingsTable_Train, CreatePredictionSet = F, BanksWithRatingsTable_Predict = NULL, ListOfVariables = VarList){
  
  myvarsdataframe <- copy(BanksWithRatingsTable_Train)
  
  #select needed variables
  myvarsdataframe <- myvarsdataframe[, eval(ListOfVariables)]
  
  #remove NAs from selected columns; unfortunately, there is no algorithm implemented yet which would select these columns automatically
  myvarsdataframe <- myvarsdataframe[!is.na(Ar1.0)]
  
  #separate non-numeric columns
  myvarsdataframeorder <- myvarsdataframe$order
  myvarsdataframeyear <- myvarsdataframe$Year
  myvarsdataframemonth <- myvarsdataframe$month
  
  #convert to numeric all the columns, except for the dependent variable and date
  myvarsdataframe <- (as.data.table(myvarsdataframe))[, lapply(.SD, as.numeric), .SDcols = setdiff(colnames(myvarsdataframe), c("order", "Year", "Month"))]
  myvarsdataframe[, ':=' ("order" = myvarsdataframeorder,
                          "Year" = myvarsdataframeyear,
                          "Month" = myvarsdataframemonth)]
  
  #get a table of principal components for the train set
  PCTable <- prcomp(na.omit(myvarsdataframe[, -c("order", "Month", "Year")]), center = T, scale. = T)
  
  #the following part will be run if you choose to generate also the PC set, for which predictions are expected
  #which is generated by applying the factor loadings obtained for the training set to the values in the new one
  if (CreatePredictionSet == T){
  #get PC loadings, which were generated for the training set
  PC_loadings <- PCTable$rotation
  
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
  PC_new[, ":="(Year = myvarsdataframe_new$Year,
                Month = myvarsdataframe_new$Month)]
  
  #return the results
  return(list("PCTable" = PCTable, "PCTablePredict" = PC_new, "orderPredict" = myvarsdataframe_new_order))
  }
  else {
    return(list("PCTable" = PCTable, "PCTablePredict" = NULL, "orderPredict" = NULL))
    }
}

library(plyr)
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
  ColumnNames <- lapply(ColumnNames,  sym)
  ColumnNames <- quote(list(csname, Year))
  #restrict the PC table to the columns enlisted in the previous step
  five_PC <- PCTable[,ColumnNames, with = F]
  return(five_PC)
}
parse(text = r)
!!c("a")
sym(c("a"))
typeof(parse(text = "a"))
i <- quote(list('a', 'b'))
r <- c('c', 'd')
parse(i,r)

substitute(m, list(m = m))

m <- lapply(c("csname", "Year"),  sym)
m <- substitute(list(m), list(m=syms(c("csname", "Year"))))

AggrTableWithNames[, eval(m)]

#m <- GeneratePCTable(AggrTableWithRating[(Year>=2016)&(Month>=4)&(Status!="absent")])
n <- GenerateRestrictedPCTable(m$PCTable, 5) 

print(as.quoted("l"))
library(rlang)
Quotation()
m$r
for (i in 1:100){
  try({
polr(as.factor(order)~., data = ten_PC[, -c("Year")], Hess = T,method=c("logistic"), start = seq(from = -0.1, to = 0.1, by = 0.2/i))
print(i)})
  
}

for (i in 1:100){
  try({
    polr(as.factor(order)~., data = five_PC[, -c("Year")], Hess = T,method=c("logistic"), start = seq(from = -0.1, to = 0.1, by = 0.2/i))
    print(i)})
  
}

for (i in 1:100){
  try({
    polr(as.factor(order)~., data = twenty_PC[, -c("Month")], Hess = T,method=c("logistic"), start = seq(from = -0.1, to = 0.1, by = 0.2/i))
    print(i)})
  
}

for (i in 1:100){
  try({
    polr(as.factor(order)~., data = fifteen_PC, Hess = T,method=c("logistic"), start = seq(from = -0.1, to = 0.1, by = 0.2/i))
    print(i)})
  
}
start <- Sys.time()
smpmod <- polr(as.factor(order)~., data = ten_PC[, -c("Year")], Hess = T,method=c("logistic"), start = seq(from = -0.1, to = 0.1, by = 0.2/26))
print(Sys.time() - start)
summary(smpmod)

errors <- c()
errors_primary_binary <- c()
errors_plus_binary <- c()
errors_minus_binary <- c()

for (i in 1:20){
  tt <- TestTrain(ten_PC[, -c("Year")])
  test <- tt$test
  train <- tt$train
  model <- polr(as.factor(order)~., data = train, Hess = T,method=c("logistic"), start = seq(from = -0.1, to = 0.1, by = 0.2/26))
  newerror <- mean(abs(as.numeric(test$order) - as.numeric(predict(model, test[, -"order"]))))
  errors <- c(errors, newerror)
  
  newerror_primary_bin <- sum(as.numeric(test$order) == as.numeric(predict(model, test[, -"order"])))
  newerror_plus_bin <- sum(as.numeric(test$order) == as.numeric(predict(model, test[, -"order"]))+1)
  newerror_minus_bin <- sum(as.numeric(test$order) == as.numeric(predict(model, test[, -"order"]))-1)
  
  errors_primary_binary <- c(errors_primary_binary, newerror_primary_bin)
  errors_plus_binary <- c(errors_plus_binary, newerror_plus_bin)
  errors_minus_binary <- c(errors_minus_binary, newerror_minus_bin)
}
mean(errors)


errors <- c()
errors_primary_binary <- c()
errors_plus_binary <- c()
errors_minus_binary <- c()

for (i in 1:20){
  tt <- TestTrain(five_PC[, -c("Year")])
  test <- tt$test
  train <- tt$train
  model <- polr(as.factor(order)~., data = train, Hess = T,method=c("logistic"), start = seq(from = -0.1, to = 0.1, by = 0.2/22))
  newerror <- mean(abs(as.numeric(test$order) - as.numeric(predict(model, test[, -"order"]))))
  errors <- c(errors, newerror)
  
  newerror_primary_bin <- sum(as.numeric(test$order) == as.numeric(predict(model, test[, -"order"])))
  newerror_plus_bin <- sum(as.numeric(test$order) == as.numeric(predict(model, test[, -"order"]))+1)
  newerror_minus_bin <- sum(as.numeric(test$order) == as.numeric(predict(model, test[, -"order"]))-1)
  
  errors_primary_binary <- c(errors_primary_binary, newerror_primary_bin)
  errors_plus_binary <- c(errors_plus_binary, newerror_plus_bin)
  errors_minus_binary <- c(errors_minus_binary, newerror_minus_bin)
}

mean(errors)

errors <- c()
errors_primary_binary <- c()
errors_plus_binary <- c()
errors_minus_binary <- c()
for (i in 1:20){
  tt <- TestTrain(twenty_PC[, -c("Month")])
  test <- tt$test
  train <- tt$train
  model <- polr(as.factor(order)~., data = train, Hess = T,method=c("logistic"), start = seq(from = -0.1, to = 0.1, by = 0.2/37))
  newerror <- mean(abs(as.numeric(test$order) - as.numeric(predict(model, test[, -"order"]))))
  errors <- c(errors, newerror)
  
  newerror_primary_bin <- sum(as.numeric(test$order) == as.numeric(predict(model, test[, -"order"])))/length(as.numeric(test$order))
  newerror_plus_bin <- sum(as.numeric(test$order) == as.numeric(predict(model, test[, -"order"]))+1)/length(as.numeric(test$order))
  newerror_minus_bin <- sum(as.numeric(test$order) == as.numeric(predict(model, test[, -"order"]))-1)/length(as.numeric(test$order))
  
  errors_primary_binary <- c(errors_primary_binary, newerror_primary_bin)
  errors_plus_binary <- c(errors_plus_binary, newerror_plus_bin)
  errors_minus_binary <- c(errors_minus_binary, newerror_minus_bin)
}

mean(errors)
mean(errors_primary_binary)
mean(errors_plus_binary)
mean(errors_minus_binary)


errors <- c()
errors_primary_binary <- c()
errors_plus_binary <- c()
errors_minus_binary <- c()
for (i in 1:20){
  tt <- TestTrain(fifteen_PC)
  test <- tt$test
  train <- tt$train
  model <- polr(as.factor(order)~., data = train, Hess = T,method=c("logistic"), start = seq(from = -0.4, to = 0.4, by = 0.8/28))
  newerror <- mean(abs(as.numeric(test$order) - as.numeric(predict(model, test[, -"order"]))))
  errors <- c(errors, newerror)
  
  newerror_primary_bin <- sum(as.numeric(test$order) == as.numeric(predict(model, test[, -"order"])))/length(as.numeric(test$order))
  newerror_plus_bin <- sum(as.numeric(test$order) == as.numeric(predict(model, test[, -"order"]))+1)/length(as.numeric(test$order))
  newerror_minus_bin <- sum(as.numeric(test$order) == as.numeric(predict(model, test[, -"order"]))-1)/length(as.numeric(test$order))
  
  errors_primary_binary <- c(errors_primary_binary, newerror_primary_bin)
  errors_plus_binary <- c(errors_plus_binary, newerror_plus_bin)
  errors_minus_binary <- c(errors_minus_binary, newerror_minus_bin)
}

mean(errors)
mean(errors_primary_binary)
mean(errors_plus_binary)
mean(errors_minus_binary)

best_model <- polr(as.factor(order)~., data = ten_PC[,-c("Month")], Hess = T,method=c("logistic"), start = seq(from = -0.1, to = 0.1, by = 0.2/23))
predicted_ratings <- predict(best_model, twenty_PC_new)
length(predicted_ratings)
View(predicted_ratings)
data.table("a" = predicted_ratings)[, .N, by = a]
mean(twenty_PC_new[, 1], na.rm = T)
twenty_PC[order == 1,mean(PC1)]
twenty_PC[order == 2,mean(PC1)]
AggrTableWithRating[order == 17, c("order", "rating")]
AggrTableWithRating[(order != 17)&(csname == "АО ЮниКредит Банк"), c("order", "rating")]

