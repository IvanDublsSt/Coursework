# Coursework
This is a base of code which was produced during my work on construction of ML model for Russian banks' bond yields.
The code files are on different positions in a scale from "Insane mindflow" to "I tried to make it as readable as possible".
Putting it in a nutshell, the functionality of the files is the following:
********************************************************************
Working with banks:

1) BankiRuDefaultsParcer - download the data on bank defaults from banki.ru (https://www.banki.ru/banks/memory/), prepares 
a table with this data, saves it. Pretty clean.

2) CBR_gather - gathers data from forms 101, 102 and 135 (balance sheet, profit and loss statement, normatives and supplements)
from DBF files, provided by Bank of Russia (currently there is no parcer, so you have to download them manually, it does not take long),
groups them to one table, adds the data on defaults from BankiRuDefaultsParcer.
Also includes aggregation from raw extremely detalized accounts to some meaningful accounts of higher orders, but currently the
aggregation is very rough. Pretty clean.

3) Process_CBR_Data - script for processing the data generated in CBR_gather (graphs, statistics). Not cleaned and chaotic. Currently,
contains almost no useful processes not duplicated by other scripts. 

4) RatingsPrediction - the MVP of an algorithm aimed at predicting the rating by AKRA (https://www.acra-ratings.ru/ratings/issuers/)
from aggregated financial accounts of banks using SVM. Pretty clean. Shows terrible accuracy, either due to roughness of aggregation, or
to some procedural mistakes
********************************************************************
Working with bonds:

1) Data_gather_bonds - takes a table with data on bonds (gathered by the script stored in https://github.com/IvanDublsSt/cbondsdw), 
preprocesses it and prepares some descriptive statistics

2) Data_bonds_separate - separates the preprocessed table on bonds into subsamples according to a combination of categorization 
principles. 

3) FinalClean - uses one of the samples to prepare an SVM model, which predicts yield of the bonds, validates it, and makes predictions 
on private banks sample

4) Finals2 - an unstructured code, which contains multiple processes of reshaping, testing and exloring the data on bonds, includng those
which are duplicated by FinalClean. Yet to be cleaned and made readable. 
********************************************************************
Detailed guide on BankiRuDefaultsParcer:

Technical requirements: packages (data.table, xml2, xlsx, stringr). Working directory is set at line 5. 
********************************************************************
Contains one user function: 

AllPagesProcessing(SavingRequired = F, SavingPath = NULL):

SavingRequired: set True if the prepared table is to be saved

SavingPath: set a string, containing path to the newly created file, if the table is to be saved (including name of the file, i.e. "Data/BankDefaults.csv"), 
taking into account that currently we are operating at working directory

********************************************************************
Detailed guide on CBR_gather:
Technical requirements: packages(data.table, foreign, openxlsx, plyr, tidyr, stringr, stringi)
To work with the file, working directory must contain a folder called "Data", which must contain separate folders, created from archives with reporting from 
CBR web-site. The scripts is applicable to at least files from 2011 to 2021. Path to the working directory is to be provided here:

setwd("C:/Personal/VariousData/CBRData") (line 12)
********************************************************************
The main functionality is in the form of the following functions: 
********************************************************************
create_pnl_table(beginning_year = 2015, oldyear = 2015, oldmonth = 12, save = F): 

beginning_year - the first year in the sample (last year is 2020)

oldyear - the year in which the format of reporting is changed (PnL format changed radically in 2016, with old accounts numeration not being compatible with the new)

oldmonth - the month in which the format of reporting is changed

save - whether saving is required; if it is, fill this argument with the path to the file, including name. Saving is performed in CSV.

Outputs a table with accounts from PnL statement (form 102). One column contains data on one account, except for columns "REGN" (containing bank registration number),
"Year", "Month" and "NAs_pnl", containing the share of accounts not provided by the bank.
**********************************************************************
create_bs_table(beginning_year = 2015, save = F):

beginning_year - the last year in the sample (last year is 2020)

save - whether saving is required; if it is, fill this argument with the path to the file, including name. Saving is performed in CSV.

Outputs a table with accounts from Balance Sheet (form 101). One column contains data on one account, except for columns "REGN" (containing bank registration number),
"Year", "Month" and "NAs_share", containing the share of accounts not provided by the bank.
**********************************************************************
create_big_table(spread_pnl_table,spread_table, save = F):

Takes as inputs PnL and BS, created by two functions above, and outputs a merged table. Since BS is updated monthly and PnL - quarterly, PnL data is considered unchanged
for three months in a row. 

**********************************************************************
create_normatives_table(beginning_year = 2015, save = F):

beginning_year - the last year in the sample (last year is 2020)

save - whether saving is required; if it is, fill this argument with the path to the file, including name. Saving is performed in CSV.

Outputs a table with required normatives, calculated for the bank (form 135). One column contains data on one account, except for columns "REGN" (containing bank registration number),
"Year", "Month".
**********************************************************************
create_normative_supports_table(beginning_year = 2015, save = F):

beginning_year - the last year in the sample (last year is 2020)

save - whether saving is required; if it is, fill this argument with the path to the file, including name. Saving is performed in CSV.

Outputs a table with support values for required normatives (volume of liquid assets, etc.), calculated for the bank (form 135). One column contains data on one account, 
except for columns "REGN" (containing bank registration number), "Year", "Month".

**********************************************************************
The script also contains functionality not yet wrapped in functions, in lines 317-343. These lines merge big table from create_big_table with normatives data, adds default data created by
BankiRuDefaultsParcer and outputs the final non-aggregated table. The resulting table can be found at: https://eduhseru-my.sharepoint.com/:f:/g/personal/isdublenskiy_edu_hse_ru/Eva5EtNYjEZNlRiASgGsQfwB0euKR5qiQZoB1LzzRzZyNQ?e=6dN1tV
Columns with names that represent a set of numbers - accounts from the balance sheet, a set of numbers_pnl - from the PnL, a set of numbers_pnlold - PnL before a significant change in the format in 2016, the name of the normative 
(or its composite, for example, liquid assets) - an account from form 135.

The file also contains data on defaults: 
year and month of default, if any; 
binary variable AlreadyDefaulted equal to 1 in the month when the license was revoked (and in months after it, if the bank continued to submit reports);
WillDefault, equal to 1 for banks whose license was revoked during the observation period; 
VoluntaryWithdrawal, equal to 1 if the bank was liquidated during the observation period.
