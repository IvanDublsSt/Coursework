library(data.table)
library(xml2)
library(stringr)

setwd("C:/Users/Fride/OneDrive/Документы/GitHub/Coursework")

GetNumberOfPages


OnePageProcessing <- function(Page){
OnePageHTML <- read_html("https://www.banki.ru/banks/memory/?PAGEN_1=1")
OnePageItems <- xml_find_all(OnePageHTML, ".//tr[@data-test = 'memory-book-item']")
OnePageOneItem <- OneItemProcessing(OnePageItems[[1]])
OnePageItemsText <- lapply(OnePageItems, OneItemProcessing)
OnePageItemsText <- do.call(rbind, OnePageItemsText)
OnePageItemsText <- as.data.table(OnePageItemsText)
colnames(OnePageItemsText) <- c("BankDefaultIndex", "Name", "regnum", "DefaultType", "DefaultDate", "BankLocalization")
return(OnePageItemsText)
}

OneItemProcessing <- function(Item){
  OnePageOneItemText <- xml_text(Item)
  OnePageOneItemText <- str_split(OnePageOneItemText, "\n")[[1]]
  OnePageOneItemText <- lapply(OnePageOneItemText, str_remove, "\\s+")
  OnePageOneItemText <- unlist(OnePageOneItemText)
  OnePageOneItemText <- OnePageOneItemText[OnePageOneItemText != ""]
  return(OnePageOneItemText)
}
