library(data.table)
library(xml2)
library(stringr)

setwd("C:/Users/Fride/OneDrive/Документы/GitHub/Coursework")

GetNumberOfPages
OnePageHTML <- read_html("https://www.banki.ru/banks/memory/?PAGEN_1=1")
OnePageHeader <- xml_find_all(OnePageHTML, ".//div[@class = 'margin-bottom-default']")[[1]]
OnePageNumbersAttribute <- xml_attr(OnePageHeader, "data-options")
OnePageNumbersAttributeSplit <- str_split(OnePageNumbersAttribute, "\n")[[1]]
OnePageItemsPerPage <- OnePageNumbersAttributeSplit[str_detect(OnePageNumbersAttributeSplit, "itemsPerPage")]
OnePageItemsPerPage <- as.numeric(str_extract(OnePageItemsPerPage, "\\d+"))
OnePageTotalItems <- OnePageNumbersAttributeSplit[str_detect(OnePageNumbersAttributeSplit, "itemsPerPage")]
OnePageTotalItems <- as.numeric(str_extract(OnePageTotalItems, "\\d+"))

OnePageProcessing <- function(URL){
  OnePageHTML <- read_html(URL)
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
