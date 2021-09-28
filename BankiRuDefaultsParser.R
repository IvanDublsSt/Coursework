library(data.table)
library(xml2)
library(stringr)

setwd("C:/Users/Fride/OneDrive/Документы/GitHub/Coursework")

GetNumberOfPages <- function(URL){
  OnePageHTML <- read_html("https://www.banki.ru/banks/memory/?PAGEN_1=1")
  Header <- xml_find_all(OnePageHTML, ".//div[@class = 'margin-bottom-default']")[[1]]
  NumbersAttribute <- xml_attr(Header, "data-options")
  NumbersAttributeSplit <- str_split(NumbersAttribute, "\n")[[1]]
  ItemsPerPage <- NumbersAttributeSplit[str_detect(NumbersAttributeSplit, "itemsPerPage")]
  ItemsPerPage <- as.numeric(str_extract(ItemsPerPage, "\\d+"))
  TotalItems <- NumbersAttributeSplit[str_detect(NumbersAttributeSplit, "totalItems")]
  TotalItems <- as.numeric(str_extract(TotalItems, "\\d+"))
  NumberOfPages <- round(TotalItems/ItemsPerPage)
  return(NumberOfPages)
}

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


AllPagesItems <- list()
for (i in 1:GetNumberOfPages("https://www.banki.ru/banks/memory/?PAGEN_1=1")){
  OnePageURL <- paste("https://www.banki.ru/banks/memory/?PAGEN_1=", as.character(i), sep = "")
  AllPagesItems[[i]] <- OnePageProcessing(OnePageURL)
  
  if (i %% 10 == 0){
    print(paste("Processed page #", as.character(i), sep = ""))
  }
}
AllPagesItems <- do.call(rbind, AllPagesItems)
AllPagesItems["DefaultIndex" == "BankLocalization", "BankLocalization" := "Unknown"]

if (SavingRequired == T){
  write.x
}