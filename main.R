source("scraper.R")
library(rvest)
library(curl)


getAllRecords<-function(URL,Pages){
  appData <- data.frame()
  for(i in 1:Pages){
    print(paste("Scraping ::",URL,"?page=",i,sep = ""))
    appData<-rbind(appData,scrapePage(paste(URL,"?page=",i,sep = "")))
  }
  return(appData)
}
outData<-getAllRecords("https://bevasarlas.tesco.hu/groceries/hu-HU/shop/pekaru/szendvicsalapok/all",2)

