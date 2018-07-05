library(rvest)
library(curl)

scrapePage<-function (url){


  statProduct = "https://bevasarlas.tesco.hu"
  
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  
  content <- read_html("scrapedpage.html",encoding = "utf8")
  
  productNames<-content %>%
    html_nodes(".product-tile--title") %>%
    html_text()
  
  price<-content%>%
    html_nodes(".price-per-sellable-unit") %>%
    html_text()
  
  priceQuant<-content%>%
    html_nodes(".price-per-quantity-weight") %>%
    html_text()
  
  productURL<-content%>%
    html_nodes(".product-tile--title") %>%
    html_attr("href")
  
  productURL<-paste(statProduct,productURL,sep = "")
  
  vec<-c()
  vec1<-c()
  for(i in productURL){
    download.file(i, destfile = "temp_files/tempPage.html", quiet=TRUE)
    tempContent <-read_html("temp_files/tempPage.html",encoding = "utf8")
    
    
    imgURL<-tempContent%>%
      html_nodes(".product-image") %>%
      html_attr("src")
    
    productDesc<-tempContent %>%
      html_node(".memo") %>%
      html_children()
      
      vec1<-c(vec1,imgURL)
      vec<-c(vec,paste(productDesc,collapse = " "))
  }
  
  productInfo<-vec
  imageURL <-vec1
  outDataframe <- cbind(productNames,price,priceQuant,imageURL,productURL,productInfo)
  return(outDataframe)
  #write.csv(outDataframe,page)

}