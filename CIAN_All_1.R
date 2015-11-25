require(XML)
require(jsonlite)
require(RCurl)
strURL <- 'http://www.cian.ru/cat.php?deal_type=2&obl_id=1&city[0]=1&minprice=2000000&p=1'

result <- getApatartPage(strURL)
#a href="/sale/flat/23081766/"
getPages<- function(strURL){
     # Connecting libraries
     require(XML)
     require(jsonlite)
     require(RCurl)
     # Loading page
     html <- getURL(strURL, .encoding='UTF-8')
     
     # Transforming page to "readable" from
     html.raw<-htmlTreeParse(
          html,
          useInternalNodes=T ,encoding="UTF-8"
     )
     
     # Declaring result array
     result <- data.frame(adress = character(), 
                          coment = character(),
                          price = character(),
                          info = character(),
                          coord = character(),
                          metro = character())
     
     # Remove all spaces function
     trim <- function(x) gsub("\\s+|\\s+$", " ", x)
     
     # Getting apartment's adress
     html.adress<-xpathApply(html.raw, 
                             path="//a[contains(@href,'sale/flat/')]", 
                             fun=xmlGetAttr, "href")
     noT <- unlist(html.adress)
     return(noT[-1])
}