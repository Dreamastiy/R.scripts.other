require(XML)
require(jsonlite)
require(RCurl)
promoMSK <- getPromoBooklet('http://www.cian.ru/sale/flat/12052210/')
strURL <- 'http://www.cian.ru/sale/flat/12052210/'

getPromoBooklet <- function(strURL){
     require(XML)
     require(jsonlite)
     require(RCurl)
     html <- getURL(strURL)#, 
                    .encoding='UTF-8')
     
     html.raw<-htmlTreeParse(
          html,
          useInternalNodes=T ,encoding="UTF-8"
     )
     result <- data.frame(adress = character(), 
                          coment = character(),
                          price = character(),
                          info = character(),
                          coord = character(),
                          metro = character())
     # Remove all spaces
     trim <- function(x) gsub("\\s+|\\s+$", " ", x)
     
     # Finding all items
     html.adress<-xpathApply(html.raw, 
                            path="//h1[@class='object_descr_addr']", 
                            fun=xmlValue)
     noT <- gsub('\n\n.+','',unlist(html.adress))
     adress <- trim(noT)
     
     html.comment<-xpathApply(html.raw, 
                              path="//div[@class= 'object_descr_text']", 
                              fun=xmlValue)
     noT <- gsub('\n\n.+','',unlist(html.comment))
     comment <- trim(noT)
     
     html.price<-xpathApply(html.raw, 
                              path="//div[@class= 'object_descr_price']", 
                              fun=xmlValue)
     noT <- gsub('\n\n\n\n.+','',unlist(html.price))
     price <- trim(noT)
     
     html.info<-xpathApply(html.raw, 
                            path="//table[@class= 'object_descr_props']", 
                            fun=xmlValue)
     noT <- gsub('\n\n\n\n.+','',unlist(html.info))
     info <- trim(noT)
     
     html.coord<-xpathApply(html.raw, 
                           path="//td[@class= 'object_descr_td_r']", 
                           fun=xmlValue)
     noT <- gsub('\n\n\n\n.+','',unlist(html.coord))
     coord <- trim(noT)
     coord <- gsub(pattern = "(.*center: \\[)(.*)(\\], zoom.*)",
          replacement = "\\2",
          x = coord)
     
     html.metro<-xpathApply(html.raw, 
                            path="//div[@class= 'object_descr_metro']", 
                            fun=xmlValue)
     noT <- gsub('\n\n\n\n.+','',unlist(html.metro))
     metro <- trim(noT)
     
     result <- rbind(result,cbind(adress, comment, price, info, coord, metro))
     result
     
     
}