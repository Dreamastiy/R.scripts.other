#strURL <- 'http://www.cian.ru/cat.php?deal_type=2&obl_id=1&city[0]=1&minprice=2000000&p=1'
final.result <-  data.frame(adress = character(), 
                            coment = character(),
                            price = character(),
                            info = character(),
                            coord = character(),
                            metro = character())
iC <- 0
for (iPage in 1:4000){
     strURL <- paste0('http://www.cian.ru/cat.php?deal_type=2&obl_id=1&city[0]=1&minprice=2000000&p=',iPage)
     flats <- getPages(strURL)
     for (iFlats in 1:length(flats)){
          #iFlats <- 1
          strURL.flats <- paste0('http://www.cian.ru', flats[iFlats])
          flat.result <- getApatartPage(strURL.flats)
          final.result <- rbind(final.result, flat.result)
          iC <- iC + 1
          if( (iC - (iC %/% 100) * 100) == 0 ){
               write.table(final.result, paste0('D:\\CIAN_',iC,'.csv'), sep='\t', dec=',' )
          }
     }
}



getApatartPage<- function(strURL){
     # Connecting libraries
#      require(XML)
#      require(jsonlite)
#      require(RCurl)
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
                             path="//h1[@class='object_descr_addr']", 
                             fun=xmlValue)
     noT <- gsub('\n\n.+','',unlist(html.adress))
     adress <- trim(noT)
     
     # Getting apartment's comment
     html.comment<-xpathApply(html.raw, 
                              path="//div[@class= 'object_descr_text']", 
                              fun=xmlValue)
     noT <- gsub('\n\n.+','',unlist(html.comment))
     comment <- trim(noT)
     
     # Getting apartment's price
     html.price<-xpathApply(html.raw, 
                            path="//div[@class= 'object_descr_price']", 
                            fun=xmlValue)
     noT <- gsub('\n\n\n\n.+','',unlist(html.price))
     price <- trim(noT)
     
     # Getting apartment's parameters in one cell
     html.info<-xpathApply(html.raw, 
                           path="//table[@class= 'object_descr_props']", 
                           fun=xmlValue)
     noT <- gsub('\n\n\n\n.+','',unlist(html.info))
     info <- trim(noT)
     
     # Getting apartment's coords
     html.coord<-xpathApply(html.raw, 
                            path="//td[@class= 'object_descr_td_r']", 
                            fun=xmlValue)
     noT <- gsub('\n\n\n\n.+','',unlist(html.coord))
     coord <- trim(noT)
     coord <- gsub(pattern = "(.*center: \\[)(.*)(\\], zoom.*)",
                   replacement = "\\2",
                   x = coord)
     
     # Getting apartment's metro station
     html.metro<-xpathApply(html.raw, 
                            path="//div[@class= 'object_descr_metro']", 
                            fun=xmlValue)
     noT <- gsub('\n\n\n\n.+','',unlist(html.metro))
     metro <- trim(noT)
     
     result <- rbind(result,cbind(adress, comment, price, info, coord, metro))
     return(result)
}

#a href="/sale/flat/23081766/"
getPages<- function(strURL){
     # Loading page
     html <- getURL(strURL, .encoding='UTF-8')
     
     # Transforming page to "readable" from
     html.raw<-htmlTreeParse(
          html,
          useInternalNodes=T ,encoding="UTF-8"
     )
     
     # Getting apartment's adress
     html.adress<-xpathApply(html.raw, 
                             path="//a[contains(@href,'sale/flat/')]", 
                             fun=xmlGetAttr, "href")
     noT <- unlist(html.adress)
     return(noT[-1])
}