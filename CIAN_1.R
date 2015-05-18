require(XML)
require(jsonlite)
require(RCurl)
promoMSK <- getPromoBooklet('http://www.cian.ru/sale/flat/12052210/')
strURL <- 'http://www.cian.ru/sale/flat/12052210/'

getPromoBooklet <- function(strURL){
     require(XML)
     require(jsonlite)
     require(RCurl)
     
     # Reading html 
     html <- getURL(strURL, 
                    .encoding='UTF-8')
     
     # Formatting a little bit
#      html <- gsub(';','',html)
#      html <- gsub('&nbsp',';',html)
#      html <- gsub('</div>',' </div>',html)
#      html <- gsub('<sup>','.',html)
#      html <- gsub('</sup>',';',html)
     
     # Parsing html
     html.raw<-htmlTreeParse(
          html,
          useInternalNodes=T
     )
     
     # Remove all spaces
     # trim <- function(x) gsub("\\s+|\\s+$", " ", x)
     
     # Finding all items
     html.parse<-xpathApply(html.raw, 
                            path="//div[@class= 'object_descr_text']", 
                            fun=xmlValue)
     
     # A little bit more formatting
     noT <- gsub('\t','',unlist(html.parse))
     noT <- gsub('\n',';',noT)
     noT <- gsub(';;;;','\n\t',noT)
     noT <- trim(noT)
     noT <- gsub('; ; ; ; ([0-9.]+);;',';;;;; \\1;;',noT)
     noT <- gsub('; ',';',noT)
     noT <- gsub('^;;','',noT)
     noT <- gsub(";;;([а-яА-Я])",'\\1',noT)
     noT <- gsub(";;;([a-zA-Z])",'\\1',noT)
     noT <- gsub("(;;;;)$",'',noT)
     noT <- gsub("^;",'NA;',noT)
     noT <- gsub(";;",';NA;',noT)
     noT <- gsub(";;",';NA;',noT)
     noT <- gsub(";$",';NA',noT)

     #noT <- as.data.frame(noT)
     not.df <- strsplit(noT,';')
     write.table(noT,paste("D:\\2.",terr,".csv",sep=""),  
                 row.names=F, 
                 quote=F, 
                 col.names = F, 
                 eol = '\n')
     
     # Writing data to file
     not.df <- as.data.frame(matrix(unlist(not.df), 
                                    nrow = length(not.df), 
                                    byrow = T))
     not.df$terr = terr
     not.df$oblast = oblast
     not.df
}