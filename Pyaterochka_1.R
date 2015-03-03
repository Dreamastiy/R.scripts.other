promoMSK <- getPromoBooklet('http://pyaterochka.ru/msk/actions/download/')

getPromoBooklet <- function(strURL){
     require(XML)
     require(jsonlite)
     require(RCurl)
     
     # Reading html 
     html <- getURL(strURL, .encoding='UTF-8')
     
     # Formatting a little bit
     html <- gsub(';','',html)
     html <- gsub('&nbsp',';',html)
     html <- gsub('</div>',' </div>',html)
     html <- gsub('<sup>','.',html)
     html <- gsub('</sup>',';',html)
     
     # Parsing html
     html.raw<-htmlTreeParse(
          html,
          useInternalNodes=T
     )
     
     # Remove all spaces
     trim <- function(x) gsub("\\s+|\\s+$", " ", x)
     
     # Finding all items
     html.parse<-xpathApply(html.raw, path="//div[@class= 'item']", fun=xmlValue)
     
     # A little bit more formatting
     noT <- gsub('\t','',unlist(html.parse))
     #noT <- gsub('\t','',html.parse)
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
     write.table(noT,"D:\\2.csv", row.names=F, quote=F, col.names = F, eol = '\n')
     
     #noT.df <- read.table(textConnection(noT), sep=';',header=F )
     # Writing data to file
     not.df <- as.data.frame(matrix(unlist(not.df), nrow = length(not.df), byrow = T))
     not.df
}