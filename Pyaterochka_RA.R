require(XML)
require(jsonlite)
require(RCurl)
source('Pyaterochka_1.R')

# Reading html 
html <- getURL('http://pyaterochka.ru/msk/actions/', .encoding='UTF-8')
fullArray <- data.frame(Sale=character(0), 
                        NpO=character(0),
                        OldPrice=character(0),
                        NewPrice=character(0),
                        ItemName=character(0),
                        Territory=character(0),
                        Oblast=character(0))
# Parsing html
html.raw<-htmlTreeParse(
     html,
     useInternalNodes=T
)

# html.parse<-xpathApply(html.raw, path="//li[@class= 'item']", fun=xmlValue)
html.parse.href <-xpathApply(html.raw, path="//ul//a[contains(@id, 'menu')]", xmlGetAttr, "href")
html.parse.href.actions <- html.parse.href[!html.parse.href=='#']
html.parse.name <-xpathApply(html.raw, path="//ul//a[contains(@id, 'menu')]", xmlValue)
html.parse.name.actions <- html.parse.name[!html.parse.href=='#']
# paste('http://pyaterochka.ru', html.parse.href.actions[[1]],'actions/', sep="")

# n <- length(html.parse.name.actions)
# 
# for(iRow in 1:n){
#      promo <- getPromoBooklet(paste('http://pyaterochka.ru', html.parse.href.actions[[iRow]],'actions/download/', sep=""), html.parse.name.actions[[iRow]])
#      fullArray <- rbind(fullArray, promo)
#      paste(iRow,'of', n)
# }

n1 <- length(html.parse.name)
for(iRow in 1:n1){
     if (html.parse.href[[iRow]]=='#'){
          oblast <- html.parse.name[[iRow]]
     } else {
          # promo <- getPromoBooklet(paste('http://pyaterochka.ru', html.parse.href[[iRow]],'actions/download/', sep=""), html.parse.name[[iRow]], oblast )
          promo <- getPromoBooklet(paste('http://pyaterochka.ru', html.parse.href[[iRow]],'download/', sep=""), html.parse.name[[iRow]], oblast )
          
          fullArray <- rbind(fullArray, promo)
          paste(iRow,'of', n)
     }
}

write.table(fullArray, "D:\\mydata1.txt", sep="\t")
uniq <- as.character(unique(fullArray[,5]))
sapply(uniq,agrep,uniq)
similar <- sapply(uniq,agrep,uniq, max.distance=0.4, value=TRUE) #', costs=c(0,0.5,0.5))
write.table(similar, "D:\\similar.txt", sep="\t")
unlist(similar)

# fullArray
