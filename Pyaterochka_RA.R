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
                        Territory=character(0))
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

n <- length(html.parse.name.actions)

for(iRow in 1:n){
     promo <- getPromoBooklet(paste('http://pyaterochka.ru', html.parse.href.actions[[iRow]],'actions/download/', sep=""), html.parse.name.actions[[iRow]])
     fullArray <- rbind(fullArray, promo)
     paste(iRow,'of', n)
}

# fullArray
