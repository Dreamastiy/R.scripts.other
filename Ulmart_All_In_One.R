require(XML)
require(jsonlite)
require(RCurl)

onePage <- getBooklet('http://m.ulmart.ru/catalog/93402/')
strCity <- 'http://m.ulmart.ru/selectCityById/18414/'
strURL <- 'http://m.ulmart.ru/catalog/93402'
html <- getURL(strCity, 
               .encoding='UTF-8')

# Remove all spaces subfunction
trim <- function(x) gsub("\\s+|\\s+$", " ", x)     

# getting info from one subpage
getOnePageBooklet <- function(strURLsub="", curl=getCurlHandle()){
     # loading the required page
     html <- getURL(strURLsub, 
                    .encoding='UTF-8', 
                    curl=curl)
     # parsing html
     html.raw <- htmlTreeParse(
          html,
          useInternalNodes=T
     )
     
     # searching for SKU nodes
     html.parse.SKU <- xpathApply(html.raw, 
                                  path="//section[@class='b-product']", 
                                  fun=xmlValue)
     
     # some regex :)
     noT <- gsub(' ([0-9]+)\\s([0-9]+) ',' \\1\\2 ',unlist(html.parse.SKU))
     noT <- gsub(';',',',noT)
     noT <- gsub('\r\n',';',noT)
     noT <- trim(noT)
     noT <- gsub("(\\s;)+", " ", noT)
     noT <- gsub("^;\\s ", "", noT)
     noT <- gsub(";\\s+([0-9]+)\\s+;", "\\1", noT)
     noT <- gsub(" ; ", "", noT)
     noT <- gsub("Артикул ", "", noT)
     noT <- gsub("\\s+Купить;\\s*", "", noT)
     noT <- gsub("\\s+руб.;\\s*", "", noT)
     noT <- gsub(";\\s+", ";", noT)
#  noT   
     # text to list
     not.df <- strsplit(noT,';')
 #    not.df
     # list to nice df
     tryCatch(
     not.df <- as.data.frame(matrix(unlist(not.df), 
                                     nrow = length(not.df), 
                                     byrow = T)), error=function(e) {print(strURLsub)}  )

}


getOneBooklet <- function(strURLmain="", curl=getCurlHandle()){
     # data frane for the result
     df <- data.frame(inStock=character(), SKU=character(), Article=numeric(), Comment=numeric(), Price=numeric()) 
     
     # loading main subpage
     html <- getURL(strURLmain, 
                    .encoding='UTF-8', 
                    curl=curl)
     # parsing main subpage
     html.raw <- htmlTreeParse(
          html,
          useInternalNodes=T
     )
     # finding last subpage
     html.parse.pages <- xpathApply(html.raw, 
                                    path="//a[@class='page g-nouline']", 
                                    fun=xmlValue)
     if(length(html.parse.pages)==0){
          urlMax <- 1
     }else{
          urlMax <- as.numeric(unlist(html.parse.pages)[length(unlist(html.parse.pages))])          
     }

     # loop for all sybpages
     tryCatch(
     for(iPage in 1:urlMax){
          strToB <- paste0(strURLmain, '?pageNum=',iPage)
          df.inter <- getOnePageBooklet(strToB, curl)
          df <- rbind(df, df.inter)
     }, error=function(e) {print(iPage)})
     # write.table(df, paste0('D:\\', as.numeric(Sys.time()) ,'.csv'), sep=";")     
     df

     
}

getAllLinks <- function(strURLlevel, curl=getCurlHandle()){
     
     # loading main level page
     html <- getURL(strURLlevel, 
                    .encoding='UTF-8', 
                    curl=curl)
     # parsing main subpage
     html.raw <- htmlTreeParse(
          html,
          useInternalNodes=T
     )
     # getting subpages
     
     html.parse.names <- xpathSApply(html.raw, 
                                    path="//ul[@class='b-catalog-nav__list']/li/a", 
                                    fun=xmlValue)
     html.parse.names <- gsub('\r\n\\s+','', html.parse.names)
     
     html.parse.urls <- xpathSApply(html.raw, 
                                    path="//ul[@class='b-catalog-nav__list']/li/a", 
                                    xmlGetAttr, 'href')
     
     cbind(html.parse.names, html.parse.urls)     
}

getAllLinks('http://m.ulmart.ru', curl=curl)
getOneBooklet('http://m.ulmart.ru/catalog/roof_rack', curl=curl)
getOnePageBooklet('http://m.ulmart.ru/catalog/93402', curl=curl)


getOneCity <- function(urlMain = "http://m.ulmart.ru", curl = getCurlHandle()){
     df.prices <- data.frame(inStock = character(), 
                             SKU = character(), 
                             Article = numeric(), 
                             Comment = numeric(), 
                             Price = numeric(), 
                             level1 = character(),
                             level2 = character())
     level1 <- getAllLinks(urlMain, curl)
     numLevel1 <- length(level1[,2])
     
     for (iLevel1 in 1:numLevel1){
          strURLsubmain <- paste0(urlMain, level1[iLevel1, 2])
          level2 <- getAllLinks(strURLsubmain, curl)
          numLevel2 <- length(level2[,2])
          
          for (iLevel2 in 1:numLevel2){
               strURLsku <- paste0(urlMain, level2[iLevel2,2])
               df.temp <- getOneBooklet(strURLsku, curl)
               df.temp$level1 <- level1[iLevel1,1]
               df.temp$level2 <- level2[iLevel2,1]
               
               df.prices <- rbind(df.prices, df.temp)
          }
          write.table(df.prices, paste0('D:\\', iLevel1 ,'.csv'), sep=";", quote = FALSE)          
     }
     df.prices
}

tempResult <- getOneCity(urlMain = "http://m.ulmart.ru", curl = curl) 


strURLlevel <- 'http://m.ulmart.ru/catalog/92993'

# create curl with cookies saving
#RCurl parameters     
agent    ="Mozilla/5.0"
curl = getCurlHandle()
curlSetOpt(cookiejar="cookies.txt",  
           useragent = agent, 
           followlocation = TRUE, 
           curl=curl)

getProxyAddress <- function(){
     htmlProxies <- getURL('http://www.google-proxy.net/', 
                           .encoding='UTF-8')
     #htmlProxies <- gsub('</td></tr>','  \n ', htmlProxies)
     htmlProxies <- gsub('\n','', htmlProxies)
     htmlProxies <- gsub('(</td><td>)|(</td></tr>)',' ; ', htmlProxies)
     # parsing main subpage
     htmlProxies.raw <- htmlTreeParse(
          htmlProxies,
          useInternalNodes=T
     )

     # finding last subpage
     html.parse.proxies <- xpathApply(htmlProxies.raw, 
                                    path="//tbody", 
                                    fun=xmlValue)
     html.parse.proxies<- gsub('( )+','', html.parse.proxies)
     final <- unlist(strsplit(as.character(html.parse.proxies),';'))
     final <- as.data.frame(matrix(final[1:800], 
                                    nrow = length(final)/8, 
                                    ncol = 8,
                                    byrow=T))
     #final <- gsub('( )+','', final)
     names(final) <- c('IP','Port','Code','Country','Proxy type','Google','Https','Last checked')
     sapply(final, as.character)
}

x <- getProxyAddress()

setProxyParameters <- function(x, nRow = 1){
     proxyIP <- as.character(x[nRow, 1])
     proxyPORT <- x[nRow, 2]
     df <- data.frame(IP = character(), PORT = numeric())
     df <- c(proxyIP, proxyPORT)
     #df$proxyPORT <- proxyPORT
     result <- as.list(df)
     result
}
opts <- list(
     proxy         = "", 
     proxyport     = ""
)
getURL("http://stackoverflow.com", .opts = opts)


# Choosing city and save to curl cookiees
html <- getURL(strCity, 
               .encoding='UTF-8', 
               curl=curl)     

pat.food <- getOneBooklet('http://m.ulmart.ru/catalog/93402', curl)
head(pat.food)
tail(pat.food)

