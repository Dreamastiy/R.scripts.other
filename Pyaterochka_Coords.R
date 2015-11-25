     require(XML)
     require(jsonlite)
     require(RCurl)
     


#      subresult <- data.frame(adress = character(), 
#                           time = character(),
#                           phone = character(),
#                           region = character(),
#                           city = character())     
     # Reading html 
     html <- getURL("http://pyaterochka.ru/msk/shops/", 
                    .encoding='UTF-8')

     # Parsing html
     html.raw<-htmlTreeParse(
          html,
          useInternalNodes=T
     )
     
     # Remove all spaces function
     trim <- function(x) gsub("\\s+|\\s+$", " ", x)
     
     # finding coords part
     html.parse<-xpathApply(html.raw, 
                            path="//script[@type= 'text/javascript']", 
                            fun=xmlValue)[[8]]
     noT <- gsub('.*map.shops = \\{(.*)\\}.*','\\1',unlist(html.parse))
     noT <- gsub('],',';',noT)
     noT <- gsub(':\\[',',',noT)
     noT <- gsub(']','',noT)
     noT <- gsub(',[0-9];',';',noT)
     noT <- gsub(',[0-9]$','',noT)
     noT <- gsub(';',',',noT)
     noT <- as.numeric(unlist(strsplit(noT , "\\," )))
     noT <- as.data.frame(matrix(noT, 
                                   nrow = length(noT)/3, 
                                   ncol = 3,
                                   byrow=T))     
     noT <- noT[order(noT[,1]),]
     coords.gps <- noT
     
     # getting cities list
     html <- getURL('http://pyaterochka.ru/msk/', .encoding='UTF-8')
     html.raw<-htmlTreeParse(
          html,
          useInternalNodes=T
     )
     html.parse.href <-xpathApply(html.raw, path="//ul//a[contains(@id, 'menu')]", xmlGetAttr, "href")
     html.parse.href.actions <- html.parse.href[!html.parse.href=='#']
     N <- length(html.parse.href.actions)
     # generating links and parsing all of them     
     
     result <- data.frame(adress = character(), 
                          time = character(),
                          phone = character(),
                          region = character(),
                          city = character())
     
     for(iCity in 1:N){
          #http://pyaterochka.ru/bogorodskoe/shops/list/
          html <- getURL(paste0('http://pyaterochka.ru', html.parse.href.actions[[iCity]],'../shops/list/'), .encoding='UTF-8')
          #html <- getURL(paste0('http://pyaterochka.ru/bogorodskoe/shops/list/'), .encoding='UTF-8')
          html.raw<-htmlTreeParse(
               html,
               useInternalNodes=T
          )     
     
          # getting region and city name
          html.parse.region<-xpathApply(html.raw, 
                                      path="//li[@class = 'active']/a[contains(@id,'menu')]",   #/a[@href= '#']", 
                                      fun=xmlValue)
          region.name <- unlist(html.parse.region)
          
          # getting stores list
          html.parse.adress<-xpathApply(html.raw, 
                                 path="//ul[@class='shoplist']//li[a/@href = '#']",   #/a[@href= '#']", 
                                 fun=xmlValue)
          noT <- gsub('\t','',html.parse.adress)
          noT <- gsub('\n\n','\n',noT)
          noT <- gsub('^\n','',noT)
          noT <- unlist(strsplit(noT, '\n'))
          if(!is.null(noT)){
               noT <- as.data.frame(matrix(noT, 
                                           nrow = length(noT)/3, 
                                           ncol = 3,
                                           byrow=T))  
               stores.adresses <- noT
               subresult <- data.frame(stores.adresses)
               subresult$region <- region.name[1]
               subresult$city <- region.name[2]
               
               result <- rbind(result, subresult)
               print(subresult$city)
          }

     }
     final.result <- result
     final.result$V3 <- gsub(';',',',final.result$V3)
     final.result$V1 <- gsub(';',',',final.result$V1)
     
     colnames(final.result) <- c('Адрес','Режим работы','Телефон','Территория','Город')
     # writing to disk
     write.table(final.result,paste("D:\\5KA-",Sys.Date(),".csv",sep=""),  
                 row.names=F, 
                 quote=F, 
                 
                 eol = '\n',
                 sep=';',
                 dec=',')
     write.table(coords.gps,paste("D:\\5KA-coords-",Sys.Date(),".csv",sep=""),  
                 row.names=F, 
                 quote=F, 
                 col.names = F, 
                 eol = '\n',
                 sep=';',
                 dec=',')
     
