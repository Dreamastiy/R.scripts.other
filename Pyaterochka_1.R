require(XML)
require(jsonlite)
require(RCurl)

# Reading html 
html <- getURL('http://pyaterochka.ru/msk/actions/download/', .encoding='UTF-8')

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
noT <- gsub('\n',';',noT)
noT <- gsub(';;;;','\n',noT)
noT <- trim(noT)
noT <- gsub('; ; ; ; ([0-9.]+);;',';;;;; \\1;;',noT)

# Writing data to file
write.table(noT,"D:\\2.csv", row.names=F, quote=F)

