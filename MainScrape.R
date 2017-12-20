getGoogleURL <- function(search.term, dateRef, daysBack,num=100, domain = '.com', quotes=TRUE) 
{
  
  dateEnd<- format(as.Date(dateRef), format="%m/%d/%Y")
  dateStart<- format(as.Date(dateRef)-daysBack, format="%m/%d/%Y")
  
  search.term <- gsub(' ', '%20', search.term)
  if(quotes) search.term <- paste('%22', search.term, '%22', sep='') 
  getGoogleURL <- paste('http://www.google', domain, '/search?q=',
                        search.term,
'&num=', num, '&tbs=cdr:1,cd_min:',dateStart,',cd_max:',dateEnd,',sbd:1', 
'&tbm=nws&source=lnt'
, sep='')
}
getTwitterURL <- function(search.term, dateRef, daysBack, domain = '.com', quotes=TRUE) 
{
  
  dateEnd<- format(as.Date(dateRef), format="%Y-%m-%d")
  dateStart<- format(as.Date(dateRef)-daysBack, format="%Y-%m-%d")
  
  search.term <- gsub(' ', '%20', search.term)
  if(quotes) search.term <- paste('%22', search.term, '%22', sep='') 
  getTwitterURL <- paste('https://twitter', domain, '/search?f=tweets&q=',
                         search.term,
                         'since%3A',dateStart,'%20%20until%3A',dateEnd, 
                         '&src=typd'
                         , sep='')
}


library(httr)
library('rvest')
library(RSentiment)
getSentimentForData <- function(dataIn, prefix)
{
  sentimentCollection <- calculate_score(dataIn)
  remove99<-c(99)
  sentimentCollectionClean<-sentimentCollection [! sentimentCollection %in% remove99]
  summaryData <-summary(sentimentCollectionClean)
  #outputData <- summaryData[1]  
  #prefix <- paste("_", prefix, sep='')
  labels<-c("Min", "FirstQ", "Med","Mean","ThirdQ","Max")
  #Prefix the labels
  labels <- paste(prefix, labels, sep='_')
  names(summaryData) <- labels
  getSentimentForData <- summaryData
}


getDataFromUrl <- function(url, selector)
{
  response <- GET(url, add_headers('user-agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.84 Safari/537.36'))
  res <- content(response)
  #print(res)
  
  #Using CSS selectors to scrape the descrip
  descrip_data_html <- html_nodes(res, selector)
  #Converting the descrip data to text
  getDataFromUrl <- html_text(descrip_data_html)
}

getSentimentSummary <- function(search.term, dateRef, daysBack)
{

  #Specifying the url for desired website to be scrapped

  #getSentimentSummary <- url
  #Reading the HTML code from the website
  #webpage <- read_html(url)

  #Using CSS selectors to scrape the titles
  #title_data_html <- html_nodes(webpage,'.l _PMs')
  #Converting the titles data to text
  #title_data <- html_text(title_data_html)
  #Call GET and add a useragent to ensure that the tbs is used.
  #response <- GET(url, add_headers('user-agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.84 Safari/537.36'))
  #res <- content(response)
  #print(res)
  
  #Using CSS selectors to scrape the descrip
  #descrip_data_html <- html_nodes(res,'.st')
  #Converting the descrip data to text
  #descrip_data <- html_text(descrip_data_html)
  #print(descrip_data)
  # labels(summaryValues) <- paste("NEWS", labels(summaryValues), sep = "_")
  #labels(summaryValues)<-labels(summaryValues)+"NEWS"
  
  url <- getGoogleURL(search.term = search.term, dateRef = dateRef, daysBack = daysBack)
  descrip_data <- getDataFromUrl(url, '.st')
  googleData<-getSentimentForData(descrip_data, "GNEWS")
  
  url <- getTwitterURL(search.term = search.term, dateRef = dateRef, daysBack = daysBack)
  descrip_data <- getDataFromUrl(url, '.tweet-text')
  twitterData<-getSentimentForData(descrip_data, "TWITTER")
  joineddata <- c(googleData, twitterData)
  getSentimentSummary <- joineddata
}
library(RJSONIO)
library(Quandl)
getBCPricesURL <- function(dateStart, daysBack)
{
  baseUrl<-"https://api.coindesk.com/v1/bpi/historical/close.json"
  
  
  getBCPricesURL<-paste(baseUrl, "?index=USD&", "start=",format(as.Date(dateStart)-daysBack, "%Y-%m-%d"),"&end=",format(as.Date(dateStart), "%Y-%m-%d"), sep='')
}
getBCPrices<-function(dateStart, daysBack, daysLag)
{
  #Build in x days in future for prices for prediction
  url<-getBCPricesURL(dateStart=as.Date(dateStart)+daysLag, daysBack=daysBack)
  
  #print("pricesUrl: ")
  #print(url)
  BCPriceJSON<-fromJSON(url)
  BCPrice<-BCPriceJSON$bpi
  names(BCPrice)<-seq(as.Date(dateStart)-daysBack, as.Date(dateStart), by="days")
  getBCPrices<-BCPrice
}
getCommodityPrice<-function(commodity, dateStart, daysBack)
{
  #"OPEC/ORB"
  #"LBMA/GOLD"
  #"EIA/AEO_2016_REF_NO_CPP_PRCE_NA_COMM_NA_ELC_NA_WENWPP_Y13CNTPKWH_A"
  #"BITCOINWATCH/MINING"
  outputData <- Quandl(commodity, start_date=format(as.Date(dateStart)-daysBack, "%Y-%m-%d"), end_date=format(as.Date(dateStart), "%Y-%m-%d"))
  colnames(outputData)<-paste(commodity, colnames(outputData), sep="_")
  outputData<-outputData[,c(1,2)]
  transposedOut <- t(outputData)
  colnames(transposedOut)<-transposedOut[1,]
  transposedOut<-transposedOut[-c(1),]
  #names(transposedOut)<-c(commodity)
  getCommodityPrice <- transposedOut
}

getSentimentAndPricesForPeriod <- function(dateStart, daysBack, futureLag=5, search.term="bitcoin")
{
  BCFuturePrice5Days<-getBCPrices(dateStart,daysBack, futureLag)
  BCCurrentPrice<-getBCPrices(dateStart,daysBack, 0)
  OIL <- getCommodityPrice("OPEC/ORB", dateStart,daysBack)
  GOLD <- getCommodityPrice("LBMA/GOLD", dateStart,daysBack)
  #ELEC <- getCommodityPrice("EIA/AEO_2016_REF_NO_CPP_PRCE_NA_COMM_NA_ELC_NA_WENWPP_Y13CNTPKWH_A", dateStart,daysBack)
  
  
  datesBuilts  <-  seq(as.Date(dateStart)-daysBack, as.Date(dateStart), by="days")
  sentimentData<-mapply(getSentimentSummary, datesBuilts, search.term=search.term, daysBack=1, USE.NAMES = TRUE)
  colnames(sentimentData)<- format(datesBuilts, "%Y-%m-%d")
  fulloutput<- rbind(BCCurrentPrice, sentimentData)
  fulloutput<- rbind(BCFuturePrice5Days, fulloutput)
  fulloutput<- rbind(OIL, fulloutput)
  fulloutput<- rbind(GOLD, fulloutput)
  #fulloutput<- rbind(ELEC, fulloutput)
  getSentimentAndPricesForPeriod<-fulloutput
}

appendSentimentAndPricesForPeriod<- function(dateStart, daysBack, filename, futureLag=5, search.term="bitcoin")
{
  fileInput<-NULL

  if(file.exists(filename))
  {
  
    fileInput <- read.csv(filename, header=TRUE,check.names=FALSE)
    rownames(fileInput)<-fileInput[,1]
    fileInput<-subset(fileInput, select=-c(1))
  }
  
  
  outputToWrite <-getSentimentAndPricesForPeriod(dateStart, daysBack, futureLag, search.term)
  if (!is.null(fileInput)) { 
    outputToWrite <- cbind(fileInput, outputToWrite)
  }
write.csv(outputToWrite, file=filename)
appendSentimentAndPricesForPeriod<-fileInput
}


#Now try get the sentiment for single date
#sentimentoutputsingle <-getSentimentSummary(search.term="bitcoin","2017-09-07", 1)

appendSentimentAndPricesForPeriod("2017-10-7", 2, "priceindexstore.csv", futureLag=5, search.term="bitcoin")

#sentimentoutputsingle
#Next get the sentiment and prices for multiple dates.
#outputsfull<-getSentimentAndPricesForPeriod("2017-10-02", 2)
#outputsfull

#url<-getBCPricesURL(dateStart="2017-10-02", daysBack=1)
#print("pricesUrl: ")
#print(url)
#dataOut<-fromJSON(url)
#dataOut$bpi
#outputsfull
