search.term <- "Bitcoin"
dateRef<-as.Date("2017-09-02")
daysBack<-1
url<-getBCPricesURL(dateStart=dateRef, daysBack=daysBack)
url
dataOut<-fromJSON(url)
dataOut$bpi





#Let's have a look at the rankings
head(title_data)
head(descrip_data)



head(sentimentCollection)
rob<-unlist(sentimentCollection)
remove99<-c(99)
sentimentCollectionClean<-rob [! rob %in% remove99]
summary(sentimentCollectionClean)

sesh<-html_session("http://www.google.com")

search <- html_form(read_html("http://www.google.com"))[[1]]
set_values(search, q = "bitcoin")
set_values(search, num = "100")
set_values(search, num = "100")
set_values(search, tbs = "cdr:1,cd_min:12/1/2017,cd_max:12/9/2017")
set_values(search, tbm = "nws")


values<-submit_form(sesh, search, submit = "btnG")
html_nodes(read_html(values), '.st')
values
values
## Not run: set_values(search, btnI = "blah")

library(twitteR)
#setup_twitter_oauth(consumer_key, consumer_secret, access_token=NULL, access_secret=NULL)
setup_twitter_oauth("RqxwQvin3yeZrQfCrrRA", "MimaKfV5IQQdCHabzxkZXan2f0W5W79IZsLPiusbfs", access_token="210046899-AQfbSWGL6hMkClufkFOadPJJLbJ0vy4F2zswyqf9", access_secret="wqUDuMl552R6EYTLjVyEHrtkTS4l0BZdIvtXLil832Cuq")
searchTwitter(searchString="bitcoin", since="2017-09-01", until="2017-09-07")

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
url<-getTwitterURL("bitcoin","2017-09-10", 5)
#webpage <- read_html(url)
response <- GET(url, add_headers('user-agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.84 Safari/537.36'))
webpage <- content(response)
#Using CSS selectors to scrape the descrip
descrip_data_html <- html_nodes(webpage,'.tweet-text')
#Converting the descrip data to text
descrip_data <- html_text(descrip_data_html)
descrip_data
webpage
