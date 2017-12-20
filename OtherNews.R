library("tm.plugin.webmining")
reutersnews <- WebCorpus(ReutersNewsSource("businessNews"))
reutersnews
googlenews <- WebCorpus(GoogleNewsSource("Microsoft"))

library(httr)

response = GET(url = 'https://api.cognitive.microsoft.com/bing/v7.0/news/search?Query=%27bitcoin%27&$format=json', 
               authenticate('','auth token', type = 'basic'))
res = content(response, encoding = 'json')

response <- GET("http://www.google.com/search?q=%22bitcoin%22&num=100&tbs=cdr:1,cd_min:08/31/2017,cd_max:09/01/2017,sbd:1&tbm=nws&source=lnt"
                , add_headers('user-agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.84 Safari/537.36'))
res <- content(response)
res

library('rvest')
descrip_data_html <- html_nodes(res,'.f')
#Converting the descrip data to text
descrip_data <- html_text(descrip_data_html)
descrip_data
