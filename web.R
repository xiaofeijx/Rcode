#网页========
library(rvest)
url = 'http://www.tianqi.com/air/'

d1 = url %>% html_session() %>% html_nodes("li .td-1st") %>% html_text(encoding="UTF-8") 
d2 = url %>% html_session() %>% html_nodes(".td-2nd a[href*='air']") %>%  html_text(encoding="UTF-8")
qu = url %>% html_session() %>% html_nodes(".td-4rd") %>% html_text(encoding="UTF-8")
d3 = qu[c(seq(1,length(qu),2))]
d4 = url %>% html_session() %>% html_nodes(".td-4rd em") %>% html_text(encoding="UTF-8") 

data = data.frame(排名 = d1,城市 = d2,空气质量指数 = d3,空气质量 = d4)

head(data)
#附html_nodes函数里面用到的CSS selector的语法网址： http://www.w3.org/TR/css3-selectors/