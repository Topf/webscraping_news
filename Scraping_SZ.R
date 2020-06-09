#install.packages("rvest")
#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("plyr")
#install.packages("writexl")

library(rvest)
library(tidyverse)
library(data.table)
library(plyr)
library(writexl)


map_dfc(.x = c("em.entrylist__title", "time.entrylist__time"),
        .f = function(x) {read_html("https://www.sueddeutsche.de/news/page/23?search=Corona&sort=date&all%5B%5D=dep&all%5B%5D=typ&all%5B%5D=sys&time=2020-05-24T00%3A00%2F2020-06-01T23%3A59&startDate=24.05.2020&endDate=01.06.2020") %>% 
            html_nodes(x) %>% 
            html_text()}) %>%
  bind_cols(url = read_html("https://www.sueddeutsche.de/news/page/23?search=Corona&sort=date&all%5B%5D=dep&all%5B%5D=typ&all%5B%5D=sys&time=2020-05-24T00%3A00%2F2020-06-01T23%3A59&startDate=24.05.2020&endDate=01.06.2020") %>% 
              html_nodes("a.entrylist__link") %>% 
              html_attr("href")) %>% 
  setNames(nm = c("title", "time", "url")) -> temp

map_df(.x = temp$url[1:50],
       .f = function(x){tibble(url = x,
                               text = read_html(x) %>% 
                                 html_nodes("div.sz-article__body") %>% 
                                 html_nodes("p:not(.sz-teaser__summary)") %>% 
                                 html_text() %>% 
                                 list
       )}) %>% 
  unnest(text) -> foo

foo

X2 <- ddply(foo, .(url), summarize,
            Xc=paste(text,collapse=","))

final <- merge(temp, X2, by="url")



