#install.packages("rvest")
#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("plyr")
#install.packages("xlsx")

library(rvest)
library(tidyverse)
library(data.table)
library(plyr)
library(xlsx)


testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}


map_dfc(.x = c("h3", "li time"),
        .f = function(x) {read_html("https://www.bild.de/suche.bild.html?type=article&query=corona&resultsStart=0&resultsPerPage=80") %>% 
            html_nodes(x) %>% 
            html_text()}) %>% 
  
  bind_cols(href = read_html("https://www.bild.de/suche.bild.html?type=article&query=corona&resultsStart=0&resultsPerPage=80") %>% 
              html_nodes("#innerWrapper > div.faux.clearfix > div > section > ol > li:nth-child(n) > div.hentry.landscape.search.t9l > a") %>% 
              html_attr("href")) %>% 
  setNames(nm = c("title", "time", "href")) -> temp

temp$href <- paste("https://www.bild.de", temp$href, sep="")

map_df(.x = temp$href[1:80],   
       .f = function(x){tibble(href = x,
                               text = read_html(x) %>% 
                                 html_nodes("#innerWrapper > main > div.faux > div > article > div.txt > p:nth-child(n)") %>% 
                                 html_text() %>% 
                                 list
       )}) %>% 
  unnest(text) -> foo

foo

foo <- foo %>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit

foo <- setDT(foo)[, lapply(.SD, function(x) toString(na.omit(x))), by = url]

X2 <- ddply(foo, .(url), summarize,
            Xc=paste(text,collapse=","))

final <- merge(temp, X2, by="url")

write_xlsx(vallah,"C:/Users/User/Desktop/drip2.xlsx")


