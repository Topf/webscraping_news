#install.packages("rvest")
#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("plyr")

library(rvest)
library(tidyverse)
library(data.table)
library(plyr)
library(writexl)

map_dfc(.x = c("span.Headline", ".Date"),
        .f = function(x) {read_html("https://www.faz.net/suche/s11.html?BTyp=redaktionelleInhalte&allboosted=&author=Vorname+Nachname&boostedresultsize=%24boostedresultsize&cid=&from=24.05.2020&index=&query=Corona&resultsPerPage=60&sort=date&to=01.06.2020&username=Benutzername") %>% 
            html_nodes(x) %>% 
            html_text()}) %>% 
  
  bind_cols(url = read_html("https://www.faz.net/suche/s11.html?BTyp=redaktionelleInhalte&allboosted=&author=Vorname+Nachname&boostedresultsize=%24boostedresultsize&cid=&from=24.05.2020&index=&query=Corona&resultsPerPage=60&sort=date&to=01.06.2020&username=Benutzername") %>% 
              html_nodes("#FAZContentRightInner > div.SuchergebnisListe > div:nth-child(n) > a") %>% 
              html_attr("href")) %>% 
  setNames(nm = c("title", "time", "url")) -> temp
  

#if(length(temp$time) == 0) {
  #temp$time <- NA

#}


temp$url <- paste("https://www.faz.net", temp$url, sep="")

temp

map_df(.x = temp$url[1:40],   
       .f = function(x){tibble(url = x,
                               text = read_html(x) %>% 
                                 html_nodes(".atc-TextParagraph") %>% 
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
