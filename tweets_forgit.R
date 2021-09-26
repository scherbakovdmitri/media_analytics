library(tidyverse)
library(reticulate)

### import Python tweepy lib, reticulate needs to be installed/configured beforehand
reticulate::py_discover_config()
tweepy=import('tweepy')


### Use auth settings from twitter account (replace with personal details)
consumer_key = 'XXXXXXXXXXXX'
consumer_key_secret = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
access_token = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'

auth = tweepy$OAuthHandler(consumer_key, consumer_key_secret)
auth$set_access_token(access_token, access_token_secret)
api = tweepy$API(auth,wait_on_rate_limit=T,wait_on_rate_limit_notify=T)

## this is a function from python with R error handler
get_status <- function (id) {
  print(id)
  return(tryCatch(api$get_status(id), error=function(e) e))
}


######## Now we search twitter writer by writer
### first we use 3rd party python tool to get ids of tweets by query
scrape2=system(intern=T,'snscrape twitter-search "Н.Г.Чернышевский"')
ids.2=(stringr::str_extract(scrape2,'\\d+$'))
### They we use imported python library to get the text of the tweet
tweets.2=sapply(ids.2,get_status)
tweets.df.2=data.frame()
for (i in 1:length(tweets.2))
{
  df= (tweets.2[[i]]$`_json`) %>% tibble() %>% t %>% as_tibble
  if (ncol(df)>0){
    colnames(df)=names(tweets.2[[i]]$`_json`)
    tweets.df.2=bind_rows(tweets.df.2,df)
  }
}
tweets.df.2.1 = tweets.df.2 %>% unnest_auto(user) %>% rename(id=id_str...3) %>% 
  distinct(id,.keep_all = T) %>% distinct(text,.keep_all = T)

### also we search for different spelling of name
scrape3=system(intern=T,'snscrape twitter-search Чернышевского')
ids.3=(stringr::str_extract(scrape3,'\\d+$'))
tweets.3=sapply(ids.3,get_status)
tweets.df.3=data.frame()

#### and for novel and name
scrape4=system(intern=T,'snscrape twitter-search Что+делать+Чернышевского')
ids.4=(stringr::str_extract(scrape4,'\\d+$'))
tweets.4=sapply(ids.4,get_status)
tweets.df.4=data.frame()
for (i in 1:length(tweets.4))
{
  df= (tweets.4[[i]]$`_json`) %>% tibble() %>% t %>% as_tibble
  if (ncol(df)>0){
    colnames(df)=names(tweets.4[[i]]$`_json`)
    tweets.df.4=bind_rows(tweets.df.4,df)
  }
}
tweets.df.4.1 = 
  tweets.df.4 %>% unnest_auto(user) %>% rename(id=id_str...3) %>% distinct(id,.keep_all = T)

tweets.df.4.2=
  tweets.df.4.1 %>% 
  mutate(text=unlist(text)) %>%
  rowwise() %>% 
  mutate(text=paste0(
    tokens(text,remove_punct=T,remove_symbols=T,remove_url=T),collapse=' ')) %>% 
  ungroup #%>% 
distinct(text,.keep_all = T) 


scrape5=system(intern=T,'snscrape twitter-search "Чернышевского+Что+делать?"')
ids.5=(stringr::str_extract(scrape5,'\\d+$'))
tweets.5=sapply(ids.5,get_status)
tweets.df.5=data.frame()
for (i in 1:length(tweets.5))
{
  df= (tweets.5[[i]]$`_json`) %>% tibble() %>% t %>% as_tibble
  if (ncol(df)>0){
    colnames(df)=names(tweets.5[[i]]$`_json`)
    tweets.df.5=bind_rows(tweets.df.5,df)
  }
}
tweets.df.5.1 = tweets.df.5 %>% unnest_auto(user) %>% rename(id=id_str...3) %>% 
  distinct(id,.keep_all = T) %>% distinct(text,.keep_all = T) %>% 
  mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY")))


### Now we compare with different writer and novel , and repeat 
scrape6=system(intern=T,'snscrape twitter-search читаю+Достоевского')
ids.6=(stringr::str_extract(scrape6,'\\d+$'))
tweets.6=sapply(ids.6,get_status)
tweets.df.6=data.frame()
for (i in 1:length(tweets.6))
{
  df= (tweets.6[[i]]$`_json`) %>% tibble() %>% t %>% as_tibble
  if (ncol(df)>0){
    colnames(df)=names(tweets.6[[i]]$`_json`)
    tweets.df.6=bind_rows(tweets.df.6,df)
  }
}
tweets.df.6.1 = tweets.df.6 %>% unnest_auto(user) %>% rename(id=id_str...3) %>% 
  distinct(id,.keep_all = T) %>% 
  mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY")))


scrape7=system(intern=T,'snscrape twitter-search "роман+Что+делать?"')
ids.7=(stringr::str_extract(scrape7,'\\d+$'))
tweets.7=sapply(ids.7,get_status)
tweets.df.7=data.frame()
for (i in 1:length(tweets.7))
{
  df= (tweets.7[[i]]$`_json`) %>% tibble() %>% t %>% as_tibble
  if (ncol(df)>0){
    colnames(df)=names(tweets.7[[i]]$`_json`)
    tweets.df.7=bind_rows(tweets.df.7,df)
  }
}
tweets.df.7.1 = tweets.df.7 %>% unnest_auto(user) %>% rename(id=id_str...3) %>% 
  distinct(id,.keep_all = T) %>% 
  mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY")))

scrape8=system(intern=T,'snscrape twitter-search "роман+Война+и+мир"')
ids.8=(stringr::str_extract(scrape8,'\\d+$'))
tweets.8=sapply(ids.8,get_status)
tweets.df.8=data.frame()
for (i in 1:length(tweets.8))
{
  df= (tweets.8[[i]]$`_json`) %>% tibble() %>% t %>% as_tibble
  if (ncol(df)>0){
    colnames(df)=names(tweets.8[[i]]$`_json`)
    tweets.df.8=bind_rows(tweets.df.8,df)
  }
}
tweets.df.8.1 = tweets.df.8 %>% unnest_auto(user) %>% rename(id=id_str...3) %>% 
  distinct(id,.keep_all = T) %>% 
  mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY")))

scrape9=system(intern=T,'snscrape twitter-search "роман+Преступление+и+наказание"')
ids.9=(stringr::str_extract(scrape9,'\\d+$'))
tweets.9=sapply(ids.9,get_status)
tweets.df.9=data.frame()
for (i in 1:length(tweets.9))
{
  df= (tweets.9[[i]]$`_json`) %>% tibble() %>% t %>% as_tibble
  if (ncol(df)>0){
    colnames(df)=names(tweets.9[[i]]$`_json`)
    tweets.df.9=bind_rows(tweets.df.9,df)
  }
}
tweets.df.9.1 = tweets.df.9 %>% unnest_auto(user) %>% rename(id=id_str...3) %>% 
  distinct(id,.keep_all = T) %>% 
  mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY")))



#### Text stats based on analyzed above tweets
require(quanteda)
corp.2.1=corpus(tweets.df.21.1 %>% mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY"))),text_field='text',docid_field='id')
tokens.2.1=tokens(corp.2.1,remove_punct = T,remove_symbols = T)%>% tokens_ngrams(1:2)
textstat_collocations(tokens.2.1,size=2:5) %>% glimpse
dfm.2.1=dfm(tokens.2.1,remove=stopwords("ru"))
textplot_network(dfm.2.1 %>% dfm_trim(min_termfreq = 30))
textstat_keyness(dfm.2.1,target=dfm.2.1$year<=2014) %>% filter(abs(p)<0.05) %>% View


corp.21=corpus(tweets.df.21.1 %>% mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY"))),text_field='text',docid_field='id')
tokens.21=tokens(corp.21,remove_punct = T,remove_symbols = T) %>% tokens_ngrams(1:4)
textstat_collocations(tokens(corp.21,remove_punct = T,remove_symbols = T) %>% 
                        tokens_remove("\\.",valuetype="regex") %>% 
                        tokens_keep(min_nchar=1),size=2:6) %>% 
  #filter(count_nested<count) %>% 
  #View
  
  dfm.21=dfm(tokens.21,remove=stopwords("ru")) %>% dfm_trim(min_termfreq = 2)
textplot_network(dfm.21 %>% dfm_trim(min_termfreq = 100))
textstat_keyness(dfm.21,target=dfm.21$year<=2014) %>% filter(abs(p)<0.05) %>% View
textplot_wordcloud(dfm.21,max_size=2)


corp.5.1=corpus(tweets.df.5.1,text_field='text',docid_field='id')
tokens.5.1=tokens(corp.5.1,remove_punct = T,remove_symbols = T) 
dfm.5.1=dfm(tokens.5.1,remove=stopwords("ru")) %>% dfm_trim(min_termfreq = 3)
textstat_collocations(tokens.4.1)
textplot_network(dfm.4.1 %>% dfm_trim(min_termfreq = 100))
textstat_keyness(dfm.4.1,target=dfm.4.1$year<=2014) %>% View

tweets.df.2.1 %>% ggplot2::ggplot(aes(fct_lump_n(factor(screen_name),n=20)))+geom_bar()+coord_flip()

####### Now we turn to sentiment analysis which is also done via 3rd party cli python tool called dost
###Reading Chernysh sentiment

paste0(tweets.df.5.1$text,collapse='_SEPARATOR_') %>% write('/tmp/sents.txt')
system('/home/ms2/dost.py')
sents=read_lines('/tmp/sents2.out')
sents=str_split(sents,'___') %>% unlist
sents=sents[1:(length(sents)-1)]
str=gsub("[']",'\\\"',sents)
df=data.frame()
for (i in 1:length(str))
  df=bind_rows(df,as.data.frame(jsonlite::fromJSON(str[i])))
df.chern=cbind(tweets.df.5.1$text,df)

###Reading Chernysh sentiment 2

paste0(tweets.df.7.1$text,collapse='_SEPARATOR_') %>% write('/tmp/sents.txt')
system('/home/ms2/dost.py')
sents=read_lines('/tmp/sents2.out')
sents=str_split(sents,'___') %>% unlist
sents=sents[1:(length(sents)-1)]
str=gsub("[']",'\\\"',sents)
df=data.frame()
for (i in 1:length(str))
  df=bind_rows(df,as.data.frame(jsonlite::fromJSON(str[i])))
df.chern.2=cbind(tweets.df.7.1$text,df)

###Reading Crime and punishment
paste0(tweets.df.8.1$text,collapse='_SEPARATOR_') %>% write('/tmp/sents.txt')
system('/home/ms2/dost.py')
sents=read_lines('/tmp/sents2.out')
sents=str_split(sents,'___') %>% unlist
sents=sents[1:(length(sents)-1)]
str=gsub("[']",'\\\"',sents)
df=data.frame()
for (i in 1:length(str))
  df=bind_rows(df,as.data.frame(jsonlite::fromJSON(str[i])))
df.dost.2=cbind(tweets.df.8.1$text,df)

###Reading Voina i mir
paste0(tweets.df.9.1$text,collapse='_SEPARATOR_') %>% write('/tmp/sents.txt')
system('/home/ms2/dost.py')
sents=read_lines('/tmp/sents2.out')
sents=str_split(sents,'___') %>% unlist
sents=sents[1:(length(sents)-1)]
str=gsub("[']",'\\\"',sents)
df=data.frame()
for (i in 1:length(str))
  df=bind_rows(df,as.data.frame(jsonlite::fromJSON(str[i])))
df.tolstoi=cbind(tweets.df.9.1$text,df)



df.chern %>% select(-1) %>% mutate_all(~ifelse(.<0.2|is.na(.),0,1)) %>% 
  summarise_all(~round(sum(.)/n(),3))
df.chern.2 %>% select(-1) %>% mutate_all(~ifelse(.<0.2|is.na(.),0,1)) %>% 
  summarise_all(~round(sum(.)/n(),3))
df.dost %>% select(-1) %>% mutate_all(~ifelse(.<0.2|is.na(.),0,1)) %>% 
  summarise_all(~round(sum(.)/n(),3))
df.dost.2 %>% select(-1) %>% mutate_all(~ifelse(.<0.2|is.na(.),0,1)) %>% 
  summarise_all(~round(sum(.)/n(),3))
df.tolstoi %>% select(-1) %>% mutate_all(~ifelse(.<0.2|is.na(.),0,1)) %>% 
  summarise_all(~round(sum(.)/n(),3))


#### Only Last name

### Chernysh
scrape21=system(intern=T,'snscrape twitter-search "Чернышевский"')
ids.21=(stringr::str_extract(scrape21,'\\d+$'))
tweets.21=sapply(ids.21,get_status)
tweets.df.21=data.frame()
for (i in 1:length(tweets.21))
{
  df= (tweets.21[[i]]$`_json`) %>% tibble() %>% t %>% as_tibble
  if (ncol(df)>0){
    colnames(df)=names(tweets.21[[i]]$`_json`)
    tweets.df.21=bind_rows(tweets.df.21,df)
  }
}

tweets.df.21.1 = tweets.df.21 %>% unnest_auto(user) %>% rename(id=id_str...3) %>% 
  distinct(id,.keep_all = T) %>% 
  mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY"))) %>%
  filter(!str_detect(str_to_lower(text),"теплоход|р-н|проезд"))
#slice_sample(n=5000) 
#  distinct(text,.keep_all=T)

corp.21=corpus(tweets.df.21.1 %>% mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY"))),text_field='text',docid_field='id')
tokens.21=tokens(corp.21,remove_punct = T,remove_symbols = T)#%>% tokens_ngrams(1:2)
textstat_collocations(tokens.21,size=2:3) %>% arrange(desc(count)) %>% View #head(50)
dfm.21=dfm(tokens.21,remove=stopwords("ru"))
textplot_network(dfm.21 %>% dfm_trim(min_termfreq = 20))


paste0(tweets.df.21.1$text,collapse='_SEPARATOR_') %>% write('/tmp/sents.txt')
system('/home/ms2/dost.py')
sents=read_lines('/tmp/sents2.out')
sents=str_split(sents,'___') %>% unlist
sents=sents[1:(length(sents)-1)]
str=gsub("[']",'\\\"',sents)
df=data.frame()
for (i in 1:length(str))
  df=bind_rows(df,as.data.frame(jsonlite::fromJSON(str[i])))
df.21=cbind(tweets.df.21.1$text,df)
#df.21=df.21 %>% distinct(.[[1]],.keep_all=T)

### Достоевский
scrape22=system(intern=T,'snscrape twitter-search "Достоевский"')
ids.22=(stringr::str_extract(scrape22,'\\d+$'))
tweets.22=sapply(sample(ids.22,10),get_status)
tweets.df.22=data.frame()
for (i in 1:length(tweets.22))
{
  df= (tweets.22[[i]]$`_json`) %>% tibble() %>% t %>% as_tibble
  if (ncol(df)>0){
    colnames(df)=names(tweets.22[[i]]$`_json`)
    tweets.df.22=bind_rows(tweets.df.22,df)
  }
}
# tweets.df.22.1 = tweets.df.22 %>% unnest_auto(user) %>% rename(id=id_str...3) %>% 
#   distinct(id,.keep_all = T)  %>% #slice_sample(n=5000) %>% 
#   mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY"))) 

corp.22=corpus(tweets.df.22.1 %>% mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY"))),text_field='text',docid_field='id')
tokens.22=tokens(corp.22,remove_punct = T,remove_symbols = T)#%>% tokens_ngrams(1:2)
textstat_collocations(tokens.22,size=2:3) %>% arrange(desc(count)) %>% View #head(50)
dfm.22=dfm(tokens.22,remove=stopwords("ru"))
textplot_network(dfm.22 %>% dfm_trim(min_termfreq = 20))

paste0(tweets.df.22.1$text,collapse='_SEPARATOR_') %>% write('/tmp/sents.txt')
system('/home/ms2/dost.py')
sents=read_lines('/tmp/sents2.out')
sents=str_split(sents,'___') %>% unlist
sents=sents[1:(length(sents)-1)]
str=gsub("[']",'\\\"',sents)
df=data.frame()
for (i in 1:length(str))
  df=bind_rows(df,as.data.frame(jsonlite::fromJSON(str[i])))
df.22=cbind(tweets.df.22.1$text,df)
#df.22=df.22 %>% distinct(.[[1]],.keep_all=T)

### Лермонтов
scrape23=system(intern=T,'snscrape twitter-search "Лермонтов"')
ids.23=(stringr::str_extract(scrape23,'\\d+$'))
tweets.23=sapply(sample(ids.23,10000),get_status)
tweets.df.23=data.frame()
for (i in 1:length(tweets.23))
{
  df= (tweets.23[[i]]$`_json`) %>% tibble() %>% t %>% as_tibble
  if (ncol(df)>0){
    colnames(df)=names(tweets.23[[i]]$`_json`)
    tweets.df.23=bind_rows(tweets.df.23,df)
  }
}
tweets.df.23.1 = tweets.df.23 %>% unnest_auto(user) %>% rename(id=id_str...3) %>% 
  distinct(id,.keep_all = T) %>% slice_sample(n=5000) %>% 
  mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY")))

corp.23=corpus(tweets.df.23.1 %>% mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY"))),text_field='text',docid_field='id')
tokens.23=tokens(corp.23,remove_punct = T,remove_symbols = T)#%>% tokens_ngrams(1:2)
textstat_collocations(tokens.23,size=2:3) %>% arrange(desc(count)) %>% View #head(50)
dfm.23=dfm(tokens.23,remove=stopwords("ru"))
textplot_network(dfm.23 %>% dfm_trim(min_termfreq = 20))

paste0(tweets.df.23.1$text,collapse='_SEPARATOR_') %>% write('/tmp/sents.txt')
system('/home/ms2/dost.py')
sents=read_lines('/tmp/sents2.out')
sents=str_split(sents,'___') %>% unlist
sents=sents[1:(length(sents)-1)]
str=gsub("[']",'\\\"',sents)
df=data.frame()
for (i in 1:length(str))
  df=bind_rows(df,as.data.frame(jsonlite::fromJSON(str[i])))
df.23=cbind(tweets.df.23.1$text,df)
#df.23=df.23 %>% distinct(.[[1]],.keep_all=T)

### Герцен
scrape24=system(intern=T,'snscrape twitter-search "Герцен"')
ids.24=(stringr::str_extract(scrape24,'\\d+$'))
tweets.24=sapply(sample(ids.24,10000),get_status)
tweets.df.24=data.frame()
for (i in 1:length(tweets.24))
{
  df= (tweets.24[[i]]$`_json`) %>% tibble() %>% t %>% as_tibble
  if (ncol(df)>0){
    colnames(df)=names(tweets.24[[i]]$`_json`)
    tweets.df.24=bind_rows(tweets.df.24,df)
  }
}
tweets.df.24.1 = tweets.df.24 %>% unnest_auto(user) %>% rename(id=id_str...3) %>% 
  distinct(id,.keep_all = T) %>% 
  mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY")))

corp.24=corpus(tweets.df.24.1 %>% mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY"))),text_field='text',docid_field='id')
tokens.24=tokens(corp.24,remove_punct = T,remove_symbols = T)#%>% tokens_ngrams(1:2)
textstat_collocations(tokens.24,size=2:3) %>% arrange(desc(count)) %>% View #head(50)
dfm.24=dfm(tokens.24,remove=stopwords("ru"))
textplot_network(dfm.24 %>% dfm_trim(min_termfreq = 20))

paste0(tweets.df.24.1$text,collapse='_SEPARATOR_') %>% write('/tmp/sents.txt')
system('/home/ms2/dost.py')
sents=read_lines('/tmp/sents2.out')
sents=str_split(sents,'___') %>% unlist
sents=sents[1:(length(sents)-1)]
str=gsub("[']",'\\\"',sents)
df=data.frame()
for (i in 1:length(str))
  df=bind_rows(df,as.data.frame(jsonlite::fromJSON(str[i])))
df.24=cbind(tweets.df.24.1$text,df)
#df.24=df.24 %>% distinct(.[[1]],.keep_all=T)


### Добролюбов
scrape25=system(intern=T,'snscrape twitter-search "Добролюбов -\\"князь Добролюбов\\""')
ids.25=(stringr::str_extract(scrape25,'\\d+$'))
tweets.25=sapply(sample(ids.25,1000),get_status)
tweets.df.25=data.frame()
for (i in 1:length(tweets.25))
{
  df= (tweets.25[[i]]$`_json`) %>% tibble() %>% t %>% as_tibble
  if (ncol(df)>0){
    colnames(df)=names(tweets.25[[i]]$`_json`)
    tweets.df.25=bind_rows(tweets.df.25,df)
  }
}
tweets.df.25.1 = tweets.df.25 %>% unnest_auto(user) %>% rename(id=id_str...3) %>% 
  distinct(id,.keep_all = T) %>% 
  mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY")))

paste0(tweets.df.25.1$text,collapse='_SEPARATOR_') %>% write('/tmp/sents.txt')
system('/home/ms2/dost.py')
sents=read_lines('/tmp/sents2.out')
sents=str_split(sents,'___') %>% unlist
sents=sents[1:(length(sents)-1)]
str=gsub("[']",'\\\"',sents)
df=data.frame()
for (i in 1:length(str))
  df=bind_rows(df,as.data.frame(jsonlite::fromJSON(str[i])))
df.25=cbind(tweets.df.25.1$text,df)
#df.25=df.25 %>% distinct(.[[1]],.keep_all=T)

### Некрасов
scrape26=system(intern=T,'snscrape twitter-search "Некрасов"')
ids.26=(stringr::str_extract(scrape26,'\\d+$'))
tweets.26=sapply(sample(ids.26,10000),get_status)
tweets.df.26=data.frame()
for (i in 1:length(tweets.26))
{
  df= (tweets.26[[i]]$`_json`) %>% tibble() %>% t %>% as_tibble
  if (ncol(df)>0){
    colnames(df)=names(tweets.26[[i]]$`_json`)
    tweets.df.26=bind_rows(tweets.df.26,df)
  }
}
tweets.df.26.1 = tweets.df.26 %>% unnest_auto(user) %>% rename(id=id_str...3) %>% 
  distinct(id,.keep_all = T) %>% 
  mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY"))) %>% 
  filter(!str_detect(str_to_lower(text),"анатолий|андрей|дмитрий|виктор|валерий")) 

corp.26=corpus(tweets.df.26.1 %>% mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY"))),text_field='text',docid_field='id')
tokens.26=tokens(corp.26,remove_punct = T,remove_symbols = T)#%>% tokens_ngrams(1:2)
textstat_collocations(tokens.26,size=2:3) %>% arrange(desc(count)) %>% View #head(50)
dfm.26=dfm(tokens.26,remove=stopwords("ru"))
textplot_network(dfm.26 %>% dfm_trim(min_termfreq = 20))

paste0(tweets.df.26.1$text,collapse='_SEPARATOR_') %>% write('/tmp/sents.txt')
system('/home/ms2/dost.py')
sents=read_lines('/tmp/sents2.out')
sents=str_split(sents,'___') %>% unlist
sents=sents[1:(length(sents)-1)]
str=gsub("[']",'\\\"',sents)
df=data.frame()
for (i in 1:length(str))
  df=bind_rows(df,as.data.frame(jsonlite::fromJSON(str[i])))
df.26=cbind(tweets.df.26.1$text,df)
#df.26=df.26 %>% distinct(.[[1]],.keep_all=T)


### Пушкин
scrape27=system(intern=T,'snscrape twitter-search "Пушкин"')
ids.27=(stringr::str_extract(scrape27,'\\d+$'))
tweets.27=sapply(sample(ids.27,1000),get_status)
tweets.df.27=data.frame()
for (i in 1:length(tweets.27))
{
  df= (tweets.27[[i]]$`_json`) %>% tibble() %>% t %>% as_tibble
  if (ncol(df)>0){
    colnames(df)=names(tweets.27[[i]]$`_json`)
    tweets.df.27=bind_rows(tweets.df.27,df)
  }
}
tweets.df.27.1 = tweets.df.27 %>% unnest_auto(user) %>% rename(id=id_str...3) %>% 
  distinct(id,.keep_all = T) %>% 
  mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY")))

paste0(tweets.df.27.1$text,collapse='_SEPARATOR_') %>% write('/tmp/sents.txt')
system('/home/ms2/dost.py')
sents=read_lines('/tmp/sents2.out')
sents=str_split(sents,'___') %>% unlist
sents=sents[1:(length(sents)-1)]
str=gsub("[']",'\\\"',sents)
df=data.frame()
for (i in 1:length(str))
  df=bind_rows(df,as.data.frame(jsonlite::fromJSON(str[i])))
df.27=cbind(tweets.df.27.1$text,df)
#df.27=df.27 %>% distinct(.[[1]],.keep_all=T)


### Гоголь
scrape28=system(intern=T,'snscrape twitter-search "Гоголь"')
scrape28.1=system(intern=T,'snscrape twitter-search "Гоголь -\\"Гоголь-центре\\" -\\"Гоголь-центру\\" -\\"Гоголь-центра\\" -\\"Гоголь-центр\\""')
ids.28=(stringr::str_extract(scrape28.1,'\\d+$'))
tweets.28=sapply(sample(ids.28,1000),get_status)
tweets.df.28=data.frame()
for (i in 1:length(tweets.28))
{
  df= (tweets.28[[i]]$`_json`) %>% tibble() %>% t %>% as_tibble
  if (ncol(df)>0){
    colnames(df)=names(tweets.28[[i]]$`_json`)
    tweets.df.28=bind_rows(tweets.df.28,df)
  }
}
tweets.df.28.1 = tweets.df.28 %>% unnest_auto(user) %>% rename(id=id_str...3) %>% 
  distinct(id,.keep_all = T) %>% 
  mutate(text=unlist(text),year=lubridate::year(lubridate::parse_date_time(created_at...1,"amdHMSzY")))

paste0(tweets.df.28.1$text,collapse='_SEPARATOR_') %>% write('/tmp/sents.txt')
system('/home/ms2/dost.py')
sents=read_lines('/tmp/sents2.out')
sents=str_split(sents,'___') %>% unlist
sents=sents[1:(length(sents)-1)]
str=gsub("[']",'\\\"',sents)
df=data.frame()
for (i in 1:length(str))
  df=bind_rows(df,as.data.frame(jsonlite::fromJSON(str[i])))
df.28=cbind(tweets.df.28.1$text,df)
#df.28=df.28 %>% distinct(.[[1]],.keep_all=T)


#### Now we plot a spider diagram on every writer and sentiment


df.sents=bind_rows(
  tribble(~neutral,~negative,~skip,~positive,~speech,~Запрос,
          1,0.3,1,0.3,0.3,"max",
          0,0,0,0,0,"min"), 
  df.21 %>% select(-1) %>% mutate_all(~ifelse(.<0.2|is.na(.),0,1)) %>% 
    summarise_all(~round(sum(.)/n(),3)) %>% mutate('Запрос'="Чернышевский"),
  df.24 %>% select(-1) %>% mutate_all(~ifelse(.<0.2|is.na(.),0,1)) %>% 
    summarise_all(~round(sum(.)/n(),3))%>% mutate('Запрос'="Герцен"),
  df.22 %>% select(-1) %>% mutate_all(~ifelse(.<0.2|is.na(.),0,1)) %>% 
    summarise_all(~round(sum(.)/n(),3)) %>% mutate('Запрос'="Достоевский"),
  df.25 %>% select(-1) %>% mutate_all(~ifelse(.<0.2|is.na(.),0,1)) %>% 
    summarise_all(~round(sum(.)/n(),3))%>% mutate('Запрос'="Добролюбов"),
  df.27 %>% select(-1) %>% mutate_all(~ifelse(.<0.2|is.na(.),0,1)) %>% 
    summarise_all(~round(sum(.)/n(),3))%>% mutate('Запрос'="Пушкин"),
  df.26 %>% select(-1) %>% mutate_all(~ifelse(.<0.2|is.na(.),0,1)) %>% 
    summarise_all(~round(sum(.)/n(),3))%>% mutate('Запрос'="Некрасов"),
  df.28 %>% select(-1) %>% mutate_all(~ifelse(.<0.2|is.na(.),0,1)) %>% 
    summarise_all(~round(sum(.)/n(),3))%>% mutate('Запрос'="Гоголь"),
  df.23 %>% select(-1) %>% mutate_all(~ifelse(.<0.2|is.na(.),0,1)) %>% 
    summarise_all(~round(sum(.)/n(),3))%>% mutate('Запрос'="Лермонтов")
) %>% select(negative,positive,neutral,skip,speech,Запрос)

df.sents %>% slice(-c(1,2)) %>% 
  print
writexl::write_xlsx('sents.xlsx')

colors_border=c(RColorBrewer::brewer.pal(8,"Set2"))#,RColorBrewer::brewer.pal(3,"Set2"))
#colors_in=c(RColorBrewer::brewer.pal(5,"Set2"),RColorBrewer::brewer.pal(3,"Set2"))
fmsb::radarchart(title="Доля твитов соответствующей тональности",
                 centerzero=F,axislabcol="black",seg=10,axistype=2,cglwd=0.3,plwd=c(4,2,2,2,2,2,2,2),
                 df.sents %>% select(-6),pty = 20,
                 ,maxmin = T, pcol=colors_border)# , pfcol=colors_in )
legend(x=1.3, y=1.3, legend = unlist(df.sents[-c(1,2),6]), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=1, pt.cex=2)

tibble(
  n=c(length(ids.21),length(ids.24),length(ids.22),length(ids.25),length(ids.27),length(ids.26),length(ids.28),length(ids.23)),
  Запрос=c("Чернышевский","Герцен","Достоевский","Добролюбов","Пушкин","Некрасов","Гоголь","Лермонтов")
) %>% 
  # pull(n) %>% 
  # barplot(ylab="упоминаемость в процентах",col = RColorBrewer::brewer.pal(5, "Set2") ,.,
  #         names.arg=c("Чернышевский","Герцен","Достоевский","Добролюбов","Пушкин","Некрасов","Гоголь","Лермонтов"),horiz=F)
  ggplot(aes(x=factor(Запрос,levels=Запрос),y = n)) + ggplot2::theme_minimal()+
  geom_col(alpha=100,color="black",fill=c(RColorBrewer::brewer.pal(5,"Set2"),RColorBrewer::brewer.pal(3,"Set2")))+scale_y_continuous(labels=scales::label_number())+
  #scale_fill_gradient()+
  ylab("Количество твитов")+xlab("Поисковый запрос")
#geom_col(y=..count..)#

### Time Dynamic with spline

src.plot.21=cbind(tweets.df.21.1,df.21) %>% 
  mutate(date=lubridate::as_date(lubridate::parse_date_time(created_at...1,"amdHMSzY"))) %>%
  mutate(fit=ifelse(!is.na(positive),positive,ifelse(!is.na(negative),-negative,NA)))

src.plot.21 %>% ggplot(aes(date,fit))+ #geom_smooth(method = "glm", method.args = list(family = "binomial"))
  geom_point(color= rgb(0, 0, 0, 0.3))+ geom_smooth(se=F,method=lm,formula = y ~ splines::bs(x, 20))+#, pch = 16, ylim = c(-1, 1),
  xlab("Год")+ylab("Тональность текста")+ggtitle("Тональность твита: негавтивная < 0, позитивная > 0",
                                                 subtitle="Запрос по слову \"Чернышевский\"")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+geom_line(y=0,color="green")



compare=bind_rows(src.plot.21.TW %>% mutate(source="Twitter"),src.plot.21.VK %>% mutate(source="VK")) %>% 
  filter(date>"2017-10-01",date<"2018-03-02",!is.na(negative)|!is.na(positive)) %>% 
  select(date,text,negative,positive,source) %>% filter(str_length(text)>10)

dfmat_sotu <- dfm(corpus(compare))
#stem = TRUE, remove_punct = TRUE)
#dfmat_sotu <- dfm_trim(dfmat_sotu, min_termfreq = 5, min_docfreq = 3)
# hierarchical clustering - get distances on normalized dfm
tstat_dist <- textstat_dist(dfm_weight(dfmat_sotu, scheme = "prop"))
#corrplot::corrplot(as.array(tstat_dist) %>% janitor::remove_empty(),is.corr=F,order="hclust")
# hiarchical clustering the distance object
pres_cluster <- hclust(as.dist(tstat_dist))
# label with document names
pres_cluster$labels <- docnames(dfmat_sotu)
# plot as a dendrogram
plot(pres_cluster, xlab = "", sub = "", main = "Euclidean Distance on Normalized Token Frequency")



###
src.plot.22=cbind(tweets.df.22.1,df.22) %>% 
  mutate(date=lubridate::as_date(lubridate::parse_date_time(created_at...1,"amdHMSzY"))) %>%
  mutate(fit=ifelse(!is.na(positive),positive,ifelse(!is.na(negative),-negative,NA)))

src.plot.22 %>% ggplot(aes(date,fit))+ #geom_smooth(method = "glm", method.args = list(family = "binomial"))
  geom_point(color= rgb(0, 0, 0, 0.3))+ geom_smooth(se=F,method=lm,formula = y ~ splines::bs(x, 20))+#, pch = 16, ylim = c(-1, 1),
  xlab("Год")+ylab("Тональность текста")+ggtitle("Тональность твита: негавтивная < 0, позитивная > 0")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+geom_line(y=0,color="green")


###
src.plot.26=cbind(tweets.df.26.1,df.26) %>% 
  mutate(date=lubridate::as_date(lubridate::parse_date_time(created_at...1,"amdHMSzY"))) %>%
  mutate(fit=ifelse(!is.na(positive),positive,ifelse(!is.na(negative),-negative,NA)))
