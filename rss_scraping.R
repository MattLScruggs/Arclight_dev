library(tidyRSS)
library(rvest)
library(xml2)
library(httr)
library(tidyverse)
library(RSQLite)
library(DBI)
#setwd("~/Arclight/R_code_and_data")
options(download.file.method = "libcurl", url.method = "libcurl")
set_config(add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"))

read_rss_feeds<-function(rss_list){

  #build empty lists
  titles<-NULL
  descs<-NULL
  dates<-NULL
  ft<-NULL
  links<-NULL
  
  #grab xml elements
  
  for (i in 1:length(rss_list)){
    tryCatch(
      {
        resp<- read_xml(rss_list[i])
        
        tmp_titles <- xml_find_all(resp, ".//item/title")%>%
          xml_text()%>%
          gsub("<.*?>", "", .)
        
        tmp_descs<-xml_find_all(resp,".//item/description")%>%
          xml_text()%>%
          gsub("<.*?>", "", .)
        
        tmp_dates<- xml_find_all(resp, ".//item/pubDate")%>%
          xml_text()%>%
          gsub("<.*?>", "", .)
        
        tmp_links<-xml_find_all(resp,".//item/link")%>%
          xml_text()%>%
          gsub("<.*?>", "", .)
        
        feed_title<-xml_find_all(resp,".//channel/title")%>%
          xml_text()%>%
          gsub("<.*?>", "", .)
        
        tmp_ft_list<-rep(feed_title, length(tmp_titles))
        
        titles<-append(titles,tmp_titles)
        descs<-append(descs, tmp_descs)
        dates<-append(dates, tmp_dates)
        ft<-append(ft, tmp_ft_list)
        links<-append(links,tmp_links)
      },
      error = function(e) {
        message(paste("Error with URL:", rss_list[i]))
        print(e)
        return(NULL) # Return NULL so the rest of the loop continues
      },
      warning = function(w) {
        message(paste("Warning with URL:", rss_list[i]))
        print(w)
        return(NULL)
      }
    )
  }
  
  
  return(data.frame(titles,
                    descs,
                    dates,
                    links,
                    ft))
  
}


rss_feeds<-c("https://feeds.bbci.co.uk/news/world/rss.xml"
             ,"https://feeds.nbcnews.com/nbcnews/public/news"
             ,"https://www.cnbc.com/id/100727362/device/rss/rss.html"
             ,"https://abcnews.go.com/abcnews/internationalheadlines"
             ,"https://www.aljazeera.com/xml/rss/all.xml"
             ,"https://www.buzzfeed.com/health.xml"
             ,"https://www.nytimes.com/svc/collections/v1/publish/https://www.nytimes.com/section/health/rss.xml"
             ,"https://feeds.washingtonpost.com/rss/world"
             ,"https://feeds.npr.org/1001/rss.xml"
             ,"https://feeds.npr.org/1128/rss.xml"
             ,"https://globalnews.ca/feed/"
             ,"https://globalnews.ca/category/health/feed/"
             ,"https://news.google.com/rss"
             ,"https://www.vox.com/rss/index.xml"
             ,"https://www.cbc.ca/webfeed/rss/rss-health"
             ,"https://www.cbc.ca/webfeed/rss/rss-world"
             ,"https://www.cbc.ca/webfeed/rss/rss-canada"
             ,"https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml"
             ,"https://feeds.content.dowjones.io/public/rss/RSSUSnews"
             ,"https://feeds.nbcnews.com/nbcnews/public/news"
             ,"https://abcnews.go.com/abcnews/topstories"
             ,"https://www.cbsnews.com/latest/rss/main"
             ,"https://www.latimes.com/local/rss2.0.xml"
             ,"http://www.stltoday.com/search/?f=rss&t=article&l=50&s=start_time&sd=desc&k%5B%5D=%23topstory"
             ,"https://chicago.suntimes.com/rss/index.xml"
             ,"https://www.minnpost.com/feed/"
             ,"https://wtop.com/feed/"
             ,"https://feeds.businessinsider.com/custom/all"
             ,"https://theintercept.com/feed/?lang=en"
             ,"https://www.newsweek.com/rss"
             ,"https://www.boston.com/feed/"
             ,"https://wgntv.com/feed/"
             ,"https://ktla.com/feed/"
             ,"https://abc7news.com/feed/"
             ,"https://abc13.com/feed/"
             ,"https://www.kxan.com/feed/"
             ,"https://kdvr.com/feed/"
             ,"https://www.wfla.com/feed/"
             ,"https://www.kron4.com/feed/"
             ,"https://wsvn.com/feed/"
             ,"https://www.wivb.com/feed/"
             ,"https://whdh.com/feed/"
             ,"https://www.news10.com/feed/"
             ,"https://www.nbcnewyork.com/?rss=y"
             ,"https://www.nbclosangeles.com/?rss=y"
             ,"https://www.nbcchicago.com/?rss=y"
             ,"https://www.nbcdfw.com/news/feed/"
             ,"https://www.nbcwashington.com/?rss=y"
             ,"https://www.nbcphiladelphia.com/?rss=y"
             ,"https://www.nbcsandiego.com/?rss=y"
             ,"https://www.nbcmiami.com/?rss=y"
             ,"https://www.dailyherald.com/rssfeed/top-stories/"
             ,"https://www.newsday.com/api/rss/recent"
             ,"https://www.phillyvoice.com/feed/"
             ,"https://timesofsandiego.com/feed/"
             ,"https://www.miamitodaynews.com/feed/"
             ,"https://www.texasobserver.org/feed/"
             ,"https://www.westword.com/denver/Rss.xml"
             ,"https://www.metrotimes.com/detroit/Rss.xml"
             ,"https://observer.com/feed/"
             ,"https://www.miaminewtimes.com/feed"
             ,"https://www.phoenixnewtimes.com/feed"
             ,"https://chicagoreader.com/feed/"
             ,"https://www.salon.com/feed"
             ,"https://www.washingtontimes.com/rss/headlines/news"
             ,"https://foresthillstimes.com/feed/"
             ,"https://www.kens5.com/feeds/syndication/rss/news"
)

daily_refresh<-read_rss_feeds(rss_feeds)

db_con<-dbConnect(SQLite(),"C:/Users/matth/Projects/Data/feeds.db")

query <- "INSERT OR IGNORE INTO rss_feeds (titles, descs, pub_dates, links, feed_title) VALUES (?, ?, ?, ?, ?)"

dbExecute(db_con, query, params = list(daily_refresh$titles, daily_refresh$descs, daily_refresh$dates, daily_refresh$links, daily_refresh$ft))

dbDisconnect(db_con)
#test_out<-dbGetQuery(db_con, "Select * from rss_feeds;")
