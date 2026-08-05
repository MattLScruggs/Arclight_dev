library(httr)    
library(rvest)  
library(xml2)    
library(purrr)   
library(stringr) 
library(tidyverse)
setwd("~/Arclight/R code and data")


base_url   <- "https://apnews.com"  
#list_path  <- "/health"  
list_path<- NULL
list_url   <- paste0(base_url, list_path)


resp_home <- GET(list_url)


html_home <- read_html(content(resp_home, as = "text", encoding = "UTF-8"))

article_selector <- "a.Link"   

raw_links <- html_home %>%
  html_nodes(article_selector) %>%     
  html_attr("href")                   

# Convert possible relative URLs to absolute URLs
article_urls <- raw_links %>%
  map_chr(~ url_absolute(.x, base_url)) %>%
  unique()

url<-article_urls[5]

scrape_article <- function(url) {
  resp <- GET(url)
  
  page <- read_html(content(resp, as = "text", encoding = "UTF-8"))
  

  title_sel  <- "h1.Page-headline"         
  date_sel   <- "bsp-timestamp"            
  body_sel   <- "div.RichTextStoryBody"       
  

  title <- page%>%
    html_element(title_sel)%>%
    html_text(trim = TRUE)
  
  date_raw <- page%>%
    html_element(date_sel)%>%
    html_attr("data-timestamp")%>%
    as.numeric()
    
  body_vec <- page%>%
    html_elements(body_sel)%>%
    html_elements("p")%>%
    html_text(trim = TRUE)
  
  date  <- as.Date(as.POSIXct((date_raw/1000), origin = "1970-01-01"))
  body  <- paste(body_vec, collapse=" ")

return(list(url=url,
                  title=title, 
                  date=date,
                  body=body))
  
}


article_table<-data.frame(matrix(nrow=length(article_urls), ncol = 4))
colnames(article_table)<-c("Doc","Title","Date","Text")


for (i in 1:nrow(article_table)){
  scr<-scrape_article(article_urls[i])
  article_table[i,1]<-scr$url
  article_table[i,2]<-scr$title
  article_table[i,3]<-scr$date
  article_table[i,4]<-scr$body
  Sys.sleep(1)
}

article_table$Date<-as.Date(article_table$Date)
article_table<-drop_na(article_table)
article_table<-article_table[article_table$Text != "",]
 
current_date<-Sys.Date()
#article_table<-article_table[article_table$Date == current_date,]


write.table(article_table, file=paste0(current_date,"_AP_News_All.txt"),sep = "|",
            row.names=FALSE, quote=FALSE)

#test<-read.table('2026-02-11_AP_News_Health.txt', sep="|", quote = "", header=TRUE)
