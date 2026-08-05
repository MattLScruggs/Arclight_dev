library(httr)    
library(rvest)  
library(xml2)    
library(purrr)   
library(stringr) 
library(tidyverse)
setwd("~/Arclight/R code and data")


base_url   <- "https://www.ecdc.europa.eu/en"  
list_path  <- "/all-topics"  
#list_path<- NULL
list_url   <- paste0(base_url, list_path)


resp_home <- GET(list_url)

if (http_error(resp_home)) {
  stop("Could not retrieve the listing page – check the URL or your network.")
}

html_home <- read_html(content(resp_home, as = "text", encoding = "UTF-8"))


raw_links <- html_home %>%
  html_elements(".view-content")%>%
  html_elements(".list-unstyled")%>%
  html_elements("a") %>% 
  html_attr("href")                   

# Convert possible relative URLs to absolute URLs
article_urls <- raw_links %>%
  map_chr(~ url_absolute(.x, base_url)) %>%
  unique()

article_urls

url<-article_urls[6]

scrape_article <- function(url) {
  resp <- GET(url)
  
  page <- read_html(content(resp, as = "text", encoding = "UTF-8"))
  

  title_sel  <- "h1"         
  date_sel   <- "bsp-timestamp"            
  body_sel   <- ".ecdc-highlighted--scope"       
  

  title <- page%>%
    html_element(title_sel)%>%
    html_text(trim = TRUE)
  
  #date_raw <- page%>%
    #html_element(date_sel)%>%
    #html_attr("data-timestamp")%>%
    #as.numeric()
    
  body_vec <- page%>%
    html_elements(body_sel)%>%
    html_elements("div.wysiwyg-content")%>%
    html_elements("p")%>%
    html_text(trim = TRUE)
  
  date  <- Sys.Date()
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
#article_table<-article_table[article_table$Text != "",]
 
current_date<-Sys.Date()
#article_table<-article_table[article_table$Date == current_date,]


write.table(article_table, file=paste0(current_date,"_ECDC_disease_data.txt"),sep = "|",
            row.names=FALSE, quote=FALSE)

#test<-read.table('2026-02-11_AP_News_Health.txt', sep="|", quote = "", header=TRUE)
