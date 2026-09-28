# Functions to (respectfully) scrape the contents of a page, or set of pages

scrape_article <- function(url, 
                           article_attributes=list(title_sel = "h1.Page-headline",
                                                          date_sel = "bsp-timestamp",
                                                          date_attr = "data-timestamp",
                                                          body_sel = "div.RichTextStoryBody",
                                                          body_paragraphs = "p")) {
  library(magrittr)
  
  title_sel<-article_attributes$title_sel
  date_sel<-article_attributes$date_sel
  date_attr<-article_attributes$date_attr
  body_sel<-article_attributes$body_sel
  body_paragraphs<-article_attributes$body_paragraphs
  
  
  resp <- httr::GET(url)
  
  page <- rvest::read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
  
  title <- page%>%
    rvest::html_element(title_sel)%>%
    rvest::html_text(trim = TRUE)
  
  date_raw <- page%>%
    rvest::html_element(date_sel)%>%
    rvest::html_attr(date_attr)%>%
    as.numeric()
  
  body_vec <- page%>%
    rvest::html_elements(body_sel)%>%
    rvest::html_elements(body_paragraphs)%>%
    rvest::html_text(trim = TRUE)
  
  date  <- as.Date(as.POSIXct((date_raw/1000), origin = "1970-01-01"))
  body  <- paste(body_vec, collapse=" ")
  
  return(list(url=url,
              title=title, 
              date=date,
              body=body))
  
}

get_story_list<-function(list_url, 
                         link_attributes=list(article_selector = "a.Link",
                                              article_attribute = "href")){
  library(magrittr)
  
  article_selector<-link_attributes$article_selector
  article_attribute<-link_attributes$article_attribute
  
  resp_home <- httr::GET(list_url)

  html_home <- rvest::read_html(httr::content(resp_home, as = "text", encoding = "UTF-8"))

  raw_links <- html_home %>%
    rvest::html_elements(article_selector) %>%     
    rvest::html_attr(article_attribute)                   


  article_urls <- raw_links %>%
    purrr::map_chr(~ xml2::url_absolute(.x, list_url)) %>%
    unique()
  
  return(article_urls)
}

create_story_table<-function(list_url, 
                             link_attributes=list(article_selector = "a.Link",
                                                            article_attribute = "href"),
                             article_attributes=list(title_sel = "h1.Page-headline",
                                                     date_sel = "bsp-timestamp",
                                                     date_attr = "data-timestamp",
                                                     body_sel = "div.RichTextStoryBody",
                                                     body_paragraphs = "p")){

  article_urls<-get_story_list(list_url, link_attributes)
  
  article_table<-data.frame(matrix(nrow=length(article_urls), ncol = 4))
  colnames(article_table)<-c("Doc","Title","Date","Text")


  for (i in 1:nrow(article_table)){
    scr<-scrape_article(article_urls[i], article_attributes)
    article_table[i,1]<-scr$url
    article_table[i,2]<-scr$title
    article_table[i,3]<-scr$date
    article_table[i,4]<-scr$body
    Sys.sleep(2)
  }

  article_table$Date<-as.Date(article_table$Date)
  article_table<-tidyr::drop_na(article_table)
  article_table<-article_table[article_table$Text != "",]


  return(article_table)

}





