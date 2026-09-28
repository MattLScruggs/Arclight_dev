library(rvest)
library(dplyr)
library(xml2)

page_url <- "https://www.cdc.gov/mmwr/index2026.html"

html_doc <- read_html(page_url)

link_nodes <- html_elements(html_doc, "ul li a")

link_text <- html_text2(link_nodes)

link_hrefs <- html_attr(link_nodes, "href")

full_links <- url_absolute(link_hrefs, base = page_url)

scraped_data <- tibble(
  title = link_text,
  url   = full_links
)

cleaned_data <- scraped_data %>%
  filter(!is.na(url), nzchar(title)) %>%
  distinct()

head(cleaned_data)



library(rvest)
library(dplyr)
library(purrr)

page_url <- "https://www.cdc.gov/mmwr/volumes/75/wr/mm7537a1.htm?s_cid=OS_mm7537a1_w"

html_doc <- read_html(page_url)

article_title <- html_doc %>%
  html_element("h1") %>%
  html_text2()

section_nodes <- html_doc %>%
  html_elements("h2, h3, p")

section_text <- html_text2(section_nodes)

nchar(section_text)
section_text[nchar(section_text)>=300]
