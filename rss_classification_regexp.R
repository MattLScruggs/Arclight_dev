library(tidyverse)
library(RSQLite)
library(DBI)
setwd("~/Arclight/R_code_and_data")

#### Connect to tables ####

db_con<-dbConnect(SQLite(),"C:/Users/matth/OneDrive/Documents/Arclight/R_code_and_data/data/feeds.db")

table_check<-dbGetQuery(db_con, "Select * from rss_feeds")

pathogen_ref <- dbGetQuery(db_con, "SELECT Pathogen, ECDC_Text, WHO_Text FROM pathogen_data")
neg_terms <- dbGetQuery(db_con, "SELECT term FROM negative_keywords")$term


#### Grab pathogen terms for matching ####

path_list<-gsub("\\sdisease|\\svirus|\\sinfection","",pathogen_ref$Pathogen)

pathogen_pattern <- paste0("\\b(", paste(path_list, collapse = "|"), ")\\b")

neg_pattern <- paste0("\\b(", paste(neg_terms, collapse = "|"), ")\\b")

articles <- dbGetQuery(db_con, "SELECT return_id, titles, descs FROM rss_feeds")

articles$full_text <- paste(articles$title, articles$descs)

#### match article titles and descriptions with pathogen terms ####

articles$is_noise <- grepl(neg_pattern, articles$full_text, ignore.case = TRUE)

matches <- regexec(pathogen_pattern, articles$full_text, ignore.case = TRUE)
found_names <- regmatches(articles$full_text, matches)

articles$detected_pathogen <- sapply(found_names, function(x) {
  if(length(x) > 0) return(tolower(x[1])) else return(NA)
})

#### assemble matched dataset ####

pathogen_ref$match_key <- tolower(pathogen_ref$Pathogen)

articles$final_classification <- ifelse(
  !is.na(articles$detected_pathogen) & !articles$is_noise, 
  "Likely Outbreak", 
  "Discard/Low Priority"
)

final_report <- merge(
  articles, 
  pathogen_ref[, c("match_key", "ECDC_Text","WHO_Text")], 
  by.x = "detected_pathogen", 
  by.y = "match_key", 
  all.x = TRUE
)



#### disconnect ####

dbDisconnect(db_con)
