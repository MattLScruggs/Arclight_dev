#### setup ####

## load packages

library(tidyverse)
setwd("~/Arclight/R_code_and_data")

#### define functions ####

## match pathogen

match_pathogen<-function(pathogen_ref, neg_terms, articles){

path_list<-gsub("\\sdisease|\\svirus|\\sinfection","",pathogen_ref$Pathogen)

pathogen_pattern <- paste0("\\b(", paste(path_list, collapse = "|"), ")\\b")

neg_pattern <- paste0("\\b(", paste(neg_terms, collapse = "|"), ")\\b")

articles$is_noise <- grepl(neg_pattern, articles$full_text, ignore.case = TRUE)

matches <- regexec(pathogen_pattern, articles$full_text, ignore.case = TRUE)
found_names <- regmatches(articles$full_text, matches)

articles$detected_pathogen <- sapply(found_names, function(x) {
  if(length(x) > 0) return(tolower(x[1])) else return(NA)
})


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

return(final_report)
}

## match location

match_location<-function(geo_data, articles){
  
  
  
  geo_pattern <- paste0("\\b(", paste(path_list, collapse = "|"), ")\\b")
  
  matches <- regexec(geo_pattern, articles$full_text, ignore.case = TRUE)
  found_names <- regmatches(articles$full_text, matches)
  
  articles$detected_pathogen <- sapply(found_names, function(x) {
    if(length(x) > 0) return(tolower(x[1])) else return(NA)
  })
  
  
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
  
  return(final_report)
}

## classify text



## make map


