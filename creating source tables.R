library(tidyverse)
library(RSQLite)
library(DBI)
setwd("~/Arclight/R_code_and_data")

db_con<-dbConnect(SQLite(),"data/feeds.db")


#### Create RSS feed table ####

dbExecute(db_con, "CREATE TABLE rss_feeds(
          return_id INTEGER PRIMARY KEY AUTOINCREMENT,
          titles TEXT UNIQUE,
          descs TEXT,
          pub_dates TEXT,
          links TEXT,
          feed_title TEXT,
          scrape_dt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);")


#### Create negative keywords table #####

dbExecute(db_con, "
  CREATE TABLE IF NOT EXISTS negative_keywords (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    term TEXT UNIQUE
  )
")

noise_terms <- c("historical", "computer virus", "software", "movie", "book review", "anniversary",
"ancient","dollars","crime","vaccine","backruptcy","equity")


dbExecute(db_con, paste0("INSERT OR IGNORE INTO negative_keywords (term) VALUES ('", 
                      paste(noise_terms, collapse = "'), ('"), "')"))



#### Create Pathogen table #### 
who_df<-read.table('WHO_fact_sheets.txt',sep="|",  
                        header=TRUE, 
                        encoding = "UTF-8",
                        strip.white=TRUE,
                        quote = "\"",
                        allowEscapes = FALSE)

ecdc_df<-read.delim('2026-03-05_ECDC_disease_data.txt',sep="|",  
                   header=TRUE, 
                   encoding = "UTF-8")%>%
                  filter(Title != "")

pathogen_data<-rbind(who_df, ecdc_df)
pathogen_data$Title[order(pathogen_data$Title)]
pathogen_data<-pathogen_data[order(pathogen_data$Title),]
pathogen_data$Title[33]<-"Campylobacter"
pathogen_data$Title[40]<-"Chikungunya"
pathogen_data$Title[45]<-"Chlamydia"
pathogen_data$Title[61]<-"COVID-19"
pathogen_data$Title[84]<-"Ebola virus disease"
pathogen_data$Title[108]<-"Gonorrhoea"
pathogen_data$Title[126]<-"HIV and AIDS"
pathogen_data$Title[129]<-"Human papillomavirus"
pathogen_data$Title[137]<-"Avian influenza"
pathogen_data$Title[139]<-"Influenza (seasonal)"
pathogen_data$Title[148]<-"Lassa fever"
pathogen_data$Title[150]<-"Legionnaires' disease"
pathogen_data$Title[197]<-"Nipah virus"
pathogen_data$Title[251]<-"Rubella"
pathogen_data$Title[252]<-"Salmonella"
pathogen_data$Title[253]<-"Salmonella"
pathogen_data$Title[259]<-"Influenza (seasonal)"
pathogen_data$Title[303]<-"Typhoid"
pathogen_data$Title[316]<-"West Nile virus"
pathogen_data$Title[324]<-"Zika virus"

infect_paths<-c(10,11,18,24,25,26,28,29,32,33,38,39,40,44,45,46,47,48,53,61,62,64,65,66,67,71,72,75,77,78,80,83,84,85,86,87,
                93,106,107,108,109,114,115,116,117,118,119,120,121,122,123,124,125,
                126,128,129,131,137,138,139,142,143,
                145,146,147,148,150,151,152,153,154,155,156,157,160,162,163,165,166,
                168,169,170,171,
                180,181,184,185,186,187,189,196,197,198,200,205,210,213,216,
                219,220,221,222,223,224,227,233,235,236,241,242,245,246,247,249,250,251,252,253,
                256,257,259,262,263,264,265,266,267,269,270,271,274,276,281,282,283,284,285,286,288,289,
                291,295,296,297,298,299,300,301,302,303,308,309,310,313,314,315,316,
                319,320,321,323,324)

pathogen_data<-pathogen_data[infect_paths,]

ecdc_matches<-regexec("ecdc",pathogen_data$Doc, ignore.case = FALSE)
site_source<-regmatches(pathogen_data$Doc, ecdc_matches)

pathogen_data$source<-sapply(site_source, function(x) {
  if(length(x) > 0) return(tolower(x[1])) else return(NA)
})

pathogen_data$source[is.na(pathogen_data$source)]<-"who"

ecdc_data<-pathogen_data[pathogen_data$source=="ecdc",]
who_data<-pathogen_data[pathogen_data$source=="who",]

path_wide<-ecdc_data%>%
  full_join(who_data, by = "Title")%>%
  select(Title, Text.x,Doc.x,Text.y, Doc.y)%>%
  rename(Pathogen=Title,
         ECDC_Text = Text.x,
         ECDC_Link = Doc.x,
         WHO_Text=Text.y,
         WHO_Link=Doc.y)%>%
  arrange(Pathogen)


dbExecute(db_con, "drop table pathogen_data")

dbExecute(db_con, "CREATE TABLE pathogen_data(
          return_id INTEGER PRIMARY KEY AUTOINCREMENT,
          Pathogen TEXT,
          ECDC_Text TEXT,
          ECDC_Link TEXT,
          WHO_Text TEXT,
          WHO_Link TEXT);")

query <- "INSERT INTO pathogen_data (Pathogen, ECDC_Text, ECDC_Link, WHO_Text, WHO_Link) VALUES (?, ?, ?, ?,?)"

dbExecute(db_con, query, params = list(path_wide$Pathogen, path_wide$ECDC_Text, path_wide$ECDC_Link, path_wide$WHO_Text, path_wide$WHO_Link))

#### Create geography table ####


state_data<-read.csv("full_dataset_csv.csv", sep=",")%>%
  filter(latitude !=0)%>%
  filter(longitude !=0)%>%
  select(city, state,country, longitude, latitude)%>%
  group_by(state,country)%>%
  summarise(lat=mean(latitude),
            lon=mean(longitude))

geo_data<-read.csv("full_dataset_csv.csv", sep=",")%>%
  select(city, state)%>%
  unique()%>%
  left_join(state_data, by = 'state')

dbExecute(db_con, "CREATE table geography (
          city text,
          state text,
          country text,
          lat float32,
          lon float32);")
query<-"INSERT INTO geography (city, state, country, lat, lon) VALUES (?,?,?,?,?)"

dbExecute(db_con, query, params = list(geo_data$city, geo_data$state, geo_data$country, geo_data$lat, geo_data$lon))

geo_test<-dbGetQuery(db_con, "SELECT * from geography;")

#### Disconnect and test query tables ####


dbDisconnect(db_con)

#test_out<-dbGetQuery(db_con, "Select * from pathogen_data;")

