library(tidyverse)
library(jsonlite)


key <- "13a4d8164563e7d71ec07bd1fe0c7817"

months <- c("January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December")



total_articles <- tibble(year = c(2019:2022),month = list(months), n=0) %>%
  unnest(month)

total_articles <- total_articles[12:37,]


url <- "https://api.elsevier.com/content/search/scopus?query="

for (i in 1:nrow(total_articles)){
  year = total_articles$year[i]
  month = total_articles$month[i]
  query <- paste0(url,"PUBDATETXT%28",month,"+",year,"%29&apikey=",key)
  ans <- fromJSON(query)
  total_articles$n[i] = as.integer(ans$`search-results`$`opensearch:totalResults`) 
  }


write.csv(total_articles, "total_articles_month.csv", row.names = FALSE)



#do the same by field
fields <- read.csv("scopus_subjects.csv")



subject_articles <- tibble(year = c(2019:2022),month = list(months), n=0) %>%
  unnest(month) 

subject_articles <- subject_articles[12:37,]

subject_articles <- subject_articles %>%
  mutate(Abb = list(fields$Abb)) %>%
  unnest(Abb)


subject_articles <- subject_articles %>%
  mutate(n=0)

colnames(subject_articles) <- c("year", "month", "total", "field")


for (i in 1:nrow(subject_articles)){
    year <- subject_articles$year[i]
    month <- subject_articles$month[i]
    field <-subject_articles$field[i]
    query <- paste0(url,"PUBDATETXT%28",month,"+",year,"%29&subj=",field,"&apikey=",key)
    ans <- fromJSON(query)
    subject_articles$total[i] = as.integer(ans$`search-results`$`opensearch:totalResults`) 
}


write.csv(subject_articles, "subject_articles_month.csv",  row.names = FALSE)
