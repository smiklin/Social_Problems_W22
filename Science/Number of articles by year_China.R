library(tidyverse)
library(jsonlite)


key <- ""

total_articles <- tibble(year = c(1900:2021), n=0)
url <- "https://api.elsevier.com/content/search/scopus?query=%28AFFILCOUNTRY%28china%29+AND+"

for (i in 1:nrow(total_articles)){
  year = total_articles$year[i]
  query <- paste0(url,"PUBYEAR+IS+",year,"%29&apikey=",key)
  ans <- fromJSON(query)
  total_articles$n[i] = as.integer(ans$`search-results`$`opensearch:totalResults`) 
  }


write.csv(total_articles, "total_articles.csv", row.names = FALSE)



#do the same by field
fields <- read.csv("scopus_subjects.csv")

subject_articles <- fields %>%
  select(Abb) %>%
  mutate(year = list(1900:2021))%>%
  unnest(year)%>%
  mutate(n=0)

colnames(subject_articles) <- c("field", "year", "total")


for (i in 1:nrow(subject_articles)){
    year <- subject_articles$year[i]
    field <-subject_articles$field[i]
    query <- paste0(url,"PUBYEAR+IS+",year,"%29&subj=",field,"&apikey=",key)
    ans <- fromJSON(query)
    subject_articles$total[i] = as.integer(ans$`search-results`$`opensearch:totalResults`) 
}


write.csv(subject_articles, "subject_articles.csv",  row.names = FALSE)
