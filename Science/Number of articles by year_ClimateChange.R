library(tidyverse)
library(jsonlite)


key <- "13a4d8164563e7d71ec07bd1fe0c7817"

total_articles <- tibble(year = c(1900:2021), n=0)
url <- "https://api.elsevier.com/content/search/scopus?query=TITLE-ABS-KEY%28Climate+Change%29+"

for (i in 1:nrow(total_articles)){
  year = total_articles$year[i]
  query <- paste0(url,"PUBYEAR+IS+",year,"&apikey=",key)
  ans <- fromJSON(query)
  total_articles$n[i] = as.integer(ans$`search-results`$`opensearch:totalResults`) 
  }

total_articles <- total_articles %>%
  select(-X)
write.csv(total_articles, "total_articles_Climate_Change.csv", row.names = FALSE)



#do the same by field
fields <- read.csv("scopus_subjects.csv")

subject_articles <- fields %>%
  select(Abb) %>%
  mutate(year = list(1980:2021))%>%
  unnest(year)%>%
  mutate(n=0)

colnames(subject_articles) <- c("field", "year", "total")


for (i in 1:nrow(subject_articles)){
    year <- subject_articles$year[i]
    field <-subject_articles$field[i]
    query <- paste0(url,"PUBYEAR+IS+",year,"&subj=",field,"&apikey=",key)
    ans <- fromJSON(query)
    subject_articles$total[i] = as.integer(ans$`search-results`$`opensearch:totalResults`) 
}


write.csv(subject_articles, "subject_articles_Climate_Change.csv",  row.names = FALSE)
