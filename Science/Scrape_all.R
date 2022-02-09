library(tidyverse)
library(jsonlite)
library(scales)



#setup
key <- "4ac7d025db4d4706c106aa0f3de643a2"

query <- "TITLE-ABS-KEY ( child AND abuse )" #add your query here

#ignore this last part, it's just defining some other data
url1 <- "https://api.elsevier.com/content/search/scopus?query="
query <- query %>%
  str_replace_all("\\s", "\\+") %>%
  str_replace_all("\\(", "\\%28") %>%
  str_replace_all("\\)", "\\%29") %>%
  str_replace_all('\\"', '\\%22')


#initialize data frame:

call <- paste0(url1,"%28",query,"%29&apikey=",key, "&view=COMPLETE&cursor=*&count=25")

ans <- fromJSON(call)
n = as.integer(ans$`search-results`$`opensearch:totalResults`)
df <- ans$`search-results`$entry 
cursor = URLencode(ans$`search-results`$cursor$`@next`, reserved = TRUE)

url2 <- "https://api.elsevier.com/content/search/scopus?"


#  scrape all data
for (i in 1:ceiling((n-25)/25)){
#for (i in 1:100){
  call <- paste0(url2, "cursor=", cursor,"&count=25&query=",query,"&apikey=",key)
  
  ans <- fromJSON(call)
  df <- df %>% bind_rows(ans$`search-results`$entry)
  cursor = URLencode(ans$`search-results`$cursor$`@next`, reserved = TRUE)
}

saveRDS(df, "All_articles.Rds")


#focus on specific columns and save as a csv
df <- df %>%
  mutate(Year = as.numeric(str_extract(`prism:coverDate`, "\\d{4}"))) %>%
  mutate(All_text= tolower(paste(`dc:title`, `dc:description`, `authkeywords`)))%>%
  select(Year, `dc:title`, `prism:publicationName`,
         `dc:description`, `prism:aggregationType`, `subtypeDescription`,
         `authkeywords`, All_text)

colnames(df) <- c("Year", "Title", "Publication", "Abstract", "Pub_Type", 
                  "Subtype", "Keywords", "All_text")

write.csv(df, "All_articles_slim.csv", row.names = FALSE)


df <- read.csv("All_articles_slim.csv")

# look for a particular co-occurring word and plot as a proportion of all articles

#term <- "battered"
term <- "maltreatment"
term <- "brain"

df %>%
  mutate(has_term = map_dbl(All_text, ~ifelse(grepl(term, .),1,0))) %>%
  group_by(Year) %>%
  summarise(has_term = sum(has_term),
            total = n())%>%
  mutate(prop = has_term/total)%>%
  ggplot(aes(x = Year, y = prop))+
  geom_line()+
  theme_minimal()+
  labs(title = paste0('Proportion of articles on a topic that feature the term "', term, '"'))+
  scale_x_continuous(breaks= pretty_breaks())




