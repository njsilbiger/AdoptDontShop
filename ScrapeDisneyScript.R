## extract Disney Characters

url<-"https://en.wikipedia.org/wiki/List_of_Disney_animated_universe_characters"

# pull in the URL
webpage <- read_html(url)

disney<-webpage %>%
  html_nodes("tr > :nth-child(1)") %>% # extract the data
  html_text(trim = TRUE)%>%
  data.frame() %>%
  slice(-c(1:3)) %>%
  slice(-c(1008:1022))%>%
  rename("Character" = ".")
  

