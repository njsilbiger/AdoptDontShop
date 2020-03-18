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
  
## scrape Harry Potter


 url<-"https://en.wikipedia.org/wiki/List_of_Harry_Potter_characters"
 # pull in the URL
 webpage <- read_html(url)
 
 
 HP<-webpage %>%
   html_nodes("a") %>% # extract the data
   html_text(trim = TRUE)%>%
   data.frame() %>%
   slice(-c(1:77)) %>%
   slice(-c(352:594))%>%
   rename("Character" = ".")%>%
   mutate(num.char = nchar(as.character(Character)))%>%
   filter(num.char>1) %>% # remove the letter of the alphabet
   select(Character)

 write.csv(HP, 'HarryPotter.csv') 
 