## Test code for the adopt don't shop shiny app
library(rvest)
library(tidyverse)
library(stringr)

### Use the adopt a pet website
startUrl<-"https://www.adoptapet.com/adoption_rescue/4223-kitten-rescue-los-angeles-california"

# make a function to pull out all the data from the website and make a datafram
getCats<-function(i) {
  cat(i, "\n")
  url <- str_c(startUrl, "?page=", i)
  
webpage <- read_html(url)

b<-webpage %>%
  html_nodes(".content") %>% # extract the data
  html_text(trim = TRUE)%>%
  data.frame() %>%
  separate(., ., into = c("Name","Sex","Breed", "Needs"), sep = "\n")

# pull the rows for all the special needs kitties
special<-which(!is.na(b$Needs))

b<-b%>%  
  mutate(Breed = ifelse(!is.na(Needs), Needs, Breed)) # special needs kitties have a new line
# add it to the name
b$Needs[special]<-"Special Needs"

b<-b %>%
  separate(Sex, into = c("Sex", "Age"), sep = ",")

return(b)
}

# extract the cat data
catdata <- c(1:20) %>% # how many pages of data to look through
  map_dfr(getCats)

## Make some plots
