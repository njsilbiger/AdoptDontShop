
#Given a list of cat names
## Test code for the adopt don't shop shiny app
library(rvest)
library(tidyverse)
library(stringr)
library(glue)
library(magick)
library(patchwork)
library(cowplot)
library(ggsci)


startUrl<-"https://www.adoptapet.com/adoption_rescue/4223-kitten-rescue-los-angeles-california"

# make a function to pull out all the data from the website and make a datafram
getCatNames<-function(i){
  cat(i, "\n")
  url <- str_c(startUrl, "?page=", i)
  
  # pull in the URL
  webpage <- read_html(url)
  
  b<-webpage %>%
    html_nodes(".content") %>% # extract the data
    html_text(trim = TRUE)%>%
    data.frame() %>%
    separate(., ., into = c("Name","Sex","Breed", "Needs"), sep = "\n")
  
  Names <- trimws(b$Name, "r")
  return(Names)
}

testnames <- tibble(Name = getCatNames(1))

# Make word cloud
#install.packages("ggwordcloud")

library(ggwordcloud)

set.seed(42)
testnames %>%
  group_by(Name) %>%
  summarize(N = n()) %>%
  ggplot(aes(label = Name, size = N)) +
  geom_text_wordcloud_area(mask = png::readPNG(system.file("extdata/hearth.png",
                                                           package = "ggwordcloud", mustWork = TRUE)),
                           rm_outside = FALSE, 
                           area_corr = TRUE,area_corr_power = 1) +
  scale_size_area(max_size = 10) +
  theme_minimal()


