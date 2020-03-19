# Name matchup
library(tidyverse)
library(rvest)
library(NutrienTrackeR)

# Food --------------------------------------------------------------------

#findFoodName(keywords, food_database = "USDA", food_group = NULL, ignore_case = FALSE)

is_it_food <- function(catname){
  in_database <- findFoodName(catname,
                              food_database = "USDA",
                              food_group = NULL,
                              ignore_case = TRUE) != "Could not find any food with the current keywords"
  val <- in_database[1]
  r <- unname(ifelse(val,1,0))
  firstnamed <- if(in_database[1]){findFoodName(catname,
                                                food_database = "USDA",
                                                food_group = NULL,
                                                ignore_case = TRUE)[1]} else(NA)
  return(list(tf = r,firstnamed = firstnamed))
}

##is it HP
is_it_HP <- function(catname){
  in_database <- str_detect(HP$Character, regex(catname, ignore_case = TRUE))
  
  val <- in_database[1]
  r <- unname(ifelse(val,1,0))
  firstnamed <- if(in_database[1]){HP$Character} else(NA)
  return(list(tf = r,firstnamed = firstnamed))
}

str_detect(as.character(catdata$Name), regex(as.character(HP$Character[1]), ignore_case = TRUE))


is_it_HP<-function(HPName){
  in_database<- str_detect(as.character(catdata$Name), regex(HPName, ignore_case = TRUE))
  val <- in_database[1]
  r <- unname(ifelse(val,1,0))
  return(r)
  
  }

catdat$Name %in% regex(HP$First, ignore_case = TRUE)

# see if any of the HP names match up regarless of case
test<-sapply(HP$Character, function (y) sapply(catdata$Name, function (x) grepl(y, x, fixed = FALSE, ignore.case = TRUE))) %>%
  as_tibble()%>%
  mutate(yes = sum(V1:V325))

vowel_counts <- tibble(words = as.character(catdat$Name), 
                       n_string = str_length(as.character(catdat$Name)),
                       n_vowel = str_count(str_length(as.character(catdat$Name)), as.character(HP$Character)),
                       prop_vowel = n_vowel / n_string)

# is_it_food("Pumpkin")
is_it_HP("Harry")
# Disney ------------------------------------------------------------------

url<-"https://en.wikipedia.org/wiki/List_of_Disney_animated_universe_characters"

# pull in the URL
webpage <- read_html(url)

disney <- webpage %>%
  html_nodes("tr > :nth-child(1)") %>% # extract the data
  html_text(trim = TRUE)%>%
  data.frame() %>%
  slice(-c(1:3)) %>%
  slice(-c(1008:1022))%>%
  rename("Character" = ".")

# Harry Potter
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

# split first and last name
HP<-HP %>%
  mutate(Character = as.character(Character))%>%
  separate(col = Character, into = c("First","Last"), sep = " ", remove = FALSE)


# Put together and make a dataframe ---------------------------------------
catdat <- read.csv('catdata.csv',header=T)

t2 <- catdat %>% 
  select(Name,Sex,Age) %>%
  mutate(Name = as.character(Name),
         Name = trimws(Name,which="both")) %>%
  group_by(Name) %>%
  mutate(Food = is_it_food(Name)$tf,
         FirstMatchedFood = is_it_food(Name)$firstnamed,
         Disney = ifelse(Name %in% disney$Character,1,0),
         HarryPotter_lastname =ifelse(Name %in% regex(HP$Last, ignore_case = TRUE),1,0),
         HarryPotter_firstname =ifelse(Name %in% regex(HP$Fist, ignore_case = TRUE),1,0))
#t2

str_detect(HP$Character, stringr::fixed(catdat$Name, ignore_case = TRUE))

str_detect("Mr Weesley", regex("Weesley", ignore_case = TRUE))

str_detect(as.character(catdata$Name)%in% regex(HP$Last, ignore_case = TRUE))


str_detect(HP$Character,  regex("Weesley", ignore_case = TRUE))
