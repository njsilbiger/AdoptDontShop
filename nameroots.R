# Name matchup
library(tidyverse)
library(rvest)
library(NutrienTrackeR)
library(fuzzyjoin)

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

# Sample of cat names -----------------------------------------------------
catdat <- read.csv('catsample.csv',header=T) %>%
          mutate(Name = as.character(Name),
                  Name = trimws(Name,which="both"))

# Harry Potter ------------------------------------------------------------
harry <-read.csv('HPnames_2.csv',header=T)

is_it_HP <- function(Name){
  x <- data.frame(Name = Name)
  y <- x %>% regex_left_join(harry,by=c(Name = "Character_regex"))
  result <- ifelse(any(!is.na(y$Character)),1,0)
  return(result)
}
  
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
          Harry = is_it_HP(Name))
