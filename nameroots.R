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

# is_it_food("Pumpkin")

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


# Put together and make a dataframe ---------------------------------------
catdat <- read.csv('catsample.csv',header=T)

t2 <- catdat %>% 
  select(Name,Sex,Age) %>%
  mutate(Name = as.character(Name),
         Name = trimws(Name,which="both")) %>%
  group_by(Name) %>%
  mutate(Food = is_it_food(Name)$tf,
         FirstMatchedFood = is_it_food(Name)$firstnamed,
         Disney = ifelse(Name %in% disney$Character,1,0))
#t2




