## Test code for the adopt don't shop shiny app
library(rvest)
library(tidyverse)
library(stringr)
library(glue)
library(magick)
library(patchwork)
library(cowplot)
library(ggsci)
library(ggwordcloud)
library(RCurl)
library(NutrienTrackeR)
library(fuzzyjoin)
library(ggtext)


## https://adoptapet.com/pet-search?clan_id=2&geo_range=5&location=91423&page=1

### Use the adopt a pet website for los angeles
startUrl<-"https://www.adoptapet.com/adoption_rescue/4223-kitten-rescue-los-angeles-california"
#startUrl<-"https://adoptapet.com/pet-search?clan_id=2&geo_range=5&"


#Zip<-"91423"
#location=91423&page=1
# make a function to pull out all the data from the website and make a datafram
getCats<-function(i) {
  cat(i, "\n")
  #url <- str_c(startUrl,"location=",Zip,"&page=", i)
  url <- str_c(startUrl,"?page=", i)
  
  # pull in the URL
webpage <- read_html(url)

b<-webpage %>%
  #html_nodes("#container")%>%
  #html_nodes("div")%>%
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

# remove the white space
b$Sex<-trimws(b$Sex)
b$Breed<-trimws(b$Breed)
b$Age<-trimws(b$Age)

## Get the cat pictures
pics<-webpage %>%
 html_nodes("img") %>% 
  map(xml_attrs) %>% 
  map_df(~as.list(.))%>%
  filter(is.na(class))%>%
  #slice(-c(nrow(.)-20:nrow(.))) %>%
  select(src)

# add empty rows of NAs to pics if there is a missing picture
# This will cause some of the pictures to mis match... there is no easy way to join correctly.. :(
if(nrow(b) != nrow(pics)){
  pics[nrow(pics)+(nrow(b)-nrow(pics)),] <- NA
}

b<-bind_cols(b,pics)
return(b)
}

# if the page has no data on it this will fill it with NA instead of giving us an error.
p<-as.data.frame(x = matrix(NA,nrow = 1, ncol = 6))
colnames(p)<-c('Name','Sex','Age', 'Breed', 'Needs', 'src')

getCats_noerror <- possibly(getCats, otherwise = p)

# extract the cat data
catdata <- c(1:11) %>% # how many pages of data to look through
  map_dfr(getCats_noerror)%>% # stack the data on top of each other
  drop_na(Breed)%>%
  #reorder youngest to oldest
  mutate(Age=factor(Age, c("Adult", "Kitten", "Senior", "Young"), levels = c("Kitten", "Young", "Adult", "Senior")))

## Make some plots
# Cats by breed

p1<-catdata %>%
  filter(Breed !="")%>%
  count(Breed, Age, Sex)%>%
  mutate(n = as.numeric(n))%>%
  ggplot(aes(x = reorder(Breed, desc(n)), y = n,  fill = Age))+
  geom_col()+
  xlab('')+
  ylab("Number of Cats")+
  ggtitle("Cats up for adoption")+
  labs(subtitle = "<span style = 'color:#FF410DFF;'>Kittens</span>,<span style = 'color:#6EE2FFFF;'> young cats</span>, <span style = 'color:#F7C530FF;'>adults</span>, <span style = 'color:#95CC5EFF;'>seniors</span>")+
 # scale_fill_uchicago()+
  scale_fill_tron()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5, hjust = 1),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 20, hjust = 0.5, colour = "white"),
        plot.subtitle = element_markdown(hjust = 0.5, size = 16),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, colour = "white"),
        panel.background = element_rect(fill = "#2B3E50", colour = "white",
                                        size = 2, linetype = "solid"),
        plot.background = element_rect(fill = "#4E5D6C"),
      #  legend.key = element_rect(fill = "#4E5D6C"),
        legend.position = "none",
        text = element_text(colour = 'white'),
        axis.text = element_text(colour = 'white'),
        axis.ticks = element_line(colour = 'white')
        
        
       # plot.background = element_rect(fill = "cornsilk", color = NA)
        ) +
  facet_wrap(~Sex, nrow = 2)


## Pick a random cat to view
catnum<-sample(nrow(catdata),1,replace=T)

# give its info
Name<-catdata$Name[catnum]
Breed<-catdata$Breed[catnum]
Age<-catdata$Age[catnum]
Sex<-catdata$Sex[catnum]

# write a statement
cat.info<-glue('My name is {Name}.\n I am a {Sex} {Breed} ({Age}).\n You can adopt me at adoptapet.com')

# put a cat on it
p4<-ggdraw() +
  draw_image(as.character(catdata$src[catnum]))+
  ggtitle('Random adoptable cat')+
  labs(subtitle  = cat.info)+
  theme_minimal()+
  theme(plot.title = element_markdown(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.text = element_blank(),
        axis.title.x = element_text(vjust = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        panel.background = element_rect(fill = "#4E5D6C", color = NA),
        plot.background = element_rect(fill = "#4E5D6C", color = NA),
        text = element_text(colour = 'white')
          )#2B3E50

# pull the plot together
  p1 +p4+
  plot_annotation(title = paste("There are",nrow(catdata), "adoptable cats in Los Angeles."),
                  theme = theme(plot.title = element_text(size = 20)))+ 
    ggsave("Catoutput.png", width = 14, height = 10)

  write.csv(catdata, file = 'catdata.csv')
  
  #####################
  ### Make a wordcloud with the name data and match it with food, disney, and Harry Potter characters
  
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
  
  # Harry Potter ------------------------------------------------------------
  harry <-read.csv('HPnames_2.csv',header=T)
  
  # create a function looking for food names
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
  
  # create functioning using fuzzy logic to look for disney names
  is_it_HP <- function(Name){
    x <- data.frame(Name = Name)
    y <- x %>% regex_left_join(harry,by=c(Name = "Character_regex"))
    result <- ifelse(any(!is.na(y$Character)),1,0)
    return(result)
  }
  
  # Put together and make a dataframe ---------------------------------------
  
  catdata2 <- catdata %>% 
    select(Name,Sex,Age) %>%
    mutate(Name = as.character(Name),
           Name = trimws(Name,which="both")) %>%
    group_by(Name) %>%
    mutate(Food = is_it_food(Name)$tf,
           FirstMatchedFood = is_it_food(Name)$firstnamed,
           Disney = ifelse(Name %in% disney$Character,1,0),
           Harry = is_it_HP(Name))%>%
    ungroup()%>%
    mutate(colorgroup = case_when(Harry == 1 ~ "Harry",
                                  Disney == 1 ~ "Disney",
                                  Food == 1 ~ "Food",
                                  TRUE ~ "Other"))%>%
    mutate(colorgroup = as.factor(colorgroup)) 
  
 ### Make a workd cloud 
  # Cat sillhouette 
  img<-"https://toppng.com/uploads/preview/cat-silhouette-1154945485050bazz8zvk.png"
  
  set.seed(42)
  catdata2 %>%
    select(Name, colorgroup)%>%
    group_by(Name, colorgroup) %>%
    summarize(N = n()) %>%
    mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))%>%
    ungroup()%>%
    ggplot(aes(label = Name, size = N, angle = angle, color = colorgroup)) +
    geom_text_wordcloud_area(mask = png::readPNG(getURLContent(img)),
                             rm_outside = FALSE, 
                             area_corr = TRUE) +
    scale_size_area(max_size = 5) +
    theme_minimal()+
    ggtitle("Cat names that are <span style = 'color:#7CAE00;'>food</span>, <span style = 'color:#00BFC4;'>Harry Potter</span>, and <span style = 'color:#F8766D;'>Disney Characters</span>")+
    labs(subtitle = "Size of names proportional to frequency")+
    theme(
      plot.title = element_markdown(lineheight = 1.1, size = 15, hjust = 0.5),
      plot.subtitle = element_markdown(hjust = 0.5),
      panel.background = element_rect(fill = "#2B3E50", colour = "white",
                                      size = 2, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "#2B3E50"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "#2B3E50"),
      plot.background = element_rect(fill = "#4E5D6C"),
      text = element_text(colour = 'white'),
      axis.text = element_text(colour = 'white'),
      axis.ticks = element_line(colour = 'white')
    )+
    ggsave("Catcloud.png", height = 6, width = 6)
  
  
  