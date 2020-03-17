## Test code for the adopt don't shop shiny app
library(rvest)
library(tidyverse)
library(stringr)
library(glue)
library(magick)
library(patchwork)

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

# remove the white space
b$Sex<-trimws(b$Sex)
b$Breed<-trimws(b$Breed)
b$Age<-trimws(b$Age)

## Get the cat pictures
pics<-webpage %>%
 html_nodes("img") %>% 
  map(xml_attrs) %>% 
  map_df(~as.list(.))%>%
  slice(-c(1:6)) %>%
  select(src)

b<-bind_cols(b,pics)
return(b)
}

# extract the cat data
catdata <- c(1:8) %>% # how many pages of data to look through
  map_dfr(getCats)
# something is wrong on page 9

## Make some plots
# Cats by breed
p1<-catdata %>%
  count(Breed)%>% # count the data
  filter(Breed !="")%>%
  drop_na()%>%
  droplevels()%>%
  ggplot()+
  geom_bar(aes(x = reorder(Breed,desc(n)),y = n), stat = "identity", fill = "lightblue")+
  xlab('')+
  ylab("Number of Cats")+
  ggtitle("Number of cats by breed in Los Angeles")+
  labs(subtitle = "and a rogue dog...")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, size = 14),
        axis.title = element_text(size = 16),
        title = element_text(size = 20))
#Cats by sex
p2<-catdata %>%
  count(Sex)%>% # count the data
  filter(Sex !="")%>%
  drop_na()%>%
  droplevels()%>%
  ggplot()+
  geom_bar(aes(x = reorder(Sex,desc(n)),y = n), stat = "identity", fill = "dodgerblue")+
  xlab('')+
  ylab("Number of Cats")+
  ggtitle("Number of cats by sex in Los Angeles")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, size = 14),
        axis.title = element_text(size = 16),
        title = element_text(size = 20))

#Cats by age
p3<-catdata %>%
  count(Age)%>% # count the data
  filter(Age !="")%>%
  drop_na()%>%
  droplevels()%>%
  ggplot()+
  geom_bar(aes(x = reorder(Age,desc(n)),y = n), stat = "identity", fill = "blue")+
  xlab('')+
  ylab("Number of Cats")+
  ggtitle("Number of cats by sex in Los Angeles")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, size = 14),
        axis.title = element_text(size = 16),
        title = element_text(size = 20))

## Pick a random cat
catnum<-sample(nrow(catdata),1,replace=T)

# give its info
Name<-catdata$Name[catnum]
Breed<-catdata$Breed[catnum]
Age<-catdata$Age[catnum]
Sex<-catdata$Sex[catnum]

cat.info<-glue('My name is {Name}.\n I am a {Sex} {Breed} ({Age}).\n You can adopt me at adoptapet.com')

# put a cat on it
p4<-ggdraw() +
  draw_image(as.character(catdata$src[catnum]))+
  ggtitle('Random adoptable cat in Los Angeles')+
  labs(caption = cat.info)+
  theme_minimal()+
  theme(title = element_text(size = 20),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
(p1+p2)/(p3 +p4) +ggsave("Catoutput.png", width = 14, height = 10)
