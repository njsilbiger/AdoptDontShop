## Test code for the adopt don't shop shiny app
library(rvest)
library(tidyverse)
library(stringr)
library(glue)
library(magick)
library(patchwork)
library(cowplot)
library(ggsci)

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
  slice(-c(nrow(.)-20:nrow(.))) %>%
  select(src)

b<-bind_cols(b,pics)
return(b)
}

# extract the cat data
catdata <- c(1:11) %>% # how many pages of data to look through
  map_dfr(getCats)
# something is wrong on page 9

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
  scale_fill_uchicago()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, size = 10),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5),
        #legend.position = "none",
        panel.grid = element_blank(),
        #plot.caption = element_text(hjust = 0.5, size = 16, vjust = 10),
        legend.title = element_blank()
        ) +
  facet_wrap(~Sex, nrow = 2)


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
  ggtitle('Random adoptable cat')+
  labs(subtitle  = cat.info)+
  theme_minimal()+
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.text = element_blank(),
        axis.title.x = element_text(vjust = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

  p1 +p4+
  plot_annotation(title = paste("There are",nrow(catdata), "adoptable cats in Los Angeles."))+ 
  theme(plot.title = element_text(size = 20))+
  ggsave("Catoutput.png", width = 14, height = 10)
