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

### Use the adopt a pet website for los angeles
startUrl<-"https://www.adoptapet.com/adoption_rescue/4223-kitten-rescue-los-angeles-california"

# make a function to pull out all the data from the website and make a datafram
getCats<-function(i) {
  cat(i, "\n")
  url <- str_c(startUrl, "?page=", i)
  
  # pull in the URL
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

#reorder youngese to oldest
b$Age<-factor(c("Adult", "Kitten", "Senior", "Young"), levels = c("Kitten", "Young", "Adult", "Senior"))

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

# extract the cat data
catdata <- c(1:11) %>% # how many pages of data to look through
  map_dfr(getCats) # stack the data on top of each other

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
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5, hjust = 1),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 20, hjust = 0.5),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14)
        
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
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.text = element_blank(),
        axis.title.x = element_text(vjust = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# pull the plot together
  p1 +p4+
  plot_annotation(title = paste("There are",nrow(catdata), "adoptable cats in Los Angeles."))+ 
  theme(plot.title = element_text(size = 20))+
  ggsave("Catoutput.png", width = 14, height = 10)

  write.csv(catdata, file = 'catdata.csv')
  
  ### Make a wordcloud with the name data
  img<-"https://toppng.com/uploads/preview/cat-silhouette-1154945485050bazz8zvk.png"
  
  set.seed(42)
  catdata %>%
    select(Name)%>%
    group_by(Name) %>%
    summarize(N = n()) %>%
    mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))%>%
    ggplot(aes(label = Name, size = N, angle = angle)) +
    geom_text_wordcloud_area(mask = png::readPNG(getURLContent(img)),
                             rm_outside = FALSE, 
                             area_corr = TRUE) +
    scale_size_area(max_size = 5) +
    theme_minimal()+
    ggsave("Catcloud.png", height = 6, width = 6)
  
  
  