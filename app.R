
library(shiny)
library(shinythemes)
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
library(sparkline)
library(timevis)
library(DT)
library(shinycssloaders)

# Load stuff here ---------------------------------------------------------
## for the word cloud
startUrl<-"https://www.adoptapet.com/adoption_rescue/4223-kitten-rescue-los-angeles-california"

# make a function to pull out all the data from the website and make a datafram
getCats<-function(i) {
  cat(i, "\n")
  url <- str_c(startUrl,"?page=", i)
  
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

# Cat sillhouette 
img<-"https://toppng.com/uploads/preview/cat-silhouette-1154945485050bazz8zvk.png"


# ui ----------------------------------------------------------------------
ui <- fluidPage(theme = shinytheme("superhero"),

    # Application title
    titlePanel("Adopt Don't Shop"),
    sidebarLayout(
      sidebarPanel(width = 3,
                   hr(), # horizontal line for visual separation
                   h6("Real time data of adoptable cats at the Los Angeles Kitten Rescue", align = "center"),
                   h6(align = "center", "All data from", tags$a(href="https://www.adoptapet.com/adoption_rescue/4223-kitten-rescue-los-angeles-california", "adoptapet.com")),
                      
                 #  div( actionButton("go", "Show me da kitties!", icon("heart", lib = "glyphicon","fa-2x"),
                  #              style="color: #fff; background-color: #f46d43; border-color: #f46d43", align = "center"), style="text-align: center;"),
                   
                   # display relevent pet information ####
              #     br(), br(),
                   hr(), 
                   div(img(src = "https://pbs.twimg.com/media/DaETc1IVMAA_ckr?format=jpg&name=large", height = "200px"), style="text-align: center;"),
                   #img(src = "https://pbs.twimg.com/media/DaETc1IVMAA_ckr?format=jpg&name=large", height = "150px", align = "center"),
                  # br(), br(),
                  # hr(), # horizontal line for visual separation
                   # flare #### 
                   br(), br(),
                   h5("Built with",
                      img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                      "by",
                      img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                      "."),
                   h6("Made by Nyssa Silbiger and Megsie Siple")
      ) ,
      
      mainPanel(with = 14,
      
           #h1(" "),         
            tabsetPanel(
                tabPanel("Cat Data", 
                         em("Please be patient while data are loading."),
                         plotOutput("plot1")
                        
                         ), 
                tabPanel("Cat Names", 
                         em("Please be patient while data are loading."),
                         plotOutput("plot3")
                         
                ), 
                
                tabPanel("Cat Tinder - Find your match!",
                         em("Please be patient while data are loading."),
                         br(),
                         column(2,align="center",
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                div( actionButton("go", HTML("Not interested.<br/>
                                                  Show me a new kitty!"),
                                                  style="color: #fff; 
                                                        background-color: #f46d43; 
                                                        border-color: #f46d43;
                                                        vertical-align- middle;",
                                                  align = "center"), style="text-align: center;"),
                                
                                br()),
                         column(8,align="center",
                         plotOutput("plot2")),
                         br(),
                         column(2,align="left",
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                div( actionButton(label = "I want to adopt!", icon("heart", lib = "glyphicon","fa-2x"),
                                                  style="color: #fff; 
                                                  background-color: #f46d43; 
                                                  border-color: #f46d43;",
                                                  onclick ="window.open('https://www.adoptapet.com/adoption_rescue/4223-kitten-rescue-los-angeles-california', '_blank')",
                                                  align = "center"), style="text-align: center; vertical-align- middle;"),
                                
                                br())
                         )
                
                
                )
      )
           
))


# server logic ------------------------------------------------------------

server <- function(input, output) {

  
  catdatainfo<-reactive({### Use the adopt a pet website for los angeles
    # extract the cat data
    catdata <- c(1:11) %>% # how many pages of data to look through
      map_dfr(getCats_noerror)%>% # stack the data on top of each other
      drop_na(Breed)%>%
      #reorder youngest to oldest
      mutate(Age=factor(Age, c("Adult", "Kitten", "Senior", "Young"), levels = c("Kitten", "Young", "Adult", "Senior"))); catdata})
  
  catnum <- eventReactive(input$go, {
    ## Pick a random cat to view
    sample(nrow(catdatainfo()),1,replace=T)
  })
  
    output$plot1 <- renderPlot(width = 550, height = 550,{
      p1<-catdatainfo() %>%
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
      
      # pull the plot together
      p1 +
        plot_annotation(title = paste("There are",nrow(catdatainfo()), "adoptable cats at the LA Kitten Rescue."),
                        theme = theme(plot.title = element_text(size = 20)))
    })
    
    output$plot3 <- renderPlot(width = 550, height = 550,{
      # set.seed(42)
      catdata2 <- catdatainfo() %>% 
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
      
      ## cat cloud
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
        scale_size_area(max_size = 6) +
        theme_minimal()+
        ggtitle("Cat names that are <span style = 'color:#7CAE00;'>food</span>, <span style = 'color:#00BFC4;'>Harry Potter</span>, and <span style = 'color:#F8766D;'>Disney Characters</span>")+
        labs(subtitle = "Size of names proportional to frequency")+
        theme(
          plot.title = element_markdown(lineheight = 1.1, size = 20, hjust = 0.5),
          plot.subtitle = element_markdown(hjust = 0.5, size  = 16),
          panel.background = element_rect(fill = "#2B3E50", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "#2B3E50"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "#2B3E50"),
          plot.background = element_rect(fill = "#4E5D6C"),
          text = element_text(colour = 'white'),
          axis.text = element_text(colour = 'white'),
          axis.ticks = element_line(colour = 'white'))
      
      
    })
    
    output$plot2 <- renderPlot(width = 400, height = 400, {
      # random cat to view
      # give its info
      
      num<-if(input$go == 0) 1 else catnum()
      
      Name<-catdatainfo()$Name[num]
      Breed<-catdatainfo()$Breed[num]
      Age<-catdatainfo()$Age[num]
      Sex<-catdatainfo()$Sex[num]
      
      # write a statement
      cat.info<-glue('My name is {Name}.\n I am a {Sex} {Breed} ({Age}).\n You can adopt me at adoptapet.com')
      
      # put a cat on it
      ggdraw() +
        draw_image(as.character(catdatainfo()$src[num]))+
        #ggtitle('Random adoptable cat')+
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
      
      
      
      
      
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
