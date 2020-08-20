pti <- c("shiny", "tidyverse", "ggplot2movies")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
    install.packages(pti)
}
    
    
library(shiny)
library(tidyverse)
library(ggplot2movies)

# Set randomness seed
set.seed(61)
# Prepare data
shiny_movie_set <- 
    movies %>% 
    filter(year >= 2000) %>%
    select(title,year,length,rating,votes,Action:Short) %>% 
    gather(genre,value,Action:Short) %>% 
    filter(value == 1) %>% 
    select(-value)

# Get genre list
genres <- 
    shiny_movie_set %>% 
    distinct(genre) %>% 
    unlist(.)

names(genres) <- NULL

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Movie Length and IMDB Scores"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("years",
                        "Years",
                        min = 2000,
                        max = 2005,
                        value = c(2002,2004),
                        ticks=FALSE,
                        sep=""),
            selectInput(
                inputId="genre",
                label="Genre",
                choices= c("All",genres),
                multiple= TRUE,
                selected= "All"
            ),
            sliderInput("minvotes",
                        "At Least X Votes",
                        min = min(shiny_movie_set$votes),
                        max = max(shiny_movie_set$votes),
                        value = median(shiny_movie_set$votes),
                        ticks=FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("moviePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$moviePlot <- renderPlot({
        
        # print("===Trigger===")
        # print(input$years)
        # print(input$genre)
        # print(input$minvotes)
        
        pl_df <-
            shiny_movie_set %>%
            filter(year >= input$years[1] &
                       year <= input$years[2] & 
                       votes >= input$minvotes)
        
        if(!("All" %in% input$genre)){
            pl_df <- pl_df %>% filter(genre %in% input$genre)
        }
        
        # print(paste0("Number of Rows: ", nrow(pl_df)))
        
        ggplot(data=pl_df, aes(x=length,y=rating, color=genre)) +
            geom_point()
    })
}


#Run the application
shinyApp(ui = ui, server = server)
        
