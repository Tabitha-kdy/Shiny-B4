# Tabitha's Assignment B4 work

library(shiny)
library(tidyverse)
library(datateachr)
library(lubridate)
library(ggplot2)
library(bslib)
library(DT)

#below, I load in the vancouver_trees dataset, and create a year column for date planted
#(this is required later for my Slider widget)
vancouver_trees_edit <- vancouver_trees %>%
                       dplyr::mutate(year = lubridate::year(date_planted)) 
                
#I remove the columns cultivar_name, assigned, root_barrier, and plant_area, as these
#columns contained data that I did not think would be of interest to tourists or locals who have a casual
#interest in cherry blossom viewing. I wanted the data table to be neat/tidy so users could find information more easily.
vancouver_trees_edit <- subset(vancouver_trees_edit, select=-c(cultivar_name, assigned, root_barrier, plant_area))                     
                       


ui <- fluidPage(
    
    #I used the Shiny app bs_theme() function to customize the look of my web page.
    theme = bs_theme(
        bg = "white", fg = "black", primary = "#ffc2d2",
        base_font = font_google("Karla"),
        code_font = font_google("Karla")
                     ),
 
    h1(id="big-heading", "Vancouver Cherry Blossoms app"),
    tags$style(HTML("#big-heading{color: #ff78b9;}")),
    
    
    strong("Welcome to the Vancouver Cherry Blossoms app, developed by Tabitha Kennedy for STAT 545B."),
    tags$br(),
    "This app was developed using the vancouver_trees dataset provided in the R Package datateachr.", 
    tags$br(),
    "To learn more about how to use this app, please see the 'About' tab.",
    tags$br(),
    tags$br(),
    "image credit: www.town.aibetsu.hokkaido.jp",
    
    #I added an image to make the app more visually appealing (and credit was given above).
    img(src = "cherry5.png"),
    sidebarLayout(
        sidebarPanel(
            
          #I added a slider for date planted, so users could filter based on the age of trees.
          #Using this filter, users can see which year and in which neighborhood cherry blossom trees were planted.
          #This information is fun to play around with, and can even be helpful in locating some older and larger cherry trees.
            sliderInput("my_slider", "Select a Range for Date Planted",
                        min = 1989, max = 2019, value = c(1989, 2010)),
            
          
          #I added check boxes for 5 cherry blossom species. More than 5 species of cherry tree exist in Vancouver,
          #however I filtered out NA values from my dataset, which reduced the number of species I could work with. 
           checkboxGroupInput("my_select", label = h3("Select Tree Species"), 
                        choices = list("Kwanzan Flowering Cherry" = 'KWANZAN FLOWERING CHERRY', 
                                       "Akebono Flowering Cherry" = 'AKEBONO FLOWERING CHERRY', 
                                       "Mt Fuji Cherry" = 'SHIROTAE(MT FUJI) CHERRY',
                                       "Rancho Sargent Cherry" = 'RANCHO SARGENT CHERRY',
                                       "Shirofugen Cherry" = 'SHIROFUGEN CHERRY'),
                        selected = "KWANZAN FLOWERING CHERRY"),
            
            
            
        ),
    mainPanel(
        
    #I added tab panels to make my shiny app appear more organized and neat.
    #The 'About' tab provides an introduction to the app and its features to new users.
        tabsetPanel(
            tags$br(),
            tabPanel("About",  p("Are you a tourist planning to visit Vancouver for the Spring season? 
                        Or maybe a local Vancouverite looking to find the next best picnic spot for Hanami? 
                        (the traditional Japanese custom of enjoying the beauty of cherry blossoms). Then
                        you're in luck!",
                                 p("This app was created just for you to help with your cherry blossom search."),
                                 p("Use the slider on the left to discover how old our cherry trees are in the city,
                                   and view your selections in the 'Cherry Trees Data' tab."),
                                 p("To specify which species are of interest, check any boxes under 'Select Tree Species'.
                                   Note: You must have at least 1 (one) species selected in order to view cherry blossom data!"),
                                 p("Have fun exploring the 'Cherry Trees Data' and 'Info Table' tabs to see your filtered results!"),
                                 p("Did you know? Cherry blossom season in Vancouver typically ranges from late-March
                                   to Mid-April. Depending on the species, first bloom and peak bloom will occur at
                                   different times of the year. The average bloom period for cherry blossoms is about 2 weeks in length.
                                   Be sure to check online for daily reports in order to catch peak season!")
                                 )),
            
            
            tabPanel("Cherry Trees Data", plotOutput("my_plot")),
            tabPanel("Info Table",  DT::dataTableOutput("my_table"))),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
    
    )),
    tags$br(),
    tags$br()
    )
    
    
    
    
server <- function(input, output) {
    
    filtered <- reactive({
        #print(input$my_slider)
        #print(input$my_radio)
        vancouver_trees_edit %>%
            na.omit(vancouver_trees_edit)%>%
            filter(year <= input$my_slider[2],
                   year >= input$my_slider[1],
                   common_name %in% input$my_select
                   )
    })
    
    output$my_plot <- renderPlot(
        filtered() %>%
            ggplot(aes(neighbourhood_name, ..count..))+
            geom_bar(fill="pink")+
            ggtitle("Number of Cherry Trees in Vancouver")+
            xlab("Neighbourhood")+
            ylab("Count")+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
        
        output$my_table <- DT::renderDataTable(width="40px",
            #you need to add () after your object filtered. it's weird, but needs to be done
            #in this odd situation.
            filtered()
        )
}
 

    
# Run the application
shinyApp(ui = ui, server = server)
            