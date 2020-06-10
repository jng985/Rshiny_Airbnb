library(tidyverse)
library(leaflet)
library(shiny)
library(shinyjs)
library(plotly)
library(wordcloud2)
library(shinyWidgets)
library(colourpicker)
library(plotly)
library(wordcloud2)

listings <- read.csv("listings_shiny.csv") 
true_nums <- listings %>% select_if(is.numeric) %>% 
  select(-id,-latitude, -longitude, -host_id) %>% colnames()
group_vars <- listings %>% 
  select_if(is.logical)%>% 
  colnames() %>%
  c("room_type", "bed_type", "borough", "cancel_policy", "host_response_time")

# Tab UIs

# Tab 1: Map
tab1 <- tabPanel(
  "Leaflet Map",
  sidebarLayout(
    sidebarPanel(
      sliderInput("n_listings", "Number of Listings:", min = 1, max = 200, value = 50, sep=''),
      radioButtons("circle_color", "Fill Column", choices = group_vars, selected = "borough"),
      selectInput("circle_size", "Circle Size", choices = true_nums, selected = "price")),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)
# Tab 2:  
tab2 <- tabPanel(
  "Bar Plots",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fluidRow(
        radioButtons("group_col", "Group Column", choices = group_vars, selected = c("borough")),
        radioButtons("fill_col", "Fill Column", choices = group_vars, selected = "borough")),
      selectInput("group_value_col", "Value Column", choices = true_nums, selected = "price"),
      radioButtons("aggfunc", "Aggregate Function", choices=c("n","min","max","mean", "sum"), selected = "mean", inline = T)),
    mainPanel(
      width = 9,
      plotlyOutput("bar_plots", height = 700)
    )
  )
)
# Tab 3: 
tab3 <- tabPanel(
  "Scatter Matrix Plot",
  sidebarLayout(
    sidebarPanel(
      pickerInput("matrix_vars", "Scatter Matrix Variables", choices=true_nums, 
                  selected = c("square_feet", "bedrooms","price"), 
                  options = list(`actions-box` = TRUE), multiple = TRUE),
      pickerInput("matrix_group", "Scatter Matrix Group", choices=group_vars, 
                  selected = c("borough"), options = list(`actions-box` = TRUE))),
    mainPanel(
      plotlyOutput("scatter_matrix", height = 600)
    )
  )
)
# Tab 4: 
tab4 <- tabPanel(
  "Regression",
  sidebarLayout(
    sidebarPanel(width = 4,
                 tags$h1("Regression"),
                 selectInput("regression_x", "Feature Variables", choices = true_nums, 
                             selected = c("beds","accommodates","bathrooms", "host_response_rate"), multiple = TRUE),
                 selectInput("regression_y", "Target Variable", choices = true_nums, selected = "price"),
                 verbatimTextOutput("summary")),
    mainPanel(width = 7, plotlyOutput("regression")
    )
  )
)
# Tab 5: 
tab5 <- tabPanel(
  "Word Clouds", 
  sidebarLayout(
    sidebarPanel(width = 6,
                 radioButtons("word_type", "Word Type", choices = c("NOUN", "ADJ", "ADV", "VERB", "PROPN"), inline = TRUE),
                 sliderInput("word_length", "Word Length:", min = 3, max = 10, value = c(4,8), sep = ''),
                 sliderInput("n_words", "Number of Words:", min = 5, max = 20, value = 15, sep = ''),
                 wordcloud2Output("word_cloud", height = 400)),
    mainPanel(width = 6,
              plotlyOutput("lollipop", height=700)
    )
  )
)
tab6 <- tabPanel(
  "About",
  column(10, offset = 1,
         br(),
         h3("Tab 1: Leaflet Map"),
         h4("Inputs"),
         h5("    Number of Listings: Shows the first n rows from the data table"),
         h5("    Fill Column: Color group variable"),
         h5("    Circle Size: Size of Circle"),
         h4("Plot listings as circles on a map. Click on a circle for some more info and a hyperlink to the listing page."),
         br(),
         h3("Tab 2: Bar Plots"),
         h4("Inputs"),
         h5("    Group Column"),
         h5("    Fill Column"),
         h5("    Value Column"),
         h4("Plot a horizontal bar plot. "),
         h4("Group by either 1 or 2 variables and plot an aggregate function on a specified numeric column."),
         h4("To group by a single variable choose the same variable for group and fill."),
         br(),
         h3("Tab 3: Scatter Matrix Plot"),
         h4("Inputs"),
         h5("    Scatter Matrix Variables: Numeric Variables"),
         h5("    Scatter Matrix Group: Color Group Variable"),
         h4("Plots a scatter matrix for numeric variables and one group variable, which colors the data points."),
         br(),
         h3("Tab 4: Regression"),
         h4("Inputs"),
         h5("    Feature Variables: Numeric Variables"),
         h5("    Target Variable: Numeric Variable"),
         h4("Choose a set of numeric predictor variables and a target variable."),
         h4("Plots a faceted grid of the regression model against each predictor and shows the model summary."),
         br(),
         h3("Tab 5: Word Clouds"),
         h4("Inputs"),
         h5("    Word Type: Type of Word (Noun, Adjective, Adverb, Verb, Proper Noun)"),
         h5("    Word Length: Range of Word Length"),
         h5("    Number of Words: Number of Words in Word Cloud"),
         h4("The words in the word cloud are extraced from from the listing names (The names column in the data table)."),
         h4("Creates a word cloud where word size indicates higher frequency in listing names."),
         br(),
         h3("The dataset comes from http://insideairbnb.com/get-the-data.html and was compiled on 12/4/2019."),
         h3("Application Source code: https://github.com/jng985/Rshiny_Airbnb"),
         br()
  )
)
# Define UI 
ui <- fluidPage(
  useShinyjs(),
  fluidRow(
    column(2, actionButton("hideshow", "Hide/Show Data")),
    column(10, offset = 4, headerPanel(title = "Exploring NYC Airbnb Data with RShiny"))
  ),
  fluidRow(
    tabsetPanel(tab1, tab2, tab3, tab4, tab5, tab6)
  ),
  fluidRow(
    column(12, DT::dataTableOutput("table"))
  )
)