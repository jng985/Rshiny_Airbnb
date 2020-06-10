library(tidyverse)
library(GGally)
library(plotly)
library(leaflet)
library(rgdal)
library(shiny)
library(shinyjs)
library(colourpicker)
library(plotly)
library(wordcloud2)
library(shinyWidgets)
library(ggforce)
library(udpipe)

listings <- read.csv("listings_shiny.csv") 
true_nums <- listings %>% select_if(is.numeric) %>% 
  select(-id,-latitude, -longitude, -host_id) %>% colnames()
group_vars <- listings %>% 
  select_if(is.logical)%>% 
  colnames() %>%
  c("room_type", "bed_type", "borough", "cancel_policy")

# Tab Functions

# Tab 1
library(rgdal)
neighbourhoods_geo <- readOGR("neighbourhoods.geojson")

get_map <- function(datatable, n, color_var, size_var) {
  
  data <- datatable %>%
    select(latitude, longitude, color_var, size_var, listing_url, id, name) %>%
    head(n) %>%
    mutate(content = paste0(sep = "<br/>", "Listing Id: <b><a href='", listing_url,"'>", id, "</a></b>","<br/>",
                            size_var, " = ", !!as.name(size_var), "<br/>", name),
           radius = scales::rescale(!!as.name(size_var), to=c(5,25)),
           color = !!as.name(color_var)) %>% 
    head(n)
  pal <- colorFactor("viridis", domain = data$color)
 
  map <- leaflet(neighbourhoods_geo) %>%
           addTiles() %>%
           addPolygons(label = neighbourhoods_geo$neighbourhood, stroke = T, weight = 2, fillOpacity = 0.3) %>%
           addCircleMarkers(lat = data$latitude, lng = data$longitude, popup = data$content, label = data$id,
                            color = ~pal(data$color), stroke = T, fillOpacity = 0.85, radius = data$radius) %>%
           addLegend(pal = pal, values = data$color, group = color_var)
  return(map)
  
}

# Tab 2
get_group_plot <- function(datatable, group, value, fill_var, aggfunc) {
  if(fill_var != group) {
    data <- datatable %>%
      select(group, value, fill_var) %>%
      drop_na() %>%
      group_by(!!as.name(group), !!as.name(fill_var)) %>%
      summarize(n_value=n(), min_value=min(!!as.name(value)), 
                mean_value=mean(!!as.name(value)),max_value=max(!!as.name(value)),
                sum_value=sum(!!as.name(value))) %>%
      select(c(paste(aggfunc, "_value", sep = ""), group, fill_var)) %>%
      arrange()
  }
  
  else {
    data <- datatable %>%
      select(group, value, fill_var) %>%
      drop_na() %>%
      group_by(!!as.name(fill_var)) %>%
      summarize(n_value=n(), min_value=min(!!as.name(value)), 
                mean_value=mean(!!as.name(value)),max_value=max(!!as.name(value)),
                sum_value=sum(!!as.name(value))) %>%
      select(c(paste(aggfunc, "_value", sep = ""), group, fill_var)) %>%
      arrange()
  }
  plot <- data %>%
    ggplot(aes(x=!!as.name(group), y=!!as.name(paste(aggfunc, "_value", sep = "")), fill=!!as.name(fill_var))) +
    geom_col(position=position_dodge()) +
    xlab(group) + 
    ylab(paste(aggfunc, value)) + 
    coord_flip() + 
    theme(panel.grid.major.y = element_blank())
  
  return(ggplotly(plot))
}

# Tab 3
get_scatter_matrix <- function(datatable, vars, group_var) {
  plot <- datatable %>%
    select(c(vars, group_var)) %>%
    mutate_if(is.factor, function(x) abbreviate(x, minlength = 3) %>% toupper()) %>%
    drop_na() %>%
    ggplot(aes(x = .panel_x, y = .panel_y, color=!!as.name(group_var))) + 
    geom_autopoint(shape = 1, size = 0.5, alpha=0.3) +
    facet_matrix(vars(everything())) +
    theme(axis.title = element_blank())
  return(ggplotly(plot))
}

# Tab 4
get_regression_summary <- function(datatable, y_var, x_vars) {
  data <- datatable %>%
    drop_na() %>%
    select(y_var, x_vars) %>%
    rename(y = y_var)
  return(lm(y ~ ., data) %>% summary())
}

get_facet_preds <- function(datatable, y_var, x_vars) {
  data <- datatable %>%
    drop_na() %>%
    select(y_var, x_vars) %>%
    rename(y = y_var)
  model <- lm(y ~ ., data)
  plot <- model %>%
    predict(data) %>%
    data.frame() %>%
    cbind(data) %>%
    rename(pred = ".") %>%
    gather(x_col, x_value, colnames(.)[c(-1,-2)]) %>%
    ggplot(aes(x=x_value, y=y)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(. ~ x_col, scales = "free", ncol = 3) +
    ylab(y_var) + 
    theme(panel.spacing = unit(4, "mm"),
          axis.title = element_blank())
  return(ggplotly(plot))
}

# Tab 5
listing_names <- read_csv("tokenized_names.csv")

get_word_lollipop <- function(datatable, word_type, word_length, n) {
  words <- datatable %>%
    left_join(listing_names, by = c("id" = "doc_id")) %>%
    subset(upos %in% c(word_type)) %>%
    mutate(token = tolower(token)) %>%
    filter(min(word_length) <= nchar(token), nchar(token) <= max(word_length))
  plot <- txt_freq(words$token) %>%
    drop_na() %>%
    head(n) %>%
    mutate(freq_pct = freq_pct/100) %>%
    ggplot(aes(x=reorder(key, freq_pct), y=freq_pct, fill = "#FF6666", label = round(100*freq_pct, 1))) +
    geom_segment(aes(x=reorder(key, freq_pct), y = 0, xend = key, yend = freq_pct), color = "#FF6666", size=3) +
    geom_point(size = 8) +
    geom_text(color = "black", size=3) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format()) + 
    theme(legend.position="none",
          panel.grid.major.y = element_blank(),
          axis.text = element_text(size = 12),
          panel.background = element_blank(),
          title = element_text(size=14),
          axis.title.y = element_blank()) +
    ylab("Frequency Percentage") +
    ggtitle(paste("Most Frequent", tolower(word_type), "Types in Listing Names"))
  return(ggplotly(plot))
}

get_word_cloud <- function(datatable, word_type, word_length, n) {
  words <- datatable %>%
    left_join(listing_names, by = c("id" = "doc_id")) %>%
    subset(upos %in% c(word_type)) %>%
    mutate(token = tolower(token)) %>%
    filter(min(word_length) <= nchar(token), nchar(token) <= max(word_length))
  freq_table <- txt_freq(words$token) %>%
    drop_na() %>%
    head(n) %>%
    mutate(freq = freq_pct)
  return(wordcloud2(freq_table))
}

server <- function(input, output) {
  observeEvent(input$hideshow, {toggle("table")})
  output$table <- DT::renderDataTable(
    listings, filter = "top", rownames= F, escape=FALSE,
    extensions = list(
      "ColReorder" = NULL,
      "Buttons" = NULL,
      "FixedColumns" = NULL,
      "Scroller" = NULL,
      "KeyTable" = NULL),
    options = list(
      scrollX=TRUE,
      keys=TRUE,
      colReorder = list(realtime = FALSE),
      search = list(regex = TRUE, caseInsensitive = FALSE, search = ""),
      lengthMenu = list(c(1, 5, 10, 25, 50), c("1", "5", "10", "25", "50")),
      pageLength = 5,
      dom ="BRfrltpi",
      fixedColumns = list(leftColumns = 1),
      buttons = list(I('colvis'), 'copy', 'csv', 'excel', 'pdf')))
  output$map <- renderLeaflet({
    get_map(listings[input$table_rows_all, ], input$n_listings, input$circle_color, input$circle_size)
  })
  output$bar_plots <- renderPlotly({
    get_group_plot(listings[input$table_rows_all, ], input$group_col, input$group_value_col, input$fill_col, input$aggfunc)
  })
  output$scatter_matrix <- renderPlotly({
    get_scatter_matrix(listings[input$table_rows_all, ], c(input$matrix_vars), input$matrix_group) 
  })
  output$word_cloud <- renderWordcloud2({
    get_word_cloud(listings[input$table_rows_all, ], input$word_type, input$word_length, input$n_words)
  })
  output$lollipop <- renderPlotly({
    get_word_lollipop(listings[input$table_rows_all, ], input$word_type, input$word_length, input$n_words)
  })
  output$regression <- renderPlotly({
    get_facet_preds(listings[input$table_rows_all, ], input$regression_y, input$regression_x)
  })
  output$summary <- renderPrint({
    get_regression_summary(listings[input$table_rows_all, ], input$regression_y, input$regression_x)
  })
}