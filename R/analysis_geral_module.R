library(echarts4r)
library(readr)
library(zoo)
library(dplyr)

grocery_table <- read_rds("./Data/grocery_table.rds")
product_table <- read_rds("./Data/product_table.rds")
grocery <-  grocery_table |>
  left_join(product_table)

source("./R/supermarkets_df.R")

supermarkets_choices <- sort(supermarkets$supermarket_name)

# UI module ---------------------------------------------------------------

analysis_geral_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 3,
          uiOutput(ns("year_slider")),
            pickerInput(ns("choose_subcategory"),
                        label = "Placeholder", 
                        choices = c("a", "b", "c", "d"),
                        options = list(
                          title = "This is a placeholder")),
            pickerInput(ns("choose_supermarket"),
                        label = "Choose the supermarkets",
                        choices = supermarkets_choices, 
                        multiple = TRUE, selected = c("Lidl", "Tesco", "Aldi"))
                    ),
             box(width = 6,
                 title = "Daily Grocery Shop Values by Supermarket Over Time",
                    echarts4rOutput(ns("graph_time"))),
             box(width = 3,
                 title = "Total Expenditure by Product Category Across All Groceries",
                    echarts4rOutput(ns("graph_category")))),
    hr(),
    br(),
      fluidRow(column(width = 3, 
                      description_ui(ns("mean_this_year"))),
             column(width = 3,
                    description_ui(ns("product_most_consumed"))
                    ),
             column(width = 3,
                    description_ui(ns("mean_purchase"))
                    ),
             column(width = 3,
                    description_ui(ns("times_grocery"))
                    )
             ),
    hr(),
    br(),
    fluidRow( 
      uiOutput(ns("geral_super")),
      box( width = 6,
           uiOutput(ns("git_days")))
                    
             )
  )
  
  
}


# Server module -----------------------------------------------------------

# Server module -----------------------------------------------------------

analysis_geral_server <- function(id, grocery_df, product_df){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive: data_grocery
    data_grocery <- reactive({
      grocery_df() %>%
        left_join(product_df())
    })
    
    # Reactive: data_grocery_filter
    data_grocery_filter <- reactive({
      data_grocery() %>%
        mutate(Year = year(grocery_day)) %>%
        filter(supermarket_name %in% input$choose_supermarket,
               Year %in% input$choose_year)
    })
    
    data_descrip <- reactiveValues({
      data_grocery_filter() |>
        filter
    })

    # MODULE
    
    description_server("mean_this_year", text_number = "17%",
                       text_header = "$35,210.43",
                       text_title = "PURCHASE 2024",
                       arrow_icon = "up")
    
    description_server("product_most_consumed", text_number = "17%",
                       text_header = "$35,210.43",
                       text_title = "pRODUCT MOST pURCHASED",
                       arrow_icon = "up")
    
    description_server("mean_purchase", text_number = "17%",
                       text_header = "$35,210.43",
                       text_title = "MEAN PURCHASE",
                       arrow_icon = "up")
    
    # Output: times_grocery
    description_server("times_grocery", text_number = "17%",
                       text_header = "$35,210.43",
                       text_title = "TIMES GROCERY",
                       arrow_icon = "up")

    # Output: geral_super
    output$geral_super <- renderUI({
      socialBox(width = 12,
                title = userBlock(
                  image = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
                  title = "Social Box",
                  subtitle = "example-01.05.2018"
                ),
                "Some text here!",
                attachmentBlock(
                  image = "https://adminlte.io/themes/v3/dist/img/user1-128x128.jpg",
                  title = "Test",
                  href = "https://google.com",
                  "This is the content"
                ),
                lapply(X = 1:10, FUN = function(i) {
                  boxComment(
                    image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
                    title = paste("Comment", i),
                    date = "01.05.2018",
                    paste0("The ", i, "-th comment")
                  )
                }),
                footer = "The footer here!"
      )
    })
    
    # Output: git_days
    output$git_days <- renderUI({
      dates <- seq.Date(as.Date("2017-01-01"), as.Date("2018-12-31"), by = "day")
      values <- rnorm(length(dates), 20, 6)
      
      year <- data.frame(date = dates, values = values)
      
      year %>% 
        mutate(year = format(date, "%Y")) %>% # get year from date
        group_by(year) %>% 
        e_charts(date) %>% 
        e_calendar(range = "2017",top="40") %>% 
        e_calendar(range = "2018",top="260") %>% 
        e_heatmap(values, coord_system = "calendar") %>% 
        e_visual_map(max = 30) %>% 
        e_title("Calendar", "Heatmap") %>%
        e_tooltip("item") 
    })
    
    # Output: year_slider
    output$year_slider <- renderUI({
      year_grocery <- data_grocery() %>%
        filter(supermarket_name %in% input$choose_supermarket) %>%
        mutate(Year = year(grocery_day)) %>%
        summarise(year_max = max(Year),
                  year_min = min(Year))
      
      sliderInput(ns("choose_year"),
                  label = "Select the Year Range",
                  min = year_grocery$year_min, 
                  max = year_grocery$year_max,
                  value = c(year_grocery$year_max - 1, year_grocery$year_max),
                  step = 1,
                  sep = "")
    })
    
    # Output: graph_time
    output$graph_time <- renderEcharts4r({
      data_grocery_filter() %>%
        group_by(grocery_day, supermarket_name) %>%
        summarise(value_day = sum(value_total)) %>%
        ungroup() %>%
        group_by(supermarket_name) %>%
        e_charts(grocery_day) %>%
        e_line(value_day, symbol = "circle") %>%
        e_tooltip(trigger = "axis") %>%
        e_datazoom(type = "slider") %>%
        e_x_axis(type = "time") %>%
        e_legend(type = "scroll")
    })
    
    # Output: graph_category
    output$graph_category <- renderEcharts4r({
      data_grocery_filter() %>%
        group_by(product_category) %>%
        summarise(total_spent = sum(value_total)) %>%
        e_charts(product_category) %>% 
        e_pie(total_spent, radius = c("40%", "70%")) %>%
        e_tooltip(trigger = "item") %>%
        e_legend(show = FALSE)
    })
  })
}



# analysis_geral_server <- function(id, grocery_df, product_df){
#   moduleServer(id, function(input, output, session){
#     ns <- session$ns
#   
#   # reactive
#     
#     data_grocery <- reactive({
#       grocery_df() |>
#         left_join(product_df())
#     })
#     
#     data_grocery_filter <- reactive({
#       data_grocery() |>
#         mutate(Year = grocery_day |> year()) |>
#         filter(supermarket_name %in% input$choose_supermarket,
#                Year %in% input$choose_year )
#     })
#   # output
#     
#     output$mean_this_year <- renderUI({
#       descriptionBlock(
#         number = "17%", 
#         numberColor = "pink", 
#         numberIcon = icon("caret-up"),
#         header = "$35,210.43", 
#         text = "TOTAL REVENUE", 
#         rightBorder = TRUE,
#         marginBottom = FALSE
#       )
#     })
#     output$product_most_consumed <- renderUI({
#       descriptionBlock(
#         number = "17%", 
#         numberColor = "pink", 
#         numberIcon = icon("caret-up"),
#         header = "$35,210.43", 
#         text = "TOTAL REVENUE", 
#         rightBorder = TRUE,
#         marginBottom = FALSE
#       )
#     })
#     output$mean_purchase <- renderUI({
#       descriptionBlock(
#         number = "17%", 
#         numberColor = "pink", 
#         numberIcon = icon("caret-up"),
#         header = "$35,210.43", 
#         text = "TOTAL REVENUE", 
#         rightBorder = TRUE,
#         marginBottom = FALSE
#       )
#     })
#     output$times_grocery <- renderUI({
#       descriptionBlock(
#         number = "17%", 
#         numberColor = "pink", 
#         numberIcon = icon("caret-up"),
#         header = "$35,210.43", 
#         text = "TOTAL REVENUE", 
#         rightBorder = TRUE,
#         marginBottom = FALSE
#       )
#     })
#     
#     output$geral_super <- renderUI({
#       socialBox(width = 12,
#         title = userBlock(
#           image = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
#           title = "Social Box",
#           subtitle = "example-01.05.2018"
#         ),
#         "Some text here!",
#         attachmentBlock(
#           image = "https://adminlte.io/themes/v3/dist/img/user1-128x128.jpg",
#           title = "Test",
#           href = "https://google.com",
#           "This is the content"
#         ),
#         lapply(X = 1:10, FUN = function(i) {
#           boxComment(
#             image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
#             title = paste("Comment", i),
#             date = "01.05.2018",
#             paste0("The ", i, "-th comment")
#           )
#         }),
#         footer = "The footer here!"
#       )
#     })
#     
#     output$git_days <- renderUI({
#       dates <- seq.Date(as.Date("2017-01-01"), as.Date("2018-12-31"), by = "day")
#       values <- rnorm(length(dates), 20, 6)
#       
#       year <- data.frame(date = dates, values = values)
#       
#       year |> 
#         dplyr::mutate(year = format(date, "%Y")) |> # get year from date
#         group_by(year) |> 
#         e_charts(date) |> 
#         e_calendar(range = "2017",top="40") |> 
#         e_calendar(range = "2018",top="260") |> 
#         e_heatmap(values, coord_system = "calendar") |> 
#         e_visual_map(max = 30) |> 
#         e_title("Calendar", "Heatmap")|>
#         e_tooltip("item") 
#     })
#     
#     output$year_slider <- renderUI({
#       
#       year_grocery = data_grocery() |>
#         filter(supermarket_name %in% input$choose_supermarket) |>
#         mutate(Year = grocery_day |> year()) |>
#         summarise(year_max = Year |> max(),
#                   year_min = Year |> min())
#       
#       sliderInput(ns("choose_year"),
#                   label = "Select the Year Range",
#                   min = year_grocery$year_min, 
#                   max = year_grocery$year_max,
#                   value = c(year_grocery$year_max - 1, year_grocery$year_max ),
#                   step = 1,
#                   sep = "")
#     })
#     
#     output$graph_time <- renderEcharts4r({
#       
#       data_grocery_filter() |>
#         group_by(grocery_day, supermarket_name) |>
#         summarise(value_day = value_total |> sum()) |>
#         ungroup() |>
#         group_by(supermarket_name) |>
#         e_charts(grocery_day) |>
#         e_line(value_day) |>
#         e_tooltip(trigger = "axis") |>
#         e_datazoom(type = "slider") |>
#         e_x_axis(type = "time") |>
#         e_legend(type = "scroll")
#       
# 
#       
#       
#     })
#     
#     output$graph_category <- renderEcharts4r({
#       
#       data_grocery_filter() |>
#         group_by(product_category) |>
#         summarise(total_spent = value_total |> sum()) |>
#         e_charts(product_category) |> 
#         e_pie(total_spent, radius = c("40%", "70%")) |>
#         e_tooltip(trigger = "item") |>
#         e_legend(show = FALSE)  
#       
#     })
# 
#     
#   })
# }



