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
    fluidRow(column(width = 6,
                    uiOutput(ns("year_slider")) ),
             column(width = 6,
                    pickerInput(ns("choose_supermarket"),
                                label = "Choose the supermarkets",
                                choices = supermarkets_choices, 
                                multiple = TRUE, selected = c("Lidl", "Tesco", "Aldi")))
             ),
    fluidRow(column(width = 3, 
                    infoBoxOutput(ns("mean_this_year")) ),
             column(width = 3,
                    infoBoxOutput(ns("product_most_consumed"))
             ),
             column(width = 3,
                    infoBoxOutput(ns("mean_purchase"))
             ),
             column(width = 3,
                    infoBoxOutput(ns("times_grocery"))
             )
    ),
    fluidRow(
             box(width = 6,
                 title = "Daily Grocery Shop Values by Supermarket Over Time",
                    echarts4rOutput(ns("graph_time"))),
             box(width = 6,
                 title = "Total Expenditure by Product Category Across All Groceries",
                    echarts4rOutput(ns("graph_category")))
             ),
    hr(),
    br(),
    fluidRow( 
      uiOutput(ns("geral_super")),
      box( width = 6,
           uiOutput(ns("git_days")),
           sidebar = boxSidebar(
             startOpen = FALSE,
             id = "year_calendar",
             uiOutput(ns("year_calendar_input"))
           )
      )
    )
  )
  
  
}

# Server module -----------------------------------------------------------

analysis_geral_server <- function(id, grocery_df, product_df){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive
    
    data_grocery <- reactive({
      grocery_df() |>
        left_join(product_df())
    })
    
    data_grocery_filter <- reactive({
      data_grocery() |>
        mutate(Year = year(grocery_day)) |>
        filter(supermarket_name %in% input$choose_supermarket,
               Year %in% input$choose_year)
    })
    
    years_grocery <- reactive({
      data_grocery() |>
        filter(supermarket_name %in% input$choose_supermarket) |>
        mutate(Year = year(grocery_day)) |>
        summarise(year_max = max(Year),
                  year_min = min(Year))
    })
    

    # Module
    
    # Output
    
    output$year_slider <- renderUI({
      
      sliderInput(ns("choose_year"),
                  label = "Select the Year Range",
                  min = years_grocery()$year_min, 
                  max = years_grocery()$year_max,
                  value = c(years_grocery()$year_max - 1, years_grocery()$year_max),
                  step = 1,
                  sep = "")
    })
    
    output$graph_time <- renderEcharts4r({
      data_grocery_filter() |>
        group_by(grocery_day, supermarket_name) |>
        summarise(value_day = sum(value_total)) |>
        ungroup() |>
        group_by(supermarket_name) |>
        e_charts(grocery_day) |>
        e_line(value_day, symbol = "circle") |>
        e_tooltip(trigger = "axis") |>
        e_datazoom(type = "slider") |>
        e_x_axis(type = "time") |>
        e_legend(type = "scroll")
    })
    
    output$graph_category <- renderEcharts4r({
      data_grocery_filter() |>
        group_by(product_category) |>
        summarise(total_spent = sum(value_total)) |>
        e_charts(product_category) |> 
        e_pie(total_spent, radius = c("40%", "70%")) |>
        e_tooltip(trigger = "item") |>
        e_legend(show = FALSE)
    })
    
    output$mean_this_year <- renderInfoBox({
      value_number = data_grocery_filter() |>
        filter(Year == 2024) |>
        group_by(supermarket_name, grocery_day) |>
        summarise(value_day = sum(value_total)) |>
        ungroup() |>
          summarise(avg_total = value_day |> 
                      mean() |>
                      round(2)) |>
        pull(avg_total)
        
      infoBox(title = "Average purchase",
              icon = icon("receipt"),
              value = value_number
                )
    })
    
    output$product_most_consumed <- renderInfoBox({
      values_number = data_grocery_filter() |>
        group_by(product_name) |>
        summarise(count_product = product_quantity |> sum()) |>
        ungroup() |>
        filter(count_product == max(count_product))
      
      infoBox(title = "Most Consumed Product",
              subtitle = values_number$product_name,
              icon = icon("star"), 
              value = values_number$count_product)
      
    })

    output$mean_purchase <- renderInfoBox({
      values_number = data_grocery_filter() |>
        mutate(Month = grocery_day |> month()) |>
        group_by(Year, Month) |>
        summarise(total_month = value_total |> sum(na.rm = TRUE)) |>
        ungroup() |>
        summarise(avg_total = total_month |> 
                    mean() |>
                    round(2)) |>
        pull(avg_total)
      
      infoBox(title = "Average Monthly",
              icon = icon("money-bill-alt"), 
              value = values_number, width = NULL)
      
    })
    
    output$times_grocery <- renderInfoBox({
      values_number = data_grocery_filter() |>
        mutate(Month = grocery_day |> month()) |>
        group_by(supermarket_name, grocery_day, Month, Year) |>
        summarise(count_purchases = n()) |>
        ungroup() |>
        summarise(avg_purchases = count_purchases |> 
                    mean() |>
                    round()) |>
        pull(avg_purchases)
      
      infoBox(title = "Average Monthly Purchase Quantity",
                 icon = icon("clipboard-list"),
                 value = values_number)
      
    })


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
      
      grocery |> 
        group_by(grocery_day) |> 
        summarise(total_day = value_total) |> 
        ungroup() |> 
        mutate(Year = grocery_day |> Year()) |>
        group_by(Year) |>
        e_charts(grocery_day) |>
        e_calendar()
      
      year |> 
        mutate(year = format(date, "%Y")) |> # get year from date
        group_by(year) |> 
        e_charts(date) |> 
        e_calendar(range = "2017",top="40") |> 
        e_calendar(range = "2018",top="260") |> 
        e_heatmap(values, coord_system = "calendar") |> 
        e_visual_map(max = 30) |> 
        e_title("Calendar", "Heatmap") |>
        e_tooltip("item") 
    })
    
    # Output: year_slider

    
    # Output: graph_time
    
    
    # Output: graph_category
    
    
    output$year_calendar_input <- renderUI({
      
      
      sliderInput(
        "obs",
        "Number of observations:",
        min = 0,
        max = 1000,
        value = 500
      )
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



