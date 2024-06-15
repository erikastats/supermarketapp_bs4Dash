library(echarts4r)

# UI module ---------------------------------------------------------------

analysis_geral_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 3,
            sliderInput(ns("choose_year"),
                        label = "Choose a year"  
                        , min = 2021, 
                        max = 2024, value = c(2022, 2024) ),
            pickerInput(ns("choose_subcategory"),
                        label = "Placeholder", 
                        choices = c("a", "b", "c", "d"),
                        options = list(
                          title = "This is a placeholder"))
                    ),
             box(width = 6,
                    echarts4rOutput(ns("graph_time"))),
             box(width = 3,
                    echarts4rOutput(ns("graph_category")))),
    hr(),
    br(),
      fluidRow(column(width = 3, 
                    uiOutput(ns("mean_this_year"))),
             column(width = 3,
                    uiOutput(ns("product_most_consumed"))
                    ),
             column(width = 3,
                    uiOutput(ns("mean_purchase"))
                    ),
             column(width = 3,
                    uiOutput(ns("times_grocery"))
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

analysis_geral_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
  #output
    
    output$mean_this_year <- renderUI({
      descriptionBlock(
        number = "17%", 
        numberColor = "pink", 
        numberIcon = icon("caret-up"),
        header = "$35,210.43", 
        text = "TOTAL REVENUE", 
        rightBorder = TRUE,
        marginBottom = FALSE
      )
    })
    output$product_most_consumed <- renderUI({
      descriptionBlock(
        number = "17%", 
        numberColor = "pink", 
        numberIcon = icon("caret-up"),
        header = "$35,210.43", 
        text = "TOTAL REVENUE", 
        rightBorder = TRUE,
        marginBottom = FALSE
      )
    })
    output$mean_purchase <- renderUI({
      descriptionBlock(
        number = "17%", 
        numberColor = "pink", 
        numberIcon = icon("caret-up"),
        header = "$35,210.43", 
        text = "TOTAL REVENUE", 
        rightBorder = TRUE,
        marginBottom = FALSE
      )
    })
    output$times_grocery <- renderUI({
      descriptionBlock(
        number = "17%", 
        numberColor = "pink", 
        numberIcon = icon("caret-up"),
        header = "$35,210.43", 
        text = "TOTAL REVENUE", 
        rightBorder = TRUE,
        marginBottom = FALSE
      )
    })
    
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
    
    output$git_days <- renderUI({
      dates <- seq.Date(as.Date("2017-01-01"), as.Date("2018-12-31"), by = "day")
      values <- rnorm(length(dates), 20, 6)
      
      year <- data.frame(date = dates, values = values)
      
      year |> 
        dplyr::mutate(year = format(date, "%Y")) |> # get year from date
        group_by(year) |> 
        e_charts(date) |> 
        e_calendar(range = "2017",top="40") |> 
        e_calendar(range = "2018",top="260") |> 
        e_heatmap(values, coord_system = "calendar") |> 
        e_visual_map(max = 30) |> 
        e_title("Calendar", "Heatmap")|>
        e_tooltip("item") 
    })
    
    output$graph_time <- renderEcharts4r({
      
      df <- data.frame(
        x = seq(50),
        y = rnorm(50, 10, 3),
        z = rnorm(50, 11, 2),
        w = rnorm(50, 9, 2)
      )
      
      df |> 
        e_charts(x) |> 
        e_line(z) |> 
        e_area(w) |> 
        e_title("Line and area charts")
    })
    
    output$graph_category <- renderEcharts4r({
      
      mtcars |> 
        head() |> 
        tibble::rownames_to_column("model") |> 
        e_charts(model) |> 
        e_pie(carb, radius = c("50%", "70%")) |> 
        e_title("Donut chart")
      
    })

    
  })
}



