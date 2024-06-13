library(lubridate)

# UI module ---------------------------------------------------------------

grocery_infobox_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4InfoBoxOutput(ns("latest_purchase")),
      bs4InfoBoxOutput(ns("count_of_purchases")),
      bs4InfoBoxOutput(ns("added_products"))
    )
  )
  
  
}


# Server module -----------------------------------------------------------

grocery_infobox_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive values
  
    
    # Observers
    
    # Output  
    
    output$latest_purchase <- renderbs4InfoBox({
      
      last_date <- data() |>
        summarise(last_date = max(grocery_day)) |> 
        mutate(last_date = format(last_date, "%d-%m-%Y") ) |>
        pull() |>
        as.character()
      
      bs4InfoBox(
        title = "Latest purchase data",
        value = last_date ,
        subtitle = NULL,
        icon = icon("calendar"),
        width = 4
      )
      
    })
    
    output$count_of_purchases <- renderbs4InfoBox({
      
      total_p = data() |> 
        distinct(grocery_day, supermarket_name) |>
        nrow()
      
      bs4InfoBox(
        title = "Total of purchases",
        value = total_p,
        subtitle = NULL,
        icon = icon("wallet"),
        width = 4
      )
      
    })
    
    output$added_products <-  renderbs4InfoBox({
      
      total_p_added <- data() |>
        summarise(total_produ = product_quantity 
                  |> sum()) |>
        pull()
      
      bs4InfoBox(
        title = "Total of products",
        value = total_p_added,
        subtitle = "Purchases until now",
        icon = icon("cart-shopping"),
        width = 4
      )
      
    })

    
  })
}



