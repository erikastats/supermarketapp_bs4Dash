
supermarkets_choices <- supermarkets |>
  pull(supermarket_name) |>
  sort()

# UI module ---------------------------------------------------------------

grocery_ui <- function(id){
  ns <- NS(id)
  tagList(
        fluidRow(
          column(4,
                 airDatepickerInput(
            inputId = ns("grocery_date"),
            label = "Select a purchase date",
            maxDate = now(),
            inline = TRUE )
          ),
          column(4,
                 pickerInput(
                   inputId = ns("supermarket_chosen"),
                   label = "Select a supermarket", 
                   choices = supermarkets_choices,
                   selected = 'Tesco', 
                   options = list(
                     title = "Select a supermarket",
                     `live-search` = TRUE )
                 ),
                 uiOutput(ns("p_name")),
                 uiOutput(ns("p_category"))),
          column(4,
                 numericInput(ns("product_value"),
                              label = "Price per unit",
                              value = 0),
                
                 sliderInput(ns("product_quantity"),
                              "Product quantity",
                             min = 1, max = 50,
                              value = 1),
                 numericInput(ns("product_discount"),
                              label = "Product discount",
                              value = 0)
                 
                 
        ),
        actionBttn(ns("save"),
                   "Add product",
                   icon = icon("plus"))
        )
  )
}


# SERVER module -----------------------------------------------------------

grocery_server <- function(id, r, data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # reactive
    product_choices <- reactive( data() |>
      distinct(product_name, product_category,
               product_is_favorite) |>
        arrange(desc(product_is_favorite), product_name)
      )
    
    # output
    output$p_name <- renderUI({
       
      pickerInput(
        inputId = ns("product_name"),
        label = "Select the product", 
        choices = product_choices()$product_name,
        options = list(
          title = "Select a product",
          `live-search` = TRUE,
          subtext = ifelse(product_choices()$product_is_favorite,
                           "favorite", "") )
               )
    })
    
    output$p_category <- renderUI({
      req(input$product_name) 
      
      category_choices = product_choices() |>
        filter(product_name == input$product_name) |>
        pull(product_category)
      
      pickerInput(
        inputId = ns("product_category"),
        label = "Select product category",
        choices = category_choices,
        options = list(
          title = "Select category" )
        )
    })
    
    # Observe
    
    observeEvent(input$save, {
      
      # Update the reactive data frame in the main app
      new_row <- tibble(
        grocery_day = input$grocery_date,
        supermarket_name = input$supermarket_chosen,
        product_name = input$product_name,
        product_category = input$product_category,
        product_value = input$product_value,
        product_quantity = input$product_quantity,
        product_discount = input$product_discount,
        last_update_grocery = now()
      )
      
      r$gro_products <- bind_rows(r$gro_products, new_row)
    })
    
    
    #' next steps
    #' test what is already done
    #' print the table
    #' create a grocery_item_id
    #' create a button to exclude item
    #' create value box with count of groceries registered
    #' create value box with the newest and oldest date of groceries registered
    #' think on a third value box
    #' create a module to print table
    #' improve the distribution of the buttons
    
  })
}

