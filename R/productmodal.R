library(lubridate)

# UI module ---------------------------------------------------------------

product_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    actionBttn(
      inputId = ns("add_product"),
      label = "Add product",
      icon = icon("plus")
    ),
    useSweetAlert()
  )
  
  
}


# Server module -----------------------------------------------------------

product_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive values
    category_choices <- reactiveVal({
      product_category |>
        pull(category) |>
        sort() |>
        unique()
    })
    
    subcategory_choices <- reactiveVal({
      product_category |>
        pull(subcategory) |>
        sort() |>
        unique()
    })
    
    # Observers
    observeEvent(input$add_product, {
      showModal(modalDialog(
        title = "",
        fluidRow(
          column(width = 2,
                 prettyToggle(inputId = ns("p_favorite"),
                              icon_on = icon('star'),
                              icon_off = icon('star'),
                              status_on = "default",
                              status_off = "info",
                              label_on = "",
                              label_off = "",
                              value = FALSE
                 )),
          column(width = 10,
                 h2("New product"))
        ),
        column(width = 12,
               br(),
               textInput(ns("p_name"), label = "Add product name:"),
               uiOutput(ns("p_category")),
               uiOutput(ns("p_subcategory")),
               br(),
               textOutput(ns("new_product"))),
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("p_save"), "Save product")
        )
      ))
    })
    
    observeEvent(input$p_category2, {
      newValue <- product_category |>
        filter(category == input$p_category2) |>
        pull(subcategory) |>
        sort() |>
        unique()
      subcategory_choices(newValue)
    })
    
    all_fields_filled <- reactive({
      !is.null(input$p_name) && input$p_name != "" &&
        !is.null(input$p_category2) &&
        !is.null(input$p_subcategory2)
    })
    
    observe({
      toggleState <- all_fields_filled()
      updateActionButton(session, ns("p_save"), disabled = !toggleState)
    })
    
    observeEvent(input$p_save, {
      sendSweetAlert(
        session = session,
        title = "Success!",
        text = "Product added with success!",
        type = "success"
      )
      
      # Update the reactive data frame in the main app
      new_row <- tibble(
        product_name = input$p_name,
        product_category = input$p_category2,
        product_subcategory = input$p_subcategory2,
        product_is_favorite = input$p_favorite,
        last_update = now()
      )
      
      r$product_data <- bind_rows(r$product_data, new_row)
    })
    
    # Outputs
    output$p_category <- renderUI({
      pickerInput(
        inputId = ns("p_category2"),
        label = "Select product category",
        choices = category_choices(),
        selected = NULL,
        multiple = FALSE
      )
    })
    
    output$p_subcategory <- renderUI({
      pickerInput(
        inputId = ns("p_subcategory2"),
        label = "Select product subcategory",
        choices = subcategory_choices(),
        selected = NULL,
        multiple = FALSE
      )
    })
    
    output$test_action <- renderPrint({
      input$add_product
    })
    
    output$new_product <- renderText({
      paste0("New product created: ", input$p_name,
             ", category: ", input$p_category2,
             ", subcategory: ", input$p_subcategory2)
    })
  })
}



