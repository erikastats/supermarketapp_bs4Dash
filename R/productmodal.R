

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
    verbatimTextOutput(ns("test_action")),
    useSweetAlert()
  )
  
  
}


# Server module -----------------------------------------------------------

product_server <-  function(id, r){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    # reactive values
    
    category_choices <-  reactiveVal({
      product_category |>
        pull(category) |>
        unique()
    })
    
    subcategory_choices <-  reactiveVal({
      
      product_category |>
        pull(subcategory) |>
        unique()
    })
    
    new_product <- reactive({
      tibble(product_name = c(input$p_name),
             product_category = c(input$p_category2),
             product_subcategory = c(input$p_subcategory2),
             product_is_favorite = c(input$p_favorite)
             )
    }) |>
      bindEvent(input$p_save)
    
    
    # Observers
    
    observeEvent(input$add_product,{
      
      showModal(modalDialog(
      title = "",
      fluidRow(
        column(width = 2,
               prettyToggle(inputId = "p_favorite",
                            icon_on = icon('star'),
                            icon_off = icon('star'),
                            status_on = "default",
                            status_of = "info",
                            label_on = "",
                            label_off = "",
                            value = FALSE
               ) ),
        column(width = 10,
               h2("New product"))
      ),
      column(width = 12,
             br(),
             textInput(ns("p_name"), label = "Add product name:" ),
             uiOutput(ns("p_category")),
             uiOutput(ns("p_subcategory")),
             br(),
             textOutput(ns("new_product"))),
      
      footer = tagList(
        modalButton("Close"),
        actionButton(ns("p_save"), "Save product")
      )
    ))} )
    
    observeEvent(input$p_category2, {
      newValue <- product_category |>
        filter(category %in% input$p_category2) |>
        pull(subcategory) |>
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
        text = "Product added with sucess!",
        type = "success"
      )
    })
    
    observe({
      
      req(nrow(new_product()) >0 )
      
      # current_favourites <- isolate(r$favourite_sports)
      
      r$product_data <- rbind(r$product_data, new_product())
    })
    
    # output

      output$p_category <- renderUI({

      pickerInput(
        inputId = ns("p_category2"),
        label = "Select product category",
        choices = category_choices(),
        selected = NULL,
        # placeholder = "Nothing selected",
        multiple = FALSE
      )
    })

    output$p_subcategory <-  renderUI({
      # validate(
      #   need(input$p_category2, 'Please choose a category first')
      # )
      pickerInput(
        inputId = ns("p_subcategory2"),
        label = "Select product subcategory",
        choices = subcategory_choices(),
        # placeholder = "Nothing selected",
        selected = NULL,
        multiple = FALSE
      )
    })
    
    output$test_action <- renderPrint({
      input$add_product
    })
    
    output$new_product <- renderText({
      validate(
        need(input$p_name, 'Please choose inform the product name'),
        need(input$p_category2, 'Please choose a category'),
        need(input$p_category2, 'Please choose a subcategory')
        
      )
      
      paste0("New product created: ", new_product()$product_name,
             ", category: ", new_product()$product_category,
             ", subcategory: ", new_product()$product_subcategory)
    })
      
  })
}
