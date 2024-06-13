
# Libraries ---------------------------------------------------------------

library(shiny)
library(reactable)
library(dplyr)
library(tidyr)
library(bs4Dash)
library(shinyWidgets)
library(stringr)
library(tibble)
library(purrr)
library(lubridate)
library(shinyFiles)


 
# Data --------------------------------------------------------------------

product_df <- readRDS("./Data/product_table.rds")

grocery_df <- readRDS("./Data/grocery_table.rds")


# Header ------------------------------------------------------------------


HEADER = dashboardHeader(
  title = "Grocery Journal"
)


# Sidebar -----------------------------------------------------------------

SIDEBAR = dashboardSidebar(
  id = "sidebar",
  dashboardBrand(
    title = "Grocery Journal",
    image = "https://www.academiedugout.fr/images/8479/1200-auto/brocolis_000.jpg"
  ),
  sidebarMenu(
    id = "sidebarMenu",
    menuItem( text = "Product", 
              tabName = "p_register",
              icon = icon("box-open")),
    menuItem("Grocery",
             tabName = "g_register",
             icon = icon("basket-shopping")),
    menuItem("Analysis",
             tabName = "analysis",
             icon = icon("chart-line"))
  )
  )


# Body --------------------------------------------------------------------

BODY  = dashboardBody(
  tabItems(
    tabItem( tabName = "p_register",
             h2("Product registration"),
             h4("Register a new product to use it to register your grocery shopping latter"),
             br(),
             br(),
             box(width = 12,
                  fluidRow(
                   column( 4, product_ui("add_product") ),
                   column( 4, product_exclude_ui("exclude_product") ),
                   column( 4, save_table_ui("save_product_table") )
                 ),
                 hr(),
                 br(),
                 reactableOutput("products_table")
                 )
             ),
    tabItem( tabName = "g_register",
             h2("Grocery registration"),
             h4("Here you´ll be able to register the grocery shop made at any time."),
             br(),
             br(),
             box(width = 12,
                 grocery_ui("product_gro") 
                 ),
             br(),
             grocery_infobox_ui('info_grocery'),
             box(width = 12,title = "New grocery product",
                 fluidRow(save_table_ui("save_grocery_table"),
                          product_exclude_ui("exclude_product_grocery")),
                 hr(),
                 reactableOutput("grocery_table"))
             
             ),
    tabItem( tabName = "analysis")
  )
)

# UI ----------------------------------------------------------------------

UI = dashboardPage(
  header = HEADER,
  sidebar = SIDEBAR,
  body = BODY,
  controlbar = dashboardControlbar(),
  title = "Grocery journal"
)

# Server ------------------------------------------------------------------

SERVER <- function(input, output, session){
  
  # reactive values
  
  r <- reactiveValues(product_data = tibble(
    product_name = character(),
    product_category = character(),
    product_subcategory = character(),
    product_is_favorite = logical(),
    last_update = ymd_hms(character())
  ))
  
  r2 <- reactiveValues( product_deleted = character()
                        )
  
  r3 <- reactiveValues(gro_products = tibble(
    grocery_day = ymd(character()),
    supermarket_name = character(),
    product_name = character(),
    product_category = character(),
    product_value = double(),
    product_quantity = integer(),
    product_discount = double(),
    last_update_grocery = ymd_hms(character())
  ))
  
  data_p <- reactive({
    product_df |> 
      bind_rows( r$product_data |>
                  mutate(p_id = pmap_chr(across(everything()),
                                         ~ paste(..., sep = "_"))) ) |>
      filter(!(p_id %in% r2$product_deleted) )
  })
  
  data_grocery <- reactive({
    grocery_df |>
      bind_rows(r3$gro_products |>
                  mutate(g_id = pmap_chr(across(everything()),
                                         ~ paste(..., sep = "_")))
                ) |>
      mutate(value_total = (product_value - product_discount)*product_quantity) |>
      filter(!(g_id %in% r2$product_deleted))
      
  })
  
  # Modules
  
  
  product_server("add_product", r)
  product_exclude_server("exclude_product",
                         r2,
                         reactive({data_p()$p_id}))
  save_table_server("save_product_table", reactive({data_p()}),
                    "./Data/product_table.rds", "product")
  
  grocery_server("product_gro", r3, reactive({data_p()}))
  save_table_server("save_grocery_table", reactive({data_grocery()}),
                    "./Data/grocery_table.rds", "grocery")
  product_exclude_server("exclude_product_grocery",
                         r2,
                         reactive({data_grocery()$g_id}))
  grocery_infobox_server("info_grocery", reactive({data_grocery()}))
  
  
  # Events
  
  # output
  output$products_table <- renderReactable({
    req(nrow(data_p()) > 0)
    data_p() |>
      arrange(desc(last_update)) |>
      select(-p_id, -product_is_favorite) |>
      reactable(
        searchable = TRUE,
        highlight = TRUE,
        paginationType = "simple",
        minRows = 10,
        defaultColDef = colDef(headerClass = "header", align = "left",
                               minWidth = 100,
                               headerStyle = list(fontWeight = "bold"),
                               footerStyle = list(fontWeight = "bold")),
        columns = list(
          product_name = colDef( name = "Product Name"),
          product_category = colDef(name = "Category"),
          product_subcategory = colDef(name = "Subcategory"),
          last_update = colDef(name = "Updated in")
        )
        
      )
  })
  
  
  output$grocery_table <- renderReactable({
    req(nrow(data_grocery()) > 0)
    
    data_grocery() |>
      arrange(desc(last_update_grocery)) |>
      select(-g_id, -last_update_grocery) |>
      reactable(
        searchable = TRUE,
        highlight = TRUE,
        paginationType = "simple",
        minRows = 10,
        defaultColDef = colDef(headerClass = "header", align = "left",
                               minWidth = 100,
                               headerStyle = list(fontWeight = "bold"),
                               footerStyle = list(fontWeight = "bold")),
        columns = list(
          grocery_day = colDef(name = "Purchase Date"),
          supermarket_name = colDef(name = "Supermarket"),
          product_name = colDef( name = "Product Name"),
          product_category = colDef(name = "Category"),
          product_value = colDef(name = "Price per unit"),
          product_quantity = colDef(name = "Quantity"),
          product_discount = colDef(name = "Discount"),
          value_total = colDef(name = "Total Cost"),
          last_update_grocery = colDef(name = "Updated in")
        )
        
      )
  })
  
}


# App ---------------------------------------------------------------------


shinyApp(ui = UI, server = SERVER)