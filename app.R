
# Libraries ---------------------------------------------------------------

library(shiny)
library(reactable)
library(dplyr)
library(tidyr)
library(bs4Dash)
library(shinyWidgets)
library(stringr)
library(tibble)


# Data --------------------------------------------------------------------

product_df <- tibble(p_name = NULL, type = NULL, subtype = NULL,
                     is_favorite = NULL)



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
                   column(6,
                          actionBttn(
                            inputId = "add_product",
                            label = "Add product",
                            icon = icon("plus")
                          )),
                   column(6,
                          actionBttn(
                            inputId = "exclude_product",
                            label = "Exclude product",
                            icon = icon("minus")
                          ))
                 ),
                 hr(),
                 br(),
                 reactableOutput("products_table", 
                                 width = 12 )
              
                 )
             ),
    tabItem( tabName = "g_register"),
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
  product_reactive <-  reactive({
    product_df
  })
  
  # Module
  
  # Events
  
  
}


# App ---------------------------------------------------------------------


shinyApp(ui = UI, server = SERVER)