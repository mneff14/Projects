### Test Info Buttons


library(shiny)
library(shinydashboard)



# Add element to tabpanel title with HTML
# Reference: https://stackoverflow.com/questions/26547589/insert-a-link-into-the-navbar-in-shiny

shinyApp(
  ui = navbarPage(
    title=HTML("<a href=\"http://stackoverflow.com\"> stackoverflow </a>"),
    tabPanel("tab1"),
    tabPanel(HTML("tab2</a></li><li><a href=\"http://stackoverflow.com\">stackoverflow"))
    
    
    
    # corner_element = htmltools::HTML(paste0('<a href=',shQuote(paste0("https://stackoverflow.com/questions/26547589/insert-a-link-into-the-navbar-in-shiny")), '>', 'Open Browser', '</a>')),
    # 
    # navbarPage(corner_element, id="page", collapsible=TRUE, inverse=FALSE,
    #            tabPanel("tab1", "Tab 1", "Contents")
    # )
    
  ),
  server = function(input, output) { }
)
      


      
      
# shinyApp(
#     ui = fluidPage(
#       
#         dashboardHeader(
#             tags$li(class = "dropdown", tags$a(href = "", class = "my_class", "Help", target="_blank")),
#             tags$li(class = "dropdown", tags$a(href = "", class = "my_class", "Contact us")),
#             tags$li(class = "dropdown", actionLink("ChangePassword", "Change Password", class = "my_class"))),
#         dashboardSidebar(),
#         dashboardBody(tags$head(
#             tags$style(HTML("
#                       .my_class {
#                       font-weight: bold;
#                       color:white;
#                       }"))
#         )),
#         title = "Dashboard example"
#     ),
#     server = function(input, output) { }
# )








# Try adding info button inside of Tabs
# Reference: https://stackoverflow.com/questions/35025145/background-color-of-tabs-in-shiny-tabpanel/43201952#43201952

# ui <- fluidPage(
#     titlePanel("Tabsets"),
#     
#     # Define class of styles
#     # tags$li(class = "nav nav-tabs", actionLink("info", "", icon = icon("question-circle"), class = "my_tabs")),
#     
#     tags$style(HTML("
#         .tabbable > .nav > li > a[data-value='Tab1'] {background-color: red;   color:white}
#         .tabbable > .nav > li > a[data-value='Tab2'] {background-color: blue;  color:white}
#         .tabbable > .nav > li > a[data-value='Tab3'] {background-color: green; color:white}
#     ")),
#     
#     # actionLink('info1', '', icon = icon('question-circle'))
#     
#     tabsetPanel(
#         tabPanel("Tab1", h2("Content 1")),
#         tabPanel("Tab2", h2("Content 2")),
#         tabPanel("Tab3", h2("Content 3"))
#     )
#     
#     
#     # tabsetPanel(
#     #     tabP
#     #     tags$ul(class = "nav nav-tabs", tags$li(href="", toggle="tab", value="t0")),
#     # )
# )
# 
# server <- function(input, output, session) {
# }
# 
# # Run the application
# shinyApp(ui = ui, server = server)








# Add tool tips in SHiny
# Reference: https://stackoverflow.com/questions/44953873/add-tooltip-to-tabs-in-shiny

# library(shinyBS)
# 
# shinyApp(
#     ui = tagList(
#         navbarPage(
#             theme = "cerulean",  # <--- To use a theme, uncomment this
#             "shinythemes",
#             tabPanel(id="test",span("Navbar 1",title="Test Title"),
#                      sidebarPanel(
#                          fileInput("file", "File input:"),
#                          textInput("txt", "Text input:", "general"),
#                          sliderInput("slider", "Slider input:", 1, 100, 30),
#                          tags$h5("Deafult actionButton:"),
#                          actionButton("action", "Search"),
#                          
#                          tags$h5("actionButton with CSS class:"),
#                          actionButton("action2", "Action button", class = "btn-primary")
#                      ),
#                      mainPanel(
#                          tabsetPanel(
#                              tabPanel(span("Tab 1", title="Test Title"),
#                                       h4("Table"),
#                                       tableOutput("table"),
#                                       h4("Verbatim text output"),
#                                       verbatimTextOutput("txtout"),
#                                       h1("Header 1"),
#                                       h2("Header 2"),
#                                       h3("Header 3"),
#                                       h4("Header 4"),
#                                       h5("Header 5")
#                              ),
#                              tabPanel("Tab 2"),
#                              tabPanel("Tab 3")
#                          )
#                      )
#             ),
#             tabPanel("Navbar 2"),
#             tabPanel("Navbar 3")
#         )
#     ),
#     server = function(input, output) {
#         output$txtout <- renderText({
#             paste(input$txt, input$slider, format(input$date), sep = ", ")
#         })
#         output$table <- renderTable({
#             head(cars, 4)
#         })
#     }
# )



