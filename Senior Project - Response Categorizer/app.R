### RESPONSE CATEGORIZER APP
### - Michael Neff - 
###
### Categorizes uploaded responses into specified categories based on specified key words



#### Libraries ####

pacman::p_load(shiny,shinyjs,shinyBS,shinybusy, shinyalert,tibble,readr,haven,readxl,dplyr,ggplot2,ggthemes,
               tidyverse,rlist,stringr,colourpicker,stringi,topicmodels,tm,quanteda,slam)




##### ----- UI ----- #####

# Need to encase UI in a function to enable bookmarking the state
# Reference: https://shiny.rstudio.com/articles/bookmarking-state.html
ui <- function(request) {
  
  fluidPage(
    
    ##### INPUT FUNCTIONS #####
    
    useShinyjs(), # Set up shinyjs for clicking buttons with code
    useShinyalert(), # For pop-up message modals (Reference: https://deanattali.com/blog/shinyalert-package/)
    
    # Add Busy Spinner
    add_busy_spinner(
      spin = "semipolar",
      color = "#337ab7",
      timeout = 100,
      position = "top-right",
      onstart = FALSE,
      margins = c(10, 10), # first element is distance from top/bottom, second element distance from right/left.
      height = "50px",
      width = "50px"
    ),
    
    # Main Title
    h1(em("Survey Response Categorizer")),
    
    
    #### Sidebar Panel ####
    sidebarPanel(
      
      #### * Browse File Button ####
      wellPanel(
        h4(id = "import_title", "Import Responses"),
        fileInput("responses", "", 
                  buttonLabel = "Browse",
                  placeholder = "Excel / CSV",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".xlsx", 
                             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
        )
      ),
      
      
      #### * Help Section ####
      
      wellPanel(
        h4("Help"),
        br(),
        flowLayout(
          #### * * Main Info Action Link 
          actionLink("info_btn_main", "About", icon = icon("question-circle")),
          #### * * Watch Tutorials Action Link 
          actionLink("watch_tutorials", "Tutorials", icon = icon("graduation-cap")),
          #### * * Show Tooltips Checkbox 
          checkboxInput("show_tooltips", "Show Tooltips", value = TRUE)
        ),
      ),
      
      
      #### * Browse File Conditional Panel ####
      # These allow other categories to be added / removed when file is uploaded
      conditionalPanel(
        condition = "output.fileValid",
        
        #### * * Find Categories for Me Button ####
        actionButton("find_categories", "Find Categories for Me",
                     style = "color: #fff; background-color: #337ab7; border-color: #2d6a9f"),
        
        br(),
        br(),
        
        #### * * Add Category Button ####
        actionButton("ctgAdd", "Add Category"),
        
        #### * * Add Category Conditional Panel ####
        conditionalPanel(
          condition = "input.ctgAdd",
          wellPanel(
            
            #### * * * Default Category Name ####
            textInput("defaultName", "Default",
                      label = "Default Category Name"),
            
            #### * * * Remove Category Buttons Placeholder ####
            tags$div(id = "removeCtgButton")
            
          ),
          
          #### * * Multiple Categories Checkbox ####
          # Option to signify whether to have one or multiple categories per response
          checkboxInput("multCtgsPerResponse", 
                        label = "Multiple Categories per Response", value = FALSE),
          
          #### * * Category Placeholder ####
          # To hold all of the categories and their rules
          fluidRow(
            wellPanel(
              tags$div(id = "categories")
            )
          )
        )
      )
    ),
    
    flowLayout(
      #### Update Button ####
      actionButton("update", "Update"),
      #### Save (Bookmark) Button ####
      bookmarkButton(label = "Save", icon = icon("bookmark")
                     # , id = "bookmark_main_save"
                     # , title = ""
      )
    ),
    
    
    br(),
    
    #### Show Reactive Values ####
    wellPanel(
      h3("Reactive Values"),
      verbatimTextOutput("show_rvs", placeholder = TRUE),
      style = "height:500px; overflow-y: scroll; overflow-x: scroll;"
    ),
    
    
    #### Main Panel ####
    mainPanel(
      
      #### * Tutorial Pop-ups ####
      
      ## All Tutorial Videos ##
      ## (Contains Action Links to all tutorial video modals (pop-ups) with embedded videos
      bsModal("tutorials_all", "Tutorials",
              trigger = "watch_tutorials", size = "s",
              # Opening
              h5('Learn how to use the Survey Response Categorizer App by watching these tutorials!'),
              hr(),
              # Links to All Tutorial Videos
              flowLayout(
                wellPanel(
                  actionLink("tutorial_link_demo", "Demo", icon = icon("chalkboard-teacher") 
                             , onclick = "window.open('https://web.microsoftstream.com/video/fae9cf0e-0d4b-45d8-9b0f-278dd999403c')"
                  ),
                )
                # , wellPanel(
                #   actionLink("tutorial_link_", "Tutorial", icon = icon("video"))
                # )
              )
      ),
      
      
      #### * Info Button Pop-ups ####
      
      ## Help Section Info Button
      bsModal("info_popup_main", "About",
              trigger = "info_btn_main", size = "s",
              # Info Text
              tags$h5('This R Shiny App enables you to easily organize open-ended 
                    responses from a survey question into categories and visualize 
                    them for your reports.'),
              tags$h5('To watch a demo or other tutorials, click the "Tutorials" link.'),
              tags$h5('For quick information, tooltips will appear over buttons and input fields
                    while the "Show Tooltips" checkbox is checked.'),
              tags$h5('Happy Categorizing!')
      ),
      
      
      #### * Topic Finder Pop-up Window ####
      ## Reference: https://ebailey78.github.io/shinyBS/docs/Modals.html
      bsModal("topic_finder", "Topic Finder", 
              trigger = "find_categories", size = "l", # Large
              
              # Main Info
              # Reference for embedding link: https://stackoverflow.com/questions/42047422/create-url-hyperlink-in-r-shiny
              tagList(
                'The Topic Finder identifies a specific number of abstract topics across all responses using a process called Topic Modeling (see ',
                a("here", href = "https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf"),
                ' for full documentation).'
              ),
              br(),
              br(),
              
              #### * * TF Buttons ####
              fluidRow(
                column(width = 2,
                       # Go Button
                       actionButton("tf_go", "Go!",
                                    style = "color: #fff; background-color: #337ab7; border-color: #2d6a9f")
                ),
                column(width = 10,
                       conditionalPanel(
                         condition = "output.topicsFound",
                         # Convert Topics Button
                         actionButton("tf_convert_topics", "Convert Topics",
                                      style = "color: #fff; background-color: #17c217; border-color: #15ad15")
                       )
                )
              ),
              #### * * TF Initial Variables ####
              wellPanel(
                
                # Number of Topics to Find
                numericInput("tf_num_topics", "Number of Topics to Find",
                             value = 5, min = 2, max = 10),
                
                # Words not to Include
                textInput("tf_words_not_include", "Words Not to Include",
                          value = "for, as, when, you, also")
              ),
              br(),
              
              #### * * TF Results UIs ####
              conditionalPanel(
                condition = "output.topicsFound",
                
                # Adjust width of Tabs
                tags$style(HTML(".tabbable > .nav > li > a {width: 250PX;}")),
                
                # Results Tabset Panel
                tabsetPanel(id = "tabset_tf_results",
                            # Topics Tab
                            tabPanel(title = strong(id = "topics_tab", "Topics"),
                                     # Placeholder for Topic UIs
                                     tags$div(id = "tf_topics")
                            ),
                            tabPanel(title = strong(id = "details_tab", "Details"),
                                     # Details Tabs
                                     navbarPage(id = "tf_details", title = "",
                                                tabPanel("View Graph",
                                                         # Output for Details Plot
                                                         plotOutput("tf_details_plot", height = "500px")
                                                ),
                                                tabPanel("View Table",
                                                         # Output for Details Data Table
                                                         dataTableOutput("tf_details_tbl"),
                                                         # Add Vertical Scroll Bar
                                                         # Reference: https://stackoverflow.com/questions/47505893/adding-a-vertical-and-horizontal-scroll-bar-to-the-dt-table-in-r-shiny
                                                         style = "height:500px; overflow-y: scroll; overflow-x: scroll;"
                                                )
                                     )
                            )                          
                )           
              )
      ), 
      
      br(),
      
      #### * Update Conditional Panel ####
      # The Plot and Table tabsets will appear when a category has been added
      conditionalPanel(
        condition = "input.update",
        
        #### * * Plot and Table Tabset ####
        tabsetPanel(id = "tabset_main_results",
          
          #### * * * Plot Tab ####
          # Tab for the main plot and customizations
          tabPanel("Plot", br(), 
                   
                   #### * * * * Main Plot Output ####
                   plotOutput("plot"),
                   br(),
                   
                   #### * * * * Main Plot Conditional Panel ####
                   # Displays the UI's for Plot Customizations as long as a plot is displayed
                   conditionalPanel(
                     condition = "output.plotDisplayed",
                     
                     # Plot Customizations Panel
                     wellPanel(
                       h2("Plot Customizations"),
                       h4(em('Push "Update" to apply changes')),
                       tabsetPanel(id = "tabset_plot_customizations",
                         
                         #### * - - - - - Aesthetics Customizations ####
                         tabPanel("Aesthetics", br(), 
                                  fluidRow(
                                    column(width = 4,
                                           # Bar Position
                                           selectInput("aesPosition", label = "Position", selected = "Dodge 2", 
                                                       choices = c("Dodge 2","Dodge","Stack","Fill"))),
                                    column(width = 4,
                                           # Bar Width
                                           numericInput("aesBarWidth", label = "Bar Width", 
                                                        value = 0.5, min = 0.1, max = 20, step = 0.1)),
                                    column(width = 4,
                                           # Order of Bars
                                           selectInput("aesOrder", label = "Order", selected = "Decr (9->1)",
                                                       choices = c("Incr (A->Z)", "Decr (Z->A)",
                                                                   "Incr (1->9)", "Decr (9->1)")))
                                  ), br(),
                                  # Radio buttons to specify if graph colors will be uniform or individual
                                  radioButtons("aesOptions", label = "", inline = TRUE, 
                                               selected = "Uniform Bar Colors", 
                                               choices = c("Uniform Bar Colors","Individual Bar Colors")),
                                  # UI rendered depending on whether Uniform or Individual aesthetics is chosen
                                  uiOutput("aesUIs")
                         ),
                         
                         #### * - - - - - Labels Customizations ####
                         tabPanel("Labels", br(),
                                  fluidRow(
                                    # Options for Plot Labels
                                    column(width = 7,
                                           # Main Title
                                           textInput("mainTitle", "Main Title", "Survey Responses by Category"),
                                           # Option to include Sub-title
                                           checkboxInput("includeSubtitle", "Include Subtitle", value = FALSE),
                                           conditionalPanel(
                                             condition = "input.includeSubtitle",
                                             # Subtitle
                                             textInput("subtitle", "Subtitle", "")
                                           ),
                                           # X Label
                                           textInput("xTitle", "X Axis Title", "Category"),
                                           # Y Label
                                           textInput("yTitle", "Y Axis Title", "Number of Responses")
                                    ),
                                    # Options for Plot Labels
                                    column(width = 5,
                                           # Include data labels check box
                                           checkboxInput("includeDataLabels", "Include Data Labels"),
                                           conditionalPanel(
                                             condition = "input.includeDataLabels",
                                             # Nudge X
                                             numericInput("dataLabelNudgeX", "Nudge X", value = 0, 
                                                          min = -20, max = 20, step = 1),
                                             # Nudge Y
                                             numericInput("dataLabelNudgeY", "Nudge Y", value = 3, 
                                                          min = -20, max = 20, step = 1)
                                           )
                                    )
                                  )
                         ),
                         
                         #### * - - - - - Plot Customizations ####
                         tabPanel("Plot", br(),
                                  h4("To Save Plot:"),
                                  h5(em("Right click (Ctrl + Click for Macs) on plot and select 'Save Image'")),
                                  br(),
                                  fluidRow(
                                    column(width = 6,
                                           # Select a theme for the plot
                                           selectInput("selectTheme", label = "Select a Theme", selected = "gray", 
                                                       choices = c("base","bw","calc","classic","clean","dark",
                                                                   "economist","economist_white","excel","excel_new",
                                                                   "few","fivethirtyeight","foundation","gdocs","gray",
                                                                   "grey","hc","igray","light","linedraw","map",
                                                                   "minimal","pander","par","solarized","solarized_2",
                                                                   "solid","stata","test","tufte","void","wsj"))),
                                    column(width = 6,
                                           # Edit Grid Lines
                                           checkboxInput("editGridLines", "Edit Grid Lines", value = FALSE),
                                           conditionalPanel(
                                             condition = "input.editGridLines",
                                             # Edit All or Individual Grid Lines
                                             radioButtons("gridLinesToEdit", label = "Grid Lines to Edit",
                                                          selected = "All", choices = c("All","Individual"),
                                                          inline = TRUE),
                                             # Edit All Grid Lines
                                             conditionalPanel(
                                               condition = "input.gridLinesToEdit == 'All'",
                                               # Blank?
                                               checkboxInput("glAllBlank", "Blank", value = TRUE),
                                               conditionalPanel(
                                                 condition = "!input.glAllBlank",
                                                 # Color
                                                 colourpicker::colourInput("glAllColor", label = "Color", value = "black", 
                                                                           allowTransparent = TRUE, returnName = TRUE),
                                                 # Line Width
                                                 numericInput("glAllLineWidth", label = "Line Width", 
                                                              value = 0.5, min = 0, max = 10, step = 0.01),
                                                 # Line Type
                                                 selectInput("glAllLineType", label = "Line Type",
                                                             selected = "solid", 
                                                             choices = c("blank","solid","dashed","dotted",
                                                                         "dotdash","longdash","twodash")),
                                                 # Line End
                                                 selectInput("glAllLineEnd", label = "Line End",
                                                             selected = "square",
                                                             choices = c("square","round","butt"))
                                               )
                                             ),
                                             # Edit Individual Grid Lines
                                             conditionalPanel(
                                               condition = "input.gridLinesToEdit == 'Individual'",
                                               br(),
                                               # Edit All or Individual Major Grid Lines
                                               radioButtons("xGridLinesToEdit",label = "X-Axis Grid Lines to Edit",
                                                            selected = "Individual", choices = c("All","Individual"),
                                                            inline = TRUE),
                                               # Edit All X Grid Lines
                                               conditionalPanel(
                                                 condition = "input.xGridLinesToEdit == 'All'",
                                                 # Blank?
                                                 checkboxInput("glXAllBlank", "Blank", value = TRUE),
                                                 conditionalPanel(
                                                   condition = "!input.glXAllBlank",
                                                   # Color
                                                   colourpicker::colourInput("glXAllColor", label = "Color", value = "black", 
                                                                             allowTransparent = TRUE, returnName = TRUE),
                                                   # Line Width
                                                   numericInput("glXAllLineWidth", label = "Line Width", 
                                                                value = 0.5, min = 0, max = 10, step = 0.01),
                                                   # Line Type
                                                   selectInput("glXAllLineType", label = "Line Type",
                                                               selected = "solid", 
                                                               choices = c("blank","solid","dashed","dotted",
                                                                           "dotdash","longdash","twodash")),
                                                   # Line End
                                                   selectInput("glXAllLineEnd", label = "Line End",
                                                               selected = "square",
                                                               choices = c("square","round","butt"))
                                                 ),
                                                 hr()
                                               ),
                                               # Edit Individual Major Grid Lines
                                               conditionalPanel(
                                                 condition = "input.xGridLinesToEdit == 'Individual'",
                                                 # Edit Major X
                                                 checkboxInput("glEditMajorX", "Edit Major X", value = FALSE),
                                                 conditionalPanel(
                                                   condition = "input.glEditMajorX",
                                                   # Blank?
                                                   checkboxInput("glMajorXBlank", "Blank", value = TRUE),
                                                   conditionalPanel(
                                                     condition = "!input.glMajorXBlank",
                                                     # Color
                                                     colourpicker::colourInput("glMajorXColor", label = "Color", value = "black", 
                                                                               allowTransparent = TRUE, returnName = TRUE),
                                                     # Line Width
                                                     numericInput("glMajorXLineWidth", label = "Line Width", 
                                                                  value = 0.5, min = 0, max = 10, step = 0.01),
                                                     # Line Type
                                                     selectInput("glMajorXLineType", label = "Line Type",
                                                                 selected = "solid", 
                                                                 choices = c("blank","solid","dashed","dotted",
                                                                             "dotdash","longdash","twodash")),
                                                     # Line End
                                                     selectInput("glMajorXLineEnd", label = "Line End",
                                                                 selected = "square",
                                                                 choices = c("square","round","butt"))
                                                   ),
                                                   hr()
                                                 ),
                                                 # Edit Minor X
                                                 checkboxInput("glEditMinorX", "Edit Minor X", value = FALSE),
                                                 conditionalPanel(
                                                   condition = "input.glEditMinorX",
                                                   # Blank?
                                                   checkboxInput("glMinorXBlank", "Blank", value = TRUE),
                                                   conditionalPanel(
                                                     condition = "!input.glMinorXBlank",
                                                     # Color
                                                     colourpicker::colourInput("glMinorXColor", label = "Color", value = "black", 
                                                                               allowTransparent = TRUE, returnName = TRUE),
                                                     # Line Width
                                                     numericInput("glMinorXLineWidth", label = "Line Width", 
                                                                  value = 0.5, min = 0, max = 10, step = 0.01),
                                                     # Line Type
                                                     selectInput("glMinorXLineType", label = "Line Type",
                                                                 selected = "solid", 
                                                                 choices = c("blank","solid","dashed","dotted",
                                                                             "dotdash","longdash","twodash")),
                                                     # Line End
                                                     selectInput("glMinorXLineEnd", label = "Line End",
                                                                 selected = "square",
                                                                 choices = c("square","round","butt"))
                                                   )
                                                 ),
                                                 hr()
                                               ),
                                               # Edit All or Individual Minor Grid Lines
                                               radioButtons("yGridLinesToEdit",label = "Y-Axis Grid Lines to Edit",
                                                            selected = "Individual", choices = c("All","Individual"),
                                                            inline = TRUE),
                                               # Edit All Major Grid Lines
                                               conditionalPanel(
                                                 condition = "input.yGridLinesToEdit == 'All'",
                                                 # Blank?
                                                 checkboxInput("glYAllBlank", "Blank", value = TRUE),
                                                 conditionalPanel(
                                                   condition = "!input.glYAllBlank",
                                                   # Color
                                                   colourpicker::colourInput("glYAllColor", label = "Color", value = "black", 
                                                                             allowTransparent = TRUE, returnName = TRUE),
                                                   # Line Width
                                                   numericInput("glYAllLineWidth", label = "Line Width", 
                                                                value = 0.5, min = 0, max = 10, step = 0.01),
                                                   # Line Type
                                                   selectInput("glYAllLineType", label = "Line Type",
                                                               selected = "solid", 
                                                               choices = c("blank","solid","dashed","dotted",
                                                                           "dotdash","longdash","twodash")),
                                                   # Line End
                                                   selectInput("glYAllLineEnd", label = "Line End",
                                                               selected = "square",
                                                               choices = c("square","round","butt"))
                                                 )
                                               ),
                                               # Edit Individual Minor Grid Lines
                                               conditionalPanel(
                                                 condition = "input.yGridLinesToEdit == 'Individual'",
                                                 # Edit Major Y
                                                 checkboxInput("glEditMajorY", "Edit Major Y", value = FALSE),
                                                 conditionalPanel(
                                                   condition = "input.glEditMajorY",
                                                   # Blank?
                                                   checkboxInput("glMajorYBlank", "Blank", value = TRUE),
                                                   conditionalPanel(
                                                     condition = "!input.glMajorYBlank",
                                                     # Color
                                                     colourpicker::colourInput("glMajorYColor", label = "Color", value = "black", 
                                                                               allowTransparent = TRUE, returnName = TRUE),
                                                     # Line Width
                                                     numericInput("glMajorYLineWidth", label = "Line Width", 
                                                                  value = 0.5, min = 0, max = 10, step = 0.01),
                                                     # Line Type
                                                     selectInput("glMajorYLineType", label = "Line Type",
                                                                 selected = "solid", 
                                                                 choices = c("blank","solid","dashed","dotted",
                                                                             "dotdash","longdash","twodash")),
                                                     # Line End
                                                     selectInput("glMajorYLineEnd", label = "Line End",
                                                                 selected = "square",
                                                                 choices = c("square","round","butt"))
                                                   ),
                                                   hr()
                                                 ),
                                                 # Edit Minor Y
                                                 checkboxInput("glEditMinorY", "Edit Minor Y", value = FALSE),
                                                 conditionalPanel(
                                                   condition = "input.glEditMinorY",
                                                   # Blank?
                                                   checkboxInput("glMinorYBlank", "Blank", value = TRUE),
                                                   conditionalPanel(
                                                     condition = "!input.glMinorYBlank",
                                                     # Color
                                                     colourpicker::colourInput("glMinorYColor", label = "Color", value = "black", 
                                                                               allowTransparent = TRUE, returnName = TRUE),
                                                     # Line Width
                                                     numericInput("glMinorYLineWidth", label = "Line Width", 
                                                                  value = 0.5, min = 0, max = 10, step = 0.01),
                                                     # Line Type
                                                     selectInput("glMinorYLineType", label = "Line Type",
                                                                 selected = "solid", 
                                                                 choices = c("blank","solid","dashed","dotted",
                                                                             "dotdash","longdash","twodash")),
                                                     # Line End
                                                     selectInput("glMinorYLineEnd", label = "Line End",
                                                                 selected = "square",
                                                                 choices = c("square","round","butt"))
                                                   )
                                                 )
                                               )
                                             ),
                                             hr()
                                           ),
                                           # Edit Plot Background
                                           checkboxInput("editPlotBackground", "Edit Plot Background", value = FALSE),
                                           conditionalPanel(
                                             condition = "input.editPlotBackground",
                                             # Blank?
                                             checkboxInput("plotBackgroundBlank", "Blank", value = TRUE),
                                             conditionalPanel(
                                               condition = "!input.plotBackgroundBlank",
                                               # Fill
                                               colourpicker::colourInput("plotBackgroundFill", label = "Fill", 
                                                                         allowTransparent = TRUE, returnName = TRUE),
                                               # Color
                                               colourpicker::colourInput("plotBackgroundColor", label = "Color", value = "black", 
                                                                         allowTransparent = TRUE, returnName = TRUE),
                                               # Line Width
                                               numericInput("plotBackgroundLineWidth", label = "Line Width", 
                                                            value = 0.5, min = 0, max = 10, step = 0.01),
                                               # Line Type
                                               selectInput("plotBackgroundLineType", label = "Line Type",
                                                           selected = "solid", 
                                                           choices = c("blank","solid","dashed","dotted",
                                                                       "dotdash","longdash","twodash"))
                                             ),
                                             hr()
                                           ),
                                           # Edit Panel Background
                                           checkboxInput("editPanelBackground", "Edit Panel Background",value = FALSE),
                                           conditionalPanel(
                                             condition = "input.editPanelBackground",
                                             # Blank?
                                             checkboxInput("panelBackgroundBlank", "Blank", value = TRUE),
                                             conditionalPanel(
                                               condition = "!input.panelBackgroundBlank",
                                               # Fill
                                               colourpicker::colourInput("PanelBackgroundFill", label = "Fill", 
                                                                         allowTransparent = TRUE, returnName = TRUE),
                                               # Color
                                               colourpicker::colourInput("panelBackgroundColor", label = "Color", value = "black", 
                                                                         allowTransparent = TRUE, returnName = TRUE),
                                               # Line Width
                                               numericInput("panelBackgroundLineWidth", label = "Line Width", 
                                                            value = 0.5, min = 0, max = 10, step = 0.01),
                                               # Line Type
                                               selectInput("panelBackgroundLineType", label = "Line Type",
                                                           selected = "solid", 
                                                           choices = c("blank","solid","dashed","dotted",
                                                                       "dotdash","longdash","twodash"))
                                             ),
                                             hr()
                                           ),
                                           # Edit Panel Border
                                           checkboxInput("editPanelBorder", "Edit Panel Border", value = FALSE),
                                           conditionalPanel(
                                             condition = "input.editPanelBorder",
                                             # Blank?
                                             checkboxInput("panelBorderBlank", "Blank", value = TRUE),
                                             conditionalPanel(
                                               condition = "!input.panelBorderBlank",
                                               # Color
                                               colourpicker::colourInput("panelBorderColor", label = "Color", value = "black", 
                                                                         allowTransparent = TRUE, returnName = TRUE),
                                               # Line Width
                                               numericInput("panelBorderLineWidth", label = "Line Width", 
                                                            value = 0.5, min = 0, max = 10, step = 0.01),
                                               # Line Type
                                               selectInput("panelBorderLineType", label = "Line Type",
                                                           selected = "solid", 
                                                           choices = c("blank","solid","dashed","dotted",
                                                                       "dotdash","longdash","twodash"))
                                             )
                                           ))
                                  )
                         )
                       )
                     )
                   )
          ),
          
          #### * * * Table Tab ####
          tabPanel("Table", br(), 
                   # Table showing which responses are in which category
                   dataTableOutput("table"),
                   # Add Vertical Scroll Bar
                   # Reference: https://stackoverflow.com/questions/47505893/adding-a-vertical-and-horizontal-scroll-bar-to-the-dt-table-in-r-shiny
                   style = "height:500px; overflow-y: scroll; overflow-x: scroll;")
        )
      ),   
      br(),
      br(),
      br(),
      br()
    )
  )
}




##### ----- SERVER ----- #####

server <- function(input, output, session) {
  
  #### Reactive Values ####
  
  ## A reactive values object whose variables can be created, altered, and removed by any function in server
  rv <- reactiveValues(
    file_is_valid = FALSE,
    all_categories = data.frame(
      id_add = character(),
      ctg_id = character(),
      ctg_num_rules = integer(),
      ctg_add_rule_button_id = character(),
      remove_ctg_id = character(),
      remove_ctg_button_id = character(),
      ctg_name_id = character(),
      ctg_name = character()
    ),
    all_rules = data.frame(
      id_add_rule = character(),                
      rule_id = character(),                
      rule_ctg_id = character(),                
      remove_rule_id = character(),                
      remove_rule_button_id = character(),                
      rule_keywords_id = character(),                
      rule_keywords = character(),                
      rule_sort_options_id = character(),  
      rule_sort_options = character(),                
      rule_standardize_id = character(),                
      rule_standardize = logical()                
    ),
    num_topics = 0,
    topics_to_add = data.frame(
      t = integer(),
      title = character(),
      terms = character(),
      ctg_id = character(),
      rule_id = character(),
      buttons_pushed = logical()
      )
  )

  
  
  #### onBookmark() ####
  ## Store all reactive values and other things when saving to state (bookmarking)
  ## Reference: https://shiny.rstudio.com/articles/advanced-bookmarking.html
  onBookmark(function(state) {
    # Save all reactive values
    state$values$all_reactive_values <- rv
    
    # Show the sharable URL modal
    showBookmarkUrlModal
  })
  
  
  #### onRestore() ####
  ## Read in all reactive values and other things when restoring a saved (bookmarked) state
  ## Reference: https://shiny.rstudio.com/articles/advanced-bookmarking.html
  onRestore(function(state) {
    
    # Reload all reactive values
    rv <- state$values$all_reactive_values
    
    # print(rv)
  })
  
  
  #### setBookmarkExclude() ####
  ## Excludes buttons and id's from being saved (bookmarked) and
  ## prevents the from running when bookmark is loaded
  ## Reference: https://shiny.rstudio.com/articles/advanced-bookmarking.html
  setBookmarkExclude(c("tf_go","tf_convert_topics"))
  # setBookmarkExclude(c("update","tf_go","tf_convert_topics"))
  
  
  
  #### getData() ####
  ## Reads in file
  getData <- reactive({
  
    inFile <- input$responses
    # Display nice error message if file hasn't been uploaded or is an unaccepted type
    validate(
      need(!is.null(input$responses), "No File Loaded"),
      need(inFile$type %in% c("text/csv", "text/comma-separated-values,text/plain", ".xlsx", 
                              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"), 
           "Please upload an Excel or CSV file with 1 column containing survey responses"),
      need(try(!is.null(readxl::read_excel(inFile$datapath, col_names = "Responses"))), 
           "Please upload an Excel or CSV file with 1 column containing survey responses")
    )
    # Read in and return the file
    data <- readxl::read_excel(inFile$datapath, col_names = "Responses")
    data <- data.frame(data, stringsAsFactors = FALSE)
    return(data)
  })

  
  #### sortData_GG() ####
  ## Sorts the data
  sortData_GG <- reactive({

    ## Initiate a Progress Bar
    withProgress(message = "Initiating", min = 0, max = 1, value = 0, {
      
      ## Retrieve the Imported Data
      data <- getData()
      
      ## Skip if data is NULL or no Categories were added
      if (is.null(data) | !input$ctgAdd) {
        return(NULL)
      }
      
      ## Read in reactive value data frames containing current data for all categories and rules
      all_categories <- rv$all_categories 
      all_rules <- rv$all_rules
      
      ## Initial Variables
      N <- nrow(all_categories) # Number of categories
      num_responses <- nrow(data) # Number of responses
      num_rules <- nrow(all_rules) # Number of rules
      default_ctg_name <- paste0(input$defaultName)
      multiple_categories_per_response <- input$multCtgsPerResponse # If false, will assign one ctg per response
      
      ## Add a new Default Category column to the variables data
      default_column = data.frame(matrix(nrow = num_responses, ncol = 1)) %>% 
        `colnames<-`(default_ctg_name)
      default_column[,1] <- 1 # Responses are by default in the default category
      data <- bind_cols(data, default_column)

      ## Function that returns true if keyword and match_value match according to sort_option
      check_match <- function(keyword = NA, match_value = NA, standardize = TRUE, sort_option = "Contains") {
        ## Variable to be returned
        found_match <- FALSE
        ## Make response and keyword lower-case if standardize is true
        if (!is.na(match_value) & keyword != "" & !is.na(keyword)) { 
          if (standardize) {
            match_value <- str_to_lower(match_value)
            keyword <- str_to_lower(keyword)
          } 
          
          ## Change matching pattern (if needed) according to sort option
          pattern <- keyword # Default is "Exactly"
          if (sort_option == "Begins With") {
            # Change pattern to regex to search for begins with
            pattern <- paste0("^",keyword)
          } else {
            if (sort_option == "Ends With") {
              pattern <- paste0(keyword,"$")
            } 
          }
          ## Check for an exact match
          if (sort_option == "Exactly") {
            if (match_value == keyword) {
              # They match!
              found_match <- TRUE
            }
          } else {
            ## Check for any other match
            if (str_detect(match_value, pattern)) {
              # They match!
              found_match <- TRUE
            } 
          }
        }
        # Function returns TRUE if there was a match
        return(found_match)
      }
      
      ## Calculate the number of steps and increment amount for the Progress bar
      ## Number of Steps  = Number of Total Categories + Number of Last Steps
      ## Increment Amount = 1 / Number of Steps
      num_progress_steps <- N + 1
      progress_inc_amt <- 1 / num_progress_steps
      
      
      ## Loop through each current Category
      ## Will add a new column to the response data if at least one rule has been added
      ## (Note: "data" will not update if any loop besides "for" is used)
      for (c in 1:N) {
        
        # Initial Current Category variables (for convenience and readability)
        # Each of these will be distinguished with a "."
        ctg.id <- all_categories$ctg_id[c]
        ctg.num_rules <- all_categories$ctg_num_rules[c]
        ctg.name <- paste0(all_categories$ctg_name[c])

        # Increment Progress Bar
        incProgress(progress_inc_amt, message = paste0("Matching Category: ", ctg.name))
        
        # Proceed if this category has any rules
        if (ctg.num_rules > 0) {
          
          # Create a new Category column to track whether each response is assigned to this category
          new_column = data.frame(matrix(nrow = num_responses, ncol = 1)) %>%
            `colnames<-`(ctg.name) # Name column after Category's current name
          new_column[,1] <- 0 # Values are set to zero (rather than FALSE) by default
          
          # Retrieve all rule data for this category
          ctg.all_rules <- all_rules %>% 
            filter(rule_ctg_id == ctg.id) %>% 
            mutate(Num_Matches = 0) # For tracking number of matches as loop through new_column
          
          # Loop through the new column
          # Will increase the value if any match is found in the adjacent response text
          new_column[,1] <- sapply(1:nrow(new_column), function(new_col_row_idx) {
            
            # Will skip categorizing response only if: 
            # - Multiple Categories per Response is False AND
            # - Adjacent response has already been categorized
            #   (Is categorized if there's a one in another column other than the default)
            if (!multiple_categories_per_response && sum(data[new_col_row_idx,2:ncol(data)]) > 1) {
              
              # Return a zero
              return(0)
              
            } else {
              
              # Initial New Column Variables
              current_response_text <- data[new_col_row_idx,1]
              
              # Loop through each Category Rule
              # Will update Num_Matches with number of matches found in the response text
              ctg.all_rules$Num_Matches <- sapply (1:nrow(ctg.all_rules), function(rule_idx) {
                
                # Initial Rule variables
                # Each of these will be distinguished with a "." for readability
                rule.sort_option <- ctg.all_rules$rule_sort_options[rule_idx]
                rule.standardize <- ctg.all_rules$rule_standardize[rule_idx]
                rule.keywords <- ctg.all_rules$rule_keywords[rule_idx] %>% 
                  stri_extract_all_words() %>% # Split the keywords into list of words
                  as.data.frame() %>%          # Convert to data frame
                  `colnames<-`("Keyword") %>%  # Change the column name
                  mutate(Num_Matches = 0)      # For tracking number of matches for each Rule's keywords
                
                # Loop through each Rule Keyword
                # Will update the Number of matches for each keyword
                rule.keywords$Num_Matches <- sapply (1:nrow(rule.keywords), function(keyword_idx) {
                  
                  # Initial Variables
                  current_keyword <- rule.keywords$Keyword[keyword_idx]
                  
                  # Proceed if the keyword is not blank
                  if (!is.na(current_keyword)) {
                    
                    # Sort Option  (check_match())
                    #   - CONTAINS:     Match_value contains keyword
                    #   - EXACTLY:      Match_value equals keyword
                    #   - BEGINS WITH:  Match_value begins with keyword
                    #   - ENDS WITH:    Match_value ends with keyword
                    
                    # Proceed based on Multiple CTG's per Response
                    #   - TRUE:  Count ALL times the keyword appears in the response text
                    #   - FALSE: Count the FIRST time the keyword matches the response text
                    if (multiple_categories_per_response) {
                      
                      # Split the response text into individual words
                      current_response_words <- current_response_text %>%
                        stri_extract_all_words() %>%   # Split the response into list of words
                        as.data.frame() %>%            # Convert to data frame
                        `colnames<-`("Response_Word")  # Change column name
                      
                      # Count the number of times the keyword matches across all response words,
                      # following the sort option
                      num_matches <- sum(apply(current_response_words, 1, function(word) {
                        check_match(keyword = current_keyword, 
                                    match_value = word, 
                                    standardize = rule.standardize, 
                                    sort_option = rule.sort_option)
                      }))
                      
                      return(num_matches)
                      
                    } else {
                      
                      # Check if the keyword matches the full response text, following the sort option
                      match_found <- check_match(keyword = current_keyword, 
                                                 match_value = current_response_text, 
                                                 standardize = rule.standardize, 
                                                 sort_option = rule.sort_option)
                      
                      # Return 1 if a match was found, 0 if not
                      return(as.integer(match_found))
                    }
                    
                  } else { return(0) } # Return no matches found if keyword is NA
                })
                
                # Return the number of Rule matches (total matches found from each keyword)
                return( sum(rule.keywords$Num_Matches) )
              })
              
              # Change current new_column row value to sum of all matches if multiple responses is checked.
              # Otherwise, change to 1 if any match was found and 0 if not.
              return( if_else(multiple_categories_per_response, 
                              sum(ctg.all_rules$Num_Matches), 
                              as.integer(sum(ctg.all_rules$Num_Matches) > 0)) )
            }
          })
          
          # Add the new column to the responses data
          data <- bind_cols(data, new_column)
          
        } else { } # There are no rules for this category, so nothing happens
      }
      
      # Increment Progress Bar
      incProgress(progress_inc_amt, message = "Finishing / Plotting...")
      
      ## Set Default Column values to 0 if the response is assigned to any other Category
      ## (Need to exclude the response text in apply() so sum() will work)
      data[,2] <- apply(data %>% select(-c("Responses")), 1, function(data_row) {
        if_else(sum(data_row) > 1, 0, 1) # Sum every column, except the first
      })
      
      ## Return the responses data with all added Category columns
      return(data)
    })
})
  
  
  
  
  #### plotGG() ####
  ## Will plot the ggplot to show how many responses are in which category
  plotGG <- reactive({
    
    ## Get the responses data sorted into categories
    data <- sortData_GG()  
    
    ## Skip if data is null or no categories have been made
    if (is.null(data) | !input$ctgAdd) {
      return(NULL)
    }
    
    ## Variables
    num_columns <- ncol(data)
    aesthetics <- NA
    barWidth <- input$aesBarWidth
    # Position variable 
    position <- switch (
      EXPR = input$aesPosition,
      "Dodge" = "position_dodge()",
      "Stack" = "position_stack()",
      "Fill"  = "position_fill()",
      "position_dodge2()"
    )
    # Order variable
    order <- switch (
      EXPR = input$aesOrder,
      "Incr (1->9)" = "reorder(Category, n)",
      "Decr (9->1)" = "reorder(Category, desc(n))",
      "Decr (Z->A)" = "reorder(Category, desc(Category))",
      "Category" # Increasing (A->Z)
    )
    
    ## Create plotting table 
    ## Transform into a table with two columns:
    ##  - Category (name of each column except the first (contains responses text))
    ##  - n        (total number of responses/mentions for each category)
    ##  - id       (each category's id for convenience in filling in the aes row)
    # Remove the Responses column
    data.categories <- data %>% 
      select(-1)
    # Create the table for plotting
    plot_data <- data.frame(
      Category = colnames(data.categories),
      n = colSums(data.categories)
    ) 
    # Add the id for each column
    #  - Default Column: id = "default"
    #  - Otherwise:      id = ctg_id from rv$all_categories
    col_ids <- lapply(1:nrow(plot_data), function(col_idx) {
      ifelse(col_idx == 1, # The Default Column is always the first row in plot-data
              "default",
              rv$all_categories$ctg_id[col_idx-1])
    })
    plot_data$id <- col_ids
    
    ## A function that fills all columns of any one row in aesthetics with passed arguments
    ## (Uses the default category and NA's as the default variables)
    fill_aes_row <- function(ctgInputID = "default_aes_indiv", ctg_name = input$defaultName) {
      
      # Create variables
      fill <- "transparent"
      color <- "transparent"
      lineWidth <- 0
      lineType <- "blank"
      input_no_fill <- eval(parse(text = paste0("input$", ctgInputID, "_no_fill")))
      input_fill <- eval(parse(text = paste0("input$", ctgInputID, "_fill")))
      input_no_color <- eval(parse(text = paste0("input$", ctgInputID, "_no_color")))
      input_color <- eval(parse(text = paste0("input$", ctgInputID, "_color")))
      input_lineWidth <- eval(parse(text = paste0("input$", ctgInputID, "_line_width")))
      input_lineType <- eval(parse(text = paste0("input$", ctgInputID, "_line_type")))
      
      # Change variables based on input values (if not NULL)
      if (!is.null(input_fill) && !is.null(input_no_fill)) {
        if (input_no_fill == FALSE) {
          fill <- input_fill
        }
      }
      if (!is.null(input_color) && !is.null(input_no_color)) {
        if (input_no_color == FALSE) {
          color <- input_color
          if (!is.null(input_lineWidth)) {
            lineWidth <- input_lineWidth
          }
          if (!is.null(input_lineType)) {
            lineType <- input_lineType
          }
        }
      }
      
      # Create a blank row to be returned
      new_aes <- data.frame(matrix(nrow = 1, ncol = 5)) %>% 
        `colnames<-`(c("category","fill","color","lineWidth","lineType"))
      
      # Replace specified row in new_aes with previously defined variables, if the category exists
      new_aes[1,1] <- ctg_name
      new_aes[1,2] <- fill
      new_aes[1,3] <- color
      new_aes[1,4] <- lineWidth
      new_aes[1,5] <- lineType

      return(new_aes)
    }
    
    ## Fill the aestetic values based on plot_data
    # Aestetic values (by Category) for the graph
    aesthetics <- as.data.frame(matrix(ncol = 5, # Category, Fill, Color, Line Width, Line Type, Bar Width
                                      nrow = num_columns - 1)) %>% # Number of Categories (including Default)
      `colnames<-`(c("category","fill","color","lineWidth","lineType"))
    
    # Loop through each category added and fill the aesthetics data frame
    for (c in 1:nrow(plot_data)) {
      
      # Category variables (Default category by default)
      ctg_id <- plot_data$id[c]
      ctg_name <- plot_data$Category[c]
      
      # Create aesthetic id for retrieving aesthetics
      aes_id <- "aes_uniform" # Uniform by default
      if (input$aesOptions == "Individual Graph aesthetics") {
        aes_id <- paste0(ctg_id, "_aes_indiv")
      } 
      
      # Fill aesthetics row with function
      aesthetics[c,] <- fill_aes_row(ctgInputID = aes_id, ctg_name = ctg_name)
    }
    
    View(aesthetics)

    ## Plot the data according to non-conditional user inputs
    plot <- ggplot(plot_data) +
      labs(title = input$mainTitle, x = input$xTitle, y = input$yTitle) + 
      eval(parse(text = paste0("theme_", input$selectTheme, "()"))) # The theme of the plot
    
    ## Add Conditional Aesthetics
    # Add the base plot with aesthetics, reordering as specified
    plot <- plot + geom_bar(aes(x = eval(parse(text = order)), y = n, 
                                fill = Category, 
                                color = Category, 
                                size = Category, 
                                linetype = Category),
                            width = barWidth,
                            stat = "identity",
                            position = eval(parse(text = position)), show.legend = FALSE)
    # Aestetic variables
    scale_fill_values <- NA
    scale_color_values <- NA
    scale_lineWidth_values <- NA
    scale_lineType_values <- "blank"
    # Add graph aesthetics
    if (!is.na(aesthetics[1,1])) {
      # Turn the vectors of chars in aesthetics into a single string separated by a comma
      scale_fill_values <- aesthetics$fill
      scale_color_values <- aesthetics$color
      scale_lineWidth_values <- aesthetics$lineWidth
      scale_lineType_values <- aesthetics$lineType
      
      # Create scale_ functions using variables to add to the plot
      plot <- plot + scale_fill_manual(values = c(scale_fill_values)) 
      plot <- plot + scale_color_manual(values = c(scale_color_values)) 
      plot <- plot + scale_size_manual(values = c(scale_lineWidth_values)) 
      plot <- plot + scale_linetype_manual(values = c(scale_lineType_values))
    }
    
    ## Implement additional conditional customizations
    ## Subtitle
    if (input$includeSubtitle) {
      # Add subtitle
      plot <- plot + labs(subtitle = input$subtitle)
    }
    ## Data Labels
    if (input$includeDataLabels) {
      # Add customized data labels
      plot <- plot + geom_text(aes(x = Category, y = n, label = n),
                               nudge_x = input$dataLabelNudgeX,
                               nudge_y = input$dataLabelNudgeY, show.legend = FALSE)
    }
    ## Grid Lines
    if (input$editGridLines) {
      # New theme elements to be changed conditionally
      new_all_grid_lines <- element_blank()
      new_all_x <- element_blank()
      new_all_y <- element_blank()
      new_major_x <- element_blank()
      new_minor_x <- element_blank()
      new_major_y <- element_blank()
      new_minor_y <- element_blank()
      # Edit all grid lines
      if (input$gridLinesToEdit == "All") {
        # Change new all grid lines theme element if blank is not selected
        if (!input$glAllBlank) {
          new_all_grid_lines <- element_line(color = input$glAllColor, size = input$glAllLineWidth, 
                                             linetype = input$glAllLineType, lineend = input$glAllLineEnd)
        }
        # Add new all grid lines theme element to plot
        plot <- plot + theme(panel.grid = new_all_grid_lines)
        
      } else {
        # Edit individual grid lines
        if (input$gridLinesToEdit == "Individual") {
          # Edit all X-Axis grid lines
          if (input$xGridLinesToEdit == "All") {
            # Change all x-axis theme element if blank is not selected
            if (!input$glXAllBlank) {
              new_all_x <- element_line(color = input$glXAllColor, size = input$glXAllLineWidth, 
                                        linetype = input$glXAllLineType, lineend = input$glXAllLineEnd)}
            # Add new all x-axis theme element
            plot <- plot + theme(panel.grid.major.x = new_all_x, 
                                 panel.grid.minor.x = new_all_x)
          } else {
            # Edit individual X-Axis grid lines
            if (input$xGridLinesToEdit == "Individual") {
              # Major X
              if (input$glEditMajorX) {
                # Edit if blank is not selected
                if (!input$glMajorXBlank) {
                  new_major_x <- element_line(color = input$glMajorXColor, size = input$glMajorXLineWidth, 
                                              linetype = input$glMajorXLineType, lineend = input$glMajorXLineEnd)
                }
                # Add new grid line theme element to the plot
                plot <- plot + theme(panel.grid.major.x = new_major_x)
              }
              # Minor X
              if (input$glEditMinorX) {
                # Edit if blank is not selected
                if (!input$glMinorXBlank) {
                  new_minor_x <- element_line(color = input$glMinorXColor, size = input$glMinorXLineWidth, 
                                              linetype = input$glMinorXLineType, lineend = input$glMinorXLineEnd)
                }
                # Add new grid line theme element to the plot
                plot <- plot + theme(panel.grid.minor.x = new_minor_x)
              }
            }
          }
          # Edit all Y-Axis grid lines
          if (input$yGridLinesToEdit == "All") {
            # Change all y-axis theme element if blank is not selected
            if (!input$glYAllBlank) {
              new_all_y <- element_line(color = input$glYAllColor, size = input$glYAllLineWidth, 
                                        linetype = input$glYAllLineType, lineend = input$glYAllLineEnd)}
            # Add new all y-axis theme element
            plot <- plot + theme(panel.grid.major.y = new_all_y, 
                                 panel.grid.minor.y = new_all_y)
          } else {
            # Edit individual X-Axis grid lines
            if (input$yGridLinesToEdit == "Individual") {
              # Major Y
              if (input$glEditMajorY) {
                # Edit if blank is not selected
                if (!input$glMajorYBlank) {
                  new_major_y <- element_line(color = input$glMajorYColor, size = input$glMajorYLineWidth, 
                                              linetype = input$glMajorYLineType, lineend = input$glMajorYLineEnd)
                }
                # Add new grid line theme element to the plot
                plot <- plot + theme(panel.grid.major.y = new_major_y)
              }
              # Minor Y
              if (input$glEditMinorY) {
                # Edit if blank is not selected
                if (!input$glMinorYBlank) {
                  new_minor_y <- element_line(color = input$glMinorYColor, size = input$glMinorYLineWidth, 
                                              linetype = input$glMinorYLineType, lineend = input$glMinorYLineEnd)
                }
                # Add new grid line theme element to the plot
                plot <- plot + theme(panel.grid.minor.y = new_minor_y)
              }
            }
          }
        }
      }
    }
    ## Plot Background
    if (input$editPlotBackground) {
      # Either element_blank or element_rect
      new_plot_background <- element_blank()
      if (!input$plotBackgroundBlank) {
        new_plot_background <- element_rect(fill = input$plotBackgroundFill, color = input$plotBackgroundColor,
                                            size = input$plotBackgroundLineWidth, 
                                            linetype = input$plotBackgroundLineType)}
      # Add new theme element to the plot
      plot <- plot + theme(plot.background = new_plot_background)
    }
    ## Panel Background
    if (input$editPanelBackground) {
      # Either element_blank or element_rect
      new_panel_background <- element_blank()
      if (!input$panelBackgroundBlank) {
        new_panel_background <- element_rect(fill = input$panelBackgroundFill, color = input$panelBackgroundColor,
                                             size = input$panelBackgroundLineWidth, 
                                             linetype = input$panelBackgroundLineType)}
      # Add new theme element to the plot
      plot <- plot + theme(panel.background = new_panel_background)
    }
    ## Panel Border
    if (input$editPanelBorder) {
      # Either element_blank or element_rect
      new_panel_border <- element_blank()
      if (!input$panelBorderBlank) {
        new_panel_border <- element_rect(fill = NA, color = input$panelBorderColor,
                                         size = input$panelBorderLineWidth, 
                                         linetype = input$panelBorderLineType)}
      # Add new theme element to the plot
      plot <- plot + theme(panel.border = new_panel_border)
    }

    return(plot)

  })
  
  
  #### plotTable() ####
  ## Will plot a table of the data
  plotTable <- reactive({
    ## Get the responses data sorted into categories
    data <- sortData_GG() 
    
    
    ## Skip if data is null or no categories have been made
    if (is.null(data) | !input$ctgAdd) {
      return(NULL)
    }  
    
    ## Re-structure the data to be plotted as a table
    table_data <- data %>% 
      as.data.frame()
    

    ## Plot the table of the data
    return(table_data)

  })
  
  
  
  
  #### find_topics() ####
  ## Uses LDA models to automatically find topics in the responses 
  find_topics <- reactive({
    
    # Start the progress bar
    # Reference: https://shiny.rstudio.com/articles/progress.html
    withProgress(message = "Finding Topics", value = 0, {
      
      ### Initialize Progress Bar
      num_steps <- 7
      incr_val <- 1/num_steps # How far to increment the progress bar at each step
      incProgress(incr_val, detail = "Reading in Data") # Increments the progress bar
      
      
      ### Read in the Data
      
      df <- read_excel("E:/mneff/Desktop/College Stuff/Spring 2019/SRC/R Reports/HCQuestions.xlsx")
      
      # Increment Progress Bar
      incProgress(amount = incr_val, detail = "Pre-processing the data")
      
      
      ### Pre-Process the Data
      ### Reference: https://stackoverflow.com/questions/55832844/remove-words-from-a-dtm
      
      # Retrieve the words not to be included in the models
      words_to_remove <- input$tf_words_not_include %>% 
        str_split(pattern = ",") %>% 
        unlist() %>% 
        str_trim(side = "both")
      
      # Convert column to Corpus (collection of "documents")
      corpus <- corpus(df, text_field = "Responses")
      
      # Create a Document Term (Feature) Matrix from the corpus
      #   (Each response becomes its own document split into words.) 
      dtm <- dfm(corpus, stem = TRUE, 
                 remove = stopwords("english"),
                 remove_punct = TRUE) %>% 
        dfm_trim(min_termfreq = 3) %>% # Remove rare terms
        dfm_remove(words_to_remove) %>% # Remove words specified by user
        convert(to = "tm") # Convert back to 'tm' library object for fitting
      
      # Remove very frequent and in-frequent terms
      #   (Will calculate the mean term frequency-inverse document frequency (tg-idf)
      #    of each term across documents and keep terms with a tf-idf of at least 0.1,
      #    which is close to the median.
      #    Then the documents will be filtered by the tf-idf to ensure steady frequency)
      # Reference: p.12 of https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf    
      term_tfidf <-
        tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
        log2(nDocs(dtm)/col_sums(dtm > 0))
      dtm <- dtm[,term_tfidf >= 0.1]
      dtm <- dtm[row_sums(dtm) > 0,]
      
      # Increment Progress Bar
      incProgress(amount = incr_val, detail = "Fitting LDA Model")
      
      
      ### Fit the models
      ### Reference: p.13 of https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf
      
      # Initial parameters
      k <- input$tf_num_topics
      SEED <- as.integer(Sys.time())
      
      # Fit several models for comparison
      #   Both models use a VEM procedure for calculating the Maximum Likelihood
      #   (This is a Variable version of the EM algorithm. It iterates between
      #    an (E)xpectation step and a (M)aximization step to calculate parameters
      #    that the EM algorithm cannot.) 
      #   See section 2.2 of https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf 
      models <- list()
      
      # Latent Dirichlet Allocation (LDA) Model (see p.1)
      #   Topics are assumed to be uncorrelated
      #   Can use the VEM or Gibbs algorithm
      models[["LDA"]] <- LDA(dtm, k = k, method = "VEM",
                             control = list(seed = SEED))
      # Increment Progress Bar
      incProgress(amount = incr_val, detail = "Fitting CTM Model")
      
      # Correlated Topic Model (CTM) (see p.2)
      #   Topics are assumed to be correlated
      #   Only uses the VEM algorithm
      #   Set a larger tolerance for the relative change in the likelihood
      models[["CTM"]] <- CTM(dtm, k = k,
                             control = list(seed = SEED,
                                            var = list(tol = 10^-4), 
                                            em  = list(tol = 10^-3)))
      # Increment Progress Bar
      incProgress(amount = incr_val, detail = "Selecting 'Best' Model")
      
      
      ### Select the "best" model
      ### Reference: p.14 of https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf
      
      # Calculate the mean entropy for each model.
      #   This measures how much the model's topics are distributed across the documents.
      #   The lower the entropy, the more precise and useful the predicted topics are.
      entropies <- sapply(models, function(x)
        mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))
      
      # Save the "best" model (the one with the lowest mean entropy)
      # Reference: https://stackoverflow.com/questions/21422188/how-to-get-name-from-a-value-in-an-r-vector-with-names
      best_model <- models[[names(entropies)[entropies == min(entropies)]]]
      
      # Increment Progress Bar
      incProgress(amount = incr_val, detail = "Retrieving Final Results")
      
      
      ### Get Final Results
      
      # Retrieve the best model's top terms for each topic
      top_terms <- as.data.frame( terms(best_model, 25) ) 
      
      # Retrieve and clean the best model's most likely topic for each response
      response_topics <- as.data.frame( topics(best_model) ) %>% 
        rownames_to_column("Response") %>% 
        rename(Topic = 'topics(best_model)')
      response_topics$Response <- as.integer(str_remove(response_topics$Response, "text"))
      
      # Add original response text to the response topics
      response_topics$Text <- unlist( sapply(response_topics$Response, function(r) df[r,1]) )
      
      # Increment Progress Bar
      incProgress(amount = incr_val, message = "Done", detail = "")
      
      # Return the results in a vector
      return( c(top_terms, response_topics) )    })
  })
  
  
  
  
  #### updatePushed_plotGG() ####
  ## Calls and returns return value of plotGG when "Update" button is pushed
  updatePushed_plotGG <- eventReactive(input$update, {
    # Saves graph of the data to reactive values
    return(plotGG())
  })
  
  
  #### updatePushed_plotTable() ####
  ## Calls and returns return value of plotTable when "Update" button is pushed
  updatePushed_plotTable <- eventReactive(input$update, {
    # Plots the table of the data
    return(plotTable())
  })
  
  
  
  
  #### Observe Event -- Add Category ####
  ## Adds the Category Name text input and Add Rule button when the Add Category button is pushed
  observeEvent(input$ctgAdd, {
    
    ## Make dynamic id's for all category UI's and the category itself
    id_add <- paste0(input$ctgAdd)
    ctg_id <- paste0("ctg_", id_add)
    ctg_name_id <- paste0(ctg_id, "_name")
    ctg_add_rule_button_id <- paste0(ctg_id, "_add_rule")
    remove_ctg_id <- paste0("remove_ctg", id_add)
    remove_ctg_button_id <- paste0("remove_ctg_button_", id_add)
    
    ## Define Initial Variables
    ctg_init_name <- paste0("Category ", id_add)
    if (nrow(rv$topics_to_add) > 0) {
      # Insert Topic's Title if new Category is a topic to be converted
      ctg_init_name <- rv$topics_to_add$title[rv$topics_to_add$ctg_id == ctg_id]
    }
    
    
    
    #### * Insert Category UI's ####
    insertUI("#categories",
             "beforeEnd",
             tags$div(
               # Has this id for removal
               id = ctg_id,
               # Category Title
               h3(paste0("Category ", id_add)),
               # The category name
               textInput(ctg_name_id, 
                         ctg_init_name, 
                         label = paste0("Category ", id_add, " Name")),
               # Layout for the Add and Remove Rules action buttons
               fluidRow(
                 column(width = 6,
                        # Button to Add Rules
                        actionButton(ctg_add_rule_button_id, "+ Rule")),
                 
                 column(width = 6,
                        # Placeholder for the Remove Rules buttons
                        tags$div(id = paste0(ctg_id, "_remove_rule_buttons")))
               ),
               # Placeholder for all cateogry rule UI's
               wellPanel(
                 # Make original div to insert 1+ rules for the category
                 tags$div(id = paste0(ctg_id, "_rules"))
               )
             ), immediate = TRUE
    )
    
    ## Insert Remove Category Button ##
    insertUI(
      selector = "#removeCtgButton",
      ui = tags$div(
        id = remove_ctg_id,
        # Check box to signify which category to remove
        actionButton(remove_ctg_button_id, paste0("Remove Category ", id_add))
      )
    )
    
    
    #### * Update Category RV's ####
    
    ## Append new category id's and initial values to reactive value data frame
    rv$all_categories = bind_rows(rv$all_categories,
                                  data.frame(
                                    id_add = id_add,
                                    ctg_id = ctg_id,
                                    remove_ctg_id = remove_ctg_id,
                                    ctg_num_rules = 0,
                                    ctg_add_rule_button_id = ctg_add_rule_button_id,
                                    remove_ctg_button_id = remove_ctg_button_id,
                                    ctg_name_id = ctg_name_id,
                                    ctg_name = ctg_init_name
                                  )    
    )
    
    ## Save other values to rv for inserting new Topics
    rv$topics_to_add$rule_id[rv$topics_to_add$ctg_id == ctg_id] <- ctg_add_rule_button_id
    
    ## Update Category Name in rv data frame when changed
    observeEvent(input[[ctg_name_id]], {
      rv$all_categories$ctg_name[rv$all_categories$ctg_id == ctg_id] <- input[[ctg_name_id]]
    })
    

    
    #### * Add/Remove All Category Tooltips ####
    observeEvent(input$show_tooltips,  {
      if (input$show_tooltips) {
        ## Add Category + Rule Button Tooltip
        addPopover(session, id = ctg_add_rule_button_id, trigger = "hover", 
                   title = "+ Rule Info",
                   content = "
                 When multiple Rules are created for a Category, a response must 
                 meet all of the Rules' criteria to be assigned to that Category.
                 ")
      } else {
        
        # Remove All Category Tooltips
        removePopover(session, id = ctg_add_rule_button_id)
      }
    })
    
    
    
    #### * Observe Event -- Remove Category ####
    ## Removes all elements in category when remove category button is pushed
    observeEvent(input[[remove_ctg_button_id]], {
      # Removes all ui's within the category
      removeUI(
        selector = paste0("#", ctg_id)
      )
      # Remove the remove button
      removeUI(
        selector = paste0("#", remove_ctg_id)
      )
      # Remove category from the Category rv data frame
      rv$all_categories <- rv$all_categories[!(rv$all_categories$ctg_id == ctg_id), ]
      
    }, ignoreInit = TRUE, once = TRUE)
    
    
    
    #### * Observe Event -- Add Rule ####
    ## Add all rule UI's when category Add Rule button is pushed
    observeEvent(input[[ctg_add_rule_button_id]], {
      ## Make id's for rule UI's and the rule itself
      id_add_rule <- paste0(input[[ctg_add_rule_button_id]])
      rule_id <- paste0("rule_", id_add_rule, ctg_id)
      rule_ctg_id <- ctg_id
      remove_rule_id <- paste0("remove_rule_", id_add_rule, ctg_id)
      remove_rule_button_id <- paste0("remove_rule_button_", id_add_rule, ctg_id)
      rule_keywords_id <- paste0(rule_id, "_keywords")
      rule_sort_options_id <- paste0(rule_id, "_sort_options")
      rule_standardize_id <- paste0(rule_id, "_standardize")

      ## Define Initial Variables
      rule_init_keywords <- ""
      if (nrow(rv$topics_to_add) > 0) {
        # Insert Topic's Title if new Category is a topic to be converted
        rule_init_keywords <- rv$topics_to_add$terms[rv$topics_to_add$ctg_id == ctg_id]
      }
      rule_choices_sort_options <- c("Exactly","Contains","Begins With","Ends With")
      rule_init_sort_options <- "Contains"
      rule_init_standardize <- TRUE
      
      
      #### * * Insert Rule UI's ####
      insertUI(immediate = TRUE,
               selector = paste0("#", ctg_id, "_rules"),
               ui = tags$div(
                 # Has this id for removal
                 id = rule_id,
                 # Rule Title
                 h4(paste0("Rule ", id_add_rule)),
                 # Key words text input
                 textInput(rule_keywords_id,
                           value = rule_init_keywords,
                           label = paste0("Keywords for Rule ", id_add_rule)),
                 # Sorting Options list
                 selectInput(rule_sort_options_id,
                             label = "Sorting Options",
                             choices = rule_choices_sort_options,
                             selected = rule_init_sort_options,
                             width = 120 ),
                 # Standardize check box that will, when checked, set all keywords and responses to lowercase
                 checkboxInput(rule_standardize_id, 
                               label = "Standardize All to Lowercase", 
                               value =  rule_init_standardize),
                 hr()
               )
      )
      
      ## Insert Remove Rule Button
      insertUI(immediate = TRUE,
               selector = paste0("#", ctg_id, "_remove_rule_buttons"),
               ui = tags$div(
                 # For removing the button
                 id = remove_rule_id,
                 # The Remove Rule button
                 actionButton(remove_rule_button_id, paste0("- Rule ", id_add_rule))
               )
      )
      
    
      #### * * Update Rule RV's ####
      
      ## Append new rule id's and initial values to reactive value data frame
      rv$all_rules = bind_rows(rv$all_rules,
                                    data.frame(
                                      id_add_rule = id_add_rule,
                                      rule_id = rule_id,
                                      rule_ctg_id = rule_ctg_id,
                                      remove_rule_id = remove_rule_id,
                                      remove_rule_button_id = remove_rule_button_id,
                                      rule_keywords_id = rule_keywords_id,
                                      rule_keywords = rule_init_keywords,
                                      rule_sort_options_id = rule_sort_options_id,
                                      rule_sort_options = rule_init_sort_options,
                                      rule_standardize_id = rule_standardize_id,
                                      rule_standardize = rule_init_standardize
                                    )
      )
      
      ## Increase number of rules in Category rv data frame
      rv$all_categories$ctg_num_rules[rv$all_categories$ctg_id == ctg_id] <- rv$all_categories$ctg_num_rules[rv$all_categories$ctg_id == ctg_id] + 1
      
      ## Update Keywords in rv data frame when changed
      observeEvent(input[[rule_keywords_id]], {
        rv$all_rules$rule_keywords[rv$all_rules$rule_id == rule_id] <- input[[rule_keywords_id]]
      })
      
      ## Update Sort Options in rv data frame when changed
      observeEvent(input[[rule_sort_options_id]], {
        rv$all_rules$rule_sort_options[rv$all_rules$rule_id == rule_id] <- input[[rule_sort_options_id]]
      })
      
      ## Update Standardize in rv data frame when changed
      observeEvent(input[[rule_standardize_id]], {
        rv$all_rules$rule_standardize[rv$all_rules$rule_id == rule_id] <- input[[rule_standardize_id]]
      })
      
      
      #### * * Add/Remove All Rule Tooltips ####
      observeEvent(input$show_tooltips, {
        if (input$show_tooltips) {
          
          ## Add Rule Keywords Tooltip
          addPopover(session, id = rule_keywords_id, trigger = "hover", 
                     title = "Keywords Info",
                     content = "
                 All Keywords will be compared to each word in every response’s 
                 text. If they “match”, that response will be assigned to this 
                 Rule’s Category. All words can be separated by any delimiter, 
                 but whitespaces will be removed. (Ex: . , | ; : )                 
                 ")
          
          ## Add Rule Sorting Options Tooltip
          addPopover(session, id = rule_sort_options_id, trigger = "hover", 
                     title = "Sorting Options Info",
                     content = "
                 Determines how the Keywords will “match” each word in a response’s text.
                 ")
          
          ## Add Rule Standardize Tooltip
          addPopover(session, id = rule_standardize_id, trigger = "hover", 
                     title = "Standardize Info",
                     content = "
                 If checked, makes all Keywords and response’s words lowercase before comparing.
                 ")
          
        } else {
          
          # Remove All Rule Tooltips
          removePopover(session, id = rule_keywords_id)
          removePopover(session, id = rule_sort_options_id)
          removePopover(session, id = rule_standardize_id)
        }
      })
      
      
      
      #### * * Observe Event -- Remove Rule ####
      ## Removes all elements in rule when remove rule button is pushed
      observeEvent(input[[remove_rule_button_id]], {
        # Removes all ui's within the rule
        removeUI(
          selector = paste0("#", rule_id)
        )
        # Remove the remove button
        removeUI(
          selector = paste0("#", remove_rule_id)
        )
        # Remove rule from the Rule rv data frame
        rv$all_rules <- rv$all_rules[!(rv$all_rules$rule_id == rule_id), ]
        
        # Decrease number of rules in Category rv data frame
        rv$all_categories$ctg_num_rules[rv$all_categories$ctg_id == ctg_id] <- rv$all_categories$ctg_num_rules[rv$all_categories$ctg_id == ctg_id] - 1
        
      }, ignoreInit = TRUE, once = TRUE)
      
    })
  })
  
  
  #### Observe Event -- Find Topics (Go!) ####
  ## Calls find_topics() when the Topic Finder "Go!" button is pushed.
  ## Returns TRUE if topics were found
  observeEvent(input$tf_go, {
    
    # Call function to find topics
    results <- find_topics()
    n <- input$tf_num_topics # The number of topics to find
    
    # Make sure topics were found and proper results are saved
    if (is.null(results)) {
      # Save NULL to reactive values and return FALSE
      rv[["topics"]] <- NULL
      rv[["response_topics"]] <- NULL
      rv[["num_topics"]] <- 0
      
    } else {
      #### * Remove All Existing Topic UIs #### 
      ## Need to do this before updating the number of topics found
      lapply(1:rv$num_topics, function (t)
        removeUI(paste0("#topic_tag_",t))
      )
      
      # Save results to reactive values and return TRUE
      rv[["topics"]] <- results[1:n] # The first n list items are the topics
      rv[["response_topics"]] <- results[n+1:length(results)] # The rest are response topics
      rv[["num_topics"]] <- n
      
      # Convert response topics from list to data frame for Results Tabs
      rv$response_topics <- data.frame(
        Response = rv$response_topics$Response,
        Topic = rv$response_topics$Topic,
        Text = rv$response_topics$Text
      ) 
      
      #### * Insert New Topic UIs ####
      lapply(1:n, function(t) {
        
        # Initial Variables
        topic <- rv$topics[t]
        topic_id <- paste0("topic_", t)
        topic_tag_id <- paste0("topic_tag_", t)
        topic_name <- names(topic)
        topic_terms <- str_c(unlist(topic[1])[1:10], collapse = " | ")
        topic_checkbox_id <- paste0("topic_checkbox_", t)
        topic_title_id <- paste0("topic_title_", t)
        topic_terms_id <- paste0("topic_terms_", t)
        topic_num_terms_id <- paste0("topic_num_terms_", t)
        
        # Insert the New UIs ana Placeholders
        insertUI("#tf_topics", "beforeBegin",
                 tags$div(
                   id = topic_tag_id,
                   wellPanel(
                     fluidRow(
                       column(width = 1,
                              # Convert to Category Checkbox
                              checkboxInput(topic_checkbox_id, label = "")
                       ),
                       column(width = 3,
                              # Topic Title Textbox
                              textInput(topic_title_id, label = "", 
                                        value = topic_name)
                       ),
                       column(width = 6,
                              # Terms Text Output
                              # Need this here so user can change the number of terms to show
                              # without having to run the model again
                              textOutput(topic_terms_id)
                       ),
                       column(width = 2,
                              # Number of Terms to Show
                              numericInput(topic_num_terms_id, "Terms",
                                           value = 10, min = 1, max = 25)
                       )
                     )
                   )
                 )
        )
        
        # Render the Topic's Terms (default is first 10)
        output[[topic_terms_id]] <- renderText({
          rv[[topic_terms_id]] <- topic_terms # Save as a reactive value
          return(topic_terms)
        })
        
        #### * * Observe Event -- Update Number of Terms Showing ####
        ## Updates the number of terms to show whenever the topic's input changes
        observeEvent(input[[topic_num_terms_id]], {
          
          num_terms <- input[[topic_num_terms_id]]
          
          # Do nothing if input is not a number
          if (!is.numeric(num_terms)) {  } else {
            
            # Proceed if input is greater than zero
            if (num_terms > 0) {
              
              # Retrieve and format the new number of terms
              new_topic_terms <- str_c(unlist(topic[1])[1:input[[topic_num_terms_id]]], 
                                       collapse = " | ")
              
              # Render the Topic's New Terms
              output[[topic_terms_id]] <- renderText({
                rv[[topic_terms_id]] <- new_topic_terms # Save as a reactive value
                return(new_topic_terms)
              })          
            }
          }
        })
      })
    }
  })
  
  
  #### Observe Event -- Convert Topics ####
  ## Adds a new Category "manually" for each selected topic
  observeEvent(input$tf_convert_topics, {
    
    # Initial variables
    num_topics_checked <- 0
    n <- rv$num_topics # Number of topics
    current_ctg_id <- input$ctgAdd + 1
    prg_incr <- 1 / n # How much to increment the Progress bar
    
    # Start the Progress Bar
    withProgress(message = "Converting Topics to Categories", value = 0, {
      
      # Loop through each topic
      for (t in 1:n) {
      # lapply(1:n, function(t) { # This breaks it!!
        
        # Proceed only if the topic is currently selected (checked)
        if (input[[paste0("topic_checkbox_", t)]] == TRUE) {
          
          # Create New Category 
          # (by pushing existing action buttons and updating the created UI)
          if (rv$file_is_valid == TRUE) {
            
            ## Without Local
            
            # Retrieve topic info (from id's)
            topic_title_id <- paste0("topic_title_", t)
            topic_terms_id <- paste0("topic_terms_", t)
            title <- input[[topic_title_id]]
            terms <- rv[[topic_terms_id]]
            new_ctg_id <- paste0("ctg_", current_ctg_id)

            # Click the 'Add Category' button
            click("ctgAdd", asis = TRUE)

            # Save variables to data frame
            new_topic_to_add <- data.frame(
              t = t,
              title = title,
              terms = terms,
              ctg_id = new_ctg_id,
              rule_id = "",
              buttons_pushed = FALSE
            )

            # Add new topic variables to data frame reactive value
            rv$topics_to_add <- bind_rows(rv$topics_to_add, new_topic_to_add)

            # Increment counter
            current_ctg_id <- current_ctg_id + 1

            
            ## With Local Doesn't work very well!
            
          } else {
            # No file imported
          }
          # Increment counter
          num_topics_checked <- num_topics_checked + 1
          
        } else { } # Nothing happens
        
        # Increment Progress Bar
        incProgress(amount = prg_incr)
        
      }
      
      # Show pop-up message to select topics before pushing button
      if (num_topics_checked == 0) { 
        shinyalert(title = "Oops!",
                   text = "Please select (check) at least one topic to convert.",
                   type = "error") 
      }
    })
  })
  
  
  #### Observe Event -- New Topic to Convert Added ####
  ## Will finish converting a newly added topic to a category
  observeEvent(rv$topics_to_add, ignoreInit = TRUE, {
    
    # Loop through each topic to add and finish conversion, if applicable
    for (t in 1:nrow(rv$topics_to_add)) {

      # Check if "+ Rule" button has been created and hasn't been pushed yet
      if (str_length(rv$topics_to_add$rule_id[t]) > 0 & 
          rv$topics_to_add$buttons_pushed[t] == FALSE) {
        
        
        # Push the "+ Rule" button
        click(rv$topics_to_add$rule_id[t], asis = TRUE)
        
        # Make sure the "+ Rule" button isn't pushed twice
        rv$topics_to_add$buttons_pushed[t] <- TRUE
        

      } else { } # Nothing Happens
    }
  })
  
  
  #### Observe Event -- Show Tooltips ####
  ## Add tooltips only if checkbox is checked
  observeEvent(input$show_tooltips, {
    if (input$show_tooltips) {
      
      #### * Add Tooltips ####
      
      # # Template Popover
      # addPopover(session, id = "", trigger = "hover",
      #            title = " Info",
      #            content = "
      #            
      #            ")
      
      # Import Responses Popover
      addPopover(session, id = "import_title", trigger = "hover",
                 title = "Importing Info",
                 content = "
                 Allows you upload a CSV or Excel file. You can either: 
                 1) Browse for the file from your local computer by pressing the “Browse” button or 
                 2) Drag and drop the file next to the button.
                 ")
      
      # Add Category Popover
      addPopover(session, id = "ctgAdd", trigger = "hover",
                 title = "Category Info",
                 content = "
                 A Category is a general theme that survey responses can belong to. 
                 Each Category has a name and Rules to control how/when responses would 
                 fall under it.                  
                 ")
      
      # Find Categories For Me Popover
      addPopover(session, id = "find_categories", trigger = "hover",
                 title = "Find Categories for Me Info",
                 content = "
                 A great place to start if you don’t know what categories to add.
                 Opens the Topic Finder window.
                 ")
      
      # Number of Topics to Find Popover
      addPopover(session, id = "tf_num_topics", trigger = "focus", 
                 title = "Number of Topics Info",
                 content = "
                 For the machine learning models to work, you must first specify
                 how many abstract topics there might be across all responses. The minimum 
                 is 2, and the maximum is 10.
                 ")
      
      # Words Not to Include Popover
      addPopover(session, id = "tf_words_not_include", trigger = "focus", 
                 title = "Words Not to Include Info",
                 content = "
                 These words will be taken out of all responses before running the 
                 machine learning models. Each word/phrase MUST be separated by a comma!
                 ")
      
      # Convert Topics Button Popover
      addPopover(session, id = "tf_convert_topics", trigger = "hover",
                 title = "Convert Topics Info",
                 content = "
                 When pushed, each selected (checked) topic will be added as a new 
                 category. A new category and rule will be created “manually”, and 
                 the topic’s title and terms will be copied over.                 
                 ")
      
      # Topics TabPanel Popover
      addPopover(session, id = "topics_tab", trigger = "hover", placement = "top",
                 title = "Topics Tab Info",
                 content = "
                 This tab shows each predicted topic with its most relevant terms. 
                 The first listed term is the most relevant to the topic. 
                 When selected, a topic can be converted to a category 
                 (hover over “Convert Topics” button for details). 
                 ")
      
      # Topics TabPanel Popover
      addPopover(session, id = "details_tab", trigger = "hover", placement = "top",
                 title = "Details Tab Info",
                 content = "
                 This tab shows the details of each predicted topic with a graph and
                 a table. “View Graph” shows how many responses have each topic as 
                 its best match, and the “View Table” shows the full text of each 
                 response with its assigned (a.k.a. best matched) topic.
                 ")
      
      # Default Category Popover
      addPopover(session, id = "defaultName", trigger = "hover",
                 title = "Default Category Info",
                 content = "
                 Every response will, by default, belong to this category, 
                 unless assigned to any other Category.
                 ")
      
      # Multiple Categories Per Response Popover
      addPopover(session, id = "multCtgsPerResponse", trigger = "hover",
                 title = "Multiple Categories Per Response Info",
                 content = '
                 If selected, all times any category is mentioned in each response 
                 will be counted. If not selected, each response will be assigned 
                 to only the first category it "matches".
                 ')
      
      # # Plot Customizations Popover
      # addPopover(session, id = "", trigger = "hover",
      #            title = " Info",
      #            content = "
      #            
      #            ")
      
      # # Plot Customizations Popover
      # addPopover(session, id = "", trigger = "hover",
      #            title = " Info",
      #            content = "
      #            
      #            ")
      
      # # Plot Customizations Popover
      # addPopover(session, id = "", trigger = "hover",
      #            title = " Info",
      #            content = "
      #            
      #            ")
    
    } else {
      
      #### * Remove Tooltips ####
      
      removePopover(session, id = "find_categories")
      removePopover(session, id = "ctgAdd")
      removePopover(session, id = "tf_num_topics")
      removePopover(session, id = "tf_words_not_include")
      removePopover(session, id = "import_title")
      removePopover(session, id = "tf_convert_topics")
      removePopover(session, id = "topics_tab")
      removePopover(session, id = "details_tab")
      removePopover(session, id = "defaultName")
      removePopover(session, id = "multCtgsPerResponse")
      removePopover(session, id = "")
    }
  })
  

  
  
  #### OUTPUT FUNTIONS ####
  
  
  
  
  #### Event Reactive -- fileValid ####
  ## Returns true if uploaded file is an accepted type
  output$fileValid <- eventReactive(input$responses, {
    is_valid <- !is.null(getData())
    
    rv$file_is_valid <- is_valid # Save as a reactive value
    return(is_valid)
  })
  outputOptions(output, 'fileValid', suspendWhenHidden = FALSE)

  
  #### Event Reactive -- categoriesAdded ####
  ## Returns true if categories have been added
  output$categoriesAdded <- eventReactive(input$update, {
    ifelse(input$ctgAdd > 0, return(TRUE), return(FALSE))    
  })
  outputOptions(output, 'categoriesAdded', suspendWhenHidden = FALSE)
  
  
  #### Event Reactive -- plotDisplayed ####
  ## Returns true if plot has been created
  output$plotDisplayed <- eventReactive(input$update, {
    return(!is.null(plotGG()))
  })
  outputOptions(output, 'plotDisplayed', suspendWhenHidden = FALSE)
  
  
  #### Event Reactive -- tableDisplayed ####
  ## Returns true if table has been created
  output$tableDisplayed <- eventReactive(input$update, {
    return(!is.null(plotTable()))
  })
  outputOptions(output, 'tableDisplayed', suspendWhenHidden = FALSE)
  
  
  #### Event Reactive -- topicsFound ####
  ## Makes sure the results are shown when topics are found
  output$topicsFound <- eventReactive(rv$num_topics, {
    # Returns true if any topics were found (as updated by Go! observe event)
    return(rv$num_topics > 0) 
  })
  outputOptions(output, 'topicsFound', suspendWhenHidden = FALSE)


  
  #### Render Plot ####
  ## Displays plot of responses
  output$plot <- renderPlot({
    # Save created plot to reactive values
    rv$plot <- updatePushed_plotGG()
    # Render plot saved in reactive values
    if (!is.null(rv$plot)) {
      print(rv$plot)
    } else {
      print("Cannot render plot; data is NULL")
    }
  })
  
  
  #### Render Table ####
  ## Displays table of responses
  output$table <- renderDataTable({
    # Save created table to reactive values
    rv$table <- updatePushed_plotTable()
    # Plot if return value is not NA
    if (!is.null(rv$table)) {
      return(rv$table)
    } else {
      print("Cannot render table; data is NULL")
    }
  }, options = list(pageLength = 10))
  
  
  #### Render Aesthetic Customization UI's ####
  ## Renders UI's based on whether Uniform or Individual aesthetics is selected in the 'aesthetics' customization tab
  output$aesUIs <- renderUI({
    
    ## Individual aesthetics
    if (input$aesOptions == "Individual Bar Colors") {
      
      ## A list of UI's for the default category
      default_uis_list <- lapply(1:1, function(i) {
        
        # Default Category id
        default_id <- paste0("default_aes_indiv")
        
        # List of Category Default UI's for individual aes customization
        list(
          hr(),
          h4(paste0("Default Category: ", input$defaultName)),
          br(),
          flowLayout(
            
            # Fill
            colourpicker::colourInput(paste0(default_id, "_fill"), label = "Fill", value = "gray", 
                        allowTransparent = TRUE, returnName = TRUE),
            
            # No Fill?
            checkboxInput(paste0(default_id, "_no_fill"), label = "No Fill", value = FALSE),
            
            # Color
            colourpicker::colourInput(paste0(default_id, "_color"), label = "Color", value = "black", 
                        allowTransparent = TRUE, returnName = TRUE),
            
            # No Color?
            checkboxInput(paste0(default_id, "_no_color"), label = "No Color", value = TRUE),
            conditionalPanel(
              condition = paste0("!input.",default_id, "_no_color"),
              fluidRow(
                column(width = 5,
                       
                       # Line Width
                       numericInput(paste0(default_id, "_line_width"), label = "Line Width", 
                                    value = 0.5, min = 0, max = 10, step = 0.01)),
                column(width = 7,
                       
                       # Linetype
                       selectInput(paste0(default_id, "_line_type"), label = "Line Type", selected = "solid", 
                                   choices = c("blank","solid","dashed","dotted","dotdash","longdash","twodash")))
              )
            )
          )
        )
      })
      
      ## A list of new UI's for each existing category
      uis_list <- lapply(1:nrow(rv$all_categories), function(i) {
        
        # Category variables
        current_ctg_id <- rv$all_categories$ctg_id[i]
        current_ctg_name <- rv$all_categories$ctg_name[i]
        
        # Proceed if the current category has at least one rule
        if (rv$all_categories$ctg_num_rules[i] > 0) {
          
          # Unique id for all ui's in category
          aes_id <- paste0(current_ctg_id, "_aes_indiv")
          list(
            
            # Category Header
            hr(),
            h4(paste0("Category ", i, ": ", current_ctg_name)),
            br(),
            flowLayout(
              
              # Fill
              colourpicker::colourInput(paste0(aes_id, "_fill"), label = "Fill", value = "gray", 
                          allowTransparent = TRUE, returnName = TRUE),
              
              # No Fill?
              checkboxInput(paste0(aes_id, "_no_fill"), label = "No Fill", value = FALSE),
              
              # Color
              colourpicker::colourInput(paste0(aes_id, "_color"), label = "Color", value = "black",
                          allowTransparent = TRUE, returnName = TRUE),
              
              # No Color?
              checkboxInput(paste0(aes_id, "_no_color"), label = "No Color", value = TRUE),
              conditionalPanel(
                condition = paste0("!input.",aes_id, "_no_color"),
                fluidRow(
                  column(width = 5,
                         
                         # Line Width
                         numericInput(paste0(aes_id, "_line_width"), label = "Line Width", 
                                      value = 0.5, min = 0, max = 10, step = 0.01)),
                  column(width = 7,
                         
                         # Linetype
                         selectInput(paste0(aes_id, "_line_type"), label = "Line Type", selected = "solid", 
                                     choices = c("blank","solid","dashed","dotted","dotdash","longdash","twodash")))
                )
              )
            )
          )
        }
      })
      
      ## Un-list the UI lists
      default_uis <- unlist(default_uis_list, recursive = FALSE)
      ctgs_uis <- unlist(uis_list, recursive = FALSE)
      
      ## Append all category UI's to default category, if not NULL
      if (!is.null(ctgs_uis)) {
        default_uis <- append(default_uis, ctgs_uis)
      }
      
      ## Render the UI's
      do.call(tagList, default_uis)
        
    } else {
      
      ## Uniform aesthetics
      if (input$aesOptions == "Uniform Bar Colors") {
        
        # Unique id for all ui's
        aes_id <- paste0("aes_uniform")
        
        # Render this list of ui's
        tagList(
          hr(),
          flowLayout(
            
            # Fill
            colourpicker::colourInput(paste0(aes_id, "_fill"), label = "Fill", value = "gray", 
                        allowTransparent = TRUE, returnName = TRUE),
            
            # No Fill?
            checkboxInput(paste0(aes_id, "_no_fill"), label = "No Fill", value = FALSE),
            
            # Color
            colourpicker::colourInput(paste0(aes_id, "_color"), label = "Color", value = "black",
                        allowTransparent = TRUE, returnName = TRUE),
            
            # No Color?
            checkboxInput(paste0(aes_id, "_no_color"), label = "No Color", value = TRUE),
            conditionalPanel(
              condition = paste0("!input.",aes_id, "_no_color"),
              fluidRow(
                column(width = 5,
                       # Line Width
                       numericInput(paste0(aes_id, "_line_width"), label = "Line Width", 
                                    value = 0.5, min = 0, max = 10, step = 0.01)),
                column(width = 7,
                       # Linetype
                       selectInput(paste0(aes_id, "_line_type"), label = "Line Type", selected = "solid", 
                                   choices = c("blank","solid","dashed","dotted","dotdash","longdash","twodash")))
              )
            )
          )
        )
      }
    }
  })
  
  
  #### Render Topic Details Plot ####
  output$tf_details_plot <- renderPlot({
    # Plot the number of responses within each found topic
    rv$response_topics %>%
      group_by(Topic) %>% 
      summarise(Count = n()) %>% # Data Label Column
      ggplot() +
      geom_col(aes(x = Topic, y = Count), fill = "dodgerblue4", color = "dodgerblue4") +
      geom_text(aes(x = Topic, y = Count, label = Count), 
                nudge_y = 15, color = "dodgerblue4") +
      scale_x_continuous(breaks = seq(1, rv$num_topics)) +
      labs(title = "Number of Responses per Topic", y = "Responses") +
      theme_hc()
  })
  
  
  #### Render Topic Details Table ####
  output$tf_details_tbl <- renderDataTable({
    # Render Data Table
    return(rv$response_topics)
  }, options = list(pageLength = 10))
  
  
  #### Render RV Table ####
  output$show_rvs <- renderPrint({
    # Show all reactive values as a data table
    print(reactiveValuesToList(rv))
  })
}




##### ----- SHINYAPP() ----- #####

shinyApp(ui = ui, server = server, enableBookmarking = "server")










