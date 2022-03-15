## app.R ##


# _run preparation -------------------------------------------------------------

# this is the option for my project, universal
options(stringsAsFactors=FALSE)
options(scipen=10)

source("All_combined_subfunctions.r")
source("shiny_modules.R")
source("subfunctions_update.r")

library(shinydashboard)
library(shiny)
library(htmlwidgets)
library(DT) # for table
library(visNetwork) # for network construction and display
library(shinyBS) # for pop up information
library(circlize) # for circos plot
library(networkD3) # for sankey plot
library(rmetalab)

#  __header ------------------------------------------------------

header <- dashboardHeader(title = span(img(src="logo_imetalab.png", width = 150), "Coocurrence Analysis"),
                          titleWidth = 400,
                          tags$li(class = "dropdown",
                                  tags$a(href="www.imetalab.ca", target="_blank",
                                         tags$img(height = "18px", alt="SNAP Logo", src="logo_M.png")
                                  )
                          )
          )

#  __side bar ------------------------------------------------------

sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    id = "tabs",
    menuItem("Data Preparation", tabName = "dashboard", icon = icon("dashboard")),
    #menuItem("Network Construction", tabName = "Plots", icon = icon("th")),
    sidebarMenuOutput("menu_network_construction"),
    sidebarMenuOutput("menu_more_result"),
    menuItem("Gallery", tabName = "gallery", icon = icon("picture-o")),
    menuItem("About", tabName = "about", icon = icon("info-circle")),
    menuItem("iMetaLab", icon = icon("home"),
             href = "http://www.imetalab.ca")
  )
)

#  __body ------------------------------------------------------

body <- dashboardBody(
  tags$head(tags$link(rel = "shortcut icon", href = "logo_M.png")),
  title = "matrix to network",
  tabItems(

    #  ___dashboard/starting  page  ------------------------------------------------------
    tabItem(tabName = "dashboard",

            fluidRow(
              box(solidHeader = TRUE,
                  title = "Network construction based on co-ocurrence analysis",
                  status = "primary",
                  width = 6,
                  "The purpose of this app is to construct a network by co-occurence analysis.",
                  br(),
                  "The theory behind is there is some connection if two items share the same profile, known as co-occurence",
                  br(),
                  "Two files are required for the current version, one is expressin matrix, with rows as entries, column as experiment, the other is
grouping information for each enty.",
                  br(),
                  "Sample files for download: ",
                  a(href = 'co_occurence_analysis.txt', 'data matrix file'),"; ",
                  a(href = 'co_occurence_analysis_meta.txt', 'meta data file')

              ),

              box(solidHeader = TRUE,
                  title = "File format",
                  status = "primary",
                  width = 6,
                  "File format has to be tsv or csv",
                  br(),
                  "After uploading your file, please check if they are read in correctly.",
                  br(),
                  "Expression data matrix needs to have entry names a row names, while meta data needs to have two columns: sample name and grouping.",
                  br(),
                  "If everything is OK, press the button on the right bottom or directly go to next tab to do network analysis."


              )
            ),
            fluidRow(
              box(
                width = 6,
                status = "primary",
                solidHeader = TRUE,
                radioButtons("dataSelection",
                             label = h4("Starting with data input"),
                             choices = c("Upload Data" = 1,
                                         "Load sample data (Preoteomics expression data (log10 of LFQ intensity) among 176 samples, from COG, protein)" = 2
                             )
                )
              ),

              conditionalPanel(
                condition = 'output.fileUploaded1',
                box(
                    width = 6,
                    status = "primary",
                    solidHeader = TRUE,
                    checkboxInput("if_preprocess_data", 'Process data matrix before construting network ?', FALSE),
                    actionButton("gotoButton",
                                 icon = icon("arrow-right"),
                                 label = "Go to Network analysis",
                                 style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                    ),

                    bsTooltip("gotoButton", "Do not click before you see the table loaded and displayed below",
                              "left", options = list(container = "body"))

                )

              )
            ),

            #  ______ introdocution------------------------------------------------------
            fluidRow(
              conditionalPanel(
                condition = "input.dataSelection == 1",
                box(title = "Upload matrix data(tsv/csv)",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    checkboxInput('header1', 'First line is Header?', TRUE),
                    checkboxInput('rowname1', 'First column is rownames?', TRUE),

                    selectInput('sep1', 'Separator',
                                c(Comma=',',
                                  Semicolon=';',
                                  Tab='\t'),
                                '\t'),
                    selectInput('quote1', 'Quote',
                                c(None='',
                                  'Double Quote'='"',
                                  'Single Quote'="'"),
                                ''),
                    fileInput('file1', 'Data matrix file: ',
                              accept = c(
                                'text/csv',
                                'text/comma-separated-values',
                                'text/tab-separated-values',
                                'text/plain',
                                '.csv',
                                '.tsv'
                              )
                    )
                ),

                box(title = "Upload meta data(tsv/csv)",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    checkboxInput('header2', 'First line is Header?', TRUE),
                    checkboxInput('rowname2', 'First column is Header?', FALSE),

                    selectInput('sep2', 'Separator',
                                c(Comma=',',
                                  Semicolon=';',
                                  Tab='\t'),
                                '\t'),



                    selectInput('quote2', 'Quote',
                                c(None='',
                                  'Double Quote'='"',
                                  'Single Quote'="'"),
                                ''),
                    fileInput('file2', 'Data matrix file: ',
                              accept = c(
                                'text/csv',
                                'text/comma-separated-values',
                                'text/tab-separated-values',
                                'text/plain',
                                '.csv',
                                '.tsv'
                              )
                    )
                )
                )


            ) ,
            #  ______ data preprocess module ------------------------------------------------------

            conditionalPanel(
              condition = "input.if_preprocess_data",
              datatableProcess_UI("data_table_preprocess",boxtitle = "Data table pre-process", boxwidth = 12)
            ),

            #  ______ uploaded table display ------------------------------------------------------
            fluidRow(
              conditionalPanel(
                condition = 'output.fileUploaded1',
                box(
                  title = "Check the matrix table",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  DT::dataTableOutput('table1_input')
                )
              ),
              conditionalPanel(
                condition = 'output.fileUploaded2',
                box(
                  title = "Check meta data table",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  DT::dataTableOutput('table2_input')
                )
              )
            ),
            #  ______ goto button ------------------------------------------------------
            conditionalPanel(
              condition = 'output.fileUploaded1',
              fluidRow(
                box(solidHeader = TRUE,
                    width = 12,
                    actionButton("gotoButton2",
                                 icon = icon("arrow-right"),
                                 label = "Go to Network analysis page!",
                                 style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                 )

                )
              )

            )

    ),

    #  ___ Analysis settings ------------------------------------------------------
    tabItem(tabName = "Plots",
            fluidRow(

              #  ______ parameters ------------------------------------------------------
              box(
                title = "Network construction settings",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                column(3,

                       selectInput('correlation_type', 'Correlation calculation method', c("spearman", "pearson")),
                       sliderInput("correlation_threshold", "correlation threshold (absolute value) to keep:",
                                   min = 0.1, max = 1, value = 0.7)
                      ),
                column(3,


                       sliderInput("p_threshold", "P threshold to keep:",
                                   min = 0.001, max = 0.05, value = 0.05),
                       selectInput('igraph_laylout', 'Network layout style', c("layout_components", "layout_as_star","layout_in_circle","layout_nicely",
                                                                               "layout_on_grid", "layout_on_sphere","layout_randomly"))

                       ),
                column(3,

                       sliderInput("node_size_cex", "Node size cofficient:",
                                   min = 1, max = 20, value = 10),
                       sliderInput("node_label_cex", "Node label font size cofficient:",
                                   min = 0.2, max = 2, value = 0.5)

                       ),

                column(3,

                       sliderInput("edge_width_cex", "edge width cofficient:",
                                   min = 1, max = 20, value = 5),
                       selectInput('node_shape', 'Node shape:', c("dot", "circle", "ellipse", "square")),
                       checkboxInput('dynamic', 'Force stablized view? Careful, slow for large dataset!', FALSE),

                       checkboxInput('advanced', 'Advanced Options to tweak the view', FALSE)


                )

              )
            ),
            #  ______ buttons ------------------------------------------------------
            fluidRow(
              box(
                  width = 12,
                  #status = "primary",
                  solidHeader = TRUE,

                  conditionalPanel(
                    condition = 'output.analysis_finished',
                    actionButton("gotoButton3",
                                 icon = icon("paper-plane"),
                                 label = "View More Result",
                                 style="float:right"
                    )
                  ),
                  actionButton("analysisButton",
                               icon = icon("paper-plane"),
                               label = "Network Construction and Display!",
                               style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"

                  )

              ),
              #  ______ Network visulization ------------------------------------------------------
              conditionalPanel(
                condition = 'output.analysis_finished',
                box(
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  visNetworkOutput("visnetwork_display", height = "1000px")
                )
              ),


              shinysky::busyIndicator(text = "Loading, please wait ...",
                                      img = "ajaxloaderq.gif",
                                      wait = 500)

            )
    ),

    #  ___ more result display ------------------------------------------------------
    tabItem(tabName = "tables",
            fluidRow(
              tabBox(
                id ="tabBox_more_result_explore",
                width = 12,

                #  ______ chord diagram  ------------------------------------------------------

                tabPanel(
                  h4("Chorddiagram Display"),
                  fluidRow(

                    box(
                      width = 4,
                      status = "primary",
                      solidHeader = TRUE,

                      fluidRow(
                        column(6,
                               sliderInput("chorddiagram_number",
                                           "Top N correlations(links) to plot",
                                           min = 1,
                                           max = 200,
                                           value = 10
                                           ),
                               numericInput("chorddiagram_general_gap",
                                            "Gap size between elements",
                                            value = 1,
                                            min = 1,
                                            max = 50,
                                            step = 1,
                                            width = "200px"

                                            ),
                               numericInput("chorddiagram_groups_gap",
                                            "Gap size between groups",
                                            value = 10,
                                            min = 1,
                                            max = 50,
                                            step = 1,
                                            width = "200px"

                               ),

                               numericInput("chorddiagram_Rotation_degree",
                                            "Rotate from the original point(3 O'clock)",
                                            value = 0,
                                            min = 0,
                                            max = 360,
                                            step = 1,
                                            width = "200px"

                               ),
                               checkboxInput("chorddiagram_plot_direction",
                                           "Plot element by clockwise direction?",
                                           TRUE
                                           ),
                               sliderInput("chorddiagram_link_transparency",
                                           "Link color transparency",
                                           min = 0,
                                           max = 1,
                                           value = 0.5
                               )


                               ),
                        column(6,
                               textInput("chorddiagram_maintitle",
                                         "Main title:",
                                         ""
                                         ),
                               selectInput("chorddiagram_annotation",
                                           "Annotation tracks",
                                           c("name", "grid", "axis"),
                                           selected = c("name", "grid"),
                                           multiple = TRUE
                                           ),
                               checkboxInput("chorddiagram_vertical_labeling",
                                             "Vertical Labeling?",
                                             FALSE
                               ),
                               selectInput("chorddiagram_map_link_color",
                                           "Map link colors to:",
                                           c("NULL", "correlation"),
                                           selected = NULL
                               )

                               )

                      ),


                      fluidRow(
                        column(6),
                        column(6,
                               actionButton("button_apply_chorddiagram",
                                            icon = icon("paper-plane"),
                                            label = "Plot",
                                            #style="float:right"
                                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                               )
                        )

                      )


                    ),

                    uiOutput("ui_chorddiagram")

                  )
                ),

                #  ______ sankey plot  ------------------------------------------------------
                tabPanel(
                  h4("Sankey Plot"),
                  fluidRow(
                    box(
                      width = 4,
                      status = "primary",
                      solidHeader = TRUE,
                      fluidRow(
                        column(6,
                               sliderInput("sankey_number",
                                           "Top N correlations(links) to plot",
                                           min = 1,
                                           max = 200,
                                           value = 20
                               ),
                               checkboxInput("sankey_grouping_with_color",
                                             "Color Goups?",
                                             FALSE
                               ),
                               checkboxInput("sankey_range_scaling",
                                             "Scale the corrlation range to make the width of links look more related to corrlation?",
                                             TRUE
                               )
                               ),
                        column(6,
                               sliderInput("sankey_node_width",
                                           "Node width",
                                           min = 1,
                                           max = 100,
                                           value = 30
                               ),
                               sliderInput("sankey_font_size",
                                           "Font size",
                                           min = 1,
                                           max = 40,
                                           value = 20
                               ),
                               sliderInput("sankey_node_padding",
                                           "Node Padding",
                                           min = 1,
                                           max = 40,
                                           value = 10
                               ),
                               sliderInput("sankey_margin",
                                           "Plot margin",
                                           min = 1,
                                           max = 40,
                                           value = 10
                               ),
                               sliderInput("sankey_height",
                                           "Plot Height",
                                           min = 400,
                                           max = 1000,
                                           value = 600
                               ),
                               checkboxInput("sankey_sink_right",
                                             "Sink Right?",
                                             TRUE
                                             ),
                               selectInput("Sankey_color_scheme",
                                           "Color theme:",
                                           c("d3.schemeCategory10" =   "d3.scaleOrdinal(d3.schemeCategory10);",
                                             "d3.schemeCategory20" =   "d3.scaleOrdinal(d3.schemeCategory20);",
                                             "d3.schemeCategory20b" = "d3.scaleOrdinal(d3.schemeCategory20b);",
                                             "d3.schemeCategory20c" = "d3.scaleOrdinal(d3.schemeCategory20c);"
                                           )

                               )
                               )


                      ),

                      fluidRow(
                        column(6),
                        column(6,
                               actionButton("button_apply_sankeyplot",
                                            icon = icon("paper-plane"),
                                            label = "Plot",
                                            #style="float:right"
                                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                               )
                        )

                      )
                      ),
                      uiOutput("ui_sankey_plot")



                  )

                ),
                #  ______ matrix plot  ------------------------------------------------------
                tabPanel(
                  h4("Correlation matrix Plot"),
                  fluidRow(

                    box(
                      width = 4,
                      status = "primary",
                      solidHeader = TRUE,


                      fluidRow(
                        column(6,
                               selectInput("correlation_plot_method",
                                           "Choose the method/shape to visualize:",
                                           c("circle", "square", "ellipse", "number", "shade",
                                             "color", "pie"),
                                           selected = "square"
                               ),
                               selectInput("correlation_plot_type",
                                           "Choose the type to plot:",
                                           c("full", "lower", "upper"),
                                           selected = "upper"
                               ),
                               selectInput("correlation_plot_order",
                                           "Choose to the order of the metrix:",
                                           c("original" = "original",
                                             "angular order of the eigenvectors"= "AOE",
                                             "first principal component order" = "FPC",
                                             "hierarchical clustering order" ="hclust",
                                             "alphabetical order"=  "alphabet"),
                                           selected = "original"
                               )
                               #,
                               #
                               # selectInput("correlation_plot_color_theme",
                               #             "Color theme",
                               #             c ("BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu","RdYlGn","Spectral")
                               #
                               # ),
                               # checkboxInput("correlation_plot_color_theme_rev",
                               #               "Reverse the color?",
                               #               FALSE
                               #               )

                               ),
                        column(6,
                               numericInput("correlation_plot_pvalue_cutoff",
                                           "P-value cutoff",
                                           max = 1,
                                           value = 0.01,
                                           width = "200px"
                                           ),
                               selectInput("correlation_plot_insig_behavor",
                                           "Behavior for the insignificant nodes",
                                           c("pch", "p-value", "blank", "n", "label_sig"),
                                           selected = "blank"
                                           ),
                               numericInput("correlation_plot_label_size",
                                            "Text label size",
                                            max = 10,
                                            min = 0.1,
                                            step = 0.1,
                                            value = 0.5,
                                            width = "200px"
                               ),
                               numericInput("correlation_plot_label_tilt",
                                            "Text label tilt degree ",
                                            step = 1,
                                            value = 30,
                                            width = "200px"
                               )
                          )
                      ),
                      fluidRow(
                        column(12,
                               useColor_UI("correlation_matrix_color",
                                           initialize_show = TRUE,
                                           method = "RcolorBrewer",
                                           Rcolorbrewer_schemes = "div",
                                           RcolorBrewer_theme = "BrBG",
                                           ncolors = 100)
                        )


                      ),
                      fluidRow(
                        column(6),
                        column(6,
                               actionButton("button_apply_plotcorrelation",
                                            icon = icon("paper-plane"),
                                            label = "Plot",
                                            #style="float:right"
                                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                               )
                        )
                      )

                    ),
                    uiOutput("ui_corrplot")
                    )
                  ),
                #  ______ table download  ------------------------------------------------------

                  tabPanel(
                    h4("Result Table Download"),
                    fluidRow(
                      box(
                        title = "Download data table",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,

                        tableDownloadUI("download_network_edges",
                                        label = "Download table for network interactions/edges"),
                        br(),

                        tableDownloadUI("download_network_vertics",
                                        label = "Download table for netwrok nodes/vertics"),
                        br(),
                        tableDownloadUI("download_network_all_correlations",
                                        label = "Download whole table of All correlations from the original data")
                      ),
                      box(
                        title = "Quick view of edges/intereactions of the network",
                        status = "primary",
                        width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed = TRUE,
                        DT::dataTableOutput('table_output_data_for_network')
                      ),
                      box(
                        title = "Quick view of nodes/Vertics of the network",
                        status = "primary",
                        width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed = TRUE,
                        DT::dataTableOutput('table_output_data_for_network_vertics')
                      ),
                      box(
                        title = "Quick view of all correlations from the original data",
                        status = "primary",
                        width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed = TRUE,

                        DT::dataTableOutput('table_output_data_correlation_orignial')
                      )

                    )
                  )
              )

            )

    ),
    #  __ Panel:  Gallery ------------------------------------------------------
    tabItem(tabName = "gallery",
            fluidRow(

              box(
                title = "correlation matrix",
                solidHeader = TRUE,
                status = "primary",
                width = 4,
                img(src='gallery/matri.PNG')
              ),
              box(
                title = "network construction",
                solidHeader = TRUE,
                status = "primary",
                width = 4,
                img(src='gallery/network_01.PNG')
              ),
              box(
                title = "Circos interaction",
                solidHeader = TRUE,
                status = "primary",
                width = 4,
                img(src='gallery/circos.PNG')
              )



            )
    ),
    #  __ Panel:  About ------------------------------------------------------
    tabItem(tabName = "about",
            fluidRow(
              box(
                title = "Contact",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                "Author: Zhibin Ning",
                br(),
                "Email: ningzhibin@gmail.com",
                br(),
                a(href = 'https://groups.google.com/forum/#!forum/lovemetalab', 'Suggestions and bug report'),
                br(),
                "This shiny app uses visNetwork package for displaying network, ","for detailed information please refer to",
                a(href = "https://datastorm-open.github.io/visNetwork/","visNetwork"),
                br(),
                "more R packages include: shiny, shinydashboard, htmlwidgets, DT, ggplot2, plotly, corrplot, colourpicker, circlize"
              ),
              box(
                title = "Change log",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                "V0.3: 20171024. Add moduel for chorddiagram plot and more options for correlation matrix plot",
                br() ,
                "V0.21: 20171017. Add a dedicated module for data processing",
                br() ,
                "V0.2: 20171010. update of the UI, add dedicated buttons for result table downlad",
                br() ,
                "V0.11: 20170910. UI update, a few bugs fix",
                br() ,
                "V0.1: 20170810. functional version onlilne",
                br()
              ),
              box(
                title = "Resources & Cheet sheet",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                a(href = 'http://colorbrewer2.org/', 'RcolorBrewer interactive reference'),
                br(),
                a(href = 'http://databall.co/shiny/shinyggplot/?utm_source=r-bloggers.com&utm_medium=referral&utm_campaign=blogpost&utm_content=text#tab-8930-1', 'ggplot2 Explorer'),
                br()

              )

            )
    )

  ),
  #  >>> CSS section, ignore for r ------------------------------------------------------

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
)

#  _  ------------------------------------------------------
server <- function(input, output, session) {

## get files from upload ------------------------------------------------------

## get file 1------------------------------------------------------
  getData1 <- reactive({
    switch(input$dataSelection,
                      "1" = {
                        inFile1 <- input$file1
                        if(is.null(input$file1)){return(NULL) }  # this is alway the first line when reading in a file
                        if(input$rowname1){
                          read.delim(inFile1$datapath, header=input$header1, sep=input$sep1,
                                     quote=input$quote1, row.names = 1)
                        }else{
                          read.delim(inFile1$datapath, header=input$header1, sep=input$sep1,
                                     quote=input$quote1)
                        }
                      },
                      "2" = {
                        my_data_matrix <- read.delim("./www/co_occurence_analysis.txt", header = T, row.names = 1)

                        test_data_matrix <- as.matrix(my_data_matrix)
                        if(!is.numeric(test_data_matrix)){mode(test_data_matrix) <- "numeric"}
                        return(test_data_matrix)
                      }

    )

  })

  # this is to generate a condition for conditional panel to show or hide data table of file1
  output$fileUploaded1 <- reactive({
    return(!is.null(getData1()))
  })
  outputOptions(output, 'fileUploaded1', suspendWhenHidden=FALSE)

  output$menu_network_construction <- renderMenu({
    if (!is.null(getData1())) {
      sidebarMenu(
        #menuItem("Play With data Matrix", tabName = "tables1", icon = icon("bar-chart"))
        menuItem("Network Construction", tabName = "Plots", icon = icon("th"))
      )
    }
  })

##  data preprocess for file 1------------------------------------------------------

  rvalues <- reactiveValues()

  observe({
    if(input$if_preprocess_data){
      # the logic is that if user choose to process data, call the modue, take the returned value
      # otherwise setup a conducto/pipe

      Data_processed <- callModule(datatableProcess, "data_table_preprocess", getData1())
    }else{
      Data_processed <- reactive({getData1()})
    }

    # put the returned reacitve expression into new reative values list, for downstream useages
     rvalues$Data_processed <- Data_processed()

      output$table1_input <- DT::renderDataTable(
        Data_processed(), # has to use the expression
        extensions = c('Scroller'),
        options = list(
          autoWidth = TRUE,
          dom = 'Brtip',
          scrollY = 200,
          scrollX = TRUE
        )
      )


  })



  #  ____ get file 2------------------------------------------------------
  getData2 <- reactive({

    switch(input$dataSelection,
           "1" = {
             inFile2 <- input$file2
             if(is.null(input$file2)){return(NULL) }  # this is alway the first line when reading in a file
             if(input$rowname2){
               read.delim(inFile2$datapath, header=input$header2, sep=input$sep2,
                          quote=input$quote2, row.names = 1)
             }else{
               read.delim(inFile2$datapath, header=input$header2, sep=input$sep2,
                          quote=input$quote2)
             }
           },
           "2" = {read.delim("./www/co_occurence_analysis_meta.txt", header = T)}

    )

  })

  # this is to generate a condition for conditional panel to show or hide data table of file1
  output$fileUploaded2 <- reactive({
    return(!is.null(getData2()))
  })
  outputOptions(output, 'fileUploaded2', suspendWhenHidden=FALSE)


  # link the output
  output$table2_input <- DT::renderDataTable(
    getData2(),extensions = c('Scroller'),
    options = list(
      #autoWidth = TRUE,
      dom = 'Brtip',
      scrollY = 200,
      scrollX = TRUE
    )
  )


  #  __ network analysis using upload files ------------------------------------------------------

  network_analysis_result <- reactive({
    if (input$analysisButton > 0){

      isolate({

        data_matrix <- rvalues$Data_processed

        co_occurence_visNetwork(data_matrix = data_matrix,
                                       data_meta = getData2(),
                                       correlation_type = input$correlation_type,
                                      correlation_threshold = input$correlation_threshold,
                                      p_threshold = input$p_threshold,
                                      shape = input$node_shape,
                                      edge_width_cex = input$edge_width_cex,
                                      node_size_cex = input$node_size_cex,
                                      node_label_cex = input$node_label_cex,
                                      igraph_laylout = input$igraph_laylout,
                                      dynamic = input$dynamic,
                                      advanced = input$advanced)

      })


    }
  })


  # this is to test of the analysis is complete and generate the result
  output$analysis_finished <- reactive({
    return(!is.null(network_analysis_result()))
  })
  outputOptions(output, 'analysis_finished', suspendWhenHidden=FALSE)


  output$menu_more_result <- renderMenu({
    if (!is.null(network_analysis_result())) {
      sidebarMenu(
        menuItem("More Result", tabName = "tables", icon = icon("bar-chart"))
      )
    }
  })
  observe({
    if (!is.null(network_analysis_result())) {
      updateTabItems(session, "tabs", "Plots")
    }
  })



  # __output plots--------------------------------------
  # ____ output visnetwrok plots--------------------------------------

  output$visnetwork_display <- renderVisNetwork({
    network_analysis_result()$plot_network_interactive
  })


  # ____ output matrix plots--------------------------------------
  # do not put this color modue into any reactive enrivoment, otherwise,it will not work properly
  mycol_correlation <- callModule(useColor, "correlation_matrix_color")
  # useage: mycol_correlation() ,because the returned color is actually a reactive expression

  observe({
    # take over the ouput
    correlation_matrix_filtered <- network_analysis_result()$correlation_matrix_filtered

    if(input$button_apply_plotcorrelation > 0 ){

      isolate({
        # plot the corrleation matrix
        svg(tempfile())
        dev.control('enable')
        corrplot(correlation_matrix_filtered$r,
                 type=input$correlation_plot_type,
                 method =input$correlation_plot_method,
                 order=input$correlation_plot_order,
                 col = mycol_correlation(),
                 p.mat = correlation_matrix_filtered$P,
                 sig.level = as.numeric(input$correlation_plot_pvalue_cutoff),
                 insig = input$correlation_plot_insig_behavor,
                 tl.col = "black",
                 tl.cex = input$correlation_plot_label_size,
                 tl.srt = input$correlation_plot_label_tilt,
                 addgrid.col =NA,
                 outline = FALSE,
                 diag=FALSE)
        p_correlation <- recordPlot()
        dev.off()


        callModule(plotInBox, "corrplot",
                   plot_object = p_correlation,
                   name_tag = "plot_correlation_filtered",
                   plot_type = "recordPlot")



        output$ui_corrplot <- renderUI({
          plotInBox_UI("corrplot", boxwidth = 8)
        })

      })
    }





  })
  # ____ output sankey plot --------------------------------------
  observe({
    # take over the ouput
    correlation_matrix_filtered <- network_analysis_result()$correlation_matrix_filtered
    my_meta <- getData2()

    if(input$button_apply_sankeyplot > 0 ){
      df_edge_filtered <- flattenCorrMatrix(correlation_matrix_filtered)

      isolate({
        topN <- as.numeric(input$sankey_number)

        if(nrow(df_edge_filtered) > topN){
          df_edge_filtered <- df_edge_filtered[order(df_edge_filtered$cor,decreasing = TRUE), ]

          df_edge_for_sankey <- df_edge_filtered[1:topN,]
        }else{
          df_edge_for_sankey <- df_edge_filtered
        }
        if(input$sankey_range_scaling){
          df_edge_for_sankey$cor <- range_standarize_100(df_edge_for_sankey$cor)
        }else{
          df_edge_for_sankey$cor <- round(100*df_edge_for_sankey$cor)
        }


        result_for_sankey <- recode_for_sankey(df_edge_for_sankey)

        df_nodes <- result_for_sankey$df_nodes
        # if you want to group
        #df_nodes$group <- sample(c("A","B","C"), nrow(df_nodes), replace = TRUE)

        if(input$sankey_grouping_with_color){
          df_nodes$group  <- my_meta$type[match(df_nodes$name,my_meta$sample)]

          output$sankeyplot <- renderSankeyNetwork({
            sankeyNetwork(Links = result_for_sankey$df_links, Nodes = df_nodes,
                          Source = "from", Target = "to",
                          Value = "cor", NodeID = "name",
                          NodeGroup = "group",
                          nodePadding = input$sankey_node_padding,
                          margin = input$sankey_margin,
                          fontSize= input$sankey_font_size,
                          nodeWidth = input$sankey_node_width,
                          sinksRight = input$sankey_sink_right,
                          colourScale = JS(input$Sankey_color_scheme)
                          )
          })
        }else{
          output$sankeyplot <- renderSankeyNetwork({
            sankeyNetwork(Links = result_for_sankey$df_links, Nodes = df_nodes,
                          Source = "from", Target = "to",
                          Value = "cor", NodeID = "name",
                          nodePadding = input$sankey_node_padding,
                          margin = input$sankey_margin,
                          fontSize= input$sankey_font_size,
                          nodeWidth = input$sankey_node_width,
                          sinksRight = input$sankey_sink_right,
                          colourScale = JS(input$Sankey_color_scheme)
                          )
          })

        }


        output$ui_sankey_plot <- renderUI({
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            sankeyNetworkOutput("sankeyplot",
                                height = paste0(input$sankey_height,"px"))
          )


        })



      })



    }

  })

  # ____ output chorddiagram  --------------------------------------

  observe({

    # take over the ouput
    correlation_matrix_filtered <- network_analysis_result()$correlation_matrix_filtered
    my_meta <- getData2()

    if(input$button_apply_chorddiagram > 0 ){

      df_edge_filtered <- flattenCorrMatrix(correlation_matrix_filtered)

      isolate({

        topN <- as.numeric(input$chorddiagram_number)

        if(nrow(df_edge_filtered) > topN){
          df_edge_filtered <- df_edge_filtered[order(df_edge_filtered$cor,decreasing = TRUE), ]

          df_edge_for_circos <- df_edge_filtered[1:topN,]
        }else{
          df_edge_for_circos <- df_edge_filtered
        }

        unique_nodes <- union(df_edge_for_circos[,1],df_edge_for_circos[,2])
        #print(unique_nodes)

        my_meta_filtered <- my_meta[match(unique_nodes, my_meta[,1]),] # get the grouping information
        my_meta_filtered <- my_meta_filtered[order(my_meta_filtered$type),] # reorder according to the groups

        #print(my_meta_filtered)

        groups_tab <- table(my_meta_filtered[,2])
        gaps_index <- cumsum(groups_tab)

        plot_order <- my_meta_filtered[,1] # set the plot order


        # # set the gaps between groups
        gaps <-rep(input$chorddiagram_general_gap, length(unique_nodes))
        gaps[gaps_index] <- input$chorddiagram_groups_gap


        # get colors
        # get the colorbrewer colors, here using quality panel
        colorbrewer_colors <- rownames(brewer.pal.info[brewer.pal.info$category == "qual",])

        grid_colors <- c()
        # assgin colors using the same order
        for(i in 1: length(groups_tab)){
          grid_colors <-  c(grid_colors, colorRampPalette(brewer.pal(8,colorbrewer_colors[i]))(groups_tab[i]))
        }

        # set the link colors using the correlation mapping
        #df_edge_for_circos$cor

        col_fun <- switch(input$chorddiagram_map_link_color,

               "NULL" = {
                  NULL
               },

               "correlation" = {
                 colorRamp2(range(df_edge_for_circos$cor),
                            c("#FFEEEE", "#FF0000"),
                            transparency = input$chorddiagram_link_transparency)

               }

            )

        # plot the chorddiagram
        svg(tempfile())
        dev.control('enable')

        #circos.par(gap.after = gaps, start.degree = 90, clock.wise = TRUE)
        circos.clear()

        circos.par(gap.degree = as.numeric(gaps),
                   start.degree = as.numeric(input$chorddiagram_Rotation_degree),
                   clock.wise = input$chorddiagram_plot_direction)

        if(input$chorddiagram_vertical_labeling){
          chordDiagram(df_edge_for_circos,
                       grid.col = grid_colors,
                       annotationTrack = "grid",
                       preAllocateTracks = list(track.height = max(strwidth(unique_nodes))),
                       col = col_fun,
                       transparency = input$chorddiagram_link_transparency,
                       order = plot_order)

          # we go back to the first track and customize sector labels
          circos.track(track.index = 1, panel.fun = function(x, y) {
            circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
                        facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
          }, bg.border = NA) # here set bg.border to NA is important


        }else{
          chordDiagram(df_edge_for_circos,
                       grid.col = grid_colors,
                       col = col_fun,
                       transparency = as.numeric(input$chorddiagram_link_transparency),
                       order = plot_order,
                       annotationTrack = input$chorddiagram_annotation
          )

        }

        title(input$chorddiagram_maintitle)
        #chordDiagram(df_edge_for_circos)
        circos.clear()

        chorddiagram <- recordPlot()
        dev.off()


        # output

        callModule(plotInBox, "chorddiagram",
                   plot_object = chorddiagram,
                   name_tag = "chorddiagram",
                   plot_type = "recordPlot")

        output$ui_chorddiagram <- renderUI({
          plotInBox_UI("chorddiagram", boxwidth = 8)
        })

      })
    }

  })


  # ____ output tables quick views --------------------------------------

  output$table_output_data_correlation_orignial <- DT::renderDataTable(
    network_analysis_result()$data_correlation_orignial,filter = 'top', extensions = c('Buttons','Scroller'),
    options = list(
      autoWidth = TRUE,
      dom = 'Bfrtip',
      buttons = c('colvis'),
      scrollY = 200,
      scrollX = TRUE)
  )

  output$table_output_data_for_network <- DT::renderDataTable(
    network_analysis_result()$data_for_network, filter = 'top', extensions = c('Buttons','Scroller'),
    options = list(
      autoWidth = TRUE,
      dom = 'Brtip',
      buttons = c('colvis'),
      scrollY = 200,
      scrollX = TRUE)
  )

  output$table_output_data_for_network_vertics <- DT::renderDataTable(
    network_analysis_result()$data_for_network_vertics,filter = 'top', extensions = c('Buttons', 'Scroller'),
    options = list(
      autoWidth = TRUE,
      dom = 'Bfrtip',
      buttons = c('colvis'),
      scrollY = 200,
      scrollX = TRUE)
  )


  # ______for file  tsv download --------------------------------------


  callModule(tableDownload,"download_network_edges",
             data = network_analysis_result()$data_for_network,
             filename_tag = "network_edges")



  callModule(tableDownload,"download_network_vertics",
             data = network_analysis_result()$data_for_network_vertics,
             filename_tag = "network_vertics")

  callModule(tableDownload,"download_network_all_correlations",
             data = network_analysis_result()$data_correlation_orignial,
             filename_tag = "network_all_correlations")




  # ____switch tab using button --------------------------------------

  observeEvent(
    input$gotoButton, {
      #newtab <- switch(input$tabs, "one" = "two","two" = "one")
      updateTabItems(session, "tabs", "Plots")
    }
  )
  observeEvent(
    input$gotoButton2, {
      #newtab <- switch(input$tabs, "one" = "two","two" = "one")
      updateTabItems(session, "tabs", "Plots")
    }
  )

  observeEvent(
    input$gotoButton3, {
      #newtab <- switch(input$tabs, "one" = "two","two" = "one")
      updateTabItems(session, "tabs", "tables")
    }
  )
}


#  App entrance: main  ------------------------------------------------------
#
ui <- dashboardPage(
  title = "Coocurrence Analysis",
  header,
  sidebar,
  body)
shinyApp(ui, server)





