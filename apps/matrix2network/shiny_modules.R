# update log
# 20171102 fixed bugs and add reverse color options
# 20171026 color picking module finished
# 

########################################################

# colorpicker module, is always something I wanted to do, to make life easier
# usage:
# see shiny_modue_colorpickingup_demo.R for demo

# note!!!!!
# on the server side, do not put the call module in any reactive enrivoment, otherwise,it will not work properly
# and use the retured colors with () in reactive enviroment, because the returned colors are actually reactive expressions

# the returned piece of UI is in a taglList, could be inserted in a box or column

# all parameters can be set in the ui end to customerize the initial colors
# the front arguments will void the read one which are dependent on 


useColor_UI <- function(id = "test", 
                        initialize_show = TRUE,
                        method = NULL, # choose the method for color generation
                        Rcolorbrewer_schemes =  NULL, # this only works when Rcolorbrewer is selected
                        RcolorBrewer_theme = NULL, # this only works when the right Rcolorbrewer and schemes are selected 
                        RPalettes = NULL, # choose the r built in color
                        Classical_color = NULL,
                        DIY_color_mode = NULL,
                        ncolors = 100
                        ){
  ns <- NS(id)
  
  #sequential, diverging, or qualitative schemes
  tagList(
    checkboxInput(ns("Show_more_options_for_color_picker"),
                  "Show/hide color customization",
                  initialize_show
                  ),
    conditionalPanel(
      condition = paste0("input['", ns("Show_more_options_for_color_picker"), "']"),
      fluidRow(
        column(6,
               selectInput(ns("Coloring_method"),
                           "Choose coloring method",
                           c("RcolorBrewer", "R built-in Palettes","Classical color collection", "DIY colors"),
                           selected = method),
               # conditional display when RcolorBrewer chosen
               conditionalPanel(
                 condition = paste0("input['", ns("Coloring_method"), "']", "=='RcolorBrewer'"),
                 selectInput(ns("Rcolorbrewer_schemes"),
                             "Choose color type",
                             c("RcolorBrewer: sequential" = "seq",
                               "RcolorBrewer: Quality" = "qual",
                               "RcolorBrewer: Diverging" = "div"),
                             selected = Rcolorbrewer_schemes
                 ),
                 uiOutput(ns("ui_RcolorBrewer_themes_choose"))
                 
               ),
               # textinput to pass a parameter to the server end, always hide
               conditionalPanel(
                 condition = "1 > 2",# here it needs a javascript expression to be FALSE
                 textInput(ns("RcolorBrewer_themes_selected"),
                           "hidden textinput used for passing argument",
                           value =  RcolorBrewer_theme)
               ),
               # conditional display when R built-in Palettes chosen
               conditionalPanel(
                 condition = paste0("input['", ns("Coloring_method"), "']", "=='R built-in Palettes'"),
                 selectInput(ns("Rbuiltin_Palettes"),
                             "Choose color type",
                             c("rainbow","heat.colors","terrain.colors", "topo.colors","cm.colors"),
                             selected = RPalettes
                 )
               ),
               # conditional display when Classical color collection chosen
               conditionalPanel(
                 condition = paste0("input['", ns("Coloring_method"), "']", "=='Classical color collection'"),
                 selectInput(ns("Classical_color_collection"),
                             "Choose color type",
                             c("red-back-green","blue-white-red"),
                             selected = Classical_color
                 )
               ),
               # conditional display when DIY color chosen
               conditionalPanel(
                 condition = paste0("input['", ns("Coloring_method"), "']", "=='DIY colors'"),
                 selectInput(ns("DIY_color_mode"),
                             "DIY color mode",
                             c("single color","Dual-Color gradient","Tri-Color gradient"),
                             selected = DIY_color_mode
                 ),
                 uiOutput(ns("ui_DIY_color_picker"))
               )
        ),
        column(6,
               numericInput(ns("Number_of_colors_to_generate"),
                            "How many colors do you want to generate?",
                            min= 3,
                            step = 1,
                            value = ncolors
               ),
               checkboxInput(ns("If_rev_colors"),
                             "Reverse colors?",
                             FALSE),
               checkboxInput(ns("If_preview_colors"),
                             "Preview colors?",
                             FALSE),
               conditionalPanel(
                 condition = paste0("input['", ns("If_preview_colors"), "']"),
                 plotOutput(ns("plot_selected_color_theme"))
               )
        )
      )
    )
  )

  
}

# this server function will return the colors generated
useColor <- function(input, output, session){

  ns <- session$ns

  install.packages.auto(colourpicker)
  install.packages.auto("RColorBrewer")
  
  #rvalues <-reactiveValues()
  # ui rendering part
  observe({
    
    switch(input$Coloring_method,
      "RcolorBrewer" = {
        # get the theme list of each class
        RcolorBrewer_theme_list <-  switch(input$Rcolorbrewer_schemes,
                              "seq" = {rownames(brewer.pal.info[brewer.pal.info$category == "seq",])},
                              "qual" = {rownames(brewer.pal.info[brewer.pal.info$category == "qual",])},
                              "div" = {rownames(brewer.pal.info[brewer.pal.info$category == "div",])}
        )
        # generate a new ui for the theme selection
        output$ui_RcolorBrewer_themes_choose <- renderUI({
          selectInput(ns("RcolorBrewer_theme"),
                      "Choose RcolorBrewer color themes",
                      RcolorBrewer_theme_list,
                      selected = input$RcolorBrewer_themes_selected
          )
        })
      },
      "DIY colors" = {
        switch(input$DIY_color_mode,
               "single color" = {
                 output$ui_DIY_color_picker <- renderUI({
                   colourInput(
                     ns("color_picker_single"), 
                     "Choose color", 
                     "#7F7F7F"
                     #showColour = "background",
                     #palette = "limited"
                   )
                 })
               },
               "Dual-Color gradient" = {
                 output$ui_DIY_color_picker <- renderUI({
                   tagList(
                     colourInput(
                       ns("color_picker_low"), 
                       "Low color", 
                       "#FA0000"
                       #showColour = "background",
                       #palette = "limited"
                     ),
                     colourInput(
                       ns("color_picker_high"), 
                       "High color", 
                       "#00B3FA"
                       #showColour = "background",
                       #palette = "limited"
                     )
                   )
                   
                 })
               },
               
               "Tri-Color gradient" = {
                 
                 output$ui_DIY_color_picker <- renderUI({
                   tagList(
                     colourInput(
                       ns("color_picker_low"), 
                       "Low color", 
                       "#FA0000"
                       #showColour = "background",
                       #palette = "limited"
                     ),
                     colourInput(
                       ns("color_picker_middle"), 
                       "Middle color", 
                       "#7F7F7F"
                       #showColour = "background",
                       #palette = "limited"
                     ),
                     colourInput(
                       ns("color_picker_high"), 
                       "High color", 
                       "#00B3FA"
                       #showColour = "background",
                       #palette = "limited"
                     )
                   )
                   
                 })
                 
               }
               
               )
      }
      
    ) 
    
  })
  
  # get the colors from the ui settings
  mycol_final <- reactive({
    mycol <- switch(input$Coloring_method,
           "RcolorBrewer" = {
             if(!is.null(input$RcolorBrewer_theme)){
               
               color_theme_name <- input$RcolorBrewer_theme
               #print(color_theme_name)
               max_color <- brewer.pal.info[which(rownames(brewer.pal.info) == color_theme_name),1]
               
               # get the colorbrewer_colors
               
               my_colbrew_colors <- brewer.pal(max_color,color_theme_name)
               #print(my_colbrew_colors)
               # return the color generated
               colorRampPalette(my_colbrew_colors)(input$Number_of_colors_to_generate)

             }else{
               #avoid returnning null to cause error 
               rainbow(input$Number_of_colors_to_generate)
             }
             
           },
           "R built-in Palettes" = {
             switch(input$Rbuiltin_Palettes,
                           "rainbow" = {rainbow(input$Number_of_colors_to_generate)},
                           "heat.colors" = {heat.colors(input$Number_of_colors_to_generate)},
                           "terrain.colors"  = {terrain.colors(input$Number_of_colors_to_generate)},
                           "topo.colors"  = {topo.colors(input$Number_of_colors_to_generate)},
                           "cm.colors" = {cm.colors(input$Number_of_colors_to_generate)}
                   )
           },
           "Classical color collection" = {
             switch (input$Classical_color_collection,
                     "red-back-green" = {colorRampPalette(c("red", "black", "green"))(input$Number_of_colors_to_generate)},
                     "blue-white-red" = {colorRampPalette(c("blue", "white", "red"))(input$Number_of_colors_to_generate)}
                     )
             
           },
           "DIY colors" = {
             # take the input of the newly generated ui and then generate the color gradient
             switch (input$DIY_color_mode,
                    "single color" = {
                      #print(input$color_picker_single)
                      input$color_picker_single},
                    "Dual-Color gradient" = {
                      if(!is.null(input$color_picker_high)){
                        colorRampPalette(c(input$color_picker_low, input$color_picker_high))(input$Number_of_colors_to_generate)
                      }
                    },
                    
                    "Tri-Color gradient" = {
                      if(!is.null(input$color_picker_high)){
                        colorRampPalette(c(input$color_picker_low, input$color_picker_middle,input$color_picker_high))(input$Number_of_colors_to_generate)
                      }
                     }
                    )
             
           }
    )
    
    ##
    if(input$If_rev_colors){
      mycol <- rev(mycol)
    }
    return(mycol)
    
  })
  
  
  
  # output the plot if 
  observe({
      if(!is.null(mycol_final()) && input$If_preview_colors){
        
        # if previous switched on
   
          # plot the image
          postscript(tempfile())
          dev.control('enable')
          
          pie(rep(1,input$Number_of_colors_to_generate),
              col = mycol_final(),
              labels = NA,
              radius = 1,
              border = NA
          )
          
          color_display <- recordPlot()
          dev.off()
          
          # generate the UI
          output$plot_selected_color_theme <- renderPlot({
            replayPlot(color_display)
          })
      }
   })
  
  # 

  
  # return colors
  
  return(mycol_final)
  
}







datatableProcess_UI <- function(id,boxtitle,boxwidth) {
  
  install.packages.auto(dplyr)
  
  ns <- NS(id)

  box(
    title = boxtitle,
    width = boxwidth,
    solidHeader = TRUE,
    status = "primary",
    collapsible = TRUE,
    
    # process settings
    column(4,
           # show or hide tables

          checkboxInput(ns("Show_original_table"),label = "Show original data table", FALSE),
          checkboxInput(ns("Show_processed_table"),label = "Show processed data table", FALSE),
           
           
          # ui for manual selection   ---------------------------------------------------
          
           checkboxInput(ns('manual_select_row_column'), 'Manual selection of rows and columns?', FALSE),
           #bsTooltip(ns("manual_select_row_column"), "Note that maunal selelction Works better for small tables",
          #          "right"),

           conditionalPanel(
             condition = paste0("input['", ns("manual_select_row_column"), "']"),
             box(
               width =12,
               status = "primary",
               solidHeader = TRUE,
                column(6, uiOutput(ns("ui_selectInput_select_row"))),
                column(6, uiOutput(ns("ui_selectInput_select_column")))
             )

           ),
          
          # ui for selection by pattern for rows  ---------------------------------------------------
          
          checkboxInput(ns('RegExp_select_row'), 'Name-Pattern selection of rows?', FALSE),

          conditionalPanel(
            condition = paste0("input['", ns("RegExp_select_row"), "']"),
            wellPanel(
              selectInput(ns("regexp_type_row"), "Pattern type:",
                          choices = list("starts with", "ends with","contains"),
                          selected = "starts with"),
              textInput(ns("regexp_value_row"), "Type the text to match", value = "e.g. sample_")

            )
          ),
          
          # ui for selection by pattern for columns  ---------------------------------------------------
          
          checkboxInput(ns('RegExp_select_column'), 'Name-Pattern selection of columns?', FALSE),
          
          conditionalPanel(
            condition = paste0("input['", ns("RegExp_select_column"), "']"),
            wellPanel(
              selectInput(ns("regexp_type_column"), "Pattern type:",
                          choices = list("starts with", "ends with","contains"),
                          selected = "starts with"),
              textInput(ns("regexp_value_column"), "Type the text to match", value = "e.g. variable_")
              
            )
          ),
          
          # ui for filtering out missing values  ---------------------------------------------------
           checkboxInput(ns('missingvalue_filtering'), 'Filtering out rows with missing values?', FALSE),
           
           conditionalPanel(
             condition = paste0("input['", ns("missingvalue_filtering"), "']"),
             wellPanel(
               sliderInput(ns("missingvalue_filtering_q"), label = "Q value of presence", min = 0,
                           max = 1, value = 0.75),
               helpText("\tQ vaule is the presence percentage threshold. For example, 0,75 means 75% of presence across all samples")
               # bsTooltip(ns("missingvalue_filtering_q"), "Q vaule is the presence percentage threshold",
               #           "right", options = list(container = "body"))
             )
           ),
           
          
          # ui for log transformation  ---------------------------------------------------
           checkboxInput(ns('matrix_transform_switch'), 'Log transform the whole data matrix?', FALSE),
          
            conditionalPanel(
              condition = paste0("input['", ns("matrix_transform_switch"), "']"),
              wellPanel(
                selectInput(ns("log_transform"), "Log tranform?",
                            choices = list("log10 tranform", "log2 tranform","ln tranform"),
                            selected = "log10 tranform")

              )
              
            ),
          
          # ui for missing value imputation  ---------------------------------------------------
           
             checkboxInput(ns('matrix_inputation'), 'Sequential missing value impuation?', FALSE),
             conditionalPanel(
               condition = paste0("input['", ns("matrix_inputation"), "']"),
               wellPanel(
                 selectInput(ns("matrix_inputation_NA_type"),
                             "Type of NA value in data",
                             choices = c("NA", "0", "Inf"),
                             selected = "NA"),
                 # 20190724, update log by Leyuan: I changed the above selectInput by adding choices =, and selected = "NA"),
                 # becauses otherwise I keep getting Error in <Anonymous>: No handler registered for type data_table_preprocess-matrix_inputation_NA_type:-selectized
                 selectInput(ns("imputation_by"), 
                              "Imnputation by:",
                              choices = c("row","column" ), 
                              selected = "row"),
                 sliderInput(ns("matrix_inputation_alpha"), label = "Alpha/Strength:", min = 0,
                             max = 1, value = 0.9)
               )
               
             ),
           
           # # ui for scaling  ---------------------------------------------------
          
            checkboxInput(ns('scale_table'), 'Scale/Z-score/Normalize table?', FALSE),
             conditionalPanel(
               condition = paste0("input['", ns("scale_table"), "']"),
               wellPanel(
                selectInput(ns("scale_by"),
                            "Scale on:",
                            choices = c("row","column" ),
                            selected = "row"),
                helpText("\tThis function centers and scales numeric matrix. ",
                         "Center means the data(row or column)'s mean is going to be 0. ",
                         "Scale is done after centering. Scale is done by divding each value by their standard deviation.",
                         "This function is called z-score some elsewhere. "
                         )

                 
               )
             ),
          # ui for transpose  ---------------------------------------------------
          checkboxInput(ns('transpose_table'), 'Transpose table?', FALSE),

          
         
         # apply button
         actionButton(ns("button_apply_processing"), 
                      icon = icon("paper-plane"),
                      label = "Apply Selected Procedure(s)",
                      style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
         ),
         
         # reset button
         # actionButton(ns("button_reset"), 
         #              icon = icon("refresh"),
         #              label = "Reset",
         #              style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
         # ) 
         
         uiOutput("Dowload processed datatable ")
         
        ),
    
      #ui for result summary   ---------------------------------------------------
       column(8,
           #display summary
           uiOutput(ns("ui_process_summary")),
           uiOutput(ns("ui_processedata_download")),
           # display check scale plot if data is scaled
           uiOutput(ns("ui_boxplot_check_scale")),
          
           
           # display tables if user select to
           conditionalPanel(
             condition = paste0("input['", ns("Show_original_table"), "']"),
             box(
               title = "Original Data",
               status = "primary", 
               solidHeader = TRUE,
               width = 12, 
               DT::dataTableOutput(ns('table_original'))
             )
           ),
           
           conditionalPanel(
             condition = paste0("input['", ns("Show_processed_table"), "']"),
             box(
               title = "Processed Data",
               status = "primary", 
               solidHeader = TRUE,
               width = 12, 
               DT::dataTableOutput(ns('table_processed'))
             )
           )
    )

    
    )
  
  }


# datatableProcess_UI_debug <- function(id,boxtitle,boxwidth) {
#   
#   install.packages.auto(dplyr)
#   
#   ns <- NS(id)
#   
#   box(
#     title = boxtitle,
#     width = boxwidth,
#     solidHeader = TRUE,
#     status = "primary",
#     
#     # process settings
#     column(4,
#            # show or hide tables
#            
#            # ui for transpose  ---------------------------------------------------
#            checkboxInput(ns('transpose_table'), 'Transpose table?', FALSE),
#            
#            
#            
#            # apply button
#            actionButton(ns("button_apply_processing"), 
#                         icon = icon("paper-plane"),
#                         label = "Apply",
#                         style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
#            )
#            
# 
#     )
#   )
#   
# }



# datatableProcess_debug <- function(input, output, session, data_table) {
#   ns <- session$ns
#   
#   data_table_processed <- reactive({
#     if( input$button_apply_processing == 0 ){ # set up the dependency on the button
#       return(data_table)
#     }else{
#       isolate({
#         
#         if(input$transpose_table){
#           data_processed <- t(data_table)
#         }
#         
#       })
#       
#       data_processed
#     }
#   })
#   
#   data_table_processed
#   
# }
datatableProcess <- function(input, output, session, data_table) {
  ns <- session$ns
  
  output$table_original <- DT::renderDataTable(
    data_table,
    extensions = c('Buttons'),
    #selection = list(target = "row+column"),
    options = list(
      dom = 'Bfrtip',
      pageLength = 5,
      scrollX = TRUE,
      buttons = I('colvis')
      #buttons = list(list(extend = 'colvis', columns = 1:2))
    )
  )

  # start data processing ----------------------------------------------------------------------
  
  data_table_processed <- reactive({

    # only if the box is checked
    if(input$manual_select_row_column){
      output$ui_selectInput_select_column <- renderUI({
        column_index <- 1:ncol(data_table)
        names(column_index) <- colnames(data_table)
        selectInput(ns("keep_column_index"),
                    "Columns to keep:",
                    column_index,
                    multiple = TRUE)
        
      })
      output$ui_selectInput_select_row <- renderUI({
        row_index <- 1:nrow(data_table)
        names(row_index) <- rownames(data_table)
        selectInput(ns("keep_row_index"),
                    "Rows to keep:",
                    row_index,
                    multiple = TRUE)
        
      })
    }
    
    
    if( input$button_apply_processing == 0 ){ # set up the dependency on the button
      return(data_table)
    }else{
      isolate({
        
        # initialize the output text for analysis summary
        process_summary <- paste0("In the orginal data matrix", "\n",
          "Number of columns:", ncol(data_table), "\n",
          "Number of rows:", nrow(data_table), "\n"
          )
        
        # _manual row and column selection ------------------------------------------------
        if(input$manual_select_row_column){
          
          # do filtering
          if (length(input$keep_row_index)) {
            rows_keep <- as.numeric(input$keep_row_index)
          }else{
            rows_keep <- 1:nrow(data_table)
          }
          #print(rows_keep)
          
          if(length(input$keep_column_index)){
            columns_keep <- as.numeric(input$keep_column_index)
            data_processed <- data_table[rows_keep, columns_keep, drop = FALSE]
          }else{
            columns_keep <- 1:ncol(data_table)
            data_processed <- data_table[rows_keep, , drop = FALSE]
          }
          
          # format the summary text
          outputtext <- paste0("Rows Selected: ", toString(rows_keep), "\n",
                                                    "Columns Selected: ", toString(columns_keep)
                                                    )
          process_summary <- paste(process_summary,outputtext, sep ="\n")
          
        }else{
          data_processed <- data_table
          
        }
        # _pattern row selection ------------------------------------------------
        
        if(input$RegExp_select_row){
          
          data_processed <- as.data.frame(t(data_processed))
          #choices = list("starts with", "ends with","contains"),
          data_processed <- switch(input$regexp_type_row,
                
                                                    
               "starts with" = {
                 t(dplyr::select(data_processed, starts_with(input$regexp_value_row)))
               },
               "ends with" = {
                 t(dplyr::select(data_processed, ends_with(input$regexp_value_row)))
               },
               "contains" = {
                 t(dplyr::select(data_processed, contains(input$regexp_value_row)))
               }
                                   
          )
          
        }
        
        # _pattern column selection ------------------------------------------------
        
        if(input$RegExp_select_column){

          #choices = list("starts with", "ends with","contains"),
          data_processed <- switch(input$regexp_type_column,

            "starts with" = {
              dplyr::select(data_processed, starts_with(input$regexp_value_column))
            },
            "ends with" = {
              dplyr::select(data_processed, ends_with(input$regexp_value_column))
            },
            "contains" = {
              dplyr::select(data_processed, contains(input$regexp_value_column))
            }

          )

        }

        
        
        
        # _missingvalue_filtering ------------------------------------------------
        if(input$missingvalue_filtering){
          
          #process_summary$missingvalue_filtering <- "yes"
          
           result_mising_value_filtering <- missingvalue_filtering(data_processed, threshold = input$missingvalue_filtering_q)
          
           data_processed <-result_mising_value_filtering$data_qualified
           data_filtered_out <-result_mising_value_filtering$filtering_summary$data_not.qualified
           
           data_filtering_number_qualified <- result_mising_value_filtering$filtering_summary$number.qualified
           data_filtering_number_notqualified <- result_mising_value_filtering$filtering_summary$number.not.qualified
          
           # output for the not qulified tables
           output$table_filtered_out <- DT::renderDataTable(
             data_filtered_out,
             extensions = c('Buttons'),
             #selection = list(target = "row+column"),
             options = list(
               dom = 'Bfrtip',
               pageLength = 5,
               scrollX = TRUE,
               buttons = I('colvis')
               #buttons = list(list(extend = 'colvis', columns = 1:2))
             )
           )
           
           # format the summary text
           outputtext <- paste0("Missing value impuation:  Yes", "\n",
                                "\tQ value: ",input$missingvalue_filtering_q, "\n",
                                "Numer of rows qualified: ", data_filtering_number_qualified, "\n",
                                "Numer of rows not qualified: ",data_filtering_number_notqualified
                                )
           process_summary <- paste(process_summary,outputtext, sep ="\n")
        }
        
        # _log transformation------------------------------------------------
        
        if(input$matrix_transform_switch){
          data_processed <- switch(input$log_transform,
                                   "log10 tranform" = {log10(data_processed)},
                                   "log2 tranform" = {log2(data_processed)},
                                   "ln tranform" = {log(data_processed)}
          )
          
           outputtext <- paste0("Log transformation: YES", "\n",
                                "Transformation type: ", input$log_transform
                                )
           process_summary <- paste(process_summary,outputtext, sep ="\n")
          
        }

        # _missing value imputation------------------------------------------------
        if(input$matrix_inputation){
          data_processed[data_processed == input$matrix_inputation_NA_type] <- NA
          if(input$imputation_by == "row"){
            data_processed_imp <- t(rrcovNA::impSeqRob(t(data_processed), alpha = input$matrix_inputation_alpha)$x)
            # reorder columns becase imputation by row might change column orders
            data_processed <- data_processed_imp[,match(colnames(data_processed),colnames(data_processed_imp))]
          }else{
            data_processed_imp <- rrcovNA::impSeqRob(data_processed, alpha = input$matrix_inputation_alpha)$x
            # reorder rows becase imputation by column might change row orders
            data_processed <- data_processed_imp[match(rownames(data_processed),rownames(data_processed_imp)),]
          }
          
          outputtext <- paste0("Data imputation:  Yes", "\n",
                               "Imputation by: ", input$imputation_by, "\n",
                               "Alpha value of imputation: ",input$matrix_inputation_alpha 
                               )
          process_summary <- paste(process_summary,outputtext, sep ="\n")
          
        }
        
        # _do scale on column or row------------------------------------------------
        if(input$scale_table){
          if(input$scale_by == "row"){
            
            data_processed_before_scaling <- data_processed
            # plot beofore scaling
            output$boxplot_before_scale  <- renderPlot({
              boxplot(t(data_processed_before_scaling),main = "Before Scaling on Row")
            })
            
            # do scaling
            data_processed <- t(scale(t(data_processed)))
            
            # plot after scaling
            output$boxplot_after_scale  <- renderPlot({
              boxplot(t(data_processed),main = "After Scaling on Row")
            })
        
          }else{
            # plot beofore scaling
            data_processed_before_scaling <- data_processed
            output$boxplot_before_scale  <- renderPlot({
              boxplot(data_processed_before_scaling,main = "Before Scaling on Column")
            })
            
            # do scaling
            data_processed <- scale(data_processed)
            
            # plot after scaling
            output$boxplot_after_scale  <- renderPlot({
              boxplot(data_processed,main = "After Scaling on Column")
            })
            
          }
          
          # generat ui
          output$ui_boxplot_check_scale <- renderUI({
            box(
              title = "Check the scale effect",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              
              column(6,
                     plotOutput(ns("boxplot_before_scale"))
              ),
              column(6,
                     plotOutput(ns("boxplot_after_scale"))
              )
              
            )
            
          })
          
          outputtext <- paste0("Scale matrix: Yes ", "\n",
                               "Scale by: ",input$scale_by 
                               )
          process_summary <- paste(process_summary,outputtext, sep ="\n")
          
        }
        
        # _transpose------------------------------------------------
        if(input$transpose_table){
          data_processed <- t(data_processed)
          
           outputtext <- paste0("Transpose data matrix: Yes")
           process_summary <- paste(process_summary,outputtext, sep ="\n")
        }

        # _summarize data process settings ---------------------------------------------------------------

        process_summary <- paste0(process_summary, "\n",
                                  "In the Processed data matrix", "\n",
                                  "Number of columns:", ncol(data_processed), "\n",
                                  "Number of rows:", nrow(data_processed), "\n"
        )
        
        output$process_summary <- renderPrint({ 
          cat(process_summary)
        })
        
        output$ui_process_summary <- renderUI({
          box(
            title = "Data process summary",
            status = "primary", 
            solidHeader = TRUE,
            width = 12, 
            verbatimTextOutput(ns("process_summary"))
            #helpText("test")
          )
        })
        
        
        callModule(tableDownload,"download_processed_table", data = data_processed, filename_tag = "Processed_dt") 
        callModule(plaintextDownload,"download_process_summary", data = process_summary, filename_tag = "Report_summary")
        
        output$ui_processedata_download <- renderUI({
          box(
            #title = "Data process summary",
            status = "primary", 
            solidHeader = TRUE,
            width = 12, 
            #verbatimTextOutput(ns("process_summary"))
            tableDownloadUI(ns("download_processed_table"), # make sure to use ns, because this is moduel in module,
                            label = "Download Processed Data table"),
            plaintextDownloadbutton(ns("download_process_summary"), # make sure to use ns, because this is moduel in module,
                            label = "Download process summary")
            
            #helpText("test")
          )
        })
        
        
        
        # _display the processed table ---------------------------------------------------------------
        output$table_processed <- DT::renderDataTable(
          data_processed,
          extensions = c('Buttons'),
          #selection = list(target = "row+column"),
          options = list(
            dom = 'Bfrtip',
            pageLength = 5,
            scrollX = TRUE,
            buttons = I('colvis')
            #buttons = list(list(extend = 'colvis', columns = 1:2))
          )
        )
        
        data_processed
        #return(mtcars)
        
      }) # isolate ends
    }
    
  }) 
 
  # return values
  data_table_processed
  #return(mtcars)
  
  # return(list(
  #   data_table_processed = rvalues$data_processeddata_processed
  #   #"summary" = process_summary
  # )
  # )
  
}



# Valid colors are: red, yellow, aqua, blue, light-blue, 
# green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

# ui function
# this name_tag is for labeling the down load file name
# plot_type = c("ggplot2","plotly", "recordPlot")



plotInBox_UI <- function(id,boxtitle = NULL,boxwidth = 6) {
  ns <- NS(id)
  box(
    title = boxtitle,
    width = boxwidth,
    solidHeader = TRUE,
    status = "primary",
    # box for plotting
    uiOutput(ns("plot_in_box")),
    
    # box for options
    box(
      width = 12,
      solidHeader = TRUE,
      #checkboxInput(ns("display"), "Show Value"),
      checkboxInput(ns("Show_plot_options"),"Plotting Options", FALSE),
      
      conditionalPanel(
        condition = paste0("input['", ns("Show_plot_options"), "']"),
        box(
          title = "Resize noninteractive plot",
          solidHeader = TRUE,
          status = "primary",
          width = 6,
          sliderInput(ns("plot_width"), "Plot Width", 0, 100, 100),
          sliderInput(ns("plot_height"), "Plot Height", 4, 1000, 700),
          actionButton(ns("replot"),
                       icon = icon("refresh"),
                       label = "Replot"
                       
          )
        ),
        box(
          title = "Export plot",
          solidHeader = TRUE,
          status = "primary",
          width = 6,
          textInput(inputId= ns("export_width"), label="width (inch)", value = 10),
          textInput(inputId= ns("export_height"), label="height (inch)", value = 8),
          textInput(inputId= ns("pointsize"), label="Text size", value = 12),
          textInput(inputId= ns("resolution"), label="PPI for PNG", value = 300),
          
          downloadButton(ns("download_SVG"), 'SVG'),
          downloadButton(ns("download_PDF"), 'PDF'),
          downloadButton(ns("download_PNG"), 'PNG')
        )
        
      )
      
    )
    
  )
}

plotInBox <- function(input, output, session, plot_object, plot_type, name_tag) {
  ns <- session$ns
  
  
  # output$plot <- renderPlot(
  #   plot(1,1)
  # )
  # 
  
  observe({
    if(plot_type == "ggplot2"){
      output$plot <- renderPlotly({
        ggplotly(plot_object)
      })
      # for SVG download
      output$download_SVG <- downloadHandler(
        filename <- paste0(name_tag,".svg"), 
        content <- function(file) {
          svg(file,
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height),
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          #plot_object
          dev.off()
        }
      )
      # for high resolution PDF download
      output$download_PDF <- downloadHandler(
        
        filename <- paste0(name_tag,".pdf"), 
        content <- function(file) {
          pdf(file,
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          
          #plot_object
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          dev.off()
        }
      )
      # for high resolution PNG download
      output$download_PNG <- downloadHandler(
        
        filename <- paste0(name_tag,".png"), 
        content <- function(file) {
          png(file,res = as.numeric(input$resolution),
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
              pointsize = as.numeric(input$pointsize),
              units = "in", bg = "white")
          
          #plot_object
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          dev.off()
        }
      )
      
    }else if(plot_type == "plotly"){
      output$plot <- renderPlotly({
        plot_object
      })
      
    }else if(plot_type == "recordPlot"){
      output$plot <- renderPlot({
        #print(plot_object)
        replayPlot(plot_object)
      })
      
      # for SVG download
      output$download_SVG <- downloadHandler(
        filename <- paste0(name_tag,".svg"), 
        content <- function(file) {
          svg(file,
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height),
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          # this is important, it has to be print, not plot, or nothing, 
          # because, the object is already a plot
          print(plot_object)
          dev.off()
        }
      )
      # for high resolution PDF download
      output$download_PDF <- downloadHandler(
        
        filename <- paste0(name_tag,".pdf"), 
        content <- function(file) {
          pdf(file,
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          
          #plot_object
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          dev.off()
        }
      )
      # for high resolution PNG download
      output$download_PNG <- downloadHandler(
        
        filename <- paste0(name_tag,".png"), 
        content <- function(file) {
          png(file,res = as.numeric(input$resolution),
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
              pointsize = as.numeric(input$pointsize),
              units = "in", bg = "white")
          # this is important, it has to be print, not plot, or nothing, 
          # because, the object is already a plot
          print(plot_object)
          dev.off()
        }
      )
    }
    
  })
  
  
  
  # render output the plotting box
  output$plot_in_box <- renderUI({
    
    box(
      width = 12,
      solidHeader = TRUE,
      
      
      if(plot_type == "ggplot2"| plot_type == "plotly"){
        plotlyOutput(ns("plot"), 
                     width = paste0(input$plot_width, "%"),
                     height = paste0(input$plot_height, "px"))
      }else if(plot_type == "recordPlot"){
        plotOutput(ns("plot"), 
                   width = paste0(input$plot_width, "%"),
                   height = paste0(input$plot_height, "px"))
      }else{
        print("wrong plot_type, continue using recordPlot")
        plotOutput(ns("plot"), 
                   width = paste0(input$plot_width, "%"),
                   height = paste0(input$plot_height, "px"))
      }
      
    )
    
  })
  
  #reactive({input$slider + 5})
}





BoxSVGPNGUI <- function(id, 
                        box_title = NULL, 
                        plot_type = "ggplot2", 
                        collapsed = FALSE,
                        collapsible = FALSE,
                        box_width = 6, 
                        plot_width = "100%", 
                        plot_height = "500px"){
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(

    box(
      title = box_title, # this name
      width = box_width,
      status = "primary", 
      solidHeader = TRUE,
      collapsible = collapsible,
      collapsed = collapsed,
      
      if(plot_type == "ggplot2"| plot_type == "plotly"){
        plotlyOutput(outputId = ns("plot"), width = plot_width, height = plot_height)
      }else if(plot_type == "recordPlot"){
        plotOutput(outputId = ns("plot"), width = plot_width, height = plot_height)
      }else{
        print("wrong plot_type, continue using recordPlot")
        plotOutput(outputId = ns("plot"), width = plot_width, height = plot_height)
      }
      ,

      checkboxInput(ns("Show_export_options"),"Export Plot", FALSE),
     
      # show if the checkbox checked
      # only show export menu for ggplot2 and recordPlot type
      if(plot_type == "ggplot2"| plot_type == "recordPlot"){
        tagList(
          conditionalPanel(
            condition = paste0("input['", ns("Show_export_options"), "']"),
            # box(
            #   title = "Resize plot",
            #   solidHeader = TRUE,
            #   status = "primary",
            #    width = 6,
            #    textInput(inputId = ns("plot_width"), label="width (inch)", value = 10),
            #    textInput(inputId = ns("plot_height"), label="height (inch)", value = 8),
            #     actionButton(ns("re-plot"), 
            #                  icon = icon("refresh"),
            #                  label = "Re-plot"
            #                  
            #     )
            # ),      
            box(
              title = "Export plot",
              solidHeader = TRUE,
              status = "primary",
               width = 6,
               textInput(inputId= ns("export_width"), label="width (inch)", value = 10),
               textInput(inputId= ns("export_height"), label="height (inch)", value = 8),
               textInput(inputId= ns("pointsize"), label="Text size", value = 12),
               textInput(inputId= ns("resolution"), label="PPI for PNG", value = 300),
               
               downloadButton(ns("download_SVG"), 'SVG'),
               downloadButton(ns("download_PDF"), 'PDF'),
               downloadButton(ns("download_PNG"), 'PNG')
              )

          )
        )
      }

    ) 
  )
}



# server function:

# this name_tag is for file name export

BoxSVGPNG <-function(input, 
                     output, 
                     session, 
                     plot_object, 
                     name_tag, 
                     plot_type){
  
  observe({
    if(plot_type == "ggplot2"){
      output$plot <- renderPlotly({
        ggplotly(plot_object)
      })
      # for SVG download
      output$download_SVG <- downloadHandler(
        filename <- paste0(name_tag,".svg"), 
        content <- function(file) {
          svg(file,
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height),
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          #plot_object
          dev.off()
        }
      )
      # for high resolution PDF download
      output$download_PDF <- downloadHandler(
        
        filename <- paste0(name_tag,".pdf"), 
        content <- function(file) {
          pdf(file,
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          
          #plot_object
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          dev.off()
        }
      )
      # for high resolution PNG download
      output$download_PNG <- downloadHandler(
        
        filename <- paste0(name_tag,".png"), 
        content <- function(file) {
          png(file,res = as.numeric(input$resolution),
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
              pointsize = as.numeric(input$pointsize),
              units = "in", bg = "white")
          
          #plot_object
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          dev.off()
        }
      )
      
    }else if(plot_type == "plotly"){
      output$plot <- renderPlotly({
        plot_object
      })

    }else if(plot_type == "recordPlot"){
      output$plot <- renderPlot({
        #print(plot_object)
        replayPlot(plot_object)
      })
      
      # for SVG download
      output$download_SVG <- downloadHandler(
        filename <- paste0(name_tag,".svg"), 
        content <- function(file) {
          svg(file,
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height),
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          # this is important, it has to be print, not plot, or nothing, 
          # because, the object is already a plot
          print(plot_object)
          dev.off()
        }
      )
      # for high resolution PDF download
      output$download_PDF <- downloadHandler(
        
        filename <- paste0(name_tag,".pdf"), 
        content <- function(file) {
          pdf(file,
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          
          #plot_object
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          dev.off()
        }
      )
      # for high resolution PNG download
      output$download_PNG <- downloadHandler(
        
        filename <- paste0(name_tag,".png"), 
        content <- function(file) {
          png(file,res = as.numeric(input$resolution),
              width = as.numeric(input$export_width), 
              height = as.numeric(input$export_height), 
              pointsize = as.numeric(input$pointsize),
              units = "in", bg = "white")
          # this is important, it has to be print, not plot, or nothing, 
          # because, the object is already a plot
          print(plot_object)
          dev.off()
        }
      )
    }
    
  })
  
}


# _____Display a data table from the analysis result within a box,  --------
# usage


BoxTableUI <- function(id, name_tag = "", boxcollapsed = FALSE, boxwidth = 12){
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    box(
      title = name_tag,
      status = "primary", 
      width = boxwidth,
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = boxcollapsed,
      DT::dataTableOutput(ns("displaydatatable"))
    )
  )
}


BoxTable <-function(input, output, session, table_object, server = TRUE){

      output$displaydatatable <- DT::renderDataTable(
        table_object, 
        server = server, 
        filter = 'top', extensions = c('Buttons','Scroller'), 
        options = list(
          autoWidth = TRUE, 
          dom = 'Bfrtip',
          buttons = c('colvis'),
          scrollY = 200,
          scrollX = TRUE)
      ) 
      
  
}



# _____tsvCSV file upload and readin ----------------------------------------------

# how to use
# on ui: 
# tsvcsvFileInput("datafile", "User data (.csv format)")
# dataTableOutput("table")

# on server
# datafile_upload <- callModule(tsvcsvFile, "datafile",
#                       stringsAsFactors = FALSE)

# output$table <- renderDataTable({
#   datafile_upload()
# })

# Module UI function
tsvcsvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    
    checkboxInput(ns('header'), 'First line is Header?', TRUE),
    checkboxInput(ns('rowname'), 'First column is rownames?', TRUE),
    
    selectInput(ns('sep'), 'Separator',
                c(Comma=',',
                  Semicolon=';',
                  Tab='\t'),
                '\t'),
    selectInput(ns('quote'), 'Quote',
                c(None='',
                  'Double Quote'='"',
                  'Single Quote'="'"),
                ''),
    fileInput(ns('file'), label,
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
}

# Module server function
tsvcsvFile <- function(input, output, session) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    if(is.null(userFile())){return(NULL) }  
    # this is alway the first line when reading in a file to avoid the error message
    
    if(input$rowname){
      read.delim(userFile()$datapath, header=input$header, sep=input$sep, 
                 quote=input$quote, row.names = 1)
    }else{
      read.delim(userFile()$datapath, header=input$header1, sep=input$sep, 
                 quote=input$quote)
    }
  })
  
  # We can run observers in here if we want to
  #bserve({
  #  msg <- sprintf("File %s was uploaded", userFile()$name)
  #  cat(msg, "\n")
  #})
  
  # Return the reactive that yields the data frame
  return(dataframe)
}


# _____TSV CSV file download ----------------------------------------------

## additional argument to define the label on the downloadButton
tableDownloadUI <- function(id, label = "Download CSV") {
  ns <- NS(id)
  
  downloadButton(ns("download_table"), label)
}

## allow users of the module to input a (reactive) data.frame to download as csv and a name for the file
tableDownload <- function(input, output, session, data, filename_tag = NULL
                        ) {
  
  output$download_table <- downloadHandler(
    filename = function() {
        paste0(filename_tag,"_", Sys.time(), ".tsv")
    },
    content = function(file) {
      #write.csv(data, file)
      write.table(data,   # do not use data(),  here accepts either values of reactive expressions
                  file, 
                  sep = "\t",
                  col.names = NA)
    }
  )
}


# _____plaintext file download ----------------------------------------------

## additional argument to define the label on the downloadButton
plaintextDownloadbutton <- function(id, label = "Download report") {
  ns <- NS(id)
  downloadButton(ns("download_report"), label)
}

## allow users of the module to input a (reactive) data.frame to download as csv and a name for the file
plaintextDownload <- function(input, output, session, data, filename_tag = "report_summary") {
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0(filename_tag,"_", Sys.time(), ".txt")
    },
    content = function(file) {
      write(data,   # do not use data(),  here accepts either values of reactive expressions
          file
          #file = "report.txt", 
        )
    }
  )
}




