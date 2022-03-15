range_standarize_1 <- function(x){
  (x-min(x))/(max(x)-min(x))
}

range_standarize_100 <- function(x){
  round(100*(x-min(x))/(max(x)-min(x)))
}


# some interaction network has edges data.frame data structure, but name as nodes names, 
# for sankey plot, in d3, only edge data.frame with node number/index accepted, from 0 
# this fucntion preprocess the edge data.frame and retures two data.frames ready for plot
# df format:
# the first col is the from/soruce column#
# the second col is the to/target column

recode_for_sankey <- function(df){
  install.packages.auto(car) # use the recode function
  nodes_names <- union(df[[1]], df[[2]])
  code <- 0:length(nodes_names)
  nodes <- data.frame(name = nodes_names)
  
  expression <- toString(paste0("'",nodes_names,"'"," = ",code,collapse=";"))
  df_recode<- as.data.frame(apply(df, 2, function(x) {x <- recode(x,expression)}))
  
  return(list(df_links = df_recode,
              df_nodes = nodes
  ))
  
}





ggsimpleline <- function(data_matrix, 
                         plot.by = "Row", 
                         x_cord = NULL, # has to be a numeric vector, with the same length with either row (for column plot) or column (for row plot)
                         index = c(1,2),
                         linetype = "solid",
                         linewidth = 1,
                         spline_smooth = FALSE,
                         # down is from ggplot2_prettier
                         maintitle = "",
                         xlab = "",
                         ylab = "",
                         axis.text.angle.x = 0,
                         axis.text.angle.y = 0,
                         vertical =  FALSE
){
  
  install.packages.auto(reshape2)
  data<- as.matrix(data_matrix)
  
  
  if(plot.by == "Row"){
    df <- t(data[index, ,drop = FALSE])
    xlabels <- colnames(data)
  } else if(plot.by == "Column"){
    df <- data[,index,drop = FALSE]
    xlabels <- rownames(data)
  }
  
  if(!is.null(x_cord)){
    rownames(df) <- x_cord
  }
  
  m<- melt(as.matrix(df))
  
  
  if(spline_smooth){
    # generate a new df for geom_line
    if(!is.null(x_cord)){
      
      df_smooth<- data.frame(apply(df, 2,function(x){spline(as.numeric(x_cord),x)$y}))
      m_smooth<- melt(as.matrix(df_smooth))
      #m_smooth$Var1 <- spline(as.numeric(x_cord))$y 
      m_smooth$Var1 <- spline(as.numeric(x_cord),df[,1])$x
      
    }else{
      df_smooth<- data.frame(apply(df, 2,function(x){spline(x)$y}))
      m_smooth<- melt(as.matrix(df_smooth))
      m_smooth$Var1 <- spline(1:nrow(df))$y # spline will generate 3 times point smoother by default ( change by n)
    }
    
    
    p <-   ggplot(m, aes(Var1, value, group=Var2, color = Var2)) + 
      geom_point()+
      geom_line(
        data=m_smooth,
        linetype = linetype,
        size = linewidth) +
      scale_colour_discrete(name = "")
    
    
  }else{
    p <- ggplot(m, aes(Var1, value, group=Var2, color = Var2)) + 
      geom_line(linetype = linetype,
                size = linewidth)  + 
      scale_colour_discrete(name = "")
    
  }
  
  p <- ggplot2_prettier(p,
                        maintitle = maintitle,
                        xlab = xlab,
                        ylab = ylab,
                        axis.text.angle.x = axis.text.angle.x,
                        axis.text.angle.y = axis.text.angle.y,
                        vertical =  FALSE
                        )
  
  return(p)
  
}





ggplot2_prettier <- function(ggplot2_object,
                             maintitle = "",
                             xlab = "",
                             ylab = "",
                             axis.text.angle.x = 0,
                             axis.text.angle.y = 0,
                             vertical =  FALSE
){
  
  p <- ggplot2_object + 
    labs(title = maintitle, 
         x = xlab, 
         y = ylab ) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = axis.text.angle.x, hjust = 1),
          axis.text.y = element_text(angle = axis.text.angle.y, hjust = 1),
          panel.grid = element_blank()) 
  
  return(p)
  
}





#________________________________________________________________________________________
#     matrix_quick_heatmap
#________________________________________________________________________________________

# ___Description___: 
# 1: 
# 2: 

# ___Arguments___:
# data matrix
# factor (usually the grouping information) to be color grouped in either column  or row

#____Usage____;

# ___Values___:
# 
# 


matrix_quick_heatmap <- function(matrix, 
                                 scale = "row",
                                 col_groupcolor_factor = NULL , 
                                 row_groupcolor_factor = NULL, 
                                 Col_tree = TRUE){
  
  install.packages.auto(gplots)
  svg(tempfile(),onefile = TRUE)
  dev.control('enable')  
  
  if(is.null(row_groupcolor_factor) & is.null(col_groupcolor_factor)){
    # if no grouping information provided
    if(Col_tree){
      heatmap.2(matrix, col=bluered, trace = "none",
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = "") 
    }else{
      heatmap.2(matrix, col=bluered, trace = "none",
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                Colv  = FALSE,
                dendrogram = "row",
                key.title  = "",
                key.xlab = "",
                key.ylab = "") 
    }
    
    
    
  }else if((length(col_groupcolor_factor) > 0) & is.null(row_groupcolor_factor)){
    # if only colum grouping information provided
    
    col_groupcolor_factor <- col_groupcolor_factor[order(col_groupcolor_factor[,2]),]
    matrix<- matrix[,match(col_groupcolor_factor[,1],colnames(matrix))]
    
    color_labeling <- factor(col_groupcolor_factor[,2]) # removing unused factors
    levels(color_labeling) <- rainbow(length(levels(color_labeling))) # rename the 
    color_labeling <- as.vector(color_labeling)
    if(Col_tree){
      
      heatmap.2(matrix, 
                col=bluered, 
                ColSideColors = color_labeling,
                trace = "none", 
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = "") 
      
      
    }else{
      heatmap.2(matrix, 
                col=bluered, 
                ColSideColors = color_labeling,
                trace = "none", 
                scale = scale,
                Colv  = FALSE, 
                dendrogram = "row",
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = ""
      ) 
    }
    
  }else if((length(row_groupcolor_factor) > 0) & is.null(col_groupcolor_factor)){
    # if only row grouping information provided
    
    row_groupcolor_factor <- row_groupcolor_factor[match(rownames(matrix), row_groupcolor_factor[,1]),]
    
    row_groupcolor_factor <- row_groupcolor_factor[order(row_groupcolor_factor[,2]),]
    matrix<- matrix[match(row_groupcolor_factor[,1],rownames(matrix)),]
    
    color_labeling <- factor(row_groupcolor_factor) # removing unused factors
    levels(color_labeling) <- rainbow(length(levels(color_labeling))) # rename the 
    color_labeling <- as.vector(color_labeling)
    if(Col_tree){
      heatmap.2(matrix,
                col=bluered, 
                ColSideColors = color_labeling,
                trace = "none", 
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = "" ) 
      
    }else{
      heatmap.2(matrix,
                col=bluered, 
                ColSideColors = color_labeling,
                trace = "none", 
                scale = scale,
                Colv  = FALSE,
                dendrogram = "row",
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = ""
      ) 
    }
  }
  p1 <- recordPlot()
  dev.off()
  return(p1)
}

