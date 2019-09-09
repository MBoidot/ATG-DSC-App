library(shiny)
library(xlsx)
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(viridis)
library(zoo)
library(lubridate)
library(plotly)
library(shinyFiles)


theme_set(theme_bw(10))
theme_update(panel.grid.major=element_line(colour="#b2b2b2", size=0.5),
             panel.grid.minor=element_line(colour="#c5c5c5", size=0.5),
             legend.title=element_text(size=18),
             axis.title.x=element_text(size=20),
             axis.title.y=element_text(size=20,angle=90,vjust=1.5),
             axis.text.x=element_text(size=16),
             axis.text.y=element_text(size=16),
             legend.text=element_text(size=16),
             plot.title=element_text(size=25, face="bold",vjust=0.5),
             strip.text.x=element_text(size=14,face="bold"),
             strip.text.y=element_text(size=14,face="bold"))


ui <- fluidPage(
  
  navbarPage("ATG-DSC App",
             tabPanel("Single file treatment",
                      pageWithSidebar(
                        headerPanel(""),
                        sidebarPanel(width = 2,
                                     fileInput("file_in","Data file", multiple = FALSE),
                                     uiOutput("choose_columns")
                                     
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Plot",
                                     #dataTableOutput("test"),
                                     #dataTableOutput("datasetTable"),

                                    fluidRow( 
                                     column(12, 
                                            h3("Metadata "),
                                            hr(),
                                            dataTableOutput("metadata_table"),
                                            hr()),

                                     column(12, 
                                            h3("Display properties"),
                                            hr(),

                                            column(2,sliderInput("width", "Width (%)", min = 0, max = 100, value = 100)),
                                            column(2,sliderInput("height", "Height (px)", min = 0, max = 2000, value = 500)),
                                            column(2,sliderInput("point_size", "Point size", min=0, max=6, value=0.25, step = 0.05)),
                                            column(2,sliderInput("point_alpha", "Alpha", min=0.25, max=1, value=1)),
                                            column(4,selectInput("col_theme", "Choose a color palette:", choices =  list(Brewer = c(`Set1` = 'Br_S1', `Set2` = 'Br_S2', `Set3` = 'Br_S3', `Spectral` = 'Br_Spectral'),
                                                                                                                Viridis = c(`Viridis` = 'Vir_vir',`Plasma` = 'Vir_plas',`Magma` = 'Vir_mag')))))),
                                    fluidRow( 
                                      
                                      column(12, 

                                             column(3,sliderInput("axlabel_size", "Axis label font size", min = 6, max = 24, value = 12)),
                                             column(3,sliderInput("strip_size", "Strip title font size", min = 6, max = 24, value = 12)))),
                                    hr(),
                                     
                                     uiOutput("plot.ui"),
                                     
                                     fluidRow(
                                       column(12,
                                              hr(),
                                              h3("Output Options"),
                                              
                                              column(3,sliderInput("Down_width", "Width (px)", min = 800, max = 4096, value = 2000)),
                                              column(3,sliderInput("Down_height", "Height (px)", min = 600, max = 4096, value = 1000)),
                                              column(2,sliderInput("point_size", "Point size", min=0, max=6, value=0.25, step = 0.05)),
                                              column(2,sliderInput("point_alpha", "Alpha", min=0.25, max=1, value=1)),
                                              br(),
                                              br(),
                                              column(2,downloadButton("dwnld_window",label = "Get plot")))
                                       
                                     )
                                    
                              ),

                            tabPanel("Data",
                                   column(12, 
                                          h3("Data "),
                                          hr(),
                                          dataTableOutput("datasetTable"),
                                          hr())
                                     
                                     ),
                            tabPanel("Help",htmlOutput("helptxt"))
                          )
                        )
                      )
                      ),
             tabPanel("Multiple file comparison"),
             tabPanel("Help")
  )
)


server = function(input, output,session){


#all meta data from DSC file
meta_all <- reactive({

  inFile <- input$file_in
  
  if (is.null(inFile)) {
    return(NULL)
  } else {

    lines <- readLines(inFile$datapath)
    headlines <- lines[1:25]
    
    meta_al <- as.data.frame(matrix(unlist(strsplit(headlines,";")),ncol=2,byrow=T))

  return(meta_al)
}
})
#extract decimal separator from metadata
dec_sep <- reactive({
  
  inFile <- input$file_in
  
  if (is.null(inFile)) {
    return(NULL)
  } else {
    
    lines <- readLines(inFile$datapath)
    decimal <- lines[4]
    return(decimal)
  }
})

#extract column separator from metadata
column_sep <- reactive({

  inFile <- input$file_in

  if (is.null(inFile)) {
    return(NULL)
  } else {

    lines <- readLines(inFile$datapath)
    col_separator <- unlist(strsplit(lines[5],":"))
    return(col_separator)
  }
})


#lecture de l'entete
metadata <- reactive({
  
  inFile <- input$file_in
  meta <-meta_all()
  
  if (is.null(inFile)) {
    return(NULL)
  } else {
    meta <- meta[c(9,11,12,14:16,20,21),]
    
    return(meta)
  }
})

#lecture des donnes
dataset <-reactive({ 
  inFile <- input$file_in

  if (is.null(inFile)) {
    return(NULL)
  } else {

    
    lines <- readLines(inFile$datapath)
    column_names <- gsub("#",'',unlist(strsplit(lines[27],";")))
    
    column_names <-sub("/", ".",column_names)
    
    #remove degree symbol from column title
    column_names <- as.character(type.convert(gsub("\\p{So}", "", column_names, perl = TRUE)))
    column_names <- gsub("\xf8", "", column_names)
    
    #check if blank was substracted and if so remove any unsubstracted column
    
    if (!is.null())
    
    
    lines <- lines[27:length(lines)]
    df <- unlist(strsplit(lines,";"))
    
    i <- 1
    while (grepl('/', df[i])){
      
      col <- i
      i <- i+1
    }
    data <- as.data.frame(matrix(unlist(strsplit(df,";")),ncol=col,byrow=T))
    data <- data[2:nrow(data),]
    colnames(data) <- column_names
    
    
    
    #echantillonage pour les gros fichiers

    if (nrow(data)>1000){
      data <- data[as.numeric(row.names(data)) %% 10 ==0,]
    }

    return(data)
    
  }
}
)

data_colnames<-reactive({
  
  colunames <- names(dataset())
  return(colunames)
})

reduced_data <- reactive({
  
  # If missing input, return to avoid error later in function
  if(is.null(input$file_in))
    return()
  
  dat <- dataset()
  
  if (is.null(input$columns) || !(input$columns %in% names(dat)))
    return()
  
  dat1 <- dat[,c(1,2)]
  dat2 <- dat[,input$columns]
  
  dat <- cbind(dat1,dat2)
  
  ddm <-melt(dat, id.vars = c("Temp..C","Time.min"))
  
  ddm$value <- as.numeric(ddm$value)
  ddm$Temp..C <- as.numeric(as.character(ddm$Temp..C))

  return(ddm)
})

color_palette <- reactive({
  plottype <- input$pl_type
  if (input$col_theme == "Vir_vir"){
      str_col_palette <- scale_color_viridis(discrete = TRUE,option="viridis")
  } else if (input$col_theme == "Vir_mag") {
      str_col_palette <- scale_color_viridis(discrete = TRUE,option="magma")
  } else if (input$col_theme == "Vir_plas") {
      str_col_palette <- scale_color_viridis(discrete = TRUE,option="plasma")
  } else if (input$col_theme == "Br_S1") {
      str_col_palette <- scale_color_brewer(palette="Set1")
  } else if (input$col_theme == "Br_S2") {
      str_col_palette <- scale_color_brewer(palette="Set2")
  } else if (input$col_theme == "Br_S3") {
      str_col_palette <- scale_color_brewer(palette="Set3")
  } else if (input$col_theme == "Br_Spectral") {
      str_col_palette <- scale_color_brewer(palette="Spectral")
  }

  return(str_col_palette)
})

output$reduced_plot <- renderPlot({
  req(input$file_in)
  ddm <- reduced_data() 
  col_pal <- color_palette()
    g <- ggplot(ddm,aes(Temp..C,value,color=variable))+geom_line(size=input$point_size,alpha=input$point_alpha)+facet_grid(variable~.,scales = "free_y")
    
    #shaping
    g <-  g + theme_bw()
    #g <- g + scale_fill_viridis(discrete = TRUE,option="plasma") 
    g <- g + col_pal
    g <- g + theme(axis.text.x=element_text(size=input$axlabel_size,angle=90,hjust=0,vjust=0.5),
                   axis.text.y=element_text(size=input$axlabel_size,hjust=0.5,vjust=0.5),
                   axis.title.x=element_text(size=input$axlabel_size+2),
                   axis.title.y=element_blank(),
                   plot.title=element_text(size=25, face="bold",vjust=1),
                   strip.text.x=element_text(size=input$strip_size,face="bold",color="white"),
                   strip.text.y=element_text(size=input$strip_size, face="bold",color="white"),
                   strip.background =element_rect(fill="#2d2d2d"),
                   legend.position="none")
    g <- g + xlab(expression("Temperature (degC)"))
    ggsave("plot.pdf", g,width =input$Down_width/300,height = input$Down_height/300)
    
  print(g)
  })

output$plot.ui <- renderUI({
  plotOutput("reduced_plot", width = paste0(input$width, "%"), height = input$height)
})

output$dwnld <- downloadHandler(
  filename = function() {
    "Plot.pdf" 
  },
  content=function(file){
    file.copy("plot.pdf", file, overwrite=TRUE)
  }
)

output$metadata_table <- renderDataTable({
  metadata()
})

output$datasetTable <- renderDataTable({
  head(dataset(),100)
})

output$choose_columns <- renderUI({
  
  # Create the checkboxes and select only one by default them all by default (don't show ville and date)
  
  checkboxGroupInput("columns", "Choose columns",
                     choices  = data_colnames()[3:length(names(dataset()))],
                     selected = data_colnames()[3:4])
})

htext <- div(
 h1("reste a coder :"),
 br())
     
output$helptxt <- renderUI(htext)
     

 output$test <- renderDataTable(
 return(reduced_data()))
}

shinyApp(ui = ui, server = server)
