################################################
##
## Server file for upside model Kobe plot app
##
## By: Tyler Clavelle
## Date: 02/24/2016
##
################################################

## Initialize Shiny server function ----
shinyServer(function(input, output) {
  
  # Subset large datafile to only 2012
  lumped <- data$lumped %>%
    filter(Year == 2012)
  
  unlumped <- data$unlumped %>%
    filter(Year == 2012)
  
  kobe_data<- reactive({
    
  # subset data based on desired kobe plot level 
    if(input$Level=="Global")
    {
      data <- lumped %>%
        filter(is.na(BvBmsy)==F & is.na(FvFmsy)==F)
      
      data$Dbase[data$Dbase!='RAM']<-'Unassessed'
    } 
    if(input$Level=="By Country")
    {
      data <- unlumped %>%
        filter(Country %in% input$Country & is.na(BvBmsy)==F & is.na(FvFmsy)==F)
      
      data$Dbase[data$Dbase!='RAM']<-'Unassessed'
    }
    
    if(input$Level=="By ISSCAAP group")
    {
      data <- lumped %>%
        filter(SpeciesCatName %in% input$isscaap & is.na(BvBmsy)==F & is.na(FvFmsy)==F)
      
      data$Dbase[data$Dbase!='RAM']<-'Unassessed'
    }
    
    if(input$Level=="By FAO Region")
    {
      regs <- names$codes[names$zone %in% input$Region]
      
      data <- lumped %>%
        filter(grepl(regs,RegionFAO) & is.na(BvBmsy)==F & is.na(FvFmsy)==F)
      
      data$Dbase[data$Dbase!='RAM']<-'Unassessed'
    }
    
    # toggle assessment level
    if(input$Dbase=='Both'){data<-data}
    if(input$Dbase=='RAM'){data<-data[data$Dbase=='RAM',]}
    if(input$Dbase=='Unassessed'){data<-data[data$Dbase=='Unassessed',]}
    
    # toggle NEIs
    if(input$Neis==FALSE){data<-data[data$IdLevel!="Neis",]}
    
    # toggle stock size
    data<- data %>%
      filter(Catch>=input$Size)
    
    # QAQC of BvBmsy values
    data<-data[data$BvBmsy!=999 |is.infinite(data$BvBmsy)!=TRUE,]
    
    # cap FvFmsy and BvBmsy values to the kobe plot axis limits
    data$FvFmsy[data$FvFmsy>4]<- 4
    
    data$BvBmsy[data$BvBmsy>2.5]<- 2.5

    #change dot color based on whether global plot or individual countries are plotted
    if(input$Level=="Global"){DotColor                ='Dbase'
    } else if (input$Level=="By Country"){DotColor    ='Country'
    } else if (input$Level=="By FAO Region"){DotColor ='Dbase'
    } else if (input$Level=="By ISSCAAP group"){DotColor    ='SpeciesCatName'}
     
    # change dotcolor to Id level if desired
    if (input$ColorID==TRUE){DotColor='IdLevel'}

    # format data for table
      tbl <- data %>%
        dplyr::select(IdOrig, Dbase, Country, SciName, BvBmsy, FvFmsy, MSY, g, k) %>%
        mutate(BvBmsy = round(BvBmsy, digits = 3),
               FvFmsy = round(FvFmsy, digits = 3),
               MSY    = round(MSY),
               k      = round(k),
               g      = round(g, digits = 4))
      
      return(list(data=data, tbl = tbl, DotColor = DotColor))
      
  })
    
  
  #plot Kobe plot for app
  output$kobe_plot <- renderPlotly({
     
    data<-kobe_data()
    DotColor<-data$DotColor
    data<-data$data
  
    d <- ggKobe(data, xvar = 'BvBmsy', yvar = 'FvFmsy', dotcolor = DotColor, density = F)
    d <- d + theme(legend.title = element_blank())
    a <- ggplotly(d, tooltip = c('x','y','colour','size','key'))

    a
    
  })
  
  # plot kobe plot for download
  kobe_plot_save <- function(){
    
    data<-kobe_data()
    DotColor<-data$DotColor
    data<-data$data
    
    d <- ggKobe(data, xvar = 'BvBmsy', yvar = 'FvFmsy', dotcolor = DotColor, density = F)
    # d <- d + theme(legend.title = element_blank())
    d
  }
  
  
    # output kobe data dable  
    output$kobe_table <- renderDataTable({
      tbl<-kobe_data()
      tbl<-tbl$tbl
      })
    
    ## produce histograms and histogram summary data tables
    # g histogram
    output$kobe_histg <- renderPlotly({
      tbl<-kobe_data()
      tbl<-tbl$tbl
      
      pts <- tbl %>%
        summarize(meang  = mean(g, na.rm = T),
                  stdevg = sd(g, na.rm = T))
      
      b <- ggplot(tbl, aes(x = g)) +
        geom_histogram(fill='gray',
                       color='black', 
                       binwidth = 0.01) +
        # coord_cartesian(ylim = c(0) +
        # scale_x_log10() +
        scatterTheme 
        
      b<-ggplotly(b)
      b
    }) 
    
    # g data table
    output$kobe_tableg <- renderTable({
      tbl<-kobe_data()
      tbl<-tbl$tbl
      
      pts <- tbl %>%
        summarize(meang  = mean(g, na.rm = T),
                  stdevg = sd(g, na.rm = T)) %>%
        rename(`Mean (g)` = meang,
               `Std. deviation (g)` = stdevg)
    })
      
    # k histograme
    output$kobe_histk <- renderPlotly({
      
      tbl<-kobe_data()
      tbl<-tbl$tbl
      c <- ggplot(tbl, aes(x = k)) +
        geom_histogram(fill='gray',color='black') +
        scale_x_log10() +
        scatterTheme
      
      c<-ggplotly(c)
      c
    }) 
    
    # K data table
    output$kobe_tablek <- renderTable({
      tbl<-kobe_data()
      tbl<-tbl$tbl
      
      pts <- tbl %>%
        summarize(meank  = mean(k, na.rm = T),
                  stdevk = sd(k, na.rm = T)) %>%
        rename(`Mean (K)` = meank,
               `Std. deviation (K)` = stdevk)
    })
    
    output$dl_tbl<-downloadHandler(
      filename = function() { paste('kobe_data',Sys.Date(),'.csv',sep='') },
      content = function(file) { write.csv(kobe_data()$tbl, file) }
    )
  
    output$downloadPDF<-downloadHandler(
    filename = function() { paste('kobe_plot', Sys.Date(), '.pdf', sep = '') },
    content  = function(file) { ggsave(file, plot = kobe_plot_save(), device = 'pdf')}
    # content = function(file) { plotly_IMAGE(a, format = "png", out_file = "kobe_plot.png")}
    )  
  })  



