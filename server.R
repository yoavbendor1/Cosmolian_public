#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
#library(data.table)
library(RColorBrewer)
library(extrafont)
#library(rapport)
library(shinyWidgets)
#library(pracma)
library(grDevices)
library(Cairo)
library(svglite)

#shinyOptions(shiny.maxRequestSize=1000*1024^2)
options(shiny.maxRequestSize=1000*1024^2)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    inFile <- reactive({
        if (is.null(input$input_file)) {
            return(NULL)
        } else {
            input$input_file
        }
    })
    
    
    myData <- reactive({
        if (is.null(inFile())) {
            return(NULL)
        } else {
            # big_table = tibble()
            
            for (n in 1:length(inFile()$datapath)){
                
                
                # find the sample name
                sample_name=inFile()$name[n]
                tmp_ind=str_locate(sample_name,"_")
                
                sample_name=substr(sample_name,1,tmp_ind-1)
                tmp_table = read_csv(inFile()$datapath[n])
                
                # add the sample name column
                tmp_table=mutate(tmp_table,sample=sample_name,.before=1)
                
                if (n==1){
                    big_table=tmp_table
                } else {
                    big_table=bind_rows(big_table,tmp_table)
                }
                
                output$sample_name=renderText(inFile()$name[n])
            }
            
            big_table$dt_correction <- factor(big_table$dt_correction)
            big_table$dt_dataset <- factor(big_table$dt_dataset)
            big_table$dune_depth_m <- factor(big_table$dune_depth_m)
            big_table$erosion_rate_m_Ma <- factor(big_table$erosion_rate_m_Ma)
            big_table$sample <- factor(big_table$sample)
            big_table$success_ind <- factor(big_table$success_ind)
            
            
            
            return(big_table)
        }
    })
    
    # update the selection scroll down
    observe({
        updateSelectInput(
            session,
            "plot_variable",
            #choices=names(myData())[7:length(names(myData()))],
            choices=names(myData())[7],
            selected = names(myData())[7])
    })
    
    observe({
        updatePickerInput(
            session,
            "x_facet_variable",
            choices = c('none',names(myData())[1:6]),
            selected = 'none')
    })
    
    # observe({
    #     updateSelectInput(
    #         session,
    #         "x_facet_variable",
    #         choices=names(myData())[1:6])
    # })
    
    # observe({
    #     updateSelectInput(
    #         session,
    #         "y_facet_variable",
    #         choices=names(myData())[1:6])
    # })
    # 
    
    observe({
        updatePickerInput(
            session,
            "y_facet_variable",
            choices = c('none',names(myData())[1:6]),
            selected = 'none')
    })
    
    # observe({
    #     updateSelectInput(
    #         session,
    #         "color_variable",
    #         choices=names(myData())[1:6])
    # })
    
    
    observe({
        updatePickerInput(
            session,
            "color_variable",
            choices = c('none',names(myData())[1:6]),
            selected = 'none')
    })
    
    # observe({
    #     updateCheckboxGroupInput(
    #         session,
    #         "success_ind",
    #         choices=unique(myData()$success_ind))
    # })
    
    
    observe({
        updatePickerInput(
            session,
            "selected_success_ind",
            choices = levels(myData()$success_ind),
            selected = 1)
    })
    
    observe({
        updatePickerInput(
            session,
            "selected_samples_ind",
            choices = levels(myData()$sample),
            selected = levels(myData()$sample[1]))
    })
    
    observe({
        updatePickerInput(
            session,
            "selected_dune_depth_m",
            choices = levels(myData()$dune_depth_m),
            selected = levels(myData()$dune_depth_m))
    })
    
    observe({
        updatePickerInput(
            session,
            "selected_erosion_rate_m_Ma",
            choices = levels(myData()$erosion_rate_m_Ma),
            selected = levels(myData()$erosion_rate_m_Ma))
    })
    
    observe({
        updatePickerInput(
            session,
            "selected_dt_correction",
            choices = levels(myData()$dt_correction),
            selected = levels(myData()$dt_correction))
    })
    
    observe({
        updatePickerInput(
            session,
            "selected_dt_dataset",
            choices = levels(myData()$dt_dataset),
            selected = levels(myData()$dt_dataset))
    })
    
    
    observe({
        updateSliderInput(
            session,
            "x_ticks_intervals",
            max=input$max_x_value)
    })
    
    myPlot <- function(){
        
        
        req(myData())
        
        big_table=myData()
        
        # filter the data based on user selection
        big_table = filter(big_table,success_ind %in% input$selected_success_ind)
        big_table = filter(big_table,sample %in% input$selected_samples_ind)
        big_table = filter(big_table,dune_depth_m %in% input$selected_dune_depth_m)
        big_table = filter(big_table,dt_correction %in% input$selected_dt_correction)
        big_table = filter(big_table,erosion_rate_m_Ma %in% input$selected_erosion_rate_m_Ma)
        big_table = filter(big_table,dt_dataset %in% input$selected_dt_dataset)
        

        p = big_table %>% 
            ggplot(aes_string(x=input$plot_variable))
        
        # if (!(input$color_variable %in% c("none"))) {
        #     p = p +
        #         geom_density(aes_string(color=input$color_variable),size=input$line_width)
        # } else {
        #     p = p +
        #         geom_density(size=input$line_width) 
        # }
        
        
        # add the histograms
        if (input$add_histogram) {
            if (!(input$color_variable %in% c("none"))) {
                hist_type="..density.."
                p = p +
                    geom_histogram(aes_string(y=hist_type,fill=input$color_variable),alpha = input$histogram_alpha,bins = input$number_of_bins, position="identity", show.legend = FALSE)
            } else {
                p = p +
                    geom_histogram(aes(y=..density..),alpha=input$histogram_alpha,bins = input$number_of_bins, position="identity",show.legend = FALSE)
            }
            
        }
        
        # plot the density again on top of the histogram
        if (!(input$color_variable %in% c("none"))) {
            p = p +
                geom_density(aes_string(color=input$color_variable),size=input$line_width, n = (input$number_of_bins+1)^2)
        } else {
            p = p +
                geom_density(size=input$line_width, n = (input$number_of_bins+1)^2) 
        }
        
        
       p = p+
            theme_light()+
            theme(
                axis.title=element_text(size=14),
                axis.text=element_text(size=14),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                strip.text = element_text(size=14))+
            xlab(input$x_title)+
            ylab(input$y_title)+
            scale_x_continuous(breaks=seq(0,input$max_x_value,input$x_ticks_intervals),
                               labels=(seq(0,input$max_x_value*1E-6,input$x_ticks_intervals*1E-6)))+
            coord_cartesian(xlim=c(0,input$max_x_value),expand = TRUE)

        if (input$activate_facets) {
            if ((!(input$y_facet_variable %in% c("none"))) & (!(input$x_facet_variable %in% c("none")))) {
                p = p + facet_grid(get(input$y_facet_variable)~get(input$x_facet_variable),scales = "free_y")
            } else if ((!(input$y_facet_variable %in% c("none")))) {
                p = p + facet_grid(get(input$y_facet_variable)~.,scales = "free_y")
            } else if ((!(input$x_facet_variable %in% c("none")))) {
                p = p + facet_grid(.~get(input$x_facet_variable),scales = "free_y")
            } else {
                p = p
            }
        }  else {
            p
        }
    p
       }
    
    output$plot1 <- renderPlot({
        myPlot()
    })
    
    output$download_plot_png <- downloadHandler(
        filename = function() { paste(input$figure_name, '.png', sep='')},
        
        content = function(file) {
            ggsave(file,plot = myPlot(), device = "png",width = input$figure_width, height = input$figure_height, units = "cm",limitsize = FALSE,dpi = input$figure_dpi)
        }
    )

    output$download_plot_pdf <- 
        downloadHandler(
            filename = function(){ paste(input$figure_name,'.pdf', sep='')},
            content = function(file) {
                ggsave(file,plot = myPlot(), device = cairo_pdf(),width = input$figure_width, height = input$figure_height, dpi = input$figure_dpi, units = "cm")
            }
        ) 
    
    output$download_plot_svg <- 
        downloadHandler(
            filename = function(){
                paste(input$figure_name,'.svg', sep='')
            },
            
            content = function(file) {
                ggsave(file,plot = myPlot(), device = svg,width = input$figure_width, height = input$figure_height, dpi = input$figure_dpi, units = "cm")
            }
        )  
    
})
