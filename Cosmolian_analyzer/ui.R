#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(periscope)

# shinyOptions(shiny.maxRequestSize=1000*1024^2)
options(shiny.maxRequestSize=1000*1024^2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Cosmolian analyzer"),
    
    plotOutput("plot1"),
    
    hr(),
    
    # Sidebar with a slider input for number of bins
    fluidRow(
        column(width = 3,
               offset = 0,
               h4('load and plot'),
               
               # opening sidebarPanel
               #sidebarPanel(
               fileInput(inputId='input_file', 
                         label = h5('upload files for analyses'),
                         multiple = TRUE
               ),
               
               selectInput(inputId="plot_variable", 
                           label = h5("Select variable to plot"),
                           choices=""),
               
               # checkboxInput(inputId = "activate_facets",
               #               label ="activate facets",
               #               value = TRUE),
               
               pickerInput(
                   inputId = "color_variable",
                   label = "Select color variable", 
                   choices = "",
                   multiple = FALSE
               ),
               
               
               h5('facetting'),
               
               materialSwitch(
                   inputId = "activate_facets",
                   label = "activate facets", 
                   status = "primary",
                   right = TRUE
               ),
               
               pickerInput(
                   inputId = "x_facet_variable",
                   label = "Select x faceting variable", 
                   choices = "",
                   multiple = FALSE
               ),
               
               # selectInput(inputId="x_facet_variable", 
               #             label = h5("Select x faceting variable"),
               #             choices=""),
               
               # selectInput(inputId="y_facet_variable", 
               #             label = h5("Select y faceting variable"),
               #             choices=""),
               
               pickerInput(
                   inputId = "y_facet_variable",
                   label = "Select y faceting variable", 
                   choices = "",
                   multiple = FALSE
               )
               
               # selectInput(inputId="color_variable", 
               #             label = h5("Select color variable"),
               #             choices="")),
               
               
               
        ),
        
        column(width = 2,
               offset = 0,
               
               h4('histogram appearance'),
               
               materialSwitch(
                   inputId = "add_histogram",
                   label = "add histogram", 
                   status = "primary",
                   right = TRUE
               ),
               
               sliderInput(inputId = "histogram_alpha",
                           label = h5("histogram alpha"),
                           min=0, max=1,
                           value=0.25, step=0.05
                           ),
               
               sliderInput(inputId = "number_of_bins",
                           label = h5("number of bins"),
                           min=1, max=250,
                           value=100, step=1
                           )
        ),
        
        column(width = 2,
               offset = 0,
               h4('data to plot'),
               
               pickerInput(inputId="selected_success_ind",
                           label = h5("success indices"),
                           choices="",
                           options = list(`actions-box` = TRUE),multiple = T),
               
               
               # checkboxGroupInput(inputId="success_ind",
               #                    label = h5("success indices"),
               #                    choices="",
               #                    selected=""),
               
               pickerInput(inputId="selected_samples_ind",
                           label = h5("select samples"),
                           choices="",
                           options = list(`actions-box` = TRUE),multiple = T),
               
               
               # checkboxGroupInput(inputId="samples_ind",
               #                    label = h5("selected samples"),
               #                    choices="",
               #                    selected=""),
               
               pickerInput(inputId="selected_dune_depth_m",
                           label = h5("select dune depths"),
                           choices="",
                           options = list(`actions-box` = TRUE),multiple = T),
               
               pickerInput(inputId="selected_erosion_rate_m_Ma",
                           label = h5("select erosion rates"),
                           choices="",
                           options = list(`actions-box` = TRUE),multiple = T),
               
               pickerInput(inputId="selected_dt_dataset",
                           label = h5("select VDR datasets"),
                           choices="",
                           options = list(`actions-box` = TRUE),multiple = T),
               
               pickerInput(inputId="selected_dt_correction",
                           label = h5("select dt correction"),
                           choices="",
                           options = list(`actions-box` = TRUE),multiple = T),
               
               
        ),
        
        
        column(width = 2,
               offset = 0,
               h4('plot appearance'),
               
               textInput(inputId="x_title",
                         label = h5("x title"),
                         value="simulation duration [Ma]"),
               
               textInput(inputId="y_title",
                         label = h5("y title"),
                         value="density"),
               
               numericInput(inputId="max_x_value",
                            label = h5("max x value"),
                            value=20000000),
               
               sliderInput(inputId = "x_ticks_intervals",
                           label = h5("x ticks intervals"),
                           min=0,max=20000000,
                           value=2.5E6, step=2.5E6/10),
               
               
               sliderInput(inputId = "line_width",
                           label = h5("line width"),
                           min=0, max=10,
                           value=1, step=0.05),
               
               
        ),
        
        column(width = 3,
               offset = 0,
               h4('download figure'),
               
               sliderInput(inputId="figure_height",
                           label = h5("set figure height [cm]"),
                           min=0,max=30,
                           value=10, step=1),
               
               sliderInput(inputId="figure_width",
                           label = h5("set figure width [cm]"),
                           min=0,max=30,
                           value=20, step=1),
               
               sliderInput(inputId="figure_dpi",
                           label = h5("set dpi"),
                           min=0,max=1000,
                           value=300, step=20),
               
               textInput(inputId="figure_name",
                         label = h5("figure name"),
                         value="Cosmolian fig"),
               
               # checkboxGroupButtons(
               #     inputId = "file_type",
               #     label = "file type",
               #     choices = c("png","pdf", "svg", "eps"),
               #     selected = c("png","pdf")
               # ),
               
               downloadButton(outputId='download_plot_png', label='Download PNG Figure'),
               
               downloadButton(outputId='download_plot_pdf', label='Download PDF Figure'),
               
               downloadButton(outputId='download_plot_svg', label='Download SVG Figure')
               
               
               #    textOutput("sample_name"),
               
        )
    ),
    
)

)
