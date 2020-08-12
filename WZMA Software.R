# Work Zone Mobility Audit
# Wayne State University
# Ilyas Ustun
# Mohsen Kamyab
# Stephen Remias

install_load_packages <- function(){

    pkgs <- c("devtools", "shiny", "shinydashboard", "RODBC", "tidyverse", 
                   "scales", "data.table", "RColorBrewer", "zoo", "lubridate", 
                   "ggthemes", "colorRamps", "officer", "flextable", "rgdal",
                   "sf", "ggmap", "gridExtra", "cowplot", "DT")

    
    list_installed <- installed.packages()
    new_pkgs <- subset(pkgs, !(pkgs %in% list_installed[, "Package"]))
    
    
    if(length(new_pkgs)!=0) {
        # if ('pacman' %in% new_pkgs) {
        #     install.packages("pacman")
        # }
        # library(pacman)
        # pacman::p_load(new_pkgs, character.only = TRUE)
        install.packages(pkgs = new_pkgs)
    }
    
    if((length(new_pkgs)<1)){
        print("No new packages added...")
    }

}

# update_load_packages <- function(){
#     
# }

# Install all the necessary packages
install_load_packages()
# update_loaded_packages()

# load the required packages
library(shiny)
require(shinydashboard)
library(RODBC)
library(tidyverse)
library(scales)
library(data.table)
library(RColorBrewer)
library(zoo)
library(lubridate)
library(ggthemes)
library(colorRamps)
library(devtools)
library(officer)
library(flextable)
library(rgdal)
library(sf)
library(ggmap)
library(gridExtra)
library(cowplot)
library(DT)




getwd()
## Make sure that these files exist in directory
source("files/ggradar2.R") 
source('files/WZMA_Dashboard.R')
segments_shp = sf::st_read('files/Shapefile/Segments/segments_shapefile.shp')
counties_shp = sf::st_read('files/Shapefile/Counties/Counties_shapefile.shp')
path_ppt_file = "files/WZMA_ToBeFilled13.pptx"


##########
# header

header <- dashboardHeader(
    # dropdownMenu(
    #     type = "messages",
    #     messageItem(
    #         from = "Lucy",
    #         message = "Place holder",
    #         href = "https://spotthestation.nasa.gov/sightings/"
    #     ),
    #     # Add a second messageItem() 
    #     messageItem(
    #         from='ilyas',
    #         message='Place holder',
    #         href='https://spotthestation.nasa.gov/faq.cfm')
    # ),
    # # Create a notification drop down menu
    # dropdownMenu(
    #     type = 'notifications',
    #     notificationItem(
    #         text="Place holder")
    # ),
    # dropdownMenu(
    #     type = "tasks",
    #     taskItem(
    #         text="Place holder",
    #         value=10)
    # )
    
)


#################### 
# sidebar


sidebar <- dashboardSidebar(
    sidebarMenu(
        # Create two `menuItem()`s, "Dashboard" and "Inputs"
        menuItem(
            text="Inputs",
            tabName="tab_inputs"
        ),
        menuItem(
            text="Select",
            tabName="tab_select_workzones"
        )
    )
)






###################
# body

tab_item1 = 
    tabItem(
        tabName='tab_inputs',
        tabBox(
            title="Inputs",
            width=12,
            tabPanel(
                title="Work Zone Info File",
                fileInput(
                    inputId="wz_file_input",
                    label="Work zone file input",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                ),
                hr()
                
            ),
            tabPanel(
                title='Work Zone Table',
                div(style = 'overflow-x: scroll', dataTableOutput(outputId='wz_table_overview'))
            ),
            
            tabPanel(
                title="Database Info",
                textInput(inputId='dsn', label='dsn name', value='RSQL'),
                textInput(inputId='username', label='user name', value='ilyas'),
                passwordInput(inputId='password', label='password', value='Wayne123'),
                actionButton(inputId='connect_database', label='connect to database'),
                textOutput(outputId='check_connection')
            ),
            tabPanel(
                title='Output directory',
                textInput(inputId = 'output_directory',
                          label= 'Input path to output directory',
                          value = "C:/R/test_app/")
                
            )
        )
    )


tab_item2 =
    tabItem(
        tabName='tab_select_workzones',
        tabBox(
            title="Select Work Zones",
            width=12,
            tabPanel(
                title='Choose workzones',
                textInput(inputId="wz_year",
                          label='Work Zone is in Year',
                          value=2015),
                uiOutput(outputId="list_wz_id"),
                div(style = 'overflow-x: scroll', dataTableOutput(outputId='filtered_wz'))
            ),
            tabPanel(
                title='Thresholds for speeds',
                textInput(inputId = 'threshold_speed_congestion',
                          label= 'Threshold speed for congestion',
                          value = 45),
                textInput(inputId = 'threshold_speed_queue',
                          label= 'Threshold speed for queue',
                          value = 15)
            ),
            tabPanel(
                title='Database Table Names',
                textInput(inputId = 'prior_year_table',
                          label= 'Prior Year Table Name',
                          value = "[MI_Mobility].[dbo].[year2014_modified_view]"),
                textInput(inputId = 'wz_year_table',
                          label= 'Work Zone Year Table Name',
                          value = "[MI_Mobility].[dbo].[year2015_modified_view]")
            ),
            tabPanel(
                title = 'Run the assessment',
                # radioButtons(inputId='radio_run_metrics',
                #              label = 'Run all of the metrics or some of them?',
                #              choices = c('All' = 'radio_run_all',
                #                          'Some' = 'radio_run_some'),
                #              selected = 'All'
                # ),
                
                actionButton(inputId='run_report', label='Run report')
            )
        )
    )



# wz_file = tableOutput(outputId="wz_file")

# box(
#     title = "Box title", width = NULL, status = "primary",
#     div(style = 'overflow-x: scroll', tableOutput('table'))
# )


body <- dashboardBody(
    tabItems(
        tab_item1,
        tab_item2
    )
    
    # dataTableOutput(outputId="wz_file")
    
    # fluidRow(
    #     box(tableOutput(outputId="wz_file"),  title= 'Work zone file') 
    #     
    # )
    
    
)

# 
# Inputs:
#     
# actionButton()
# checkboxGroupInput()
# checkboxInput()
# dateInput()
# dateRangeInput() 
# fileInput()
# numericInput()
# passwordInput() 
# radioButtons() 
# selectInput() 
# sliderInput() 
# submitButton() 
# textAreaInput()
# textInput()



# Render functions:
# renderPrint() --> textOutput(), verbatimTextOutput()
# renderText() --> textOutput()
# renderTable() -->  tableOutput()
# rederDataTable() --> dataTableOutput()
# renderPlot() --> plotOutput()
# renderImage() --> imageOutput()
# renderUI() --> uiOutput()  #html or shiny object




server <- function(input, output, session) { 
    
    
    if (!interactive()) {
        session$onSessionEnded(function() {
            stopApp()
            q("no")
        })
    }
    
    # df=read.csv(input$wz_file_input$datapath)
    
    df = eventReactive(input$wz_file_input, {
        df = read.csv(input$wz_file_input$datapath, stringsAsFactors = FALSE)
        df$start_datetime = as.POSIXct(df$start_datetime, format = "%m/%d/%Y %H:%M", tz='') 
        df$end_datetime = as.POSIXct(df$end_datetime, format = "%m/%d/%Y %H:%M", tz='')
        df$duration_days = difftime(df$end_datetime, df$start_datetime, units = 'day') %>% as.numeric()
        
        return(df)
        
    })
    
    df_date_filtered <- reactive({
        # d <- subset(df(), Date>=input$dateRange[1] )
        # d <- subset(d, Date<=input$dateRange[2] )
        # df() %>% filter(start %in% input$selected_list)
        # d
        df_date_filtered = df() %>% filter(start_datetime >= paste0(input$wz_year,'-01-01'),
                        end_datetime < paste0(input$wz_year,'-12-31'),
                        start_datetime < end_datetime,
                        duration_days > 1/24)
        return(df_date_filtered)
                            
    })
    # start_datetime,end_datetime
    
    # wz2 = wz %>% filter(wz_start >= '2015-01-01', 
    #                     wz_end < '2015-12-31', 
    #                     wz_start < wz_end)

    
    
    # output$wz_file <- renderDataTable({
    #     req(input$wz_file_input)
    #     
    #     tryCatch(
    #         {
    #             df <- read.csv(input$wz_file_input$datapath)
    #             # header = input$header,
    #             # sep = input$sep,
    #             # quote = input$quote)
    #         },
    #         error = function(e) {
    #             # return a safeError if a parsing error occurs
    #             stop(safeError(e))
    #         } 
    #     )
    #     return(head(df))
    # })
    
    output$wz_table_overview <- renderDataTable({
        req(df_date_filtered())
        
        df_date_filtered_show = dplyr::select(df_date_filtered(), -tmc_list)
        df_date_filtered_show$duration_days = df_date_filtered_show$duration_days %>% round(2) 
        df_date_filtered_show$end_mm = df_date_filtered_show$end_mm %>% round(2) 
        df_date_filtered_show$start_mm = df_date_filtered_show$start_mm %>% round(2)
        return(df_date_filtered_show)
    }, options = list(pageLength = 5,
                      lengthMenu = c(5, 10, 15, 20, 50))
    )
    
    db <- eventReactive(input$connect_database, {
        odbcConnect(dsn = input$dsn, uid = input$username, pwd = input$password)
    # odbChannel <- odbcConnect("RSQL", uid = 'ilyas', pwd = 'Wayne123!')
        # return(odbChannel)
    })
    
    output$check_connection = renderPrint({
        
        req(input$connect_database)
        # odbChannel <- db()
        
        return(db())
    })
    
    output$list_wz_id<-renderUI({
        # req(output$wz_file)
        selectInput(inputId = "selected_list", 
                    label = "Select from the list", 
                    choices = c("select all", df_date_filtered()$wz_id), multiple=TRUE)
        
    })
    
    
    df_filtered = eventReactive(input$selected_list, 
                                valueExpr = {
                                    if ("select all" %in% input$selected_list) {
                                        all_choices = c("select all", df_date_filtered()$wz_id)
                                        selected_choices <- setdiff(all_choices, "select all")
                                        print("All work zones are selected:")
                                        print(selected_choices)
                                        # updateSelectInput(session, "selected_list", selected = selected_choices)
                                        df_filtered = df_date_filtered() %>% filter(wz_id %in% selected_choices) 
                                    } else {
                                        # input_selected_list = input$selected_list[! input$selected_list %in% "select all"]
                                        # df_date_filtered() %>% filter(wz_id %in% input$selected_list[! input$selected_list %in% "select all"]) 
                                        # selected_choices <- setdiff(input$selected_list, "select all")
                                        # updateSelectInput(session, "selected_list", selected = selected_choices)
                                        print("Work zones that are selected:")
                                        print(input$selected_list)
                                        df_filtered = df_date_filtered() %>% filter(wz_id %in% input$selected_list) 
                                        # Need to filter out "select all"
                                    }
                                    return(df_filtered)
                                })
    
    
    output$filtered_wz <- renderDataTable({
        req(df_filtered())
        df_filtered_show = dplyr::select(df_filtered(), -tmc_list)
        df_filtered_show$duration_days = df_filtered_show$duration_days %>% round(2) 
        df_filtered_show$end_mm = df_filtered_show$end_mm %>% round(2) 
        df_filtered_show$start_mm = df_filtered_show$start_mm %>% round(2) 
        return(df_filtered_show)
    }, options = list(pageLength = 5,
                      lengthMenu = c(5, 10, 15, 20, 50))
    )
    
    
    observeEvent(input$run_report, {
        
        # func_run_two_page_report(wz=df_filtered(), odbChannel = db(), input$output_directory)  
        func_run_two_page_report(wz=df_filtered(), 
                                 odbChannel = db(), 
                                 OUTPUT_DIRECTORY=input$output_directory, 
                                 table_name_wz=input$wz_year_table, 
                                 table_name_prior=input$prior_year_table,
                                 threshold_speed_congestion = as.numeric(input$threshold_speed_congestion), 
                                 threshold_speed_queue = as.numeric(input$threshold_speed_queue),
                                 segments_shp, 
                                 counties_shp,
                                 path_ppt_file)
        # if (input$radio_run_metrics == 'radio_run_all') {
        #     
        #     func_run_two_page_report(wz=df_filtered(), odbChannel = db(), input$output_directory)  
        # }
        # 
        # else {
        #     func_run_heatmap(wz=df_filtered(), odbChannel = db())    
        # }
        
        # return(odbChannel)
    })
    
    
    
}




#################
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body
)


shinyApp(ui, server)
