library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(plotly)
library(readxl)
library(directlabels)
library(magrittr)

ui <- fluidPage(div(class = "fluid-page",
                    div(class = "row text-center",
                        div(class = "col-xl-12",style = "margin-left: 180px; margin-right: 180px;",
                            br(),
                            strong(h1('Multiplex Analysis')),
                            hr(),
                            hr(),
                            br(),
                            br(),
                            radioGroupButtons(
                                inputId = "display_type",
                                label = "",
                                #This is all the types of pages
                                choices = c("Results"),
                                status = "info",
                                justified = TRUE,
                                checkIcon = list(
                                    yes = icon("ok", 
                                               lib = "glyphicon"))
                            ),
                            hr(),
                            uiOutput("selection"),
                            br(),
                            br(),
                            br(),
                            br(),
                            plotlyOutput("graph", width = "90%", height = 600),
                            br(),
                            DTOutput("table"),
                            br(),
                            br()
                            
                            
                        )
                        
                        
                    )
)



)

server <- function(input, output, session) {
    
    showModal(modalDialog(
        title = "Enter Multiplex Data",
        easyClose = FALSE,
        footer = NULL,
        size = "m",
        fileInput("csvFile", "Input Fluoresence File (csv)", width = 1200),
        fileInput("csvFile2", "Input Plate File (xlsx)", width = 1200),
        downloadLink('downloadData', 'Download Template/Example Plate File'),
        br(),
        br(),
        actionButton("submit_files", label = "Done")
    ))
    
    
    #Get the raw data from upload
    #TODO: Add error message if data not csv file, in wrong format, etc.
    rawData <- reactive({
        inFile <- input$csvFile
        if (is.null(inFile)) return(NULL)
        data <- read.csv(inFile$datapath, header = TRUE)
        data
    })
    
    #Get the template from upload
    #TODO: Add error message if data not csv file, in wrong format, etc.
    rawData2 <- reactive({
        inFile <- input$csvFile2
        if (is.null(inFile)) return(NULL)
        data <- read_excel(inFile$datapath, sheet = "Data")
        data
    })
    
    observeEvent(input$submit_files, {
        
        removeModal()
        
    })
    
    #Merge them bois
    full_data <- reactive({
        TitrationLevels <- rawData()
        TemplateExample <- rawData2()
        
        colnames(TitrationLevels)[1] <- "Cycle"
        
        titration_long <- TitrationLevels %>% 
            pivot_longer(-Cycle)
        
        full_data <- left_join(titration_long, TemplateExample, by = c("name" = "Sample"))
        
        colnames(full_data)[4] <- "Probe concentration"
        colnames(full_data)[6] <- "Target Concentration"
        
        
        full_data
    })
    
    output$selection <- renderUI({
        full_data <- full_data()
        column(12, align = "center",
               pickerInput(
                   inputId = "filter_target",
                   label = "Select Controls", 
                   choices = as.character(unique(full_data$Target)),
                   options = list(`actions-box` = TRUE), 
                   multiple = TRUE
                   ))
        })
    
    output$table <- renderDT({
        
        # TODO: endpoint w/ choice of what cycle it starts on
        
        full_data <- full_data()
        
        find_outliers <- full_data %>% 
            filter(Cycle %in% 45:60 & Target %in% input$filter_target) %>%
            group_by(Target, name) %>%
            mutate(avg = mean(value)) %>%
            ungroup() %>%
            group_by(Target) %>%
            mutate(Q1 = quantile(value, .25),
                   Q3 = quantile(value, .75),
                   IQR_x = iqr(value),
                   mini = Q1 - 1.5*IQR_x,
                   maxi = Q3 + 1.5*IQR_x,
                   outlier = case_when(avg > mini & avg < maxi ~FALSE, 
                                       TRUE ~ TRUE)) %>%
            select(name, avg, Q1, Q3, IQR_x, mini, maxi, outlier) %>%
            unique() %>%
            filter(outlier)
        
        full_data %<>% filter(name %in% find_outliers$name)
        
            endpoint <- full_data %>% 
                filter(Cycle > 49) %>% 
                group_by(Target) %>%
                mutate(avg_f = mean(value),
                       sd = sd(value)) %>% distinct()
            
            controls <- endpoint %>% 
                group_by(Target) %>% 
                summarize(avg = mean(value), 
                          sd = sd(value), 
                          var = var(value)) %>%
                filter(Target %in% input$filter_target)
            
            more_dat <- full_data %>% 
                group_by(Target, Cycle) %>%
                mutate(avg_point = mean(value),
                       sd = sd(value),
                       se = sd/sqrt(n())) %>% 
                ungroup()
            
            n <- controls$Target %>% n_distinct()
            combos <- list()
            datalist <- list()
            t = 1
            
            for (i in 1:n) {
                x <- combn(controls$Target, i)
                
                for (j in 1:ncol(x)) {
                    combos[t] <- paste(x[,j], collapse = ",")
                    t = t + 1
                }
            }
            
            i = 1
            for (target in unique(combos)) {
                
                dat <- more_dat %>% 
                    filter(Target %in% c(str_split(target, ",")[[1]])) %>%
                    group_by(Cycle, Target) %>% 
                    summarize(avg_point = mean(avg_point),
                              se = mean(se)) %>%
                    ungroup() %>%
                    group_by(Cycle) %>%
                    summarize(avg_point = sum(avg_point),
                              se = sqrt(sum(se^2))) %>%
                    mutate(Target = target)
                
                datalist[[i]] = dat
                i = i + 1
            }
            
            all_controls <- do.call(rbind, datalist)
            datatable(all_controls)
    })
    
    output$graph <- renderPlotly({

        # TODO: endpoint w/ choice of what cycle it starts on

        full_data <- full_data()
        
        find_outliers <- full_data %>% 
            filter(Cycle %in% 45:60 & Target %in% input$filter_target) %>%
            group_by(Target,name) %>%
            mutate(avg = mean(value)) %>%
            ungroup() %>%
            group_by(Target) %>%
            mutate(Q1 = quantile(value, .25),
                   Q3 = quantile(value, .75),
                   IQR_x = iqr(value),
                   mini = Q1 - 1.5*IQR_x,
                   maxi = Q3 + 1.5*IQR_x,
                   outlier = case_when(avg > mini & avg < maxi ~FALSE, 
                                       TRUE ~ TRUE)) %>%
            select(name, avg, Q1, Q3, IQR_x, mini, maxi, outlier) %>%
            unique() %>%
            filter(outlier)
        
        full_data %<>% filter(!name %in% find_outliers$name)
        
        endpoint <- full_data %>% 
            filter(Cycle > 49) %>% 
            group_by(Target) %>%
            mutate(avg_f = mean(value),
                   sd = sd(value)) %>% distinct()
        
        controls <- endpoint %>% 
            group_by(Target) %>% 
            summarize(avg = mean(value), 
                      sd = sd(value), 
                      var = var(value)) %>%
            filter(Target %in% input$filter_target)
        
        more_dat <- full_data %>%
            group_by(Target, Cycle) %>%
            mutate(avg_point = mean(value),
                   sd = sd(value),
                   se = sd/sqrt(n())) %>% 
            ungroup()
        
        
        n <- controls$Target %>% n_distinct()
        combos <- list()
        datalist <- list()
        t = 1

        for (i in 1:n) {
            x <- combn(controls$Target, i)
            
            for (j in 1:ncol(x)) {
                combos[t] <- paste(x[,j], collapse = ",")
                t = t + 1
            }
        }
        
        i = 1
        for (target in unique(combos)) {
            dat <- more_dat %>% 
                filter(Target %in% c(str_split(target, ",")[[1]])) %>%
                group_by(Cycle, Target) %>% 
                summarize(avg_point = mean(avg_point),
                          se = mean(se)) %>%
                ungroup() %>%
                group_by(Cycle) %>%
                summarize(avg_point = sum(avg_point),
                          se = sqrt(sum(se^2))) %>%
                mutate(Target = target)
            
            datalist[[i]] = dat
            i = i + 1
        }
        
        all_controls <- do.call(rbind, datalist)

        ggplotly(
            ggplot() +

                     geom_line(data = full_data, aes(x = Cycle, y = value, group = name,
                                                     color = Target)) +
                     geom_ribbon(data = all_controls, aes(x = Cycle, ymin = avg_point - 2*se,
                                                      ymax = avg_point + 2*se, group = Target),
                                 alpha = .5) +
                     geom_line(data = all_controls, aes(x = Cycle, y = avg_point, group = Target),
                               color = "firebrick") +
                     theme_bw() +
                     labs(title = "Standard Curve",
                          x = "Cycle",
                          y = "Fluorescence") +
                     theme(plot.title = element_text(hjust = .5))) %>%
            config(displayModeBar = F)

    })
}

shinyApp(ui = ui, server = server)