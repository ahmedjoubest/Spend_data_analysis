# Libraries  ---------------
library(shiny)
library(visdat)
library(dplyr)
library(shinyWidgets)
library(shinycssloaders)
library(naniar)
library(plotly)
library(highcharter)
# Read Data --------------- (Eliminate last 3 rows for coehrence concerns)
# DF_2020 <- readxl::read_excel("Spend Data 2020.xlsx", na = "n/a")
# DF_2021 <- readxl::read_excel("Spend Data 2021.xlsx", na = "n/a")
# DF_2020 <- DF_2020[-c((nrow(DF_2020)-2):nrow(DF_2020)),]
# DF_2021 <- DF_2021[-c((nrow(DF_2021)-2):nrow(DF_2021)),]
# # Small correction to bind data  ---------------
# colnames(DF_2020)[29] <- colnames(DF_2021)[29]
# DF <- rbind(DF_2020,DF_2021) 
# # Sort by date
# DF <- DF %>% arrange(`Year Period`)
#  # Delete space from column names
# names(DF) <-  gsub(" ", "_", names(DF))

# UI  ---------------
ui <- fluidPage(
  
  # Custom tabs area
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: #F5F5F5;  color:black}
    .tabbable > .nav                 {background-color: #F5F5F5;  color:black}
    .tabbable > .nav > li[class=active]    > a {background-color: white; color:black}
  ")),
  
  titlePanel(textOutput("tittlepanel"), windowTitle = "EDP Spend Data"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 pickerInput(
                   inputId = "VendorParent",label = "Vendor Parent",
                   choices = unique(DF$Vendor_Parent),
                   multiple = T,
                   selected = unique(DF$Vendor_Parent)[c(1,3,5,6)],
                   options = pickerOptions(actionsBox = T, liveSearch = T, size = 10,
                                           dropdownAlignLeft = T)
                 )
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Missing Data",
                 br(),
                 h2("Visualize Missing Data: Global exploring"),
                 br(),
                 p("Among 33 columns, only these 8 columns contains NAs."),
                 br(),
                 pickerInput(
                   inputId = "columns_NA",label = "Select Columns",
                   choices = c("Plant_Number", "Plant_Name","Material_Number","Material_Name","UOM","Quantity","Cost_Center","Cost_Center_Name"),
                   multiple = T,
                   selected = c("Plant_Number", "Plant_Name","Material_Number","Material_Name","UOM","Quantity","Cost_Center","Cost_Center_Name"),
                   options = pickerOptions(actionsBox = T, liveSearch = T, size = 10,
                                           dropdownAlignRight = T)
                 ),
                 plotOutput("NA_Data", height = "550px")  %>% 
                   withSpinner(color="#3C8DBC",type=4, proxy.height = "127px",size = 0.9),
                 br(),
                 h2("Visualize Missing Data: by category"),
                 br(),
                 p("This plot shows the number of missings in each column, broken down by a categorical variable from the dataset."),
                 p("You can choose the columns from the 8 columns containing NAs (Y), as well as the categorical variable (X)."),
                 br(),
                 fluidRow(
                   column(6,
                          pickerInput(
                            inputId = "missing_X",label = "Choose the categorical variable (X)",
                            choices = colnames(DF),
                            selected = c("Vendor_Parent"),
                            options = pickerOptions(actionsBox = T, liveSearch = T, size = 10,
                                                    dropdownAlignRight = T)
                          )),
                   column(6,
                          pickerInput(
                            inputId = "missing_Y",label = "Select Columns to display (Y)",
                            choices = c("Plant_Number", "Plant_Name","Material_Number","Material_Name","UOM","Quantity","Cost_Center","Cost_Center_Name"),
                            multiple = T,
                            selected = c("Plant_Number", "Plant_Name","Material_Number","Material_Name","UOM","Quantity","Cost_Center","Cost_Center_Name"),
                            options = pickerOptions(actionsBox = T, liveSearch = T, size = 10,
                                                    dropdownAlignRight = T)
                          ))
                 ),
                 br(),br(),
                 plotlyOutput("miss_fct",height = "550px") %>% 
                   withSpinner(color="#3C8DBC",type=4, proxy.height = "127px",size = 0.9),
                 br(),
        ),
        tabPanel("Numerical Fields",
                 br(),
                 h2("Analyze the numerical fields: Boxplots"),
                 br(),
                 p("4 numerical fields to analyze through box plots. You can also analyze by categorical data."),
                 br(),
                 fluidRow(
                   column(6,
                          pickerInput(
                            inputId = "box_plot_x",label = "Choose the numerical field",
                            choices = c("Quantity", "Spend_In_Original_Currency","Spend_In_USD",
                                        "Spend_In_EURO"),
                            selected = c("Spend_In_EURO"),
                            options = pickerOptions(actionsBox = T, liveSearch = T, size = 10,
                                                    dropdownAlignRight = T)
                          )),
                   column(6,
                          pickerInput(
                            inputId = "box_plot_var",label = "Choose the categorical variable",
                            choices = colnames(DF)[c(1:22,24:29)],
                            selected = c("Vendor_Parent"),
                            options = pickerOptions(actionsBox = T, liveSearch = T, size = 10,
                                                    dropdownAlignRight = T)
                          ))
                 ),
                 plotlyOutput("boxplot"),
                 br(),
                 h2("Analyze the numerical fields: distrubtion"),
                 br(),
                 p("Using the same 2 filters at the top. You can analyze the distribution of your numerical fields"),
                 br(),
                 highchartOutput("distribution")
        ),
        tabPanel("Categorical Data",
                 br(),
                 h2("Categorical Data: Frequencies"),
                 br(),
                 p("Analyze through the Select Input below the attributes frequencies for each categorical field"),
                 br(),
                 pickerInput(
                   inputId = "piechart_var",label = "Choose the categorical variable",
                   choices = colnames(DF)[c(1:22,24:29)],
                   selected = c("Vendor_Parent", "Vendor_Name", "Country_Name"),
                   options = pickerOptions(actionsBox = T, liveSearch = T, size = 10,
                                           dropdownAlignRight = T),
                   multiple = T
                 ),
                 fluidRow(
                   tagList(
                     lapply(1:30,function(x){
                       column(4,
                              highchartOutput(paste0("piechart_",x), width = "380px"))
                     })
                   )
                 )
        )
      ))
  )
)

# Server  ---------------
server <- function(input, output, session) {
  
  values <- reactiveValues(
    DF = NULL
  )
  
  # Observe to filter Data
  observeEvent(c(input$VendorParent), ignoreNULL = T,{
    values$DF <- DF %>% filter(
      Vendor_Parent %in% input$VendorParent
    )
  })
  
  output$NA_Data <- renderPlot({
    vis_miss(
      values$DF %>% 
        select(input$columns_NA),
      warn_large_data = F
    )
  })
  
  output$miss_fct <- renderPlotly({
    gg_miss_fct(x = values$DF %>% select(c(input$missing_Y, input$missing_X)),
                fct = .data[[input$missing_X]]) + 
      labs(title = "Missing Data by categorical variable")
  })
  
  output$boxplot <- renderPlotly({
    plot_ly(values$DF, x = ~.data[[input$box_plot_var]], y = ~.data[[input$box_plot_x]], type = "box") %>% 
      layout(title = 'Boxplot by categorical fields', xaxis = list(title=input$box_plot_var),
             yaxis = list(title=input$box_plot_x))
  })
  
  output$distribution <- renderHighchart({

    df <- values$DF[,c(input$box_plot_x,input$box_plot_var)]
    names(df) <- c("variable","category")
    
    hc <- highchart()
    
    for(i in 1:length(unique(df$category))){
      cat <- unique(df$category)[i]
      hc <- hc %>% 
        hc_add_series(
          density(
            df[df$category == cat,][["variable"]], na.rm = T
          ),
          type = "area",
          name = cat
        )
    }
    
    hc
   # df <- values$DF
   # gg <- ggplot(data = df ) +  
   #   geom_density(aes(x=.data[[input$box_plot_x]], color=.data[[input$box_plot_var]]))+ 
   #   ylab("") + 
   #   xlab("")
   # 
   # ggplotly(gg)%>% 
   #   layout(plot_bgcolor='#e5ecf6',   
   #          xaxis = list(   
   #            title= input$box_plot_var, 
   #            zerolinecolor = '#ffff',   
   #            zerolinewidth = 2,   
   #            gridcolor = 'ffff'),   
   #          yaxis = list(   
   #            title='Distribution', 
   #            zerolinecolor = '#ffff',   
   #            zerolinewidth = 2,   
   #            gridcolor = 'ffff'),
   #          title = '') 
  })
  
  lapply(
    1:30,function(i){
      output[[paste0("piechart_",i)]] <- renderHighchart({
        # browser()  
        category <- input$piechart_var[i]
        if(is.na(category)){
          return()
        } else {
          df <- table(values$DF[,category]) %>% as.data.frame()
          colnames(df)[1] <- c("name")
          hc <- df %>%
            hchart(
              "pie", hcaes(x = name, y = Freq),
              name = category
            )
          return(
            hc
          )
        }
      })
    }
  )
}

shinyApp(ui, server)
