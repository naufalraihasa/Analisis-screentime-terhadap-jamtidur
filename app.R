library(shiny)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)
library(ggplot2)
library(viridis)
library(magrittr)
library(dplyr)
library(plotly)
library(readxl)
library(corrplot)
library(bslib)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    title = "Prediksi Jam Tidur", titleWidth = 300),#dashboardHeader
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        menuItem("Home", tabName = "home"),
        menuItem("Data", tabName = "data"),
        menuItem("Visualization", tabName = "plot"),
        menuItem("Prediction", tabName = "regresi")
      )
    ),#dashboardSidebar()
    dashboardBody(
      shinyDashboardThemes(theme = "flat_red"),
      tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
      
    '))),
      tabItems(
        tabItem(tabName = "home",
                
                box(title = "Analisis Screen on time terhadap lama jam tidur", 
                    status = "danger", 
                    solidHeader = TRUE,
                    width = 10,
                    img(src = "header.png" ),
                ),
                
                box(title = "KELOMPOK 3", 
                    status = "danger", 
                    solidHeader = TRUE,
                    width = 10,
                    h2("Anggota Kelompok :"),
                    p("1. Davano 	  (162112133028)"),
                    p("2. Naufal		(162112133064)"),
                    p("3. Bella	(162112133073)"),
                    p("4. Restika	   (162112133092)"),
                    p("5. Theresa			   (162112133106)")
                    )
                
                ),#tabItem Home
        tabItem(tabName = "data", 
                mainPanel(tabsetPanel(
                  id = "tabset",
                  tabPanel("Deskripsi",
                           fluidRow(
                             box(title = "Deskripsi", status = "danger", solidHeader = TRUE, width = 15, height = 10,
                                 tableOutput("deskripsi")),
                           ) #fluidrow
                  ),#tabpaneldeskripsi
                  tabPanel("Dataset",
                           fluidRow(
                             box(title = "Data yang digunakan", status = "danger",  solidHeader = TRUE, width = 15,
                                 tableOutput("dataset")),
                           ) #fluidrow
                  ),#tabpanelDataset
                  
                  tabPanel("Summary", 
                           fluidRow(
                             box(title = "Descriptive Statistics", status = "danger", solidHeader = TRUE, width = 15, height = 10,
                                 verbatimTextOutput("summary")
                             )
                           )
                  )
                )#mainPanel
                )#Tabsetpanel
        ),#tabItem data
        tabItem(tabName = "plot", 
                mainPanel(
                  tabsetPanel(
                    id = "tabset",
                    tabPanel("Barplot",
                             fluidRow(
                               box(title = "Barplot", 
                                   status = "danger", 
                                   solidHeader = TRUE,
                                   width = 20,
                                   plotOutput("bar")#plotOutput
                                   )#boxfluidrow1
                             ),#fluidrow1
                             fluidRow(
                               box(
                                 status = "danger",
                                 width = 20,
                                 solidHeader = FALSE,
                                 selectInput(
                                   inputId = "var1" , 
                                   label = "Pilih Variabel :",
                                   choices = c("Jenis Kelamin" = "jenis_kelamin",
                                               "segmentasi" = "segmentasi"
                                     )#c
                                 )#selectInput
                               ),#boxfluidrow2
                             )#fluidrow2
                             ),#tabPanel
                    
                    
                    tabPanel("Histogram",
                             fluidRow(
                               box(title = "Histogram", 
                                   status = "danger", 
                                   solidHeader = TRUE,
                                   width = 20,
                                   plotOutput("hist")#plotOutput
                               )#boxfluidrow1
                             ),#fluidrow1
                             fluidRow(
                               box(
                                 status = "danger",
                                 solidHeader = FALSE,
                                 selectInput(
                                   inputId = "var2" , 
                                   label = "Pilih Variabel :",
                                   choices = c("Media Sosial" = "media_sosial",
                                               "Aplikasi Hiburan" = "aplikasi_hiburan",
                                               "Games Online" = "game_online",
                                               "Battery" = "batterai",
                                               "Jam tidur" = "jam_tidur"
                                   )#c
                                 )#selectInput
                               ),#boxfluidrow2
                               box(
                                 status = "danger",
                                 solidHeader = TRUE,
                                 title = "Slider Input",
                                 sliderInput(
                                   inputId = "bins",
                                   label = "Jumlah bin : ",
                                   min = 1,
                                   max = 57,
                                   value = 5
                                 )#sliderInput
                               )#box2fluidrow2
                             )#fluidrow2
                             ),#tabPanel
                    
                    tabPanel("Boxplot",
                             fluidRow(
                               box(title = "Boxplot", 
                                   status = "danger", 
                                   solidHeader = TRUE,
                                   width = 20,
                                   plotlyOutput("boxplot")#plotOutput
                               )#boxfluidrow1
                             ),#fluidrow1
                             fluidRow(
                               box(
                                 status = "danger",
                                 solidHeader = FALSE,
                                 selectInput(
                                   inputId = "var5" , 
                                   label = "Pilih Variabel :",
                                   choices = c("Media Sosial" = "media_sosial",
                                               "Aplikasi Hiburan" = "aplikasi_hiburan",
                                               "Games Online" = "game_online",
                                               "Battery" = "batterai",
                                               "Jam tidur" = "jam_tidur"
                                   )#c
                                 )#selectInput
                               ),#boxfluidrow2
                             )#fluidrow2
                    ),#tabPanel
                    
                    tabPanel("matrix",
                             fluidRow(
                               box(
                                 title = "Scatterplot korelasi data jam tidur",
                                 status = "danger",
                                 solidHeader = TRUE,
                                 width = 6,
                                 column(width =12,plotOutput('cors'))
                               ),#boxinsidefluidrowoutside
                               box(
                                 title = "Scatterplot korelasi data jam tidur",
                                 status = "danger",
                                 solidHeader = TRUE,
                                 width = 6,
                                 column(width = 12,plotOutput('corrm'))
                               )#boxinsidefluidrowoutside
                               
                             )#fluidrow
                             ),#tabpanel
                    
                    tabPanel("scatterplot",
                             fluidRow(
                               box(
                                 title = "Scatterplot korelasi data jam tidur",
                                 status = "danger",
                                 solidHeader = TRUE,
                                 width = 10,
                                 plotOutput("scatterplot")
                               ),#boxinsidefluidrowoutside
                               fluidRow(
                                 box(
                                   status = "danger",
                                   width = 5,
                                   solidHeader = FALSE,
                                   selectInput(
                                     inputId = "var3" , 
                                     label = "Pilih Variabel X :",
                                     choices = c("Media Sosial" = "media_sosial",
                                                 "Aplikasi Hiburan" = "aplikasi_hiburan",
                                                 "Games Online" = "game_online",
                                                 "Battery" = "batterai",
                                                 "Jam tidur" = "jam_tidur"
                                     )#c
                                   )#selectInput
                                 ),#box
                                 
                                 box(
                                   status = "danger",
                                   width = 5,
                                   solidHeader = FALSE,
                                   selectInput(
                                     inputId = "var4" , 
                                     label = "Pilih Variabel Y :",
                                     choices = c("Media Sosial" = "media_sosial",
                                                 "Aplikasi Hiburan" = "aplikasi_hiburan",
                                                 "Games Online" = "game_online",
                                                 "Battery" = "batterai",
                                                 "Jam tidur" = "jam_tidur"
                                     )#c
                                   )#selectInput
                                 ),#box
                                 
                                 box(
                                   title = "Controls",
                                   status = "danger",
                                   width = 5,
                                   solidHeader = TRUE,
                                   sliderInput(
                                     "slider1",
                                     "Jumlah Observasi :", 10,57,5
                                   )#sliderinput
                                 ),#box
                                 
                                 infoBoxOutput(
                                   "correlation"
                                   )#infoboxoutput
                               )#fluidrowindside1
                           )#fluidrow
                             )#tabPanel
                  )#tabsetpanel
                )#mainpanel
                ),#tabItem plot
        tabItem(tabName = "regresi",
                fluidRow(
                  box(
                    status = "danger", solidHeader = FALSE,
                    textInput("mediasosial", "Masukan Lama Waktu Bermain Media Sosial (menit)",""),
                    textInput("aplikasihiburan", "Masukan Lama Waktu Bermain Aplikasi Hiburan (menit)",""),
                    textInput("gameonline", "Masukan Lama Waktu Bermain Games Online (menit)",""),
                    textInput("baterai", "Masukan Lama Waktu Bermain Battery (menit)",""),
                    actionButton('go',"Predict")
                  ),#box1
                  box(
                    status = "danger",
                    h2("Prediksi lama jam tidur"),
                    h3(textOutput("jam_tidur"))
                  )#box2
                )#fluidrow
                )#tabItem plot
      )#tabItems
    ),#dashboardBody()
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  setwd("C:/Kelompok 10_UAS_Dashboard")#setwd
  #file.choose("AnalisisScreenTime.xlsx") # select file
  data<- read_excel("data.xlsx") #data
  output$dataset <- renderTable({
    df <- data
    head(df)
  }) #output$dataset
  
  
  data2 <- read_excel("data2.xlsx") #data2
  df2 <- data2 #df2
  dfsum <- read_excel("datasummary.xlsx") #datasum
  output$summary <- renderPrint({
    summary(dfsum)
  })#output$summary
  
  datacor <- read_excel("datacor.xlsx") #datacor
  df3 <- datacor #
  
  #Correlation Matrix
  M <- cor(datacor)
  col <- colorRampPalette(c("#BB4444", "#EE9988", 
                            "#FFFFFF", "#77AADD",
                            "#4477AA"))
  
  
  cor.mtest <- function(mat, ...) 
  {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) 
    {
      for (j in (i + 1):n)
      {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(datacor)
  
  #Correlation Matrix
  output$corrm <- renderPlot({
    corrplot(M, method = "color", col = col(200),  
             type = "upper", order = "hclust", 
             addCoef.col = "black", # Add coefficient of correlation
             tl.col="black", tl.srt = 45, # Text label color and rotation
             
             # Combine with significance
             p.mat = p.mat, sig.level = 0.05, insig = "blank", 
             
             # hide correlation coefficient
             # on the principal diagonal
             diag = FALSE 
    )
    
    
  })
  
  
  output$cors <- renderPlot(
    corrplot(M, method="color",tl.col = "black",order = "hclust",tl.srt = 45)
  )
  
  
  output$bar<- renderPlot({
    counts <- table(data2[, input$var1, drop = FALSE])
    B <-barplot(counts, 
            beside=TRUE,  
            ylim=c(0, max(counts) + 15), 
            main="Barplot", 
            col=2:3,
            border=0)
    text(B, counts + 5, counts, font=2, col=2:3)
  })#output$bar
  
  
  output$hist <- renderPlot({
    datahist <- data2[, input$var2, drop = FALSE]
    datahist = unlist(datahist)
    datahist = as.numeric(datahist)
    ggplot(data2, aes(datahist)) +
      geom_histogram(alpha = 0.9, color = "black", fill = viridis(input$bins), bins = input$bins) +
      labs( y="Frequency") +
      theme(axis.text = element_text(size=12, face="bold"),
            axis.title = element_text(size=12, face="bold"))
  })#output$hist 
  
  output$boxplot <- renderPlotly({
    
    p2 <- df2 %>%
      plot_ly() %>%
      add_boxplot(~get(input$var5)) %>%
      layout(yaxis = list(showticklabels = F))
    
  })#output$box
  
  
  output$scatterplot <- renderPlot({
    datascatter <- data2[1:input$slider1, ]
    scatterdata1 <- datascatter[, input$var3, drop = FALSE]
    scatterdata1 = unlist(scatterdata1)
    scatterdata1 = as.numeric(scatterdata1)
    scatterdata2 <- datascatter[, input$var4, drop = FALSE]
    scatterdata2 = unlist(scatterdata2)
    scatterdata2 = as.numeric(scatterdata2)
    ggplot(datascatter, aes(scatterdata1, scatterdata2)) +
      labs(x=input$var3, y=input$var4) +
      geom_point(aes(color = scatterdata1))
  })
  
  output$correlation <- renderInfoBox({
    scatterdata1 <- data2[, input$var3, drop = FALSE]
    scatterdata1 = unlist(scatterdata1)
    scatterdata1 = as.numeric(scatterdata1)
    scatterdata2 <- data2[, input$var4, drop = FALSE]
    scatterdata2 = unlist(scatterdata2)
    scatterdata2 = as.numeric(scatterdata2)
    infoBox("Correlation Coefficient",
            paste0(cor(scatterdata1, scatterdata2)),
            color = "red")
  })
  
  
  df3 = reactiveValues()
  observeEvent(
    input$go, {
      df3$bmediasosial <- as.numeric(input$mediasosial)
      df3$baplikasihiburan <- as.numeric(input$aplikasihiburan)
      df3$bgameonline <- as.numeric(input$gameonline)
      df3$bbaterai <- as.numeric(input$baterai)
      
      newPredict = data.frame(
        media_sosial = df3$bmediasosial,
        aplikasi_hiburan = df3$baplikasihiburan,
        game_online = df3$bgameonline,
        batterai = df3$bbaterai
      ) #newpredict
      
      model = lm(jam_tidur ~ media_sosial+aplikasi_hiburan+game_online+batterai, data=df2)
      
      df3$op = predict(model, newPredict)
      
    } #sebelah go
  )#observeEvent
  
  output$jam_tidur <- renderPrint({df3$op})

}#server

# Run the application 
shinyApp(ui = ui, server = server)
