
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(lubridate)
library(padr)
library(rvest)
library(plotly)

# Define UI for application that draws a histogram
ui<-fluidPage(
  dashboardPage(skin = "red",
                dashboardHeader(title = "Kekerasan pada Anak dan Perempuan di DKI Jakarta", titleWidth = 650),
                dashboardSidebar(
                  sidebarMenu(id = 'sidebarmenu',
                              # first menu item
                              menuItem("Apa itu kekerasan?", tabName = "penjelasan1", icon = icon("question-circle")),
                              menuItem(numericInput("ke", "Bulan", value = 1, min = 1, max = 12)),
                              # second menu item with 2 sub menus
                              menuItem('Grafik',
                                       icon = icon('bar-chart-o'),
                                       menuSubItem('Kekerasan di DKI Jakarta part 1',
                                                   tabName = 'chart1',
                                                   icon = icon('bar-chart-o')),
                                       menuSubItem('Kekerasan di DKI Jakarta part 2',
                                                   tabName = 'chart2',
                                                   icon = icon('bar-chart-o')))
                              )),
                dashboardBody(
                  tabItems(
                    tabItem("penjelasan1", h3("Kekerasan dalam arti sempit merujuk pada tindakan berupa serangan, perusakan, penghacuran terhadap diri (fisik) seseorang maupun milik atau sesuatu yang secara potensial menjadi milik orang lain.<br/>
                    Berarti, dalam pengertian ini kekerasan merujuk pada tindakan fisik yang bersifat personal, yaitu mengarah pada orang atau kelompok tertentu yang dilakukan secara sengaja, langsung, dan aktual.<br/>
                                              Kekerasan dalam arti luas merujuk pada tindakan fisik maupun tindakan psikologik yang dilakukan seseorang atau sekelompok orang, baik yang dilakukan secara sengaja maupun secara tidak sengaja, langsung atau tidak langsung, personal atau struktural.<br/>
                                              Dalam dashboard ini memuat beberapa informasi terkait kekerasan di Jakarta, data didapat dari data.jakarta.go.id yang memuat data kekerasan pada tahun 2020 dan sebagian 2021.<br/>")),
                    tabItem(tabName = "chart1",
                            # First Row
                            fluidRow(
                              box(title = "Apakah Korban Melapor?", plotlyOutput("plot1", height = 250),
                                  width=6),
                              box(title = "Jumlah Korban berdasarkan Wilayah", plotlyOutput("plot2", height = 250),
                                  width=6),
                              box(title = "Jumlah Korban berdasarkan Usia", plotlyOutput("plot3", height = 250),
                                  width=6),
                              box(title = "Jumlah Pelaku berdasarkan Usia", plotlyOutput("plot4", height = 250)
                                  ))),
                    tabItem(tabName = "chart2",
                            # First Row
                            fluidRow(box(title = "Top 5 Layanan Korban", plotlyOutput("plot5", height = 250), width = 6),
                            box(title = "Jumlah Korban berdasarkan Jenis Kekerasan", plotlyOutput("plot6", height = 250),
                                width=6),
                            box(title = "Jumlah Korban berdasarkan Pelaku", plotlyOutput("plot7", height = 250),
                                width=6),
                            box(title = "Pekerjaan Pelaku dan Korban", plotlyOutput("plot8", height = 250),
                                width=6),
                            )),
                    tabItem(tabName = "db",
                            # First Row
                            fluidRow(tabBox(id="tabchart1",
                                            tabPanel("World",DT::dataTableOutput("Tab1", height = "450px"), width = 9),
                                            tabPanel("Indonesia",DT::dataTableOutput("Tab2", height = "450px"), width = 9), width = 12)))
                   ))))
# Define server logic required to draw a histogram
server<-shinyServer(function(input, output, session){
  # https://raw.githubusercontent.com/muhammadwachid/tetris/main/dataset/kekerasan-pekerjaan-pelaku-klien.csv
  
  vio_job <- reactive({
    withProgress(message = 'Preparing Violance dataset', value = 0, {
      vio_job <- read_csv('https://raw.githubusercontent.com/muhammadwachid/tetris/main/dataset/kekerasan-pekerjaan-pelaku-klien.csv')
    })
  })
  
  vio_sus <- reactive({
    withProgress(message = 'Preparing Violance dataset', value = 0, {
      vio_sus <- read_csv('https://raw.githubusercontent.com/muhammadwachid/tetris/main/dataset/kekerasan-pelaku.csv')
    })
  })
  
  vio_type <- reactive({
    withProgress(message = 'Preparing Violance dataset', value = 0, {
      vio_type <- read_csv('https://raw.githubusercontent.com/muhammadwachid/tetris/main/dataset/kekerasan-jenis-kekerasan.csv')
    })
  })
  
  vio_service <- reactive({
    withProgress(message = 'Preparing Violance dataset', value = 0, {
      vio_service <- read_csv('https://raw.githubusercontent.com/muhammadwachid/tetris/main/dataset/kekerasan-jenis-pelayanan.csv')
    })
  })
  
  vio_age <- reactive({
    withProgress(message = 'Preparing Violance dataset', value = 0, {
      vio_age <- read_csv('https://raw.githubusercontent.com/muhammadwachid/tetris/main/dataset/kekerasan-usia-pelaku-klien.csv')
    })
  })
  
  vio_vic <- reactive({
    withProgress(message = 'Preparing Violance dataset', value = 0, {
      vio_vic <- read_csv('https://raw.githubusercontent.com/muhammadwachid/tetris/main/dataset/kekerasan-pelapor.csv')
    })
  })
  
  vio_plc <- reactive({
    withProgress(message = 'Preparing violance dataset', value = 0, {
      vio_plc <- read_csv('https://raw.githubusercontent.com/muhammadwachid/tetris/main/dataset/kekerasan-tempat-kejadian.csv')
    })
  })
  
  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
  
  output$plot1 <- renderPlotly({
    
    data1 <- vio_vic()%>%
      filter(bulan == input$ke)
    
    pie_vic <- plot_ly(data1, labels = ~jenis_lapor, values = ~jumlah_laporan, type = 'pie',
                       textposition = 'inside',
                       textinfo = 'label+percent',
                       insidetextfont = list(color = '#FFFFFF'),
                       hoverinfo = 'text',
                       text = ~paste(jenis_lapor, jumlah_laporan),
                       marker = list(colors = colors,
                                     line = list(color = '#FFFFFF', width = 1)),
                       #The 'pull' attribute can also be used to create space between the sectors
                       showlegend = FALSE)
    pie_vic
  })
  
  output$plot2 <- renderPlotly({
    
    data2 <- vio_plc()%>%
      filter(bulan == input$ke)
    
    bar_plc <- plot_ly(data2, x = ~kabupaten_kota, y = ~jumlah_korban_anak, type = 'bar', name = 'Jumlah Korban Anak')
    bar_plc <- bar_plc %>% add_trace(y = ~jumlah_korban_perempuan, name = 'Jumlah Korban Perempuan')
    bar_plc <- bar_plc %>% layout(yaxis = list(title = 'Jumlah Korban Menurut WIlayah'), barmode = 'stack')%>%
      layout(xaxis = list(categoryorder = "total descending"))
    bar_plc
  })
  
  output$plot3 <- renderPlotly({
    
    data3 <- vio_age()%>%
      filter(bulan == input$ke)
    
    bar_age_vic <- plot_ly(data3,
                           x = ~usia_klien_dan_pelaku,
                           y = ~jumlah_klien,
                           name = "jumlah korban",
                           type = "bar") %>%
      layout(xaxis = list(categoryorder = "total descending"))
    bar_age_vic
  })
  
  output$plot4 <- renderPlotly({
    
    data3 <- vio_age()%>%
      filter(bulan == input$ke)
    
    bar_age_sus <- plot_ly(data3,
                           x = ~usia_klien_dan_pelaku,
                           y = ~jumlah_pelaku,
                           name = "jumlah pelaku",
                           type = "bar") %>%
      layout(xaxis = list(categoryorder = "total descending"))
    bar_age_sus
  })
  
  output$plot5 <- renderPlotly({
    
    data5 <- vio_service()%>%
      filter(bulan == input$ke)
    
    bar_service <- plot_ly(head(data5, 5),
                           x = ~jenis_pelayanan,
                           y = ~jumlah_korban_terlayani,
                           name = "jumlah korban",
                           type = "bar") %>%
      layout(xaxis = list(categoryorder = "total descending"))
    bar_service
  })
  
  output$plot6 <- renderPlotly({
    
    data6 <- vio_type()%>%
      filter(bulan == input$ke)
    
    bar_type <- plot_ly(data=data6,
                        x = ~jenis_kekerasan,
                        y = ~jumlah_korban_jenis_kekerasan,
                        name = "jumlah korban",
                        type = "bar") %>%
      layout(xaxis = list(categoryorder = "total descending"))
    bar_type
  })
  output$plot7 <- renderPlotly({
    
    data7 <- vio_sus()%>%
      filter(bulan == input$ke)
    
    bar_sus <- plot_ly(data7,
                       x = ~pelaku_kekerasan,
                       y = ~jumlah_korban_kekerasan,
                       name = "Jumlah Korban",
                       type = "bar") %>%
      layout(xaxis = list(categoryorder = "total descending"))
    bar_sus
  })
  
  output$plot8 <- renderPlotly({
    
    data8 <- vio_job()%>%
      filter(bulan == input$ke)
    bar_job <- plot_ly(data8, x = ~pekerjaan_klien_dan_pelaku, y = ~jumlah_pekerjaan_klien, type = 'bar', name = 'Pekerjaan Korban')
    bar_job <- bar_job %>% add_trace(y = ~jumlah_pekerjaan_pelaku, name = 'Pekerjaan Pelaku')
    bar_job <- bar_job %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
    
    bar_job
  })

})

# Run the application 
shinyApp(ui = ui, server = server)
