
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(lubridate)
library(padr)
library(rvest)
library(plotly)

vio <- read_csv('https://raw.githubusercontent.com/muhammadwachid/tetris/main/dataset/kekerasan-pelapor.csv')

kemenppa <- read_csv('kemenPPA.csv')

header <- dashboardHeader()
anchor <- tags$a(href='http://dqlab.id',
                 tags$img(src='https://dqlab.id/files/dqlab/cache/6b8c33bdec694a9af1b696bef97d2d25_x_Thumbnail200.png', height='18', width='72'),
                 'Kekerasan pada Anak dan Perempuan di DKI Jakarta')

header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML(".name "))),
  anchor,
  class = 'name')

# Define UI for application that draws a histogram
ui<-fluidPage(
  dashboardPage(skin = "red",
                dashboardHeader(title = anchor, titleWidth = 600),
                dashboardSidebar(
                  sidebarMenu(id = 'sidebarmenu',
                              # first menu item
                              menuItem("Apa itu kekerasan?", tabName = "penjelasan1", icon = icon("question-circle")),
                              menuItem(selectInput("ke", "bulan ke -", unique(vio$bulan), multiple = TRUE)),
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
                    tabItem("penjelasan1", h4("Kekerasan dalam arti sempit merujuk pada tindakan berupa serangan, perusakan, penghacuran terhadap diri (fisik) seseorang maupun milik atau sesuatu yang secara potensial menjadi milik orang lain.", br(), br(),
                    "Berarti, dalam pengertian ini kekerasan merujuk pada tindakan fisik yang bersifat personal, yaitu mengarah pada orang atau kelompok tertentu yang dilakukan secara sengaja, langsung, dan aktual.", br(), br(),
                    "Kekerasan dalam arti luas merujuk pada tindakan fisik maupun tindakan psikologik yang dilakukan seseorang atau sekelompok orang, baik yang dilakukan secara sengaja maupun secara tidak sengaja, langsung atau tidak langsung, personal atau struktural.", br(), br(),
                    "Kementerian Pemberdayaan Perempuan dan Perlindungan Anak (Kemen PPPA) mencatat, laporan kekerasan terhadap perempuan dan anak meningkat dalam kurun waktu tiga tahun terakhir(2019-2021).",br(), br(), (radioButtons("y_axis",
                                                                                                                                                                                                                                          "Visualisasi laporan PPA",
                                                                                                                                                                                                                                          choices = list("laporan_anak"                  = "laporan_anak", 
                                                                                                                                                                                                                                                         "laporan_perempuan"       = "laporan_perempuan",
                                                                                                                                                                                                                                                         "korban_anak"      = "korban_anak",
                                                                                                                                                                                                                                                         "korban_perempuan"    = "korban_perempuan"),
                                                                                                                                                                                                                                          selected = "korban_perempuan")), br(), br(), fluidRow(box(plotlyOutput("plot9"), width=6)),
                    "Dalam dashboard ini memuat beberapa informasi terkait kekerasan di Jakarta, data didapat dari data.jakarta.go.id yang memuat data kekerasan pada tahun 2020 dan sebagian 2021.")),
                    tabItem(tabName = "chart1",
                            # First Row
                            fluidRow(
                              box(title = "Apakah Korban Melapor?", plotlyOutput("plot1", height = 250),
                                  width=5),
                              box(title = "Jumlah Korban berdasarkan Wilayah (2020)", plotlyOutput("plot2", height = 250),
                                  width=7),
                              box(title = "Jumlah Korban berdasarkan Usia (2020)", plotlyOutput("plot3", height = 250),
                                  width=6),
                              box(title = "Jumlah Pelaku berdasarkan Usia (2020)", plotlyOutput("plot4", height = 250)
                              ))),
                    tabItem(tabName = "chart2",
                            # First Row
                            fluidRow(box(title = "Pekerjaan Pelaku dan Korban (2020)", plotlyOutput("plot8", height = 250), width = 7),
                                     box(title = "Jumlah Korban berdasarkan Jenis Kekerasan (2020)", plotlyOutput("plot6", height = 250),
                                         width=5),
                                     box(title = "Top 5 Layanan Korban (2020)", plotlyOutput("plot5", height = 250),
                                         width=6),
                                     box(title = "Jumlah Korban berdasarkan Pelaku (2021 hingga Oktober)", plotlyOutput("plot7", height = 250),
                                         width=6),
                            ))))))
# Define server logic required to draw a histogram
server<-shinyServer(function(input, output, session){
  # kemenppa <- read_csv('kemenPPA.csv')

  
  kemenppa <- reactive({
    withProgress(message = 'Preparing Violance dataset', value = 0, {
      kemenppa <- read_csv('kemenPPA.csv')
    })
  })
  
  
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
    
    bulanke <- if (is.null(input$ke)) unique(vio_vic()$bulan) else as.numeric(input$ke)
    data1 <- filter(vio_vic(), bulan %in% bulanke)
    
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
    
    bulanke <- if (is.null(input$ke)) unique(vio_plc()$bulan) else as.numeric(input$ke)
    data2 <- filter(vio_plc(), bulan %in% bulanke)%>%
      arrange(desc(jumlah_korban_perempuan + jumlah_korban_anak))
    data2a <- subset(data2, kabupaten_kota!="Klien Luar Prov. DKI")
    
    bar_plc <- plot_ly(data2a, x = ~kabupaten_kota, y = ~jumlah_korban_anak, type = 'bar', name = 'Anak')
    bar_plc <- bar_plc %>% add_trace(y = ~jumlah_korban_perempuan, name = 'Perempuan')
    bar_plc <- bar_plc %>% layout(xaxis = list(title = 'Wilayah'), yaxis = list(title = 'Jumlah Korban'), barmode = 'stack')%>%
      layout(xaxis = list(categoryorder = "total descending"))
    bar_plc
  })
  
  output$plot3 <- renderPlotly({
    
    bulanke <- if (is.null(input$ke)) unique(vio_age()$bulan) else as.numeric(input$ke)
    data3 <- filter(vio_age(), bulan %in% bulanke)
    
    bar_age_vic <- plot_ly(data3,
                           x = ~usia_klien_dan_pelaku,
                           y = ~jumlah_klien,
                           name = "jumlah korban",
                           type = "bar") %>%
      layout(xaxis = list(categoryorder = "total descending"))%>%
      layout(xaxis = list(title = 'Usia Korban'), yaxis = list(title = 'Jumlah Korban'))
    bar_age_vic
  })
  
  output$plot4 <- renderPlotly({
    
    bulanke <- if (is.null(input$ke)) unique(vio_age()$bulan) else as.numeric(input$ke)
    data3 <- filter(vio_age(), bulan %in% bulanke)

    bar_age_sus <- plot_ly(data3,
                           x = ~usia_klien_dan_pelaku,
                           y = ~jumlah_pelaku,
                           name = "jumlah pelaku",
                           type = "bar") %>%
      layout(xaxis = list(categoryorder = "total descending"))%>%
      layout(xaxis = list(title = 'Usia Pelaku'), yaxis = list(title = 'Jumlah Pelaku'))
    bar_age_sus
  })
  
  output$plot5 <- renderPlotly({
    
    bulanke <- if (is.null(input$ke)) unique(vio_service()$bulan) else as.numeric(input$ke)
    data5 <- filter(vio_service(), bulan %in% bulanke)%>%
      arrange(desc(jumlah_korban_terlayani))%>%
      group_by(jenis_pelayanan)
    data5<-head(data5, 5)
    
    bar_service <- plot_ly(data5,
                           x = ~jenis_pelayanan,
                           y = ~jumlah_korban_terlayani,
                           name = "jumlah korban",
                           type = "bar") %>%
      layout(xaxis = list(categoryorder = "total descending"))%>%
      layout(xaxis = list(title = 'Jenis Pelayanan'), yaxis = list(title = 'Jumlah Korban Terlayani'))
    bar_service
  })
  
  output$plot6 <- renderPlotly({
    
    bulanke <- if (is.null(input$ke)) unique(vio_type()$bulan) else as.numeric(input$ke)
    data6 <- filter(vio_type(), bulan %in% bulanke)
    
    bar_type <- plot_ly(data=data6,
                        x = ~jenis_kekerasan,
                        y = ~jumlah_korban_jenis_kekerasan,
                        name = "jumlah korban",
                        type = "bar") %>%
      layout(xaxis = list(categoryorder = "total descending"))%>%
      layout(xaxis = list(title = 'Jenis Kekerasan'), yaxis = list(title = 'Jumlah Korban'))
    bar_type
  })
  output$plot7 <- renderPlotly({
    
    bulanke <- if (is.null(input$ke)) unique(vio_sus()$bulan) else as.numeric(input$ke)
    data7 <- filter(vio_sus(), bulan %in% bulanke)
    
    bar_sus <- plot_ly(data7,
                       x = ~pelaku_kekerasan,
                       y = ~jumlah_korban_kekerasan,
                       name = "Jumlah Korban",
                       type = "bar") %>%
      layout(xaxis = list(categoryorder = "total descending"))%>%
      layout(xaxis = list(title = 'Pelaku'), yaxis = list(title = 'Jumlah Korban'))
    bar_sus
  })
  
  output$plot8 <- renderPlotly({
    
    bulanke <- if (is.null(input$ke)) unique(vio_job()$bulan) else as.numeric(input$ke)
    data8 <- filter(vio_job(), bulan %in% bulanke)
    
    bar_job <- plot_ly(data8, x = ~pekerjaan_klien_dan_pelaku, y = ~jumlah_pekerjaan_klien, type = 'bar', name = 'Korban')
    bar_job <- bar_job %>% add_trace(y = ~jumlah_pekerjaan_pelaku, name = 'Pelaku')%>%
      layout(xaxis = list(categoryorder = "total descending"))
    bar_job <- bar_job %>%
      layout(xaxis = list(title = 'Pekerjaan'), yaxis = list(title = 'Jumlah'), barmode = 'group')
    
    bar_job
  })
  
  data9 <-
    reactive({
      tmp <- kemenppa() %>% select(tahun, y = input$y_axis)
      return(tmp)
    })
  output$plot9 = renderPlotly({
    plot_ly(data = data9(), x = ~tahun, y = ~y, 
            type = "scatter", mode = "lines")%>%
      layout(yaxis = list(title = 'Jumlah'))
  })
})

# Run the application 
shinyApp(ui = ui, server = server)
