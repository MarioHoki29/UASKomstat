# Memuat library yang diperlukan
library(dplyr)
library(shiny)
library(ggplot2)
library(shinyjs)  # Untuk interaksi JavaScript
library(shinydashboard)  # Untuk tampilan dashboard modern
library(shinyWidgets)  # Untuk widget yang lebih interaktif
library(DT)  # Untuk tabel interaktif

# Membaca data utama dan data pembobotan
url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv"
data <- read.csv(url)
url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"
weights <- read.csv(url)

# Memeriksa beberapa baris pertama dari data untuk memastikan struktur yang benar
head(data)
head(weights)

# Memeriksa nama kolom dalam data
print(names(data))

# Pastikan jumlah kolom di data utama dan pembobotan cocok
print(dim(data))  # Jumlah baris dan kolom data utama
print(dim(weights))  # Jumlah baris dan kolom data pembobotan

# Pastikan kolom 'POVERTY' ada dan bertipe numerik
if (!"POVERTY" %in% names(data)) {
  stop("Kolom POVERTY tidak ditemukan dalam data.")
}

# Mengonversi kolom 'POVERTY' menjadi numerik jika diperlukan
data$POVERTY <- as.numeric(data$POVERTY)

# Lakukan pembobotan pada data
data_weighted <- data %>%
  mutate(across(starts_with("V"), 
                ~ . * weights[[paste0("V", which(names(weights) == cur_column()))]]))  # Menerapkan bobot ke setiap kolom yang dimulai dengan 'V'

# Memeriksa hasil pembobotan
head(data_weighted)

# Menyimpan hasil pembobotan ke dalam file CSV baru
write.csv(data_weighted, file.path(tempdir(), "data_weighted.csv"), row.names = FALSE)

# Membuat UI menggunakan shinydashboard
ui <- dashboardPage(
  skin = "blue",  # Tema warna dashboard
  
  # Header
  dashboardHeader(
    title = tags$div(
      tags$img(src = "https://via.placeholder.com/30", style = "margin-right: 10px;"),
      "Dashboard Kerentanan Sosial"
    ),
    titleWidth = 300
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebarMenu",
      menuItem("Landing Page", tabName = "landing", icon = icon("home")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Eksplorasi Data", tabName = "explore", icon = icon("chart-bar")),
      menuItem("Uji Asumsi Data", tabName = "assumption", icon = icon("vial")),
      menuItem("Statistik Inferensia", tabName = "stats", icon = icon("calculator")),
      menuItem("Metadata", tabName = "metadata", icon = icon("info-circle"))
    )
  ),
  
  # Body
  dashboardBody(
    # Custom CSS untuk meningkatkan estetika
    tags$head(
      tags$style(HTML("
        /* General Styling */
        body {
          font-family: 'Roboto', sans-serif;
          background-color: #f4f7fa;
        }
        .content-wrapper {
          background-color: #ffffff;
          border-radius: 8px;
          padding: 20px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        }
        h1, h2, h3, h4 {
          color: #2c3e50;
          font-weight: 500;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          background-color: #ffffff;
          padding: 20px;
          margin-bottom: 20px;
        }
        .btn-primary {
          background-color: #3498db;
          border-color: #2980b9;
          border-radius: 5px;
          transition: background-color 0.3s ease;
        }
        .btn-primary:hover {
          background-color: #2980b9;
        }
        table.dataTable {
          border-collapse: collapse;
          width: 100%;
        }
        table.dataTable th, table.dataTable td {
          border: 1px solid #e0e0e0;
          padding: 12px;
        }
        table.dataTable th {
          background-color: #3498db;
          color: white;
        }
        .plot-container {
          border: 2px solid #3498db;
          border-radius: 8px;
          padding: 10px;
          background-color: #f9f9f9;
        }
        .sidebar-menu li a {
          font-size: 16px;
          padding: 15px;
          transition: background-color 0.3s ease;
        }
        .sidebar-menu li a:hover {
          background-color: #3498db;
          color: white;
        }
      "))
    ),
    
    # Tab Items
    tabItems(
      # Landing Page
      tabItem(tabName = "landing",
              fluidRow(
                box(
                  width = 12,
                  h1("Selamat Datang di Dashboard Kerentanan Sosial"),
                  p("Dashboard ini menyediakan analisis interaktif tentang kerentanan sosial di Indonesia berdasarkan data Survei Sosial Ekonomi Nasional (SUSENAS)."),
                  p("Fitur utama meliputi eksplorasi data, visualisasi, uji statistik, dan informasi metadata. Gunakan menu di sisi kiri untuk navigasi."),
                  tags$hr(),
                  h4("Petunjuk Penggunaan"),
                  p("Pilih tab 'Data' untuk melihat dataset, 'Eksplorasi Data' untuk visualisasi, 'Uji Asumsi Data' untuk uji statistik, atau 'Metadata' untuk informasi lebih lanjut.")
                )
              )
      ),
      
      # Data Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  width = 12,
                  h4("Tampilan Data"),
                  selectInput("category", "Pilih Kategori:", choices = colnames(data_weighted), selected = "POVERTY"),
                  actionButton("go_data", "Tampilkan Data", class = "btn-primary", icon = icon("eye")),
                  downloadButton("download_data", "Unduh Data", class = "btn-primary", icon = icon("download")),
                  DT::dataTableOutput("data_table")
                )
              )
      ),
      
      # Eksplorasi Data Tab
      tabItem(tabName = "explore",
              fluidRow(
                box(
                  width = 12,
                  h4("Eksplorasi Data"),
                  actionButton("explore", "Tampilkan Visualisasi", class = "btn-primary", icon = icon("chart-line")),
                  plotOutput("data_plot", height = "400px", width = "100%"),
                  downloadButton("download_plot", "Unduh Grafik", class = "btn-primary", icon = icon("download")),
                  tags$hr(),
                  h4("Statistik Deskriptif"),
                  tableOutput("data_summary")
                )
              )
      ),
      
      # Uji Asumsi Data Tab
      tabItem(tabName = "assumption",
              fluidRow(
                box(
                  width = 6,
                  h4("Uji Normalitas"),
                  actionButton("normality_test", "Jalankan Uji Normalitas", class = "btn-primary", icon = icon("vial")),
                  tableOutput("normality_result")
                ),
                box(
                  width = 6,
                  h4("Uji Homogenitas"),
                  actionButton("homogeneity_test", "Jalankan Uji Homogenitas", class = "btn-primary", icon = icon("vial")),
                  tableOutput("homogeneity_result")
                )
              )
      ),
      
      # Statistik Inferensia Tab
      tabItem(tabName = "stats",
              fluidRow(
                box(
                  width = 6,
                  h4("Uji t"),
                  actionButton("t_test", "Jalankan Uji t", class = "btn-primary", icon = icon("calculator")),
                  tableOutput("t_test_result")
                ),
                box(
                  width = 6,
                  h4("Uji ANOVA"),
                  actionButton("anova_test", "Jalankan Uji ANOVA", class = "btn-primary", icon = icon("calculator")),
                  tableOutput("anova_result")
                ),
                box(
                  width = 12,
                  downloadButton("download_stats", "Unduh Hasil Statistik", class = "btn-primary", icon = icon("download"))
                )
              )
      ),
      
      # Metadata Tab
      tabItem(tabName = "metadata",
              fluidRow(
                box(
                  width = 12,
                  h4("Informasi Metadata"),
                  p("Dataset ini berisi informasi mengenai berbagai faktor yang mempengaruhi kerentanan sosial di Indonesia, termasuk:"),
                  tags$ul(
                    tags$li("DISTRICTCODE: Kode wilayah/kabupaten"),
                    tags$li("CHILDREN: Persentase populasi di bawah lima tahun"),
                    tags$li("FEMALE: Persentase populasi perempuan"),
                    tags$li("ELDERLY: Persentase usia 65 tahun dan kelebihan populasi"),
                    tags$li("FHEAD: Persentase rumah tangga dengan kepala rumah tangga perempuan"),
                    tags$li("POVERTY: Persentase orang miskin"),
                    tags$li("NOELECTRIC: Persentase rumah tangga yang tidak menggunakan listrik"),
                    tags$li("LOWEDU: Persentase penduduk dengan pendidikan rendah"),
                    tags$li("GROWTH: Pertumbuhan populasi"),
                    tags$li("ILLITERATE: Persentase penduduk yang tidak bisa membaca dan menulis")
                  ),
                  p("Semua data ini dikumpulkan dari Survei Sosial Ekonomi Nasional (SUSENAS) dan diolah untuk membantu analisis kerentanan sosial.")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Menampilkan Data berdasarkan kategori yang dipilih
  observeEvent(input$go_data, {
    selected_category <- input$category
    output$data_table <- DT::renderDataTable({
      DT::datatable(data_weighted %>% select(all_of(selected_category)),
                    options = list(pageLength = 10, autoWidth = TRUE))
    })
  })
  
  # Eksplorasi Data: Statistik Deskriptif dan Visualisasi
  observeEvent(input$explore, {
    output$data_summary <- renderTable({
      summary(data_weighted)
    })
    
    output$data_plot <- renderPlot({
      ggplot(data_weighted, aes(x = CHILDREN, y = POVERTY)) +
        geom_point(color = "#3498db", size = 3, alpha = 0.6) +
        theme_minimal() +
        labs(title = "Distribusi Kemiskinan vs Anak",
             x = "Persentase Anak (%)",
             y = "Persentase Kemiskinan (%)") +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title = element_text(size = 12),
          panel.grid.major = element_line(color = "#e0e0e0"),
          panel.grid.minor = element_blank()
        )
    })
  })
  
  # Uji Normalitas
  observeEvent(input$normality_test, {
    normality_result <- shapiro.test(data_weighted$POVERTY)
    output$normality_result <- renderTable({
      data.frame(
        "Statistik Uji" = normality_result$statistic,
        "P-Value" = normality_result$p.value
      )
    })
  })
  
  # Uji Homogenitas
  observeEvent(input$homogeneity_test, {
    group_sizes <- table(data_weighted$DISTRICTCODE)
    if (any(group_sizes < 2)) {
      showModal(modalDialog(
        title = "Peringatan",
        "Beberapa grup memiliki kurang dari dua pengamatan, uji Bartlett tidak dapat dilakukan.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      homogeneity_result <- bartlett.test(POVERTY ~ DISTRICTCODE, data = data_weighted)
      output$homogeneity_result <- renderTable({
        data.frame(
          "Statistik Uji" = homogeneity_result$statistic,
          "P-Value" = homogeneity_result$p.value
        )
      })
    }
  })
  
  # Uji t
  observeEvent(input$t_test, {
    t_result <- t.test(data_weighted$POVERTY)
    output$t_test_result <- renderTable({
      data.frame(
        "Statistik Uji" = t_result$statistic,
        "P-Value" = t_result$p.value
      )
    })
  })
  
  # Uji ANOVA
  observeEvent(input$anova_test, {
    anova_result <- summary(aov(POVERTY ~ DISTRICTCODE, data = data_weighted))
    output$anova_result <- renderTable({
      data.frame(
        "F-Value" = anova_result[[1]]$`F value`[1],
        "P-Value" = anova_result[[1]]$`Pr(>F)`[1]
      )
    })
  })
  
  # Fitur Download Data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data_weighted_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_weighted, file, row.names = FALSE)
    }
  )
  
  # Fitur Download Plot
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 8, height = 6)
    }
  )
  
  # Fitur Download Statistik
  output$download_stats <- downloadHandler(
    filename = function() {
      paste("stats_result_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      stats <- data.frame(
        Test = c("Normalitas", "Homogenitas"),
        Statistik = c(
          shapiro.test(data_weighted$POVERTY)$statistic,
          tryCatch(
            bartlett.test(POVERTY ~ DISTRICTCODE, data = data_weighted)$statistic,
            error = function(e) NA
          )
        ),
        P_Value = c(
          shapiro.test(data_weighted$POVERTY)$p.value,
          tryCatch(
            bartlett.test(POVERTY ~ DISTRICTCODE, data = data_weighted)$p.value,
            error = function(e) NA
          )
        )
      )
      write.csv(stats, file, row.names = FALSE)
    }
  )
}

# Jalankan aplikasi Shiny
shinyApp(ui = ui, server = server)