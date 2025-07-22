# =============================================================================
# DASHBOARD ANALISIS DATA SOCIAL VULNERABILITY INDEX (SoVI)
# =============================================================================
# 
# Metadata Struktural:
# Tujuan       : Analisis komprehensif data SoVI dengan uji statistik lengkap
# Sumber Data  : Social Vulnerability Index dataset
# Referensi    : https://www.sciencedirect.com/science/article/pii/S2352340921010180
# Struktur Data: 
#   - Variabel numerik: berbagai indikator kerentanan sosial
#   - Data spasial: informasi geografis kabupaten/kota
#   - Matriks jarak: untuk analisis spasial lanjutan
# 
# Fitur Utama:
# 1. Persiapan Data: Pembersihan NA dan outlier dengan metode IQR
# 2. Manajemen Data: Kategorisasi variabel kontinu untuk analisis grup
# 3. Eksplorasi Data: Statistik deskriptif dan visualisasi interaktif
# 4. Uji Asumsi: Normalitas (Shapiro-Wilk) dan homogenitas (Levene)
# 5. Uji Inferensial: t-test, proporsi, varians, ANOVA, regresi berganda
# 6. Visualisasi: Histogram, boxplot, dan peta tematik terintegrasi
# 7. Export: Data (XLS/CSV/SAV) dan hasil analisis (PDF/PNG)
# =============================================================================

# Load pustaka yang diperlukan
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(ggplot2)
library(plotly)
library(sf)
library(shinythemes)
library(shinyWidgets)
library(htmltools)
library(rmarkdown)
library(knitr)
library(car)       # Untuk uji Levene
library(lmtest)    # Untuk uji Breusch-Pagan
library(writexl)   # Untuk ekspor Excel
library(haven)     # Untuk ekspor SAV
library(gridExtra) # Untuk download PNG

# Baca data dengan error handling
tryCatch({
  sovi_data <- read.csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv") %>%
    mutate(DISTRICTCODE = as.character(DISTRICTCODE))
  distance_matrix <- read.csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv")
  
  # Coba berbagai path untuk shapefile
  shp_paths <- c(
    "C:/Users/ASUS/Downloads/STIS SEMESTER 4/UAS Komstat/Administrasi_Kabupaten.shp",
    "Administrasi_Kabupaten.shp",
    "./Administrasi_Kabupaten.shp"
  )
  
  shp <- NULL
  for(path in shp_paths) {
    if(file.exists(path)) {
      shp <- st_read(path)
      break
    }
  }
  
  if(is.null(shp)) {
    warning("Shapefile tidak ditemukan. Peta tidak akan tersedia.")
    # Buat dummy shapefile untuk menghindari error
    shp <- data.frame(kodekab = character(0), geometry = I(list()))
    shp <- st_as_sf(shp)
  }
  
  # Gabungkan shapefile dengan data
  shp_merged <- shp %>%
    mutate(kodekab = as.character(kodekab)) %>%
    left_join(sovi_data, by = c("kodekab" = "DISTRICTCODE"))
  
}, error = function(e) {
  stop("Error loading data: ", e$message)
})

# Definisikan UI
ui <- dashboardPage(
  # Header dashboard
  dashboardHeader(title = "Dashboard Analisis Data SoVI"),
  
  # Sidebar dengan item menu
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "home", icon = icon("home")),
      menuItem("Persiapan Data", tabName = "data_preparation", icon = icon("broom")),
      menuItem("Manajemen Data", tabName = "data_management", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "data_exploration", icon = icon("chart-bar")),
      menuItem("Uji Asumsi", tabName = "assumption_testing", icon = icon("check-circle")),
      menuItem("Uji Beda Rata-rata", tabName = "mean_tests", icon = icon("balance-scale")),
      menuItem("Uji Proporsi & Varians", tabName = "prop_var_tests", icon = icon("percentage")),
      menuItem("ANOVA", tabName = "anova", icon = icon("table")),
      menuItem("Regresi Linear Berganda", tabName = "regression", icon = icon("line-chart"))
    )
  ),
  
  # Body dashboard
  dashboardBody(
    # Tambahkan CSS untuk tampilan dan scroll horizontal pada tabel
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f4f6f9; }
        .box { border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); }
        .shiny-download-link { background-color: #007bff; color: white; padding: 8px 16px; border-radius: 4px; }
        .shiny-download-link:hover { background-color: #0056b3; }
        h3 { color: #2c3e50; }
        .sidebar-menu li a { font-size: 16px; }
        .interpretation-box { background-color: #fff3cd; padding: 15px; border-radius: 5px; margin-top: 10px; }
        .data-table { margin-top: 20px; overflow-x: auto; }
        .data-table table { width: 100%; }
        .error-message { color: red; font-weight: bold; }
        .plot-container { display: flex; flex-wrap: wrap; gap: 10px; }
        .plot-item { flex: 1; min-width: 300px; }
        .warning-box { background-color: #f8d7da; color: #721c24; padding: 15px; border-radius: 5px; margin: 10px 0; }
      "))
    ),
    
    tabItems(
      # Tab 1: Beranda
      tabItem(tabName = "home",
              fluidRow(
                box(width = 12, title = "Selamat Datang di Dashboard SoVI", status = "primary", solidHeader = TRUE,
                    h3("Deskripsi Dashboard"),
                    div(style = "background-color: #e7f3ff; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                        h4("Metadata Struktural Dataset"),
                        p(strong("Nama Dataset:"), "Social Vulnerability Index (SoVI) Data"),
                        p(strong("Sumber Referensi:"), 
                          a("Data descriptor: Social vulnerability to environmental hazards in Indonesian coastal cities", 
                            href = "https://www.sciencedirect.com/science/article/pii/S2352340921010180", 
                            target = "_blank")),
                        p(strong("Struktur Data:")),
                        tags$ul(
                          tags$li("Variabel numerik: Berbagai indikator kerentanan sosial (demografi, ekonomi, infrastruktur)"),
                          tags$li("Variabel spasial: Informasi geografis kabupaten/kota Indonesia"),
                          tags$li("Unit observasi: Kabupaten/kota pesisir di Indonesia"),
                          tags$li("Skala pengukuran: Rasio dan interval untuk analisis statistik parametrik")
                        ),
                        p(strong("Kegunaan Analitis:")),
                        tags$ul(
                          tags$li("Identifikasi pola kerentanan sosial secara spasial"),
                          tags$li("Perbandingan tingkat kerentanan antar wilayah"),
                          tags$li("Analisis faktor-faktor yang mempengaruhi kerentanan"),
                          tags$li("Pemodelan prediktif untuk penilaian risiko")
                        )
                    ),
                    p("Dashboard ini dirancang untuk menganalisis data Social Vulnerability Index (SoVI) dengan pendekatan statistik komprehensif. 
                      Fitur meliputi persiapan data, manajemen data, eksplorasi data, uji asumsi statistik, uji inferensial (uji beda rata-rata, proporsi, varians, ANOVA), 
                      dan regresi linear berganda. Semua hasil, plot, peta, dan tabel dapat diunduh sebagai PDF, XLS, CSV, atau SAV untuk data, dan PDF atau PNG untuk plot."),
                    h3("Fitur Utama"),
                    p("- **Persiapan Data**: Menghapus NA dan outlier, menampilkan data awal atau bersih."),
                    p("- **Manajemen Data**: Mengkategorikan data kontinu menjadi faktor untuk analisis grup."),
                    p("- **Eksplorasi Data**: Statistik deskriptif, histogram, boxplot, peta, dan tabel data."),
                    p("- **Uji Asumsi**: Uji normalitas (Shapiro-Wilk) dan homogenitas (Levene)."),
                    p("- **Uji Statistik**: Uji t, proporsi, varians, dan ANOVA."),
                    p("- **Regresi**: Regresi linear berganda dengan opsi pembatalan variabel independen."),
                    p("- **Interaktif**: Output interaktif dengan interpretasi otomatis yang komprehensif."),
                    p("- **Download**: Unduh data (XLS, CSV, SAV) dan hasil analisis (PDF) atau plot (PDF, PNG)."),
                    h3("Petunjuk Penggunaan"),
                    p("Gunakan menu di sisi kiri untuk navigasi. Berikut panduan setiap menu:"),
                    h4("1. Persiapan Data"),
                    p("- Data awal ditampilkan secara default. Pilih opsi pembersihan (hapus NA, outlier, atau keduanya) dan klik 'Bersihkan Data'."),
                    p("- Tabel menampilkan data bersih setelah pembersihan. Unduh data sebagai XLS, CSV, atau SAV."),
                    h4("2. Manajemen Data"),
                    p("- Pilih variabel numerik dan jumlah kategori, lalu klik 'Kategorisasi'."),
                    p("- Variabel faktor baru akan tersedia untuk uji berbasis grup. Unduh data sebagai XLS, CSV, atau SAV."),
                    h4("3. Eksplorasi Data"),
                    p("- Pilih variabel untuk statistik deskriptif dan visualisasi (histogram, boxplot, peta ditampilkan bersamaan)."),
                    p("- Lihat tabel, statistik, plot, dan interpretasi. Unduh statistik sebagai PDF, plot sebagai PDF atau PNG."),
                    h4("4. Uji Asumsi"),
                    p("- Uji normalitas atau homogenitas. Untuk homogenitas, buat variabel faktor di 'Manajemen Data'."),
                    p("- Hasil, interpretasi, dan tabel dapat diunduh sebagai PDF."),
                    h4("5. Uji Beda Rata-rata"),
                    p("- Lakukan uji t satu atau dua sampel. Untuk dua sampel, pilih variabel faktor."),
                    p("- Hasil, interpretasi, dan tabel dapat diunduh sebagai PDF."),
                    h4("6. Uji Proporsi & Varians"),
                    p("- Uji proporsi memerlukan variabel kategorik, uji varians memerlukan variabel faktor."),
                    p("- Buat variabel faktor di 'Manajemen Data' jika tidak tersedia."),
                    p("- Hasil, interpretasi, dan tabel dapat diunduh sebagai PDF."),
                    h4("7. ANOVA"),
                    p("- Lakukan ANOVA satu atau dua arah dengan variabel faktor."),
                    p("- Hasil, interpretasi, dan tabel dapat diunduh sebagai PDF."),
                    h4("8. Regresi Linear Berganda"),
                    p("- Pilih variabel dependen dan independen (bisa dikosongkan)."),
                    p("- Hasil, uji asumsi, interpretasi, dan tabel dapat diunduh sebagai PDF."),
                    h4("Catatan"),
                    p("- Semua analisis menggunakan data sesuai pilihan di 'Persiapan Data' (asli atau bersih)."),
                    p("- Tunggu indikator pemuatan selesai saat memproses analisis."),
                    p("- Pastikan variabel sesuai untuk setiap analisis."),
                    p("- Buat variabel faktor di 'Manajemen Data' untuk uji berbasis grup.")
                )
              )
      ),
      
      # Tab 2: Persiapan Data
      tabItem(tabName = "data_preparation",
              fluidRow(
                box(width = 12, title = "Persiapan Data", status = "info", solidHeader = TRUE,
                    radioButtons("clean_option", "Opsi Pembersihan",
                                 choiceNames = list(
                                   HTML("Gunakan Data Asli: Tidak melakukan pembersihan, menggunakan data mentah seperti aslinya."),
                                   HTML("Hapus NA: Menghapus semua baris yang mengandung nilai NA pada variabel numerik untuk memastikan data lengkap."),
                                   HTML("Hapus Outlier: Menghapus nilai ekstrem berdasarkan metode IQR (1.5 * IQR) untuk mengurangi dampak anomali."),
                                   HTML("Hapus NA dan Outlier: Menggabungkan penghapusan NA dan outlier untuk data yang lebih bersih dan konsisten.")
                                 ),
                                 choiceValues = c("raw", "remove_na", "remove_outlier", "both"),
                                 selected = "raw"),
                    actionButton("clean_btn", "Pilih Data"),
                    textOutput("clean_error"),
                    div(class = "data-table", DTOutput("cleaned_data_table")),
                    selectInput("download_format_clean", "Pilih Format Download",
                                choices = c("Excel (XLS)" = "xlsx", "CSV" = "csv", "SPSS (SAV)" = "sav")),
                    downloadButton("download_cleaned_data", "Download Data")
                )
              )
      ),
      
      # Tab 3: Manajemen Data
      tabItem(tabName = "data_management",
              fluidRow(
                box(width = 12, title = "Kategorisasi Data Kontinu", status = "info", solidHeader = TRUE,
                    selectInput("var_to_categorize", "Pilih Variabel", choices = NULL),
                    numericInput("n_bins", "Jumlah Kategori", value = 3, min = 2, max = 10),
                    actionButton("categorize_btn", "Kategorisasi"),
                    textOutput("categorize_error"),
                    div(class = "interpretation-box", textOutput("categorize_interpretation")),
                    div(class = "data-table", DTOutput("categorized_data_table")),
                    verbatimTextOutput("categorize_output"),
                    selectInput("download_format_cat", "Pilih Format Download",
                                choices = c("Excel (XLS)" = "xlsx", "CSV" = "csv", "SPSS (SAV)" = "sav")),
                    downloadButton("download_categorized", "Download Data Kategorisasi")
                )
              )
      ),
      
      # Tab 4: Eksplorasi Data
      tabItem(tabName = "data_exploration",
              fluidRow(
                box(width = 12, title = "Statistik Deskriptif", status = "info", solidHeader = TRUE,
                    selectInput("var_desc", "Pilih Variabel", choices = NULL),
                    verbatimTextOutput("desc_stats"),
                    div(class = "data-table", DTOutput("desc_data_table")),
                    downloadButton("download_desc_stats", "Download Statistik Deskriptif (PDF)")
                ),
                box(width = 12, title = "Visualisasi Data", status = "info", solidHeader = TRUE,
                    selectInput("var_plot", "Pilih Variabel", choices = NULL),
                    div(class = "plot-container",
                        div(class = "plot-item", plotlyOutput("histogram_plot", height = "400px")),
                        div(class = "plot-item", plotlyOutput("boxplot_plot", height = "400px"))
                    ),
                    plotlyOutput("map_plot", height = "500px"),
                    selectInput("download_format_plot", "Pilih Format Download Plot",
                                choices = c("PDF" = "pdf", "PNG" = "png")),
                    downloadButton("download_plot", "Download Plot")
                ),
                box(width = 12, title = "Interpretasi Otomatis", status = "warning", solidHeader = TRUE,
                    div(class = "interpretation-box", textOutput("exploration_interpretation"))
                )
              )
      ),
      
      # Tab 5: Uji Asumsi
      tabItem(tabName = "assumption_testing",
              fluidRow(
                box(width = 6, title = "Uji Normalitas", status = "info", solidHeader = TRUE,
                    selectInput("var_norm", "Pilih Variabel", choices = NULL),
                    verbatimTextOutput("normality_test"),
                    downloadButton("download_normality", "Download Hasil Uji Normalitas (PDF)")
                ),
                box(width = 6, title = "Uji Homogenitas", status = "info", solidHeader = TRUE,
                    selectInput("var_homog", "Pilih Variabel", choices = NULL),
                    selectInput("group_homog", "Pilih Grup", choices = c("Tidak Ada" = ""), selected = NULL),
                    conditionalPanel(
                      condition = "input.group_homog == ''",
                      div(class = "warning-box", 
                          HTML("<strong>Perhatian:</strong> Uji homogenitas memerlukan variabel faktor untuk membandingkan varians antar grup.<br>
                               <strong>Solusi:</strong> Buat variabel faktor di tab 'Manajemen Data' dengan mengkategorikan variabel numerik, 
                               kemudian pilih variabel faktor tersebut pada dropdown 'Pilih Grup' di atas."))
                    ),
                    verbatimTextOutput("homogeneity_test"),
                    textOutput("homogeneity_error"),
                    downloadButton("download_homogeneity", "Download Hasil Uji Homogenitas (PDF)")
                ),
                box(width = 12, title = "Interpretasi Otomatis", status = "warning", solidHeader = TRUE,
                    div(class = "interpretation-box", textOutput("assumption_interpretation"))
                )
              )
      ),
      
      # Tab 6: Uji Beda Rata-rata
      tabItem(tabName = "mean_tests",
              fluidRow(
                box(width = 6, title = "Uji T Satu Sampel", status = "info", solidHeader = TRUE,
                    selectInput("var_t1", "Pilih Variabel", choices = NULL),
                    numericInput("mu_t1", "Nilai Hipotesis (mu)", value = 0),
                    verbatimTextOutput("t_test_one"),
                    downloadButton("download_t_test_one", "Download Uji T Satu Sampel (PDF)")
                ),
                box(width = 6, title = "Uji T Dua Sampel", status = "info", solidHeader = TRUE,
                    selectInput("var_t2", "Pilih Variabel", choices = NULL),
                    selectInput("group_t2", "Pilih Grup", choices = c("Tidak Ada" = ""), selected = NULL),
                    conditionalPanel(
                      condition = "input.group_t2 == ''",
                      div(class = "warning-box", 
                          HTML("<strong>Perhatian:</strong> Uji t dua sampel memerlukan variabel faktor untuk membandingkan rata-rata antar grup.<br>
                               <strong>Solusi:</strong> Buat variabel faktor di tab 'Manajemen Data' dengan mengkategorikan variabel numerik, 
                               kemudian pilih variabel faktor tersebut pada dropdown 'Pilih Grup' di atas."))
                    ),
                    verbatimTextOutput("t_test_two"),
                    textOutput("t_test_two_error"),
                    downloadButton("download_t_test_two", "Download Uji T Dua Sampel (PDF)")
                ),
                box(width = 12, title = "Interpretasi Otomatis", status = "warning", solidHeader = TRUE,
                    div(class = "interpretation-box", textOutput("mean_test_interpretation"))
                )
              )
      ),
      
      # Tab 7: Uji Proporsi & Varians
      tabItem(tabName = "prop_var_tests",
              fluidRow(
                box(width = 6, title = "Uji Proporsi", status = "info", solidHeader = TRUE,
                    selectInput("var_prop", "Pilih Variabel Kategorik", choices = c("Tidak Ada" = ""), selected = NULL),
                    numericInput("p0_prop", "Proporsi Hipotesis (p0)", value = 0.5, min = 0, max = 1),
                    conditionalPanel(
                      condition = "input.var_prop == ''",
                      div(class = "warning-box", 
                          HTML("<strong>Perhatian:</strong> Uji proporsi memerlukan variabel kategorik/faktor.<br>
                               <strong>Solusi:</strong> Buat variabel faktor di tab 'Manajemen Data' dengan mengkategorikan variabel numerik, 
                               kemudian pilih variabel faktor tersebut pada dropdown 'Pilih Variabel Kategorik' di atas."))
                    ),
                    verbatimTextOutput("prop_test"),
                    textOutput("prop_test_error"),
                    downloadButton("download_prop_test", "Download Uji Proporsi (PDF)")
                ),
                box(width = 6, title = "Uji Varians", status = "info", solidHeader = TRUE,
                    selectInput("var_var", "Pilih Variabel", choices = NULL),
                    selectInput("group_var", "Pilih Grup", choices = c("Tidak Ada" = ""), selected = NULL),
                    conditionalPanel(
                      condition = "input.group_var == ''",
                      div(class = "warning-box", 
                          HTML("<strong>Perhatian:</strong> Uji varians memerlukan variabel faktor untuk membandingkan varians antar grup.<br>
                               <strong>Solusi:</strong> Buat variabel faktor di tab 'Manajemen Data' dengan mengkategorikan variabel numerik, 
                               kemudian pilih variabel faktor tersebut pada dropdown 'Pilih Grup' di atas."))
                    ),
                    verbatimTextOutput("var_test"),
                    textOutput("var_test_error"),
                    downloadButton("download_var_test", "Download Uji Varians (PDF)")
                ),
                box(width = 12, title = "Interpretasi Otomatis", status = "warning", solidHeader = TRUE,
                    div(class = "interpretation-box", textOutput("prop_var_interpretation"))
                )
              )
      ),
      
      # Tab 8: ANOVA
      tabItem(tabName = "anova",
              fluidRow(
                box(width = 6, title = "ANOVA Satu Arah", status = "info", solidHeader = TRUE,
                    selectInput("var_anova1", "Pilih Variabel", choices = NULL),
                    selectInput("group_anova1", "Pilih Grup", choices = c("Tidak Ada" = ""), selected = NULL),
                    verbatimTextOutput("anova_one"),
                    textOutput("anova_one_error"),
                    downloadButton("download_anova_one", "Download ANOVA Satu Arah (PDF)")
                ),
                box(width = 6, title = "ANOVA Dua Arah", status = "info", solidHeader = TRUE,
                    selectInput("var_anova2", "Pilih Variabel", choices = NULL),
                    selectInput("group1_anova2", "Pilih Grup 1", choices = c("Tidak Ada" = ""), selected = NULL),
                    selectInput("group2_anova2", "Pilih Grup 2", choices = c("Tidak Ada" = ""), selected = NULL),
                    verbatimTextOutput("anova_two"),
                    textOutput("anova_two_error"),
                    downloadButton("download_anova_two", "Download ANOVA Dua Arah (PDF)")
                ),
                box(width = 12, title = "Interpretasi Otomatis", status = "warning", solidHeader = TRUE,
                    div(class = "interpretation-box", textOutput("anova_interpretation"))
                )
              )
      ),
      
      # Tab 9: Regresi Linear Berganda
      tabItem(tabName = "regression",
              fluidRow(
                box(width = 6, title = "Regresi Linear Berganda", status = "info", solidHeader = TRUE,
                    selectInput("dep_var", "Variabel Dependen", choices = NULL),
                    selectizeInput("indep_vars", "Variabel Independen", choices = NULL, multiple = TRUE, 
                                   options = list(placeholder = "Pilih variabel (kosongkan untuk batal)", allowEmptyOption = TRUE)),
                    conditionalPanel(
                      condition = "!input.indep_vars || input.indep_vars.length == 0",
                      div(class = "warning-box", 
                          HTML("<strong>Perhatian:</strong> Regresi linear berganda memerlukan setidaknya satu variabel independen.<br>
                               <strong>Petunjuk:</strong> Pilih satu atau lebih variabel numerik sebagai prediktor dari dropdown 'Variabel Independen' di atas. 
                               Variabel yang dipilih harus berbeda dari variabel dependen untuk menganalisis hubungan kausal."))
                    ),
                    verbatimTextOutput("regression_output"),
                    textOutput("regression_error"),
                    downloadButton("download_regression", "Download Hasil Regresi (PDF)")
                ),
                box(width = 6, title = "Uji Asumsi Regresi", status = "info", solidHeader = TRUE,
                    verbatimTextOutput("regression_assumptions"),
                    downloadButton("download_reg_assumptions", "Download Uji Asumsi Regresi (PDF)")
                ),
                box(width = 12, title = "Interpretasi Otomatis", status = "warning", solidHeader = TRUE,
                    div(class = "interpretation-box", textOutput("regression_interpretation"))
                )
              )
      )
    )
  )
)

# Definisikan logika server
server <- function(input, output, session) {
  # Nilai reaktif untuk data yang dibersihkan dan status pembersihan
  cleaned_data <- reactiveVal(sovi_data)
  clean_status <- reactiveVal("raw")
  
  # Tampilkan data awal secara default
  output$cleaned_data_table <- renderDT({
    datatable(cleaned_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Perbarui pilihan variabel numerik dan faktor secara dinamis
  observe({
    num_vars <- names(cleaned_data())[sapply(cleaned_data(), is.numeric)]
    factor_vars <- names(cleaned_data())[sapply(cleaned_data(), is.factor)]
    if (length(factor_vars) == 0) {
      factor_vars <- c("Tidak Ada" = "")
    }
    
    updateSelectInput(session, "var_to_categorize", choices = num_vars, selected = num_vars[1])
    updateSelectInput(session, "var_desc", choices = num_vars, selected = num_vars[1])
    updateSelectInput(session, "var_plot", choices = num_vars, selected = num_vars[1])
    updateSelectInput(session, "var_norm", choices = num_vars, selected = num_vars[1])
    updateSelectInput(session, "var_homog", choices = num_vars, selected = num_vars[1])
    updateSelectInput(session, "var_t1", choices = num_vars, selected = num_vars[1])
    updateSelectInput(session, "var_t2", choices = num_vars, selected = num_vars[1])
    updateSelectInput(session, "var_var", choices = num_vars, selected = num_vars[1])
    updateSelectInput(session, "var_anova1", choices = num_vars, selected = num_vars[1])
    updateSelectInput(session, "var_anova2", choices = num_vars, selected = num_vars[1])
    updateSelectInput(session, "dep_var", choices = num_vars, selected = num_vars[1])
    updateSelectizeInput(session, "indep_vars", choices = setdiff(num_vars, input$dep_var), selected = NULL)
    
    updateSelectInput(session, "group_homog", choices = factor_vars, selected = NULL)
    updateSelectInput(session, "group_t2", choices = factor_vars, selected = NULL)
    updateSelectInput(session, "var_prop", choices = factor_vars, selected = NULL)
    updateSelectInput(session, "group_var", choices = factor_vars, selected = NULL)
    updateSelectInput(session, "group_anova1", choices = factor_vars, selected = NULL)
    updateSelectInput(session, "group1_anova2", choices = factor_vars, selected = NULL)
    updateSelectInput(session, "group2_anova2", choices = factor_vars, selected = NULL)
  })
  
  # Fungsi untuk mendapatkan status pembersihan sebagai teks
  get_clean_status_text <- reactive({
    switch(clean_status(),
           "raw" = "Data asli digunakan tanpa pembersihan.",
           "remove_na" = "Data telah dibersihkan dengan menghapus baris yang mengandung nilai NA pada variabel numerik.",
           "remove_outlier" = "Data telah dibersihkan dengan menghapus nilai ekstrem berdasarkan metode IQR (1.5 * IQR).",
           "both" = "Data telah dibersihkan dengan menghapus baris NA dan nilai ekstrem berdasarkan metode IQR (1.5 * IQR).")
  })
  
  # Persiapan Data
  observeEvent(input$clean_btn, {
    withProgress(message = 'Memproses pembersihan data...', value = 0, {
      tryCatch({
        data <- sovi_data
        incProgress(0.3)
        if (input$clean_option == "remove_na") {
          data <- na.omit(data)
        } else if (input$clean_option == "remove_outlier") {
          num_cols <- names(data)[sapply(data, is.numeric)]
          for (col in num_cols) {
            if (sum(!is.na(data[[col]])) > 1) { # Pastikan cukup data non-NA
              iqr <- IQR(data[[col]], na.rm = TRUE)
              if (iqr == 0) next # Lewati jika IQR 0
              lower <- quantile(data[[col]], 0.25, na.rm = TRUE) - 1.5 * iqr
              upper <- quantile(data[[col]], 0.75, na.rm = TRUE) + 1.5 * iqr
              data <- data[data[[col]] >= lower & data[[col]] <= upper | is.na(data[[col]]), ]
            }
          }
        } else if (input$clean_option == "both") {
          data <- na.omit(data)
          num_cols <- names(data)[sapply(data, is.numeric)]
          for (col in num_cols) {
            if (sum(!is.na(data[[col]])) > 1) {
              iqr <- IQR(data[[col]], na.rm = TRUE)
              if (iqr == 0) next
              lower <- quantile(data[[col]], 0.25, na.rm = TRUE) - 1.5 * iqr
              upper <- quantile(data[[col]], 0.75, na.rm = TRUE) + 1.5 * iqr
              data <- data[data[[col]] >= lower & data[[col]] <= upper | is.na(data[[col]]), ]
            }
          }
        }
        incProgress(0.7)
        if (nrow(data) == 0) stop("Data kosong setelah pembersihan. Coba opsi lain.")
        cleaned_data(data)
        clean_status(input$clean_option)
        output$clean_error <- renderText("")
        output$cleaned_data_table <- renderDT({
          datatable(cleaned_data(), options = list(pageLength = 10, scrollX = TRUE))
        })
        incProgress(1)
      }, error = function(e) {
        output$clean_error <- renderText(paste("Error saat pembersihan data:", e$message))
      })
    })
  })
  
  # Download data bersih
  output$download_cleaned_data <- downloadHandler(
    filename = function() {
      paste("cleaned_data", input$download_format_clean, sep = ".")
    },
    content = function(file) {
      data <- cleaned_data()
      if (input$download_format_clean == "csv") {
        write.csv(data, file, row.names = FALSE)
      } else if (input$download_format_clean == "xlsx") {
        write_xlsx(data, file)
      } else if (input$download_format_clean == "sav") {
        write_sav(data, file)
      }
    }
  )
  
  # Manajemen Data: Kategorisasi
  observeEvent(input$categorize_btn, {
    withProgress(message = 'Memproses kategorisasi data...', value = 0, {
      tryCatch({
        var <- input$var_to_categorize
        n_bins <- input$n_bins
        data <- cleaned_data()[[var]]
        incProgress(0.3)
        if (all(is.na(data)) || length(unique(na.omit(data))) < 2) {
          stop("Variabel tidak valid untuk kategorisasi (hanya NA atau kurang dari 2 nilai unik).")
        }
        breaks <- seq(min(data, na.rm = TRUE), max(data, na.rm = TRUE), length.out = n_bins + 1)
        new_data <- cut(data, breaks = breaks, include.lowest = TRUE, labels = paste("Kategori", 1:n_bins))
        new_col_name <- paste0(var, "_kategori")
        new_df <- cleaned_data() %>% mutate(!!new_col_name := as.factor(new_data))
        cleaned_data(new_df)
        incProgress(0.7)
        
        output$categorize_output <- renderPrint({
          summary(new_df[[new_col_name]])
        })
        
        output$categorized_data_table <- renderDT({
          datatable(new_df, options = list(pageLength = 10, scrollX = TRUE))
        })
        
        output$categorize_interpretation <- renderText({
          stats <- summary(new_df[[new_col_name]])
          paste(
            get_clean_status_text(),
            "Variabel", var, "telah dikategorikan menjadi", n_bins, "kategori berdasarkan rentang nilai dari",
            round(min(data, na.rm = TRUE), 2), "hingga", round(max(data, na.rm = TRUE), 2), ".",
            "Distribusi kategori menunjukkan:", paste(names(stats), ":", stats, collapse = "; "), ".",
            "Kategorisasi ini memungkinkan analisis berbasis grup untuk uji statistik seperti uji t dua sampel, ANOVA, atau uji varians.",
            "Jika distribusi kategori tidak merata, pertimbangkan untuk menyesuaikan jumlah kategori atau menggunakan metode pembersihan lain di tab 'Persiapan Data'."
          )
        })
        
        output$categorize_error <- renderText("")
        incProgress(1)
      }, error = function(e) {
        output$categorize_error <- renderText(paste("Error saat kategorisasi:", e$message))
      })
    })
  })
  
  # Download data kategorisasi
  output$download_categorized <- downloadHandler(
    filename = function() {
      paste("categorized_data", input$download_format_cat, sep = ".")
    },
    content = function(file) {
      data <- cleaned_data()
      if (input$download_format_cat == "csv") {
        write.csv(data, file, row.names = FALSE)
      } else if (input$download_format_cat == "xlsx") {
        write_xlsx(data, file)
      } else if (input$download_format_cat == "sav") {
        write_sav(data, file)
      }
    }
  )
  
  # Eksplorasi Data: Statistik Deskriptif
  output$desc_stats <- renderPrint({
    summary(cleaned_data()[[input$var_desc]])
  })
  
  output$desc_data_table <- renderDT({
    datatable(cleaned_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$exploration_interpretation <- renderText({
    stats <- summary(cleaned_data()[[input$var_desc]])
    mean_val <- mean(cleaned_data()[[input$var_desc]], na.rm = TRUE)
    sd_val <- sd(cleaned_data()[[input$var_desc]], na.rm = TRUE)
    paste(
      get_clean_status_text(),
      "Statistik deskriptif untuk variabel", input$var_desc, "menunjukkan rentang nilai dari",
      round(stats[1], 2), "hingga", round(stats[6], 2), "dengan rata-rata", round(mean_val, 2),
      "dan simpangan baku", round(sd_val, 2), ".",
      "Median adalah", round(stats[3], 2), ", menunjukkan pusat distribusi data.",
      ifelse(mean_val > stats[3], "Rata-rata lebih besar dari median, mengindikasikan kemungkinan distribusi miring ke kanan.",
             ifelse(mean_val < stats[3], "Rata-rata lebih kecil dari median, mengindikasikan kemungkinan distribusi miring ke kiri.",
                    "Rata-rata sama dengan median, mengindikasikan distribusi simetris.")),
      "Histogram menunjukkan distribusi frekuensi nilai, boxplot menyoroti outlier dan rentang interkuartil, dan peta menampilkan pola spasial.",
      "Jika distribusi tidak normal atau terdapat outlier signifikan, pertimbangkan transformasi data atau uji non-parametrik di tab 'Uji Asumsi'."
    )
  })
  
  output$download_desc_stats <- downloadHandler(
    filename = "descriptive_stats.pdf",
    content = function(file) {
      temp_file <- tempfile(fileext = ".Rmd")
      writeLines(sprintf("
        ---
        title: 'Statistik Deskriptif'
        output: pdf_document
        ---
        ```{r, echo=FALSE, results='asis'}
        library(knitr)
        kable(summary(cleaned_data()[[%s]]))
        ```
      ", shQuote(input$var_desc)), temp_file)
      rmarkdown::render(temp_file, output_file = file, envir = new.env())
    }
  )
  
  # Eksplorasi Data: Visualisasi
  output$histogram_plot <- renderPlotly({
    var <- input$var_plot
    p <- ggplot(cleaned_data(), aes_string(x = var)) + 
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      labs(title = paste("Histogram dari", var))
    ggplotly(p)
  })
  
  output$boxplot_plot <- renderPlotly({
    var <- input$var_plot
    p <- ggplot(cleaned_data(), aes_string(y = var)) + 
      geom_boxplot(fill = "skyblue") +
      labs(title = paste("Boxplot dari", var))
    ggplotly(p)
  })
  
  output$map_plot <- renderPlotly({
    var <- input$var_plot
    if(nrow(shp_merged) == 0) {
      # Jika shapefile tidak tersedia, tampilkan pesan
      plot_ly() %>%
        add_text(x = 0.5, y = 0.5, text = "Peta tidak tersedia\n(Shapefile tidak ditemukan)", 
                 textfont = list(size = 16, color = "red")) %>%
        layout(title = paste("Peta distribusi dari", var),
               xaxis = list(visible = FALSE),
               yaxis = list(visible = FALSE))
    } else {
      # Buat peta dengan nama daerah saat hover
      p <- ggplot(shp_merged) + 
        geom_sf(aes(fill = .data[[var]], 
                    text = paste("Daerah:", ifelse(!is.na(NAMOBJ), NAMOBJ, "Tidak diketahui"),
                                 "<br>Kode:", kodekab,
                                 "<br>", var, ":", round(.data[[var]], 2)))) +
        scale_fill_gradientn(colors = c("blue", "lightblue", "yellow", "orange", "red"), na.value = "grey") +
        labs(title = paste("Peta distribusi dari", var),
             fill = var) +
        theme_minimal() +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank())
      ggplotly(p, tooltip = "text")
    }
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("plot_", input$var_plot, "_", Sys.Date(), ".", input$download_format_plot, sep = "")
    },
    content = function(file) {
      var <- input$var_plot
      
      # Buat plot histogram
      histogram_plot <- ggplot(cleaned_data(), aes_string(x = var)) + 
        geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
        labs(title = paste("Histogram dari", var),
             subtitle = paste("Data:", get_clean_status_text()),
             x = var, y = "Frekuensi") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
      
      # Buat plot boxplot
      boxplot_plot <- ggplot(cleaned_data(), aes_string(y = var)) + 
        geom_boxplot(fill = "lightgreen", color = "darkgreen", alpha = 0.7) +
        labs(title = paste("Boxplot dari", var),
             subtitle = paste("Deteksi outlier dan distribusi kuartil"),
             y = var) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
      
      # Buat plot peta
      if(nrow(shp_merged) > 0) {
        map_plot <- ggplot(shp_merged) + 
          geom_sf(aes(fill = .data[[var]]), color = "white", size = 0.1) +
          scale_fill_gradientn(colors = c("blue", "lightblue", "yellow", "orange", "red"), 
                               na.value = "grey90",
                               name = var) +
          labs(title = paste("Peta Distribusi Spasial", var),
               subtitle = "Social Vulnerability Index - Kabupaten/Kota Indonesia") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                plot.subtitle = element_text(hjust = 0.5, size = 10),
                legend.position = "bottom")
      } else {
        map_plot <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Peta tidak tersedia\n(Shapefile tidak ditemukan)", 
                   size = 12, color = "red") +
          labs(title = paste("Peta dari", var)) +
          theme_void()
      }
      
      if (input$download_format_plot == "pdf") {
        pdf(file, width = 12, height = 16)
        print(histogram_plot)
        print(boxplot_plot)
        print(map_plot)
        dev.off()
      } else if (input$download_format_plot == "png") {
        png(file, width = 1200, height = 1600, res = 150)
        combined_plot <- grid.arrange(histogram_plot, boxplot_plot, map_plot, 
                                      ncol = 1, heights = c(1, 1, 1.2))
        print(combined_plot)
        dev.off()
      }
    }
  )
  
  # Uji Asumsi: Normalitas
  output$normality_test <- renderPrint({
    tryCatch({
      shapiro.test(cleaned_data()[[input$var_norm]])
    }, error = function(e) {
      output$assumption_interpretation <- renderText(paste("Error pada uji normalitas:", e$message))
      return(NULL)
    })
  })
  
  output$assumption_interpretation <- renderText({
    tryCatch({
      norm_result <- shapiro.test(cleaned_data()[[input$var_norm]])
      p_val <- norm_result$p.value
      stats <- summary(cleaned_data()[[input$var_norm]])
      mean_val <- mean(cleaned_data()[[input$var_norm]], na.rm = TRUE)
      sd_val <- sd(cleaned_data()[[input$var_norm]], na.rm = TRUE)
      paste(
        get_clean_status_text(),
        "Uji normalitas (Shapiro-Wilk) untuk variabel", input$var_norm, "menghasilkan p-value =", round(p_val, 4), ".",
        ifelse(p_val > 0.05, 
               paste("Karena p-value > 0.05, data dianggap berdistribusi normal. Ini mendukung penggunaan uji parametrik seperti uji t atau ANOVA."),
               paste("Karena p-value <= 0.05, data tidak berdistribusi normal. Pertimbangkan uji non-parametrik seperti Mann-Whitney atau Kruskal-Wallis.")),
        "Statistik deskriptif menunjukkan rata-rata", round(mean_val, 2), ", simpangan baku", round(sd_val, 2), 
        ", dan rentang dari", round(stats[1], 2), "hingga", round(stats[6], 2), ".",
        "Jika normalitas tidak terpenuhi, transformasi data (misalnya, log atau akar kuadrat) dapat dipertimbangkan, atau gunakan tab 'Eksplorasi Data' untuk memeriksa distribusi lebih lanjut."
      )
    }, error = function(e) {
      paste("Error pada uji normalitas:", e$message)
    })
  })
  
  output$download_normality <- downloadHandler(
    filename = "normality_test.pdf",
    content = function(file) {
      temp_file <- tempfile(fileext = ".Rmd")
      writeLines(sprintf("
        ---
        title: 'Hasil Uji Normalitas'
        output: pdf_document
        ---
        ```{r, echo=FALSE}
        shapiro.test(cleaned_data()[[%s]])
        ```
      ", shQuote(input$var_norm)), temp_file)
      rmarkdown::render(temp_file, output_file = file, envir = new.env())
    }
  )
  
  # Uji Asumsi: Homogenitas
  output$homogeneity_test <- renderPrint({
    req(input$group_homog != "")
    tryCatch({
      group_data <- as.factor(cleaned_data()[[input$group_homog]])
      if (nlevels(group_data) < 2) stop("Grup harus memiliki setidaknya dua level")
      leveneTest(cleaned_data()[[input$var_homog]] ~ group_data)
    }, error = function(e) {
      output$homogeneity_error <- renderText(paste("Error pada uji homogenitas:", e$message))
      return(NULL)
    })
  })
  
  output$homogeneity_error <- renderText("")
  
  output$assumption_interpretation <- renderText({
    req(input$group_homog != "")
    tryCatch({
      group_data <- as.factor(cleaned_data()[[input$group_homog]])
      if (nlevels(group_data) < 2) stop("Grup harus memiliki setidaknya dua level")
      homog_result <- leveneTest(cleaned_data()[[input$var_homog]] ~ group_data)
      p_val <- homog_result$`Pr(>F)`[1]
      stats <- summary(cleaned_data()[[input$var_homog]])
      mean_val <- mean(cleaned_data()[[input$var_homog]], na.rm = TRUE)
      sd_val <- sd(cleaned_data()[[input$var_homog]], na.rm = TRUE)
      paste(
        get_clean_status_text(),
        "Uji homogenitas (Levene) untuk variabel", input$var_homog, "berdasarkan grup", input$group_homog, 
        "menghasilkan p-value =", round(p_val, 4), ".",
        ifelse(p_val > 0.05, 
               paste("Karena p-value > 0.05, varians antar grup dianggap homogen. Ini mendukung penggunaan uji parametrik seperti ANOVA atau uji t dua sampel."),
               paste("Karena p-value <= 0.05, varians antar grup tidak homogen. Pertimbangkan uji non-parametrik atau transformasi data.")),
        "Statistik deskriptif menunjukkan rata-rata", round(mean_val, 2), ", simpangan baku", round(sd_val, 2), 
        ", dan rentang dari", round(stats[1], 2), "hingga", round(stats[6], 2), ".",
        "Jika homogenitas tidak terpenuhi, pertimbangkan uji Welch ANOVA atau transformasi data di tab 'Eksplorasi Data'."
      )
    }, error = function(e) {
      paste("Error pada uji homogenitas:", e$message)
    })
  })
  
  output$download_homogeneity <- downloadHandler(
    filename = "homogeneity_test.pdf",
    content = function(file) {
      req(input$group_homog != "")
      temp_file <- tempfile(fileext = ".Rmd")
      writeLines(sprintf("
        ---
        title: 'Hasil Uji Homogenitas'
        output: pdf_document
        ---
        ```{r, echo=FALSE}
        library(car)
        leveneTest(cleaned_data()[[%s]] ~ as.factor(cleaned_data()[[%s]]))
        ```
      ", shQuote(input$var_homog), shQuote(input$group_homog)), temp_file)
      rmarkdown::render(temp_file, output_file = file, envir = new.env())
    }
  )
  
  # Uji Beda Rata-rata: T-test Satu Sampel
  output$t_test_one <- renderPrint({
    tryCatch({
      t.test(cleaned_data()[[input$var_t1]], mu = input$mu_t1)
    }, error = function(e) {
      output$mean_test_interpretation <- renderText(paste("Error pada uji t satu sampel:", e$message))
      return(NULL)
    })
  })
  
  output$mean_test_interpretation <- renderText({
    tryCatch({
      t_result <- t.test(cleaned_data()[[input$var_t1]], mu = input$mu_t1)
      p_val <- t_result$p.value
      mean_val <- mean(cleaned_data()[[input$var_t1]], na.rm = TRUE)
      sd_val <- sd(cleaned_data()[[input$var_t1]], na.rm = TRUE)
      stats <- summary(cleaned_data()[[input$var_t1]])
      paste(
        get_clean_status_text(),
        "Uji t satu sampel untuk variabel", input$var_t1, "terhadap hipotesis rata-rata", input$mu_t1, 
        "menghasilkan p-value =", round(p_val, 4), ".",
        ifelse(p_val > 0.05, 
               paste("Karena p-value > 0.05, tidak ada bukti signifikan bahwa rata-rata berbeda dari", input$mu_t1, "."),
               paste("Karena p-value <= 0.05, rata-rata berbeda secara signifikan dari", input$mu_t1, ".")),
        "Rata-rata sampel adalah", round(mean_val, 2), "dengan simpangan baku", round(sd_val, 2), 
        "dan rentang dari", round(stats[1], 2), "hingga", round(stats[6], 2), ".",
        "Hasil ini menunjukkan", ifelse(p_val <= 0.05, "adanya perbedaan signifikan yang dapat memengaruhi interpretasi kerentanan sosial.",
                                        "tidak adanya perbedaan signifikan, sehingga hipotesis nol diterima."),
        "Lanjutkan dengan memeriksa normalitas data di tab 'Uji Asumsi' untuk memvalidasi asumsi uji t."
      )
    }, error = function(e) {
      paste("Error pada uji t satu sampel:", e$message)
    })
  })
  
  output$download_t_test_one <- downloadHandler(
    filename = "t_test_one.pdf",
    content = function(file) {
      temp_file <- tempfile(fileext = ".Rmd")
      writeLines(sprintf("
        ---
        title: 'Hasil Uji T Satu Sampel'
        output: pdf_document
        ---
        ```{r, echo=FALSE}
        t.test(cleaned_data()[[%s]], mu = %s)
        ```
      ", shQuote(input$var_t1), input$mu_t1), temp_file)
      rmarkdown::render(temp_file, output_file = file, envir = new.env())
    }
  )
  
  # Uji Beda Rata-rata: T-test Dua Sampel
  output$t_test_two <- renderPrint({
    req(input$group_t2 != "")
    tryCatch({
      group_data <- as.factor(cleaned_data()[[input$group_t2]])
      if (nlevels(group_data) < 2) stop("Grup harus memiliki setidaknya dua level")
      t.test(cleaned_data()[[input$var_t2]] ~ group_data)
    }, error = function(e) {
      output$t_test_two_error <- renderText(paste("Error pada uji t dua sampel:", e$message))
      return(NULL)
    })
  })
  
  output$t_test_two_error <- renderText("")
  
  output$mean_test_interpretation <- renderText({
    # Interpretasi untuk uji t satu sampel (selalu tersedia)
    t_one_interpretation <- tryCatch({
      t_result <- t.test(cleaned_data()[[input$var_t1]], mu = input$mu_t1)
      p_val <- t_result$p.value
      mean_val <- mean(cleaned_data()[[input$var_t1]], na.rm = TRUE)
      sd_val <- sd(cleaned_data()[[input$var_t1]], na.rm = TRUE)
      stats <- summary(cleaned_data()[[input$var_t1]])
      paste(
        "INTERPRETASI UJI T SATU SAMPEL:\n",
        get_clean_status_text(),
        "Uji t satu sampel untuk variabel", input$var_t1, "terhadap hipotesis rata-rata", input$mu_t1, 
        "menghasilkan p-value =", round(p_val, 4), ".",
        ifelse(p_val > 0.05, 
               paste("Karena p-value > 0.05, tidak ada bukti signifikan bahwa rata-rata berbeda dari", input$mu_t1, "."),
               paste("Karena p-value <= 0.05, rata-rata berbeda secara signifikan dari", input$mu_t1, ".")),
        "Rata-rata sampel adalah", round(mean_val, 2), "dengan simpangan baku", round(sd_val, 2), 
        "dan rentang dari", round(stats[1], 2), "hingga", round(stats[6], 2), ".",
        "Hasil ini menunjukkan", ifelse(p_val <= 0.05, "adanya perbedaan signifikan yang dapat memengaruhi interpretasi kerentanan sosial.",
                                        "tidak adanya perbedaan signifikan, sehingga hipotesis nol diterima."),
        "Lanjutkan dengan memeriksa normalitas data di tab 'Uji Asumsi' untuk memvalidasi asumsi uji t."
      )
    }, error = function(e) {
      paste("INTERPRETASI UJI T SATU SAMPEL: Error -", e$message)
    })
    
    # Interpretasi untuk uji t dua sampel (jika tersedia)
    t_two_interpretation <- if(input$group_t2 != "") {
      tryCatch({
        group_data <- as.factor(cleaned_data()[[input$group_t2]])
        if (nlevels(group_data) < 2) stop("Grup harus memiliki setidaknya dua level")
        t_result <- t.test(cleaned_data()[[input$var_t2]] ~ group_data)
        p_val <- t_result$p.value
        group_means <- tapply(cleaned_data()[[input$var_t2]], group_data, mean, na.rm = TRUE)
        group_sd <- tapply(cleaned_data()[[input$var_t2]], group_data, sd, na.rm = TRUE)
        stats <- summary(cleaned_data()[[input$var_t2]])
        paste(
          "\n\nINTERPRETASI UJI T DUA SAMPEL:\n",
          "Uji t dua sampel untuk variabel", input$var_t2, "berdasarkan grup", input$group_t2, 
          "menghasilkan p-value =", round(p_val, 4), ".",
          ifelse(p_val > 0.05, 
                 paste("Karena p-value > 0.05, tidak ada bukti signifikan bahwa rata-rata berbeda antar grup", input$group_t2, "."),
                 paste("Karena p-value <= 0.05, rata-rata berbeda secara signifikan antar grup", input$group_t2, ".")),
          "Rata-rata grup:", paste(names(group_means), "=", round(group_means, 2), collapse = "; "), ".",
          "Simpangan baku grup:", paste(names(group_sd), "=", round(group_sd, 2), collapse = "; "), ".",
          "Rentang data keseluruhan dari", round(stats[1], 2), "hingga", round(stats[6], 2), ".",
          "Hasil ini menunjukkan", ifelse(p_val <= 0.05, 
                                          "adanya perbedaan signifikan antar grup yang dapat digunakan untuk analisis kerentanan sosial berbasis grup.",
                                          "tidak adanya perbedaan signifikan, sehingga grup mungkin memiliki karakteristik serupa."),
          "Pastikan homogenitas varians dengan uji Levene di tab 'Uji Asumsi' sebelum menginterpretasikan hasil ini."
        )
      }, error = function(e) {
        paste("\n\nINTERPRETASI UJI T DUA SAMPEL: Error -", e$message)
      })
    } else {
      "\n\nINTERPRETASI UJI T DUA SAMPEL: Belum tersedia - Pilih variabel faktor di dropdown 'Pilih Grup' untuk uji t dua sampel."
    }
    
    paste(t_one_interpretation, t_two_interpretation)
  })
  
  output$download_t_test_two <- downloadHandler(
    filename = "t_test_two.pdf",
    content = function(file) {
      req(input$group_t2 != "")
      temp_file <- tempfile(fileext = ".Rmd")
      writeLines(sprintf("
        ---
        title: 'Hasil Uji T Dua Sampel'
        output: pdf_document
        ---
        ```{r, echo=FALSE}
        t.test(cleaned_data()[[%s]] ~ as.factor(cleaned_data()[[%s]]))
        ```
      ", shQuote(input$var_t2), shQuote(input$group_t2)), temp_file)
      rmarkdown::render(temp_file, output_file = file, envir = new.env())
    }
  )
  
  # Uji Proporsi
  output$prop_test <- renderPrint({
    req(input$var_prop != "")
    tryCatch({
      tab <- table(cleaned_data()[[input$var_prop]])
      if (length(tab) < 2) stop("Variabel harus memiliki setidaknya dua level")
      prop.test(tab[1], sum(tab), p = input$p0_prop)
    }, error = function(e) {
      output$prop_test_error <- renderText(paste("Error pada uji proporsi:", e$message))
      return(NULL)
    })
  })
  
  output$prop_test_error <- renderText("")
  
  output$prop_var_interpretation <- renderText({
    # Interpretasi untuk uji proporsi
    prop_interpretation <- if(input$var_prop != "") {
      tryCatch({
        tab <- table(cleaned_data()[[input$var_prop]])
        if (length(tab) < 2) stop("Variabel harus memiliki setidaknya dua level")
        prop_result <- prop.test(tab[1], sum(tab), p = input$p0_prop)
        p_val <- prop_result$p.value
        prop_observed <- tab[1] / sum(tab)
        paste(
          "INTERPRETASI UJI PROPORSI:\n",
          get_clean_status_text(),
          "Uji proporsi untuk variabel kategorik", input$var_prop, "terhadap proporsi hipotesis", input$p0_prop, 
          "menghasilkan p-value =", round(p_val, 4), ".",
          ifelse(p_val > 0.05, 
                 paste("Karena p-value > 0.05, proporsi tidak berbeda secara signifikan dari", input$p0_prop, "."),
                 paste("Karena p-value <= 0.05, proporsi berbeda secara signifikan dari", input$p0_prop, ".")),
          "Proporsi teramati untuk kategori pertama adalah", round(prop_observed, 2), 
          "dengan distribusi kategori:", paste(names(tab), "=", tab, collapse = "; "), ".",
          "Hasil ini menunjukkan", ifelse(p_val <= 0.05, 
                                          "adanya perbedaan signifikan yang dapat memengaruhi interpretasi distribusi kategorik dalam konteks SoVI.",
                                          "tidak adanya perbedaan signifikan, sehingga proporsi sesuai dengan hipotesis."),
          "Untuk analisis lebih lanjut, pertimbangkan membuat variabel kategorik tambahan di tab 'Manajemen Data' atau memeriksa distribusi di tab 'Eksplorasi Data'."
        )
      }, error = function(e) {
        paste("INTERPRETASI UJI PROPORSI: Error -", e$message)
      })
    } else {
      "INTERPRETASI UJI PROPORSI: Belum tersedia - Pilih variabel kategorik/faktor untuk melakukan uji proporsi. Buat variabel faktor di tab 'Manajemen Data' jika belum tersedia."
    }
    
    # Interpretasi untuk uji varians
    var_interpretation <- if(input$group_var != "") {
      tryCatch({
        group_data <- as.factor(cleaned_data()[[input$group_var]])
        if (nlevels(group_data) < 2) stop("Grup harus memiliki setidaknya dua level")
        var_result <- var.test(cleaned_data()[[input$var_var]] ~ group_data)
        p_val <- var_result$p.value
        group_sd <- tapply(cleaned_data()[[input$var_var]], group_data, sd, na.rm = TRUE)
        stats <- summary(cleaned_data()[[input$var_var]])
        paste(
          "\n\nINTERPRETASI UJI VARIANS:\n",
          "Uji varians untuk variabel", input$var_var, "berdasarkan grup", input$group_var, 
          "menghasilkan p-value =", round(p_val, 4), ".",
          ifelse(p_val > 0.05, 
                 paste("Karena p-value > 0.05, varians antar grup dianggap sama."),
                 paste("Karena p-value <= 0.05, varians antar grup berbeda secara signifikan.")),
          "Simpangan baku grup:", paste(names(group_sd), "=", round(group_sd, 2), collapse = "; "), ".",
          "Rentang data keseluruhan dari", round(stats[1], 2), "hingga", round(stats[6], 2), ".",
          "Hasil ini menunjukkan", ifelse(p_val <= 0.05, 
                                          "adanya perbedaan variabilitas antar grup yang dapat memengaruhi analisis lebih lanjut.",
                                          "tidak adanya perbedaan variabilitas, mendukung asumsi homogenitas untuk uji t dua sampel atau ANOVA."),
          "Periksa distribusi data di tab 'Eksplorasi Data' untuk memahami pola variabilitas lebih lanjut."
        )
      }, error = function(e) {
        paste("\n\nINTERPRETASI UJI VARIANS: Error -", e$message)
      })
    } else {
      "\n\nINTERPRETASI UJI VARIANS: Belum tersedia - Pilih variabel faktor di dropdown 'Pilih Grup' untuk uji varians. Buat variabel faktor di tab 'Manajemen Data' jika belum tersedia."
    }
    
    paste(prop_interpretation, var_interpretation)
  })
  
  output$download_prop_test <- downloadHandler(
    filename = "prop_test.pdf",
    content = function(file) {
      req(input$var_prop != "")
      temp_file <- tempfile(fileext = ".Rmd")
      writeLines(sprintf("
        ---
        title: 'Hasil Uji Proporsi'
        output: pdf_document
        ---
        ```{r, echo=FALSE}
        tab <- table(cleaned_data()[[%s]])
        prop.test(tab[1], sum(tab), p = %s)
        ```
      ", shQuote(input$var_prop), input$p0_prop), temp_file)
      rmarkdown::render(temp_file, output_file = file, envir = new.env())
    }
  )
  
  # Uji Varians
  output$var_test <- renderPrint({
    req(input$group_var != "")
    tryCatch({
      group_data <- as.factor(cleaned_data()[[input$group_var]])
      if (nlevels(group_data) < 2) stop("Grup harus memiliki setidaknya dua level")
      var.test(cleaned_data()[[input$var_var]] ~ group_data)
    }, error = function(e) {
      output$var_test_error <- renderText(paste("Error pada uji varians:", e$message))
      return(NULL)
    })
  })
  
  output$var_test_error <- renderText("")
  
  output$prop_var_interpretation <- renderText({
    req(input$group_var != "")
    tryCatch({
      group_data <- as.factor(cleaned_data()[[input$group_var]])
      if (nlevels(group_data) < 2) stop("Grup harus memiliki setidaknya dua level")
      var_result <- var.test(cleaned_data()[[input$var_var]] ~ group_data)
      p_val <- var_result$p.value
      group_sd <- tapply(cleaned_data()[[input$var_var]], group_data, sd, na.rm = TRUE)
      stats <- summary(cleaned_data()[[input$var_var]])
      paste(
        get_clean_status_text(),
        "Uji varians untuk variabel", input$var_var, "berdasarkan grup", input$group_var, 
        "menghasilkan p-value =", round(p_val, 4), ".",
        ifelse(p_val > 0.05, 
               paste("Karena p-value > 0.05, varians antar grup dianggap sama."),
               paste("Karena p-value <= 0.05, varians antar grup berbeda secara signifikan.")),
        "Simpangan baku grup:", paste(names(group_sd), "=", round(group_sd, 2), collapse = "; "), ".",
        "Rentang data keseluruhan dari", round(stats[1], 2), "hingga", round(stats[6], 2), ".",
        "Hasil ini menunjukkan", ifelse(p_val <= 0.05, 
                                        "adanya perbedaan variabilitas antar grup yang dapat memengaruhi analisis lebih lanjut.",
                                        "tidak adanya perbedaan variabilitas, mendukung asumsi homogenitas untuk uji t dua sampel atau ANOVA."),
        "Periksa distribusi data di tab 'Eksplorasi Data' untuk memahami pola variabilitas lebih lanjut."
      )
    }, error = function(e) {
      paste("Error pada uji varians:", e$message)
    })
  })
  
  output$download_var_test <- downloadHandler(
    filename = "var_test.pdf",
    content = function(file) {
      req(input$group_var != "")
      temp_file <- tempfile(fileext = ".Rmd")
      writeLines(sprintf("
        ---
        title: 'Hasil Uji Varians'
        output: pdf_document
        ---
        ```{r, echo=FALSE}
        var.test(cleaned_data()[[%s]] ~ as.factor(cleaned_data()[[%s]]))
        ```
      ", shQuote(input$var_var), shQuote(input$group_var)), temp_file)
      rmarkdown::render(temp_file, output_file = file, envir = new.env())
    }
  )
  
  # ANOVA Satu Arah
  output$anova_one <- renderPrint({
    req(input$group_anova1 != "")
    tryCatch({
      group_data <- as.factor(cleaned_data()[[input$group_anova1]])
      if (nlevels(group_data) < 2) stop("Grup harus memiliki setidaknya dua level")
      summary(aov(cleaned_data()[[input$var_anova1]] ~ group_data))
    }, error = function(e) {
      output$anova_one_error <- renderText(paste("Error pada ANOVA satu arah:", e$message))
      return(NULL)
    })
  })
  
  output$anova_one_error <- renderText("")
  
  output$anova_interpretation <- renderText({
    req(input$group_anova1 != "")
    tryCatch({
      group_data <- as.factor(cleaned_data()[[input$group_anova1]])
      if (nlevels(group_data) < 2) stop("Grup harus memiliki setidaknya dua level")
      anova_result <- summary(aov(cleaned_data()[[input$var_anova1]] ~ group_data))
      p_val <- anova_result[[1]]$`Pr(>F)`[1]
      group_means <- tapply(cleaned_data()[[input$var_anova1]], group_data, mean, na.rm = TRUE)
      stats <- summary(cleaned_data()[[input$var_anova1]])
      paste(
        get_clean_status_text(),
        "ANOVA satu arah untuk variabel", input$var_anova1, "berdasarkan grup", input$group_anova1, 
        "menghasilkan p-value =", round(p_val, 4), ".",
        ifelse(p_val > 0.05, 
               paste("Karena p-value > 0.05, tidak ada bukti signifikan bahwa rata-rata berbeda antar kelompok", input$group_anova1, "."),
               paste("Karena p-value <= 0.05, rata-rata berbeda secara signifikan antar kelompok", input$group_anova1, ".")),
        "Rata-rata grup:", paste(names(group_means), "=", round(group_means, 2), collapse = "; "), ".",
        "Rentang data keseluruhan dari", round(stats[1], 2), "hingga", round(stats[6], 2), ".",
        "Hasil ini menunjukkan", ifelse(p_val <= 0.05, 
                                        "adanya perbedaan signifikan antar grup yang dapat digunakan untuk analisis lebih lanjut seperti uji post-hoc.",
                                        "tidak adanya perbedaan signifikan, menunjukkan kesamaan karakteristik antar grup."),
        "Pastikan asumsi normalitas dan homogenitas terpenuhi di tab 'Uji Asumsi' sebelum menarik kesimpulan."
      )
    }, error = function(e) {
      paste("Error pada ANOVA satu arah:", e$message)
    })
  })
  
  output$download_anova_one <- downloadHandler(
    filename = "anova_one.pdf",
    content = function(file) {
      req(input$group_anova1 != "")
      temp_file <- tempfile(fileext = ".Rmd")
      writeLines(sprintf("
        ---
        title: 'Hasil ANOVA Satu Arah'
        output: pdf_document
        ---
        ```{r, echo=FALSE}
        summary(aov(cleaned_data()[[%s]] ~ as.factor(cleaned_data()[[%s]])))
        ```
      ", shQuote(input$var_anova1), shQuote(input$group_anova1)), temp_file)
      rmarkdown::render(temp_file, output_file = file, envir = new.env())
    }
  )
  
  # ANOVA Dua Arah
  output$anova_two <- renderPrint({
    req(input$group1_anova2 != "", input$group2_anova2 != "")
    tryCatch({
      group1_data <- as.factor(cleaned_data()[[input$group1_anova2]])
      group2_data <- as.factor(cleaned_data()[[input$group2_anova2]])
      if (nlevels(group1_data) < 2 || nlevels(group2_data) < 2) stop("Setiap grup harus memiliki setidaknya dua level")
      summary(aov(cleaned_data()[[input$var_anova2]] ~ group1_data * group2_data))
    }, error = function(e) {
      output$anova_two_error <- renderText(paste("Error pada ANOVA dua arah:", e$message))
      return(NULL)
    })
  })
  
  output$anova_two_error <- renderText("")
  
  output$anova_interpretation <- renderText({
    req(input$group1_anova2 != "", input$group2_anova2 != "")
    tryCatch({
      group1_data <- as.factor(cleaned_data()[[input$group1_anova2]])
      group2_data <- as.factor(cleaned_data()[[input$group2_anova2]])
      if (nlevels(group1_data) < 2 || nlevels(group2_data) < 2) stop("Setiap grup harus memiliki setidaknya dua level")
      anova_result <- summary(aov(cleaned_data()[[input$var_anova2]] ~ group1_data * group2_data))
      p_val_main1 <- anova_result[[1]]$`Pr(>F)`[1]
      p_val_main2 <- anova_result[[1]]$`Pr(>F)`[2]
      p_val_interaction <- anova_result[[1]]$`Pr(>F)`[3]
      stats <- summary(cleaned_data()[[input$var_anova2]])
      paste(
        get_clean_status_text(),
        "ANOVA dua arah untuk variabel", input$var_anova2, "berdasarkan grup", input$group1_anova2, 
        "dan", input$group2_anova2, "menghasilkan p-value efek utama", input$group1_anova2, "=", round(p_val_main1, 4), 
        ", efek utama", input$group2_anova2, "=", round(p_val_main2, 4), 
        ", dan interaksi =", round(p_val_interaction, 4), ".",
        ifelse(p_val_main1 <= 0.05, paste("Efek utama", input$group1_anova2, "signifikan."), 
               paste("Efek utama", input$group1_anova2, "tidak signifikan.")),
        ifelse(p_val_main2 <= 0.05, paste("Efek utama", input$group2_anova2, "signifikan."), 
               paste("Efek utama", input$group2_anova2, "tidak signifikan.")),
        ifelse(p_val_interaction <= 0.05, 
               paste("Interaksi antara", input$group1_anova2, "dan", input$group2_anova2, "signifikan, menunjukkan efek gabungan yang kompleks."),
               paste("Tidak ada interaksi signifikan antara", input$group1_anova2, "dan", input$group2_anova2, ".")),
        "Rentang data dari", round(stats[1], 2), "hingga", round(stats[6], 2), ".",
        "Hasil ini menunjukkan", ifelse(p_val_main1 <= 0.05 || p_val_main2 <= 0.05 || p_val_interaction <= 0.05, 
                                        "adanya pengaruh signifikan dari salah satu atau kedua faktor, atau interaksi mereka, yang relevan untuk analisis SoVI.",
                                        "tidak adanya pengaruh signifikan dari faktor atau interaksi mereka."),
        "Periksa asumsi normalitas dan homogenitas di tab 'Uji Asumsi', dan pertimbangkan uji post-hoc jika ada efek signifikan."
      )
    }, error = function(e) {
      paste("Error pada ANOVA dua arah:", e$message)
    })
  })
  
  output$download_anova_two <- downloadHandler(
    filename = "anova_two.pdf",
    content = function(file) {
      req(input$group1_anova2 != "", input$group2_anova2 != "")
      temp_file <- tempfile(fileext = ".Rmd")
      writeLines(sprintf("
        ---
        title: 'Hasil ANOVA Dua Arah'
        output: pdf_document
        ---
        ```{r, echo=FALSE}
        summary(aov(cleaned_data()[[%s]] ~ as.factor(cleaned_data()[[%s]]) * as.factor(cleaned_data()[[%s]])))
        ```
      ", shQuote(input$var_anova2), shQuote(input$group1_anova2), shQuote(input$group2_anova2)), temp_file)
      rmarkdown::render(temp_file, output_file = file, envir = new.env())
    }
  )
  
  # Regresi Linear Berganda
  output$regression_output <- renderPrint({
    req(length(input$indep_vars) > 0)
    tryCatch({
      dep_var <- input$dep_var
      indep_vars <- input$indep_vars
      if (dep_var %in% indep_vars) stop("Variabel dependen tidak boleh dipilih sebagai variabel independen")
      formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))
      summary(lm(formula, data = cleaned_data()))
    }, error = function(e) {
      output$regression_error <- renderText(paste("Error pada regresi:", e$message))
      return(NULL)
    })
  })
  
  output$regression_error <- renderText("")
  
  output$regression_assumptions <- renderPrint({
    req(length(input$indep_vars) > 0)
    tryCatch({
      dep_var <- input$dep_var
      indep_vars <- input$indep_vars
      if (dep_var %in% indep_vars) stop("Variabel dependen tidak boleh dipilih sebagai variabel independen")
      formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))
      model <- lm(formula, data = cleaned_data())
      list(
        Normalitas = shapiro.test(residuals(model)),
        Homoskedastisitas = bptest(model)
      )
    }, error = function(e) {
      output$regression_error <- renderText(paste("Error pada uji asumsi regresi:", e$message))
      return(NULL)
    })
  })
  
  output$regression_interpretation <- renderText({
    req(length(input$indep_vars) > 0)
    tryCatch({
      dep_var <- input$dep_var
      indep_vars <- input$indep_vars
      if (dep_var %in% indep_vars) stop("Variabel dependen tidak boleh dipilih sebagai variabel independen")
      formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = "+")))
      model <- lm(formula, data = cleaned_data())
      summary_model <- summary(model)
      p_val <- pf(summary_model$fstatistic[1], summary_model$fstatistic[2], summary_model$fstatistic[3], lower.tail = FALSE)
      r_squared <- summary_model$r.squared
      adj_r_squared <- summary_model$adj.r.squared
      coef_pvals <- summary_model$coefficients[, 4]
      stats <- summary(cleaned_data()[[dep_var]])
      paste(
        get_clean_status_text(),
        "Regresi linear berganda untuk variabel dependen", dep_var, "dengan variabel independen", paste(indep_vars, collapse = ", "), 
        "menghasilkan p-value keseluruhan =", round(p_val, 4), ".",
        ifelse(p_val > 0.05, 
               paste("Karena p-value > 0.05, model tidak signifikan secara statistik."),
               paste("Karena p-value <= 0.05, model signifikan secara statistik.")),
        "R-squared =", round(r_squared, 4), ", menunjukkan bahwa", round(r_squared * 100, 2), 
        "% variabilitas dalam", dep_var, "dapat dijelaskan oleh variabel independen.",
        "Adjusted R-squared =", round(adj_r_squared, 4), ", menyesuaikan untuk jumlah prediktor.",
        "P-value koefisien:", paste(names(coef_pvals)[-1], "=", round(coef_pvals[-1], 4), collapse = "; "), ".",
        "Koefisien signifikan (p <= 0.05) menunjukkan variabel independen yang berpengaruh kuat terhadap", dep_var, ".",
        "Rentang data dependen dari", round(stats[1], 2), "hingga", round(stats[6], 2), ".",
        "Hasil ini menunjukkan", ifelse(p_val <= 0.05, 
                                        "model dapat digunakan untuk memprediksi kerentanan sosial berdasarkan variabel independen yang dipilih.",
                                        "model tidak cukup kuat untuk prediksi, pertimbangkan variabel independen lain."),
        "Periksa asumsi regresi (normalitas residual dan homoskedastisitas) di hasil uji asumsi untuk memvalidasi model."
      )
    }, error = function(e) {
      paste("Error pada regresi:", e$message)
    })
  })
  
  output$download_regression <- downloadHandler(
    filename = "regression_results.pdf",
    content = function(file) {
      req(length(input$indep_vars) > 0)
      temp_file <- tempfile(fileext = ".Rmd")
      writeLines(sprintf("
        ---
        title: 'Hasil Regresi'
        output: pdf_document
        ---
        ```{r, echo=FALSE}
        summary(lm(%s ~ %s, data = cleaned_data()))
        ```
      ", shQuote(input$dep_var), paste(shQuote(input$indep_vars), collapse = "+")), temp_file)
      rmarkdown::render(temp_file, output_file = file, envir = new.env())
    }
  )
  
  output$download_reg_assumptions <- downloadHandler(
    filename = "regression_assumptions.pdf",
    content = function(file) {
      req(length(input$indep_vars) > 0)
      temp_file <- tempfile(fileext = ".Rmd")
      writeLines(sprintf("
        ---
        title: 'Uji Asumsi Regresi'
        output: pdf_document
        ---
        ```{r, echo=FALSE}
        library(lmtest)
        model <- lm(%s ~ %s, data = cleaned_data())
        list(
          Normalitas = shapiro.test(residuals(model)),
          Homoskedastisitas = bptest(model)
        )
        ```
      ", shQuote(input$dep_var), paste(shQuote(input$indep_vars), collapse = "+")), temp_file)
      rmarkdown::render(temp_file, output_file = file, envir = new.env())
    }
  )
}

# Jalankan aplikasi
shinyApp(ui = ui, server = server)