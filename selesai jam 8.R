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
library(car)    # Untuk uji Levene
library(lmtest) # Untuk uji Breusch-Pagan
library(shinybusy) # Untuk indikator pemuatan
library(writexl)   # Untuk ekspor Excel

# Baca data
sovi_data <- read.csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv") %>%
  mutate(DISTRICTCODE = as.character(DISTRICTCODE)) # Konversi DISTRICTCODE ke karakter
distance_matrix <- read.csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv")
shp <- st_read("C:/Users/ASUS/Downloads/STIS SEMESTER 4/UAS Komstat/Administrasi_Kabupaten.shp") # Sesuaikan path

# Gabungkan shapefile dengan data berdasarkan kodekab dan DISTRICTCODE
shp_merged <- shp %>%
  mutate(kodekab = as.character(kodekab)) %>%
  left_join(sovi_data, by = c("kodekab" = "DISTRICTCODE"))

# Definisikan UI
ui <- dashboardPage(
  # Header dashboard
  dashboardHeader(title = "Dashboard Analisis Data SoVI"),
  
  # Sidebar dengan item menu
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "home", icon = icon("home")),
      menuItem("Pembersihan Data", tabName = "data_cleaning", icon = icon("broom")),
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
    # Tambahkan indikator pemuatan
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f4f6f9; }
        .box { border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); }
        .shiny-download-link { background-color: #007bff; color: white; padding: 8px 16px; border-radius: 4px; }
        .shiny-download-link:hover { background-color: #0056b3; }
        h3 { color: #2c3e50; }
        .sidebar-menu li a { font-size: 16px; }
        .interpretation-box { background-color: #fff3cd; padding: 15px; border-radius: 5px; }
        .data-table { margin-top: 20px; }
        .error-message { color: red; font-weight: bold; }
      "))
    ),
    
    tabItems(
      # Tab 1: Beranda
      tabItem(tabName = "home",
              fluidRow(
                box(width = 12, title = "Selamat Datang di Dashboard SoVI", status = "primary", solidHeader = TRUE,
                    h3("Deskripsi Dashboard"),
                    p("Dashboard ini dirancang untuk menganalisis data Social Vulnerability Index (SoVI) dari ", 
                      a("ScienceDirect", href = "https://www.sciencedirect.com/science/article/pii/S2352340921010180"), 
                      ". Fitur meliputi pembersihan data, manajemen data, eksplorasi data, uji asumsi statistik, uji inferensial (uji beda rata-rata, proporsi, varians, ANOVA), dan regresi linear berganda. Semua hasil, plot, peta, dan tabel dapat diunduh sebagai PDF, CSV, Excel, atau teks dipisahkan titik koma."),
                    h3("Fitur Utama"),
                    p("- **Pembersihan Data**: Menghapus NA dan outlier, menampilkan data awal atau bersih."),
                    p("- **Manajemen Data**: Mengkategorikan data kontinu menjadi faktor untuk analisis grup."),
                    p("- **Eksplorasi Data**: Statistik deskriptif, histogram, boxplot, peta, dan tabel data."),
                    p("- **Uji Asumsi**: Uji normalitas (Shapiro-Wilk) dan homogenitas (Levene)."),
                    p("- **Uji Statistik**: Uji t, proporsi, varians, dan ANOVA."),
                    p("- **Regresi**: Regresi linear berganda dengan opsi pembatalan variabel independen."),
                    p("- **Interaktif**: Output interaktif dengan interpretasi otomatis."),
                    p("- **Download**: Unduh data (CSV, Excel, teks) dan hasil analisis (PDF)."),
                    h3("Petunjuk Penggunaan"),
                    p("Gunakan menu di sisi kiri untuk navigasi. Berikut panduan setiap menu:"),
                    h4("1. Pembersihan Data"),
                    p("- Data awal ditampilkan secara default. Pilih opsi pembersihan (hapus NA, outlier, atau keduanya) dan klik 'Bersihkan Data'."),
                    p("- Tabel menampilkan data bersih setelah pembersihan. Unduh data sebagai PDF, CSV, Excel, atau teks."),
                    h4("2. Manajemen Data"),
                    p("- Pilih variabel numerik dan jumlah kategori, lalu klik 'Kategorisasi'."),
                    p("- Variabel faktor baru akan tersedia untuk uji berbasis grup. Unduh data sebagai PDF, CSV, Excel, atau teks."),
                    h4("3. Eksplorasi Data"),
                    p("- Pilih variabel untuk statistik deskriptif atau visualisasi (histogram, boxplot, peta)."),
                    p("- Lihat tabel, statistik, plot, dan interpretasi. Unduh sebagai PDF."),
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
                    p("- Tunggu indikator pemuatan selesai saat memproses analisis."),
                    p("- Pastikan variabel sesuai untuk setiap analisis."),
                    p("- Buat variabel faktor di 'Manajemen Data' untuk uji berbasis grup.")
                )
              )
      ),
      
      # Tab 2: Pembersihan Data
      tabItem(tabName = "data_cleaning",
              fluidRow(
                box(width = 6, title = "Pembersihan Data", status = "info", solidHeader = TRUE,
                    radioButtons("clean_option", "Opsi Pembersihan",
                                 choices = c("Gunakan Data Asli" = "raw",
                                             "Hapus NA" = "remove_na",
                                             "Hapus Outlier" = "remove_outlier",
                                             "Hapus NA dan Outlier" = "both")),
                    actionButton("clean_btn", "Bersihkan Data"),
                    textOutput("clean_error"),
                    DTOutput("cleaned_data_table"),
                    selectInput("download_format_clean", "Pilih Format Download",
                                choices = c("PDF" = "pdf", "CSV" = "csv", "Excel" = "xlsx", "Teks (Titik Koma)" = "txt")),
                    downloadButton("download_cleaned_data", "Download Data Bersih")
                ),
                box(width = 6, title = "Penjelasan Pembersihan", status = "warning", solidHeader = TRUE,
                    div(class = "interpretation-box",
                        p("**Langkah Pembersihan**:"),
                        p("- **Hapus NA**: Menghapus baris dengan nilai NA pada variabel numerik."),
                        p("- **Hapus Outlier**: Menghapus nilai ekstrem berdasarkan metode IQR (1.5 * IQR)."),
                        p("- **Hapus NA dan Outlier**: Menggabungkan kedua langkah di atas."),
                        p("Data awal ditampilkan secara default. Setelah pembersihan, tabel menunjukkan data bersih. Unduh data dalam format PDF, CSV, Excel, atau teks.")
                    )
                )
              )
      ),
      
      # Tab 3: Manajemen Data
      tabItem(tabName = "data_management",
              fluidRow(
                box(width = 6, title = "Kategorisasi Data Kontinu", status = "info", solidHeader = TRUE,
                    selectInput("var_to_categorize", "Pilih Variabel", choices = NULL),
                    numericInput("n_bins", "Jumlah Kategori", value = 3, min = 2, max = 10),
                    actionButton("categorize_btn", "Kategorisasi"),
                    textOutput("categorize_error"),
                    DTOutput("categorized_data_table"),
                    verbatimTextOutput("categorize_output"),
                    selectInput("download_format_cat", "Pilih Format Download",
                                choices = c("PDF" = "pdf", "CSV" = "csv", "Excel" = "xlsx", "Teks (Titik Koma)" = "txt")),
                    downloadButton("download_categorized", "Download Data Kategorisasi")
                ),
                box(width = 6, title = "Interpretasi Otomatis", status = "warning", solidHeader = TRUE,
                    div(class = "interpretation-box", textOutput("categorize_interpretation"))
                )
              )
      ),
      
      # Tab 4: Eksplorasi Data
      tabItem(tabName = "data_exploration",
              fluidRow(
                box(width = 6, title = "Statistik Deskriptif", status = "info", solidHeader = TRUE,
                    selectInput("var_desc", "Pilih Variabel", choices = NULL),
                    verbatimTextOutput("desc_stats"),
                    DTOutput("desc_data_table"),
                    downloadButton("download_desc_stats", "Download Statistik Deskriptif (PDF)")
                ),
                box(width = 6, title = "Visualisasi Data", status = "info", solidHeader = TRUE,
                    selectInput("var_plot", "Pilih Variabel", choices = NULL),
                    radioButtons("plot_type", "Tipe Plot", choices = c("Histogram", "Boxplot", "Peta")),
                    plotlyOutput("data_plot"),
                    downloadButton("download_plot", "Download Plot (PDF)")
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
                    verbatimTextOutput("prop_test"),
                    textOutput("prop_test_error"),
                    downloadButton("download_prop_test", "Download Uji Proporsi (PDF)")
                ),
                box(width = 6, title = "Uji Varians", status = "info", solidHeader = TRUE,
                    selectInput("var_var", "Pilih Variabel", choices = NULL),
                    selectInput("group_var", "Pilih Grup", choices = c("Tidak Ada" = ""), selected = NULL),
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
  # Nilai reaktif untuk data yang dibersihkan dan dikategorikan
  cleaned_data <- reactiveVal(sovi_data)
  
  # Tampilkan data awal secara default
  output$cleaned_data_table <- renderDT({
    datatable(cleaned_data(), options = list(pageLength = 10, autoWidth = TRUE))
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
  
  # Pembersihan Data
  observeEvent(input$clean_btn, {
    showSpinner()
    tryCatch({
      data <- sovi_data
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
      if (nrow(data) == 0) stop("Data kosong setelah pembersihan. Coba opsi lain.")
      cleaned_data(data)
      output$clean_error <- renderText("")
      output$cleaned_data_table <- renderDT({
        datatable(cleaned_data(), options = list(pageLength = 10, autoWidth = TRUE))
      })
    }, error = function(e) {
      output$clean_error <- renderText(paste("Error saat pembersihan data:", e$message))
    })
    hideSpinner()
  })
  
  # Download data bersih
  output$download_cleaned_data <- downloadHandler(
    filename = function() {
      paste("cleaned_data", input$download_format_clean, sep = ".")
    },
    content = function(file) {
      data <- cleaned_data()
      if (input$download_format_clean == "pdf") {
        temp_file <- tempfile(fileext = ".Rmd")
        writeLines("
          ---
          title: 'Data Bersih'
          output: pdf_document
          ---
          ```{r, echo=FALSE, results='asis'}
          library(knitr)
          kable(cleaned_data())
          ```
        ", temp_file)
        rmarkdown::render(temp_file, output_file = file, envir = new.env())
      } else if (input$download_format_clean == "csv") {
        write.csv(data, file, row.names = FALSE)
      } else if (input$download_format_clean == "xlsx") {
        write_xlsx(data, file)
      } else if (input$download_format_clean == "txt") {
        write.table(data, file, sep = ";", row.names = FALSE, quote = FALSE)
      }
    }
  )
  
  # Manajemen Data: Kategorisasi
  observeEvent(input$categorize_btn, {
    showSpinner()
    tryCatch({
      var <- input$var_to_categorize
      n_bins <- input$n_bins
      data <- cleaned_data()[[var]]
      if (all(is.na(data)) || length(unique(na.omit(data))) < 2) {
        stop("Variabel tidak valid untuk kategorisasi (hanya NA atau kurang dari 2 nilai unik).")
      }
      breaks <- seq(min(data, na.rm = TRUE), max(data, na.rm = TRUE), length.out = n_bins + 1)
      new_data <- cut(data, breaks = breaks, include.lowest = TRUE, labels = paste("Kategori", 1:n_bins))
      new_col_name <- paste0(var, "_kategori")
      new_df <- cleaned_data() %>% mutate(!!new_col_name := as.factor(new_data))
      cleaned_data(new_df)
      
      output$categorize_output <- renderPrint({
        summary(new_df[[new_col_name]])
      })
      
      output$categorized_data_table <- renderDT({
        datatable(new_df, options = list(pageLength = 10, autoWidth = TRUE))
      })
      
      output$categorize_interpretation <- renderText({
        paste("Variabel", var, "telah dikategorikan menjadi", n_bins, 
              "kategori berdasarkan rentang nilai. Distribusi kategori menunjukkan pola data secara kualitatif.")
      })
      
      output$categorize_error <- renderText("")
    }, error = function(e) {
      output$categorize_error <- renderText(paste("Error saat kategorisasi:", e$message))
    })
    hideSpinner()
  })
  
  # Download data kategorisasi
  output$download_categorized <- downloadHandler(
    filename = function() {
      paste("categorized_data", input$download_format_cat, sep = ".")
    },
    content = function(file) {
      data <- cleaned_data()
      if (input$download_format_cat == "pdf") {
        temp_file <- tempfile(fileext = ".Rmd")
        writeLines("
          ---
          title: 'Data Kategorisasi'
          output: pdf_document
          ---
          ```{r, echo=FALSE, results='asis'}
          library(knitr)
          kable(cleaned_data())
          ```
        ", temp_file)
        rmarkdown::render(temp_file, output_file = file, envir = new.env())
      } else if (input$download_format_cat == "csv") {
        write.csv(data, file, row.names = FALSE)
      } else if (input$download_format_cat == "xlsx") {
        write_xlsx(data, file)
      } else if (input$download_format_cat == "txt") {
        write.table(data, file, sep = ";", row.names = FALSE, quote = FALSE)
      }
    }
  )
  
  # Eksplorasi Data: Statistik Deskriptif
  output$desc_stats <- renderPrint({
    summary(cleaned_data()[[input$var_desc]])
  })
  
  output$desc_data_table <- renderDT({
    datatable(cleaned_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  output$exploration_interpretation <- renderText({
    stats <- summary(cleaned_data()[[input$var_desc]])
    paste("Statistik deskriptif untuk", input$var_desc, "menunjukkan rentang nilai dari", stats[1], 
          "hingga", stats[6], "dengan rata-rata", stats[4], ". Visualisasi", input$plot_type, 
          "membantu memahami distribusi data.")
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
  output$data_plot <- renderPlotly({
    var <- input$var_plot
    if (input$plot_type == "Histogram") {
      p <- ggplot(cleaned_data(), aes_string(x = var)) + 
        geom_histogram(bins = 30, fill = "skyblue", color = "black") +
        labs(title = paste("Histogram dari", var))
      ggplotly(p)
    } else if (input$plot_type == "Boxplot") {
      p <- ggplot(cleaned_data(), aes_string(y = var)) + 
        geom_boxplot(fill = "skyblue") +
        labs(title = paste("Boxplot dari", var))
      ggplotly(p)
    } else {
      p <- ggplot(shp_merged) + 
        geom_sf(aes(fill = .data[[var]])) +
        scale_fill_gradientn(colors = c("blue", "white", "red")) +
        labs(title = paste("Peta dari", var))
      ggplotly(p)
    }
  })
  
  output$download_plot <- downloadHandler(
    filename = "plot.pdf",
    content = function(file) {
      var <- input$var_plot
      if (input$plot_type == "Histogram") {
        p <- ggplot(cleaned_data(), aes_string(x = var)) + 
          geom_histogram(bins = 30, fill = "skyblue", color = "black") +
          labs(title = paste("Histogram dari", var))
      } else if (input$plot_type == "Boxplot") {
        p <- ggplot(cleaned_data(), aes_string(y = var)) + 
          geom_boxplot(fill = "skyblue") +
          labs(title = paste("Boxplot dari", var))
      } else {
        p <- ggplot(shp_merged) + 
          geom_sf(aes(fill = .data[[var]])) +
          scale_fill_gradientn(colors = c("blue", "white", "red")) +
          labs(title = paste("Peta dari", var))
      }
      ggsave(file, plot = p, device = "pdf")
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
      if (p_val > 0.05) {
        paste("Uji normalitas (Shapiro-Wilk) untuk", input$var_norm, "menunjukkan p-value =", 
              round(p_val, 4), "> 0.05, sehingga data dianggap berdistribusi normal.")
      } else {
        paste("Uji normalitas (Shapiro-Wilk) untuk", input$var_norm, "menunjukkan p-value =", 
              round(p_val, 4), "<= 0.05, sehingga data tidak berdistribusi normal.")
      }
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
      if (p_val > 0.05) {
        paste("Uji t satu sampel untuk", input$var_t1, "menunjukkan p-value =", round(p_val, 4), 
              "> 0.05, sehingga tidak ada bukti signifikan bahwa rata-rata berbeda dari", input$mu_t1, ".")
      } else {
        paste("Uji t satu sampel untuk", input$var_t1, "menunjukkan p-value =", round(p_val, 4), 
              "<= 0.05, sehingga rata-rata berbeda secara signifikan dari", input$mu_t1, ".")
      }
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
    req(input$var_prop != "")
    tryCatch({
      tab <- table(cleaned_data()[[input$var_prop]])
      if (length(tab) < 2) stop("Variabel harus memiliki setidaknya dua level")
      prop_result <- prop.test(tab[1], sum(tab), p = input$p0_prop)
      p_val <- prop_result$p.value
      if (p_val > 0.05) {
        paste("Uji proporsi untuk", input$var_prop, "menunjukkan p-value =", round(p_val, 4), 
              "> 0.05, sehingga proporsi tidak berbeda secara signifikan dari", input$p0_prop, ".")
      } else {
        paste("Uji proporsi untuk", input$var_prop, "menunjukkan p-value =", round(p_val, 4), 
              "<= 0.05, sehingga proporsi berbeda secara signifikan dari", input$p0_prop, ".")
      }
    }, error = function(e) {
      paste("Error pada uji proporsi:", e$message)
    })
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
      if (p_val > 0.05) {
        paste("ANOVA satu arah untuk", input$var_anova1, "menunjukkan p-value =", round(p_val, 4), 
              "> 0.05, sehingga tidak ada bukti signifikan bahwa rata-rata berbeda antar kelompok", input$group_anova1, ".")
      } else {
        paste("ANOVA satu arah untuk", input$var_anova1, "menunjukkan p-value =", round(p_val, 4), 
              "<= 0.05, sehingga rata-rata berbeda secara signifikan antar kelompok", input$group_anova1, ".")
      }
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
      if (p_val > 0.05) {
        paste("Regresi linear berganda untuk", dep_var, "dengan variabel independen", paste(indep_vars, collapse = ", "), 
              "menunjukkan p-value =", round(p_val, 4), "> 0.05, sehingga model tidak signifikan secara statistik.")
      } else {
        paste("Regresi linear berganda untuk", dep_var, "dengan variabel independen", paste(indep_vars, collapse = ", "), 
              "menunjukkan p-value =", round(p_val, 4), "<= 0.05, sehingga model signifikan secara statistik.")
      }
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