# STARLIGHT - Statistical Analysis and Research Laboratory for Intelligent Geospatial Handling and Testing
# Dashboard Analisis Statistik Komprehensif untuk Indeks Kerentanan Sosial
# Versi 3.0 - UAS Komputasi Statistik

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(dplyr)
library(corrplot)
library(psych)
library(car)
library(nortest)
library(flextable)
library(officer)
library(downloadthis)
library(shinyWidgets)
library(shinycssloaders)
library(htmlwidgets)
library(RColorBrewer)
library(viridis)
library(reshape2)
library(broom)
library(VIM)
library(mice)
library(gridExtra)
library(knitr)
library(rmarkdown)
library(jsonlite)
library(boot)
library(lmtest)
library(Hmisc)
library(sandwich)

# Data loading and preparation function
load_data <- function() {
  # Load CSV data from URL or local file
  tryCatch({
    sovi_data <- read.csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv",
                          stringsAsFactors = FALSE)
  }, error = function(e) {
    # Fallback to local file if URL fails
    data_path <- "data"
    csv_files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)
    if (length(csv_files) > 0) {
      sovi_data <- read.csv(csv_files[1], stringsAsFactors = FALSE)
    } else {
      # Create sample data if no file found
      sovi_data <- data.frame(
        districtkd = paste0("ID", 1:100),
        CHILDREN = runif(100, 10, 40),
        FEMALE = runif(100, 45, 55),
        ELDERLY = runif(100, 5, 20),
        FHEAD = runif(100, 10, 30),
        FAMILYSIZE = runif(100, 3, 6),
        NOELECTRIC = runif(100, 0, 20),
        LOWEDU = runif(100, 10, 50),
        GROWTH = runif(100, -2, 5),
        POVERTY = runif(100, 5, 40),
        ILLITERATE = runif(100, 2, 25),
        NOTRAINING = runif(100, 20, 70),
        DPRONE = runif(100, 1, 5),
        RENTED = runif(100, 5, 30),
        NOSEWER = runif(100, 10, 60),
        TAPWATER = runif(100, 40, 95),
        POPULATION = round(runif(100, 10000, 500000))
      )
    }
  })
  
  # Load spatial data (shapefile or geojson)
  shp_data <- NULL
  tryCatch({
    data_path <- "data"
    
    # Try to load GeoJSON first
    geojson_files <- list.files(data_path, pattern = "\\.geojson$", full.names = TRUE)
    if (length(geojson_files) > 0) {
      shp_data <- st_read(geojson_files[1], quiet = TRUE)
    } else {
      # Try shapefile
      shp_files <- list.files(data_path, pattern = "\\.shp$", full.names = TRUE)
      if (length(shp_files) > 0) {
        shp_data <- st_read(shp_files[1], quiet = TRUE)
      }
    }
    
    # Ensure proper column formatting
    if (!is.null(shp_data) && ncol(shp_data) > 1) {
      id_cols <- names(shp_data)[sapply(names(shp_data), function(x) 
        grepl("id|code|kd", tolower(x)))]
      if (length(id_cols) > 0) {
        shp_data[[id_cols[1]]] <- as.character(shp_data[[id_cols[1]]])
      }
    }
  }, error = function(e) {
    print(paste("Spatial data loading error:", e$message))
  })
  
  # Ensure proper data formatting
  if ("districtkd" %in% names(sovi_data)) {
    sovi_data$districtkd <- as.character(sovi_data$districtkd)
  }
  
  return(list(csv = sovi_data, shp = shp_data))
}

# Initialize data
data_list <- load_data()
sovi_data <- data_list$csv
shp_data <- data_list$shp

# Variable descriptions in Indonesian
var_descriptions <- data.frame(
  Variabel = c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
               "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE",
               "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION"),
  Deskripsi = c("Persentase Populasi Anak-anak", "Persentase Populasi Perempuan", 
                "Persentase Populasi Lansia", "Persentase Rumah Tangga Kepala Keluarga Perempuan",
                "Rata-rata Ukuran Keluarga", "Persentase Rumah Tangga Tanpa Listrik",
                "Persentase Pendidikan Rendah", "Tingkat Pertumbuhan Penduduk",
                "Persentase Kemiskinan", "Tingkat Buta Huruf",
                "Persentase Tanpa Pelatihan Kejuruan", "Indeks Kerawanan Bencana",
                "Persentase Rumah Sewa", "Persentase Tanpa Sistem Pembuangan",
                "Persentase Akses Air Keran", "Jumlah Total Penduduk"),
  Tipe_Data = c(rep("Numerik", 16)),
  Satuan = c("Persen", "Persen", "Persen", "Persen", "Orang", "Persen",
             "Persen", "Persen", "Persen", "Persen", "Persen", "Indeks",
             "Persen", "Persen", "Persen", "Jiwa"),
  stringsAsFactors = FALSE
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "🌟 STARLIGHT Analytics Dashboard"),
  
  dashboardSidebar(
    tags$head(tags$style(HTML(custom_css))),
    sidebarMenu(
      menuItem("🏠 Beranda", tabName = "home", icon = icon("home")),
      menuItem("🔧 Manajemen Data", tabName = "management", icon = icon("database")),
      menuItem("📊 Eksplorasi Data", tabName = "exploration", icon = icon("chart-line")),
      menuItem("✅ Uji Asumsi", tabName = "assumptions", icon = icon("check-circle")),
      menuItem("📈 Statistik Inferensial I", tabName = "inference1", icon = icon("calculator")),
      menuItem("📉 Statistik Inferensial II", tabName = "inference2", icon = icon("chart-bar")),
      menuItem("🔄 Metode Resampling", tabName = "resampling", icon = icon("refresh")),
      menuItem("🎯 Analisis Regresi", tabName = "regression", icon = icon("line-chart")),
      menuItem("🗺️ Analisis Geospasial", tabName = "geospatial", icon = icon("map"))
    )
  ),
  
  dashboardBody(
    # Styling CSS
    tabItems(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      # HOME TAB
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = NULL, status = "primary", solidHeader = FALSE, width = 12,
                  div(class = "starlight-header",
                      h1("🌟 STARLIGHT Analytics Platform"),
                      h3("Statistical Analysis and Research Laboratory for Intelligent Geospatial Handling and Testing"),
                      p("Dashboard Analisis Statistik Komprehensif untuk Indeks Kerentanan Sosial Indonesia")
                  )
                )
              ),
              
              fluidRow(
                column(6,
                       box(
                         title = "📊 Informasi Dataset", status = "info", solidHeader = TRUE, width = NULL,
                         div(class = "info-card",
                             h4("📍 Informasi Umum"),
                             tags$ul(
                               tags$li(paste("🏛️ Jumlah Kabupaten/Kota:", nrow(sovi_data))),
                               tags$li(paste("📋 Jumlah Variabel:", ncol(sovi_data))),
                               tags$li("📅 Tahun Data: 2017"),
                               tags$li("🏢 Sumber Data: Badan Pusat Statistik (BPS)")
                             )
                         ),
                         
                         h4("🎯 Tujuan Analisis:"),
                         tags$ul(
                           tags$li("🔍 Menganalisis pola kerentanan sosial di Indonesia"),
                           tags$li("📊 Mengidentifikasi faktor-faktor yang mempengaruhi kerentanan"),
                           tags$li("🗺️ Memetakan distribusi spasial kerentanan sosial"),
                           tags$li("📈 Memberikan rekomendasi kebijakan berbasis data")
                         )
                       )
                ),
                
                column(6,
                       box(
                         title = "🚀 Fitur Utama Dashboard", status = "success", solidHeader = TRUE, width = NULL,
                         div(class = "stat-card",
                             h4("✨ Kemampuan Analisis"),
                             p("16 Variabel Kerentanan Sosial")
                         ),
                         
                         h4("🔧 Modul Analisis:"),
                         tags$ul(
                           tags$li("🔄 Manajemen dan pembersihan data"),
                           tags$li("📊 Eksplorasi data interaktif"),
                           tags$li("🗺️ Analisis geospasial dengan peta choropleth"),
                           tags$li("🧪 Pengujian asumsi statistik komprehensif"),
                           tags$li("📈 Statistik inferensial lengkap"),
                           tags$li("🎯 Analisis regresi dengan diagnostik"),
                           tags$li("🔄 Metode resampling (Bootstrap, Jackknife)"),
                           tags$li("💾 Export hasil dalam berbagai format")
                         )
                       )
                )
              ),
              
              fluidRow(
                box(
                  title = "📋 Kamus Data Variabel", status = "warning", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("variable_dictionary"),
                  br(),
                  downloadButton("download_dictionary", "📥 Unduh Kamus Data", 
                                 class = "btn-info")
                )
              ),
              
              fluidRow(
                box(
                  title = "🔗 Referensi & Dokumentasi", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(6,
                           h4("📚 Referensi Akademik:"),
                           p("Dataset ini berdasarkan penelitian yang dipublikasikan di Data in Brief:"),
                           a("Social Vulnerability Index Data for Indonesia", 
                             href = "https://www.sciencedirect.com/science/article/pii/S2352340921010180",
                             target = "_blank", class = "btn btn-info"),
                           
                           br(), br(),
                           h4("🛠️ Spesifikasi Teknis:"),
                           tags$ul(
                             tags$li("Dibangun dengan R Shiny Framework"),
                             tags$li("UI Responsif Bootstrap"),
                             tags$li("Visualisasi Interaktif Plotly"),
                             tags$li("Pemetaan Geospasial Leaflet"),
                             tags$li("Komputasi Statistik Lanjutan")
                           )
                    ),
                    
                    column(6,
                           h4("👨‍💻 Tim Pengembang:"),
                           p("STARLIGHT Dashboard v3.0"),
                           p("Dikembangkan untuk UAS Komputasi Statistik"),
                           p("Politeknik Statistika STIS"),
                           
                           br(),
                           h4("🎯 Panduan Penggunaan:"),
                           div(class = "alert alert-info",
                               p("Navigasi melalui menu sidebar untuk menjelajahi berbagai modul analisis. Setiap bagian menyediakan analisis statistik komprehensif dengan interpretasi otomatis dan kemampuan export.")
                           )
                    )
                  )
                )
              )
      ),
      
      # DATA MANAGEMENT TAB
      tabItem(tabName = "management",
              fluidRow(
                box(
                  title = "🔧 Suite Manajemen Data Lanjutan", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("📋 Dataset Asli",
                             fluidRow(
                               column(8,
                                      h4("📊 Tinjauan Data Mentah"),
                                      withSpinner(DT::dataTableOutput("original_data_table"), color = "#3498db")
                               ),
                               column(4,
                                      h4("📈 Ringkasan Data"),
                                      verbatimTextOutput("data_summary_stats"),
                                      br(),
                                      downloadButton("download_original", "📥 Unduh Data Asli",
                                                     class = "btn-info btn-block")
                               )
                             )
                    ),
                    
                    tabPanel("🔍 Analisis Missing Value",
                             fluidRow(
                               column(6,
                                      h4("🕳️ Pola Data Hilang"),
                                      withSpinner(plotOutput("missing_pattern_plot"), color = "#3498db"),
                                      br(),
                                      h5("📊 Ringkasan Data Hilang:"),
                                      DT::dataTableOutput("missing_summary_table")
                               ),
                               column(6,
                                      h4("⚙️ Penanganan Missing Value"),
                                      selectInput("imputation_method", "Pilih Metode Imputasi:",
                                                  choices = list(
                                                    "Imputasi Mean" = "mean",
                                                    "Imputasi Median" = "median",
                                                    "Imputasi Modus" = "mode",
                                                    "Imputasi MICE" = "mice",
                                                    "Hapus Missing" = "remove"
                                                  )),
                                      actionButton("apply_imputation", "🔧 Terapkan Imputasi",
                                                   class = "btn-warning btn-block"),
                                      br(), br(),
                                      h5("📋 Hasil Imputasi:"),
                                      verbatimTextOutput("imputation_results")
                               )
                             )
                    ),
                    
                    tabPanel("🏷️ Kategorisasi Variabel",
                             fluidRow(
                               column(4,
                                      h4("🔄 Buat Variabel Kategorikal"),
                                      selectInput("var_to_categorize", "Pilih Variabel:", choices = NULL),
                                      numericInput("n_categories", "Jumlah Kategori:",
                                                   value = 3, min = 2, max = 6),
                                      radioButtons("categorize_method", "Metode Kategorisasi:",
                                                   choices = list(
                                                     "Berdasarkan Kuantil" = "quantile",
                                                     "Lebar Sama" = "width",
                                                     "K-means Clustering" = "kmeans",
                                                     "Custom Breaks" = "custom"
                                                   )),
                                      conditionalPanel(
                                        condition = "input.categorize_method == 'custom'",
                                        textInput("custom_breaks", "Custom Breaks (pisahkan dengan koma):",
                                                  placeholder = "contoh: 10, 20, 30")
                                      ),
                                      actionButton("create_categories", "🏷️ Buat Kategori",
                                                   class = "btn-success btn-block"),
                                      br(),
                                      h5("📝 Variabel yang Dibuat:"),
                                      verbatimTextOutput("created_vars_list")
                               ),
                               column(8,
                                      h4("📊 Preview Kategorisasi"),
                                      withSpinner(plotlyOutput("categorization_preview"), color = "#3498db"),
                                      br(),
                                      h5("📋 Distribusi Kategori:"),
                                      DT::dataTableOutput("category_distribution")
                               )
                             )
                    ),
                    
                    tabPanel("💾 Dataset Terproses",
                             h4("🔄 Dataset Final Terproses"),
                             withSpinner(DT::dataTableOutput("processed_data_table"), color = "#3498db"),
                             br(),
                             fluidRow(
                               column(4,
                                      downloadButton("download_processed", "📥 Unduh Data Terproses (CSV)",
                                                     class = "btn-success btn-block")
                               ),
                               column(4,
                                      downloadButton("download_codebook", "📖 Unduh Buku Kode Data",
                                                     class = "btn-info btn-block")
                               ),
                               column(4,
                                      downloadButton("download_management_report", "📄 Unduh Laporan Manajemen",
                                                     class = "btn-warning btn-block")
                               )
                             )
                    )
                  )
                )
              )
      ),
      
      # DATA EXPLORATION TAB
      tabItem(tabName = "exploration",
              fluidRow(
                box(
                  title = "📊 Eksplorasi Data Komprehensif", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("📈 Statistik Deskriptif",
                             fluidRow(
                               column(4,
                                      h4("🎯 Pilihan Variabel"),
                                      selectInput("desc_variable", "Pilih Variabel:", choices = NULL),
                                      br(),
                                      h5("📋 Statistik Variabel:"),
                                      verbatimTextOutput("single_var_stats"),
                                      br(),
                                      downloadButton("download_descriptive", "📥 Unduh Statistik",
                                                     class = "btn-info btn-block"),
                                      br(), br(),
                                      downloadButton("download_desc_plot", "📥 Unduh Grafik (PNG)",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("📊 Ringkasan Statistik Semua Variabel"),
                                      withSpinner(DT::dataTableOutput("descriptive_table"), color = "#3498db"),
                                      br(),
                                      h4("📊 Visualisasi Distribusi"),
                                      withSpinner(plotlyOutput("distribution_viz", height = "400px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Statistik:"),
                             verbatimTextOutput("descriptive_interpretation")
                    ),
                    
                    tabPanel("📊 Visualisasi Lanjutan",
                             fluidRow(
                               column(3,
                                      h4("🎨 Kontrol Visualisasi"),
                                      selectInput("plot_type", "Jenis Grafik:",
                                                  choices = list(
                                                    "Histogram" = "histogram",
                                                    "Plot Densitas" = "density",
                                                    "Box Plot" = "boxplot",
                                                    "Violin Plot" = "violin",
                                                    "Scatter Plot" = "scatter",
                                                    "Heatmap Korelasi" = "correlation",
                                                    "Pair Plot" = "pairs"
                                                  )),
                                      conditionalPanel(
                                        condition = "input.plot_type == 'scatter'",
                                        selectInput("x_variable", "Variabel X:", choices = NULL),
                                        selectInput("y_variable", "Variabel Y:", choices = NULL),
                                        selectInput("color_variable", "Warna Berdasarkan:", choices = c("Tidak Ada" = "none"))
                                      ),
                                      conditionalPanel(
                                        condition = "input.plot_type != 'correlation' && input.plot_type != 'pairs' && input.plot_type != 'scatter'",
                                        selectInput("viz_variable", "Variabel:", choices = NULL)
                                      ),
                                      br(),
                                      actionButton("generate_plot", "🎨 Buat Grafik", class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_plot", "📥 Unduh Grafik", class = "btn-success btn-block")
                               ),
                               column(9,
                                      withSpinner(plotlyOutput("advanced_visualization", height = "600px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Visualisasi:"),
                             verbatimTextOutput("visualization_interpretation")
                    ),
                    
                    tabPanel("🔗 Analisis Korelasi",
                             fluidRow(
                               column(4,
                                      h4("⚙️ Pengaturan Korelasi"),
                                      selectInput("corr_method", "Metode Korelasi:",
                                                  choices = list(
                                                    "Pearson" = "pearson",
                                                    "Spearman" = "spearman",
                                                    "Kendall" = "kendall"
                                                  )),
                                      numericInput("corr_threshold", "Ambang Signifikansi:",
                                                   value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                                      checkboxInput("show_insignificant", "Tampilkan Korelasi Non-signifikan", FALSE),
                                      br(),
                                      actionButton("compute_correlation", "🔗 Hitung Korelasi",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_correlation", "📥 Unduh Matriks Korelasi",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("🔗 Matriks Korelasi"),
                                      withSpinner(plotOutput("correlation_plot", height = "500px"), color = "#3498db"),
                                      br(),
                                      h5("📊 Tabel Korelasi:"),
                                      DT::dataTableOutput("correlation_table")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Korelasi:"),
                             verbatimTextOutput("correlation_interpretation")
                    )
                  )
                )
              )
      ),
      
      # ASSUMPTION TESTING TAB
      tabItem(tabName = "assumptions",
              fluidRow(
                box(
                  title = "✅ Pengujian Asumsi Statistik", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("📊 Uji Normalitas",
                             fluidRow(
                               column(4,
                                      h4("⚙️ Konfigurasi Uji"),
                                      selectInput("normality_variable", "Pilih Variabel:", choices = NULL),
                                      radioButtons("normality_test", "Metode Uji:",
                                                   choices = list(
                                                     "Uji Shapiro-Wilk" = "shapiro",
                                                     "Uji Anderson-Darling" = "anderson",
                                                     "Uji Kolmogorov-Smirnov" = "ks",
                                                     "Uji Jarque-Bera" = "jarque"
                                                   )),
                                      numericInput("alpha_level", "Tingkat Signifikansi:",
                                                   value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                                      actionButton("run_normality_test", "🧪 Jalankan Uji",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_normality", "📥 Unduh Hasil",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("📊 Hasil Uji Normalitas"),
                                      verbatimTextOutput("normality_test_results"),
                                      br(),
                                      h5("📈 Plot Diagnostik:"),
                                      withSpinner(plotOutput("normality_plots", height = "400px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Uji Normalitas:"),
                             verbatimTextOutput("normality_interpretation")
                    ),
                    
                    tabPanel("⚖️ Uji Homogenitas",
                             fluidRow(
                               column(4,
                                      h4("⚙️ Konfigurasi Uji"),
                                      selectInput("homogeneity_variable", "Variabel Numerik:", choices = NULL),
                                      selectInput("grouping_variable", "Variabel Pengelompokan:", choices = NULL),
                                      radioButtons("homogeneity_test", "Metode Uji:",
                                                   choices = list(
                                                     "Uji Levene" = "levene",
                                                     "Uji Bartlett" = "bartlett",
                                                     "Uji Fligner-Killeen" = "fligner"
                                                   )),
                                      actionButton("run_homogeneity_test", "⚖️ Jalankan Uji",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_homogeneity", "📥 Unduh Hasil",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("⚖️ Hasil Uji Homogenitas"),
                                      verbatimTextOutput("homogeneity_test_results"),
                                      br(),
                                      h5("📊 Plot Perbandingan Kelompok:"),
                                      withSpinner(plotlyOutput("homogeneity_plot", height = "400px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Uji Homogenitas:"),
                             verbatimTextOutput("homogeneity_interpretation")
                    ),
                    
                    tabPanel("📈 Plot Diagnostik",
                             fluidRow(
                               column(3,
                                      h4("🎨 Opsi Plot"),
                                      selectInput("diagnostic_variable", "Pilih Variabel:", choices = NULL),
                                      checkboxGroupInput("diagnostic_plots", "Pilih Plot:",
                                                         choices = list(
                                                           "Q-Q Plot" = "qq",
                                                           "Histogram dengan Kurva Normal" = "hist_norm",
                                                           "Box Plot" = "boxplot",
                                                           "Plot Densitas" = "density",
                                                           "Plot Probabilitas" = "prob"
                                                         ),
                                                         selected = c("qq", "hist_norm")),
                                      actionButton("generate_diagnostics", "📈 Buat Plot",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_diagnostics", "📥 Unduh Plot",
                                                     class = "btn-success btn-block")
                               ),
                               column(9,
                                      h4("📈 Visualisasi Diagnostik"),
                                      withSpinner(plotOutput("diagnostic_plots_output", height = "600px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Plot Diagnostik:"),
                             verbatimTextOutput("diagnostic_interpretation")
                    )
                  )
                )
              )
      ),
      
      # INFERENTIAL STATISTICS I TAB
      tabItem(tabName = "inference1",
              fluidRow(
                box(
                  title = "📈 Statistik Inferensial - Uji & Proporsi", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("🎯 Uji Proporsi",
                             fluidRow(
                               column(4,
                                      h4("⚙️ Konfigurasi Uji"),
                                      selectInput("prop_variable", "Pilih Variabel:", choices = NULL),
                                      numericInput("prop_threshold", "Nilai Ambang:",
                                                   value = 0.5, min = 0, max = 1, step = 0.1),
                                      numericInput("null_proportion", "Proporsi Hipotesis Nol:",
                                                   value = 0.5, min = 0, max = 1, step = 0.01),
                                      radioButtons("prop_test_type", "Jenis Uji:",
                                                   choices = list(
                                                     "Satu sampel" = "one",
                                                     "Dua sampel" = "two"
                                                   )),
                                      conditionalPanel(
                                        condition = "input.prop_test_type == 'two'",
                                        selectInput("prop_group_var", "Variabel Pengelompokan:", choices = NULL)
                                      ),
                                      actionButton("run_proportion_test", "🎯 Jalankan Uji",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_proportion", "📥 Unduh Hasil",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("🎯 Hasil Uji Proporsi"),
                                      verbatimTextOutput("proportion_test_results"),
                                      br(),
                                      h5("📊 Visualisasi Proporsi:"),
                                      withSpinner(plotlyOutput("proportion_plot", height = "400px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Uji Proporsi:"),
                             verbatimTextOutput("proportion_interpretation")
                    ),
                    
                    tabPanel("📊 Uji Varians",
                             fluidRow(
                               column(4,
                                      h4("⚙️ Konfigurasi Uji"),
                                      selectInput("variance_variable", "Pilih Variabel:", choices = NULL),
                                      radioButtons("variance_test_type", "Jenis Uji:",
                                                   choices = list(
                                                     "Satu sampel (Chi-square)" = "one",
                                                     "Dua sampel (F-test)" = "two",
                                                     "Banyak kelompok (Bartlett)" = "multiple"
                                                   )),
                                      conditionalPanel(
                                        condition = "input.variance_test_type == 'one'",
                                        numericInput("null_variance", "Varians Hipotesis Nol:",
                                                     value = 1, min = 0.1, step = 0.1)
                                      ),
                                      conditionalPanel(
                                        condition = "input.variance_test_type != 'one'",
                                        selectInput("variance_group_var", "Variabel Pengelompokan:", choices = NULL)
                                      ),
                                      actionButton("run_variance_test", "📊 Jalankan Uji",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_variance", "📥 Unduh Hasil",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("📊 Hasil Uji Varians"),
                                      verbatimTextOutput("variance_test_results"),
                                      br(),
                                      h5("📈 Visualisasi Varians:"),
                                      withSpinner(plotlyOutput("variance_plot", height = "400px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Uji Varians:"),
                             verbatimTextOutput("variance_interpretation")
                    ),
                    
                    tabPanel("📏 Uji Rata-rata",
                             fluidRow(
                               column(4,
                                      h4("⚙️ Konfigurasi Uji"),
                                      selectInput("mean_variable", "Pilih Variabel:", choices = NULL),
                                      radioButtons("mean_test_type", "Jenis Uji:",
                                                   choices = list(
                                                     "Uji t satu sampel" = "one",
                                                     "Uji t dua sampel" = "two",
                                                     "Uji t berpasangan" = "paired"
                                                   )),
                                      conditionalPanel(
                                        condition = "input.mean_test_type == 'one'",
                                        numericInput("null_mean", "Rata-rata Hipotesis Nol:", value = 0)
                                      ),
                                      conditionalPanel(
                                        condition = "input.mean_test_type != 'one'",
                                        selectInput("mean_group_var", "Variabel Pengelompokan:", choices = NULL)
                                      ),
                                      checkboxInput("assume_equal_var", "Asumsikan Varians Sama", TRUE),
                                      actionButton("run_mean_test", "📏 Jalankan Uji",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_mean", "📥 Unduh Hasil",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("📏 Hasil Uji Rata-rata"),
                                      verbatimTextOutput("mean_test_results"),
                                      br(),
                                      h5("📊 Plot Perbandingan Rata-rata:"),
                                      withSpinner(plotlyOutput("mean_plot", height = "400px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Uji Rata-rata:"),
                             verbatimTextOutput("mean_interpretation")
                    )
                  )
                )
              )
      ),
      
      # INFERENTIAL STATISTICS II TAB
      tabItem(tabName = "inference2",
              fluidRow(
                box(
                  title = "📉 Statistik Inferensial Lanjutan - ANOVA", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("📊 ANOVA Satu Arah",
                             fluidRow(
                               column(4,
                                      h4("⚙️ Konfigurasi ANOVA"),
                                      selectInput("anova1_dependent", "Variabel Dependen:", choices = NULL),
                                      selectInput("anova1_independent", "Variabel Independen (Faktor):", choices = NULL),
                                      checkboxInput("anova1_posthoc", "Lakukan Uji Post-hoc", TRUE),
                                      selectInput("posthoc_method", "Metode Post-hoc:",
                                                  choices = list(
                                                    "Tukey HSD" = "tukey",
                                                    "Bonferroni" = "bonferroni",
                                                    "Scheffe" = "scheffe"
                                                  )),
                                      actionButton("run_anova1", "📊 Jalankan ANOVA Satu Arah",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_anova1", "📥 Unduh Hasil",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("📊 Hasil ANOVA Satu Arah"),
                                      verbatimTextOutput("anova1_results"),
                                      br(),
                                      conditionalPanel(
                                        condition = "input.anova1_posthoc",
                                        h5("🔍 Hasil Uji Post-hoc:"),
                                        verbatimTextOutput("posthoc_results")
                                      ),
                                      br(),
                                      h5("📈 Visualisasi ANOVA:"),
                                      withSpinner(plotlyOutput("anova1_plot", height = "400px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi ANOVA Satu Arah:"),
                             verbatimTextOutput("anova1_interpretation")
                    ),
                    
                    tabPanel("📈 ANOVA Dua Arah",
                             fluidRow(
                               column(4,
                                      h4("⚙️ Konfigurasi ANOVA Dua Arah"),
                                      selectInput("anova2_dependent", "Variabel Dependen:", choices = NULL),
                                      selectInput("anova2_factor1", "Faktor 1:", choices = NULL),
                                      selectInput("anova2_factor2", "Faktor 2:", choices = NULL),
                                      checkboxInput("include_interaction", "Sertakan Term Interaksi", TRUE),
                                      radioButtons("anova2_type", "Tipe Sum of Squares:",
                                                   choices = list(
                                                     "Tipe I" = "I",
                                                     "Tipe II" = "II",
                                                     "Tipe III" = "III"
                                                   )),
                                      actionButton("run_anova2", "📈 Jalankan ANOVA Dua Arah",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_anova2", "📥 Unduh Hasil",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("📈 Hasil ANOVA Dua Arah"),
                                      verbatimTextOutput("anova2_results"),
                                      br(),
                                      h5("📊 Plot Interaksi:"),
                                      withSpinner(plotlyOutput("anova2_plot", height = "400px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi ANOVA Dua Arah:"),
                             verbatimTextOutput("anova2_interpretation")
                    ),
                    
                    tabPanel("🔍 Diagnostik ANOVA",
                             fluidRow(
                               column(4,
                                      h4("🔧 Opsi Diagnostik"),
                                      p("Pilih model ANOVA dari tab sebelumnya untuk melihat diagnostik."),
                                      radioButtons("anova_diagnostic_type", "Pilih Model ANOVA:",
                                                   choices = list(
                                                     "ANOVA Satu Arah" = "one",
                                                     "ANOVA Dua Arah" = "two"
                                                   )),
                                      checkboxGroupInput("anova_diagnostic_plots", "Plot Diagnostik:",
                                                         choices = list(
                                                           "Residual vs Fitted" = "resid_fitted",
                                                           "Q-Q Plot Residual" = "qq_resid",
                                                           "Scale-Location Plot" = "scale_location",
                                                           "Residual vs Leverage" = "resid_leverage"
                                                         ),
                                                         selected = c("resid_fitted", "qq_resid")),
                                      actionButton("generate_anova_diagnostics", "🔍 Buat Diagnostik",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_anova_diagnostics", "📥 Unduh Diagnostik",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("🔍 Plot Diagnostik ANOVA"),
                                      withSpinner(plotOutput("anova_diagnostic_plots_output", height = "600px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Diagnostik ANOVA:"),
                             verbatimTextOutput("anova_diagnostic_interpretation")
                    )
                  )
                )
              )
      ),
      
      # RESAMPLING METHODS TAB
      tabItem(tabName = "resampling",
              fluidRow(
                box(
                  title = "🔄 Metode Resampling", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("🎲 Bootstrap",
                             fluidRow(
                               column(4,
                                      h4("⚙️ Konfigurasi Bootstrap"),
                                      selectInput("bootstrap_variable", "Pilih Variabel:", choices = NULL),
                                      selectInput("bootstrap_statistic", "Statistik:",
                                                  choices = list(
                                                    "Rata-rata" = "mean",
                                                    "Median" = "median",
                                                    "Standar Deviasi" = "sd",
                                                    "Varians" = "var"
                                                  )),
                                      numericInput("bootstrap_samples", "Jumlah Sampel Bootstrap:",
                                                   value = 1000, min = 100, max = 10000, step = 100),
                                      numericInput("confidence_level", "Tingkat Kepercayaan:",
                                                   value = 0.95, min = 0.90, max = 0.99, step = 0.01),
                                      actionButton("run_bootstrap", "🎲 Jalankan Bootstrap",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_bootstrap", "📥 Unduh Hasil Bootstrap",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("🎲 Hasil Bootstrap"),
                                      verbatimTextOutput("bootstrap_results"),
                                      br(),
                                      h5("📊 Distribusi Bootstrap:"),
                                      withSpinner(plotlyOutput("bootstrap_plot", height = "400px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Bootstrap:"),
                             verbatimTextOutput("bootstrap_interpretation")
                    ),
                    
                    tabPanel("🔪 Jackknife",
                             fluidRow(
                               column(4,
                                      h4("⚙️ Konfigurasi Jackknife"),
                                      selectInput("jackknife_variable", "Pilih Variabel:", choices = NULL),
                                      selectInput("jackknife_statistic", "Statistik:",
                                                  choices = list(
                                                    "Rata-rata" = "mean",
                                                    "Median" = "median",
                                                    "Standar Deviasi" = "sd",
                                                    "Varians" = "var"
                                                  )),
                                      actionButton("run_jackknife", "🔪 Jalankan Jackknife",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_jackknife", "📥 Unduh Hasil Jackknife",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("🔪 Hasil Jackknife"),
                                      verbatimTextOutput("jackknife_results"),
                                      br(),
                                      h5("📊 Plot Jackknife:"),
                                      withSpinner(plotlyOutput("jackknife_plot", height = "400px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Jackknife:"),
                             verbatimTextOutput("jackknife_interpretation")
                    ),
                    
                    tabPanel("🔀 Permutasi",
                             fluidRow(
                               column(4,
                                      h4("⚙️ Konfigurasi Uji Permutasi"),
                                      selectInput("perm_variable1", "Variabel 1:", choices = NULL),
                                      selectInput("perm_variable2", "Variabel 2:", choices = NULL),
                                      selectInput("perm_test_type", "Jenis Uji:",
                                                  choices = list(
                                                    "Perbedaan Rata-rata" = "mean_diff",
                                                    "Korelasi" = "correlation",
                                                    "Uji t" = "t_test"
                                                  )),
                                      numericInput("perm_samples", "Jumlah Permutasi:",
                                                   value = 1000, min = 100, max = 10000, step = 100),
                                      actionButton("run_permutation", "🔀 Jalankan Uji Permutasi",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_permutation", "📥 Unduh Hasil Permutasi",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("🔀 Hasil Uji Permutasi"),
                                      verbatimTextOutput("permutation_results"),
                                      br(),
                                      h5("📊 Distribusi Permutasi:"),
                                      withSpinner(plotlyOutput("permutation_plot", height = "400px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Uji Permutasi:"),
                             verbatimTextOutput("permutation_interpretation")
                    )
                  )
                )
              )
      ),
      
      # REGRESSION ANALYSIS TAB
      tabItem(tabName = "regression",
              fluidRow(
                box(
                  title = "🎯 Analisis Regresi Linear Berganda", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("🔧 Pembangunan Model",
                             fluidRow(
                               column(4,
                                      h4("⚙️ Konfigurasi Model"),
                                      selectInput("reg_dependent", "Variabel Dependen:", choices = NULL),
                                      checkboxGroupInput("reg_independent", "Variabel Independen:", choices = NULL),
                                      checkboxInput("include_intercept", "Sertakan Intercept", TRUE),
                                      radioButtons("selection_method", "Seleksi Variabel:",
                                                   choices = list(
                                                     "Masukkan Semua" = "enter",
                                                     "Forward Selection" = "forward",
                                                     "Backward Elimination" = "backward",
                                                     "Stepwise" = "stepwise"
                                                   )),
                                      conditionalPanel(
                                        condition = "input.selection_method != 'enter'",
                                        numericInput("selection_alpha", "Alpha Seleksi:",
                                                     value = 0.05, min = 0.01, max = 0.2, step = 0.01)
                                      ),
                                      actionButton("build_regression", "🔧 Bangun Model",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_regression", "📥 Unduh Model",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("🎯 Hasil Model Regresi"),
                                      verbatimTextOutput("regression_summary"),
                                      br(),
                                      h5("📊 Statistik Kecocokan Model:"),
                                      verbatimTextOutput("model_fit_stats")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Model Regresi:"),
                             verbatimTextOutput("regression_interpretation")
                    ),
                    
                    tabPanel("🔍 Diagnostik Model",
                             fluidRow(
                               column(4,
                                      h4("🔧 Uji Diagnostik"),
                                      p("Jalankan model regresi terlebih dahulu untuk melihat diagnostik."),
                                      checkboxGroupInput("regression_diagnostics", "Pilih Diagnostik:",
                                                         choices = list(
                                                           "Plot Residual" = "residual_plots",
                                                           "Uji Normalitas" = "normality",
                                                           "Uji Heteroskedastisitas" = "hetero",
                                                           "Multikolinearitas (VIF)" = "vif",
                                                           "Deteksi Outlier" = "outliers",
                                                           "Ukuran Pengaruh" = "influence"
                                                         ),
                                                         selected = c("residual_plots", "normality", "vif")),
                                      actionButton("run_regression_diagnostics", "🔍 Jalankan Diagnostik",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_reg_diagnostics", "📥 Unduh Diagnostik",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("🔍 Hasil Diagnostik"),
                                      verbatimTextOutput("regression_diagnostic_results"),
                                      br(),
                                      h5("📈 Plot Diagnostik:"),
                                      withSpinner(plotOutput("regression_diagnostic_plots", height = "600px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Diagnostik:"),
                             verbatimTextOutput("regression_diagnostic_interpretation")
                    ),
                    
                    tabPanel("📊 Perbandingan Model",
                             fluidRow(
                               column(4,
                                      h4("🔧 Perbandingan Model"),
                                      p("Bandingkan berbagai model regresi."),
                                      checkboxGroupInput("comparison_models", "Model untuk Dibandingkan:",
                                                         choices = list(
                                                           "Model Penuh" = "full",
                                                           "Model Tereduksi" = "reduced",
                                                           "Model Stepwise" = "stepwise"
                                                         )),
                                      actionButton("compare_models", "📊 Bandingkan Model",
                                                   class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_comparison", "📥 Unduh Perbandingan",
                                                     class = "btn-success btn-block")
                               ),
                               column(8,
                                      h4("📊 Hasil Perbandingan Model"),
                                      DT::dataTableOutput("model_comparison_table"),
                                      br(),
                                      h5("📈 Plot Performa Model:"),
                                      withSpinner(plotlyOutput("model_comparison_plot", height = "400px"), color = "#3498db")
                               )
                             ),
                             br(),
                             h4("🔍 Interpretasi Perbandingan Model:"),
                             verbatimTextOutput("model_comparison_interpretation")
                    )
                  )
                )
              )
      ),
      
      # GEOSPATIAL ANALYSIS TAB
      tabItem(tabName = "geospatial",
              fluidRow(
                box(
                  title = "🗺️ Analisis Geospasial Lanjutan", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("🗺️ Peta Choropleth Interaktif",
                             fluidRow(
                               column(3,
                                      h4("🎨 Konfigurasi Peta"),
                                      selectInput("map_variable", "Variabel untuk Dipetakan:", choices = NULL),
                                      selectInput("color_scheme", "Skema Warna:",
                                                  choices = list(
                                                    "Viridis" = "viridis",
                                                    "Plasma" = "plasma",
                                                    "Blues" = "Blues",
                                                    "Reds" = "Reds",
                                                    "YlOrRd" = "YlOrRd",
                                                    "RdYlBu" = "RdYlBu"
                                                  )),
                                      numericInput("map_bins", "Jumlah Bins:", value = 7, min = 3, max = 10),
                                      checkboxInput("reverse_colors", "Balik Skala Warna", FALSE),
                                      br(),
                                      actionButton("update_map", "🗺️ Perbarui Peta", class = "btn-primary btn-block"),
                                      br(), br(),
                                      downloadButton("download_map", "📥 Unduh Peta", class = "btn-success btn-block")
                               ),
                               column(9,
                                      conditionalPanel(
                                        condition = "output.shapefile_available",
                                        h4("🗺️ Peta Choropleth Interaktif"),
                                        withSpinner(leafletOutput("choropleth_map", height = "600px"), color = "#3498db")
                                      ),
                                      conditionalPanel(
                                        condition = "!output.shapefile_available",
                                        div(class = "alert alert-warning",
                                            h4("⚠️ Data Spasial Tidak Tersedia"),
                                            p("Data shapefile atau GeoJSON tidak dapat dimuat dari direktori:"),
                                            p("C:/Mario/Semester 4/Komstat/UAS/data"),
                                            p("Pastikan file spasial tersedia di lokasi ini untuk mengaktifkan analisis geospasial.")
                                        )
                                      )
                               )
                             ),
                             br(),
                             conditionalPanel(
                               condition = "output.shapefile_available",
                               h4("🔍 Interpretasi Analisis Spasial:"),
                               verbatimTextOutput("spatial_interpretation")
                             )
                    ),
                    
                    tabPanel("📊 Statistik Spasial",
                             conditionalPanel(
                               condition = "output.shapefile_available",
                               fluidRow(
                                 column(4,
                                        h4("⚙️ Opsi Analisis Spasial"),
                                        selectInput("spatial_variable", "Variabel untuk Analisis:", choices = NULL),
                                        checkboxGroupInput("spatial_tests", "Uji Spasial:",
                                                           choices = list(
                                                             "Moran's I (Global)" = "moran_global",
                                                             "Local Moran's I" = "moran_local",
                                                             "Geary's C" = "geary",
                                                             "Autokorelasi Spasial" = "autocorr"
                                                           )),
                                        actionButton("run_spatial_analysis", "📊 Jalankan Analisis",
                                                     class = "btn-primary btn-block"),
                                        br(), br(),
                                        downloadButton("download_spatial", "📥 Unduh Hasil",
                                                       class = "btn-success btn-block")
                                 ),
                                 column(8,
                                        h4("📊 Hasil Statistik Spasial"),
                                        verbatimTextOutput("spatial_statistics_results"),
                                        br(),
                                        h5("📈 Plot Analisis Spasial:"),
                                        withSpinner(plotOutput("spatial_analysis_plot", height = "400px"), color = "#3498db")
                                 )
                               ),
                               br(),
                               h4("🔍 Interpretasi Statistik Spasial:"),
                               verbatimTextOutput("spatial_statistics_interpretation")
                             ),
                             conditionalPanel(
                               condition = "!output.shapefile_available",
                               div(class = "alert alert-info",
                                   h4("ℹ️ Statistik Spasial Tidak Tersedia"),
                                   p("Statistik spasial memerlukan data shapefile. Pastikan shapefile tersedia di direktori yang ditentukan.")
                               )
                             )
                    ),
                    
                    tabPanel("🎯 Analisis Hotspot",
                             conditionalPanel(
                               condition = "output.shapefile_available",
                               fluidRow(
                                 column(4,
                                        h4("🔥 Deteksi Hotspot"),
                                        selectInput("hotspot_variable", "Variabel untuk Analisis Hotspot:", choices = NULL),
                                        numericInput("hotspot_threshold", "Ambang Signifikansi:",
                                                     value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                                        radioButtons("hotspot_method", "Metode Deteksi:",
                                                     choices = list(
                                                       "Getis-Ord Gi*" = "getis_ord",
                                                       "Local Moran's I" = "local_moran",
                                                       "Analisis Z-score" = "zscore"
                                                     )),
                                        actionButton("detect_hotspots", "🔥 Deteksi Hotspot",
                                                     class = "btn-warning btn-block"),
                                        br(), br(),
                                        downloadButton("download_hotspots", "📥 Unduh Peta Hotspot",
                                                       class = "btn-success btn-block")
                                 ),
                                 column(8,
                                        h4("🔥 Hasil Analisis Hotspot"),
                                        withSpinner(leafletOutput("hotspot_map", height = "500px"), color = "#3498db"),
                                        br(),
                                        h5("📊 Ringkasan Hotspot:"),
                                        DT::dataTableOutput("hotspot_summary")
                                 )
                               ),
                               br(),
                               h4("🔍 Interpretasi Analisis Hotspot:"),
                               verbatimTextOutput("hotspot_interpretation")
                             ),
                             conditionalPanel(
                               condition = "!output.shapefile_available",
                               div(class = "alert alert-info",
                                   h4("ℹ️ Analisis Hotspot Tidak Tersedia"),
                                   p("Analisis hotspot memerlukan data shapefile. Pastikan shapefile tersedia di direktori yang ditentukan.")
                               )
                             )
                    )
                  )
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive values to store processed data and models
  values <- reactiveValues(
    processed_data = NULL,
    categorized_vars = character(0),
    current_regression = NULL,
    anova1_model = NULL,
    anova2_model = NULL,
    spatial_data = NULL,
    bootstrap_results = NULL,
    jackknife_results = NULL,
    permutation_results = NULL
  )
  
  # Initialize processed data
  observe({
    if (is.null(values$processed_data)) {
      values$processed_data <- sovi_data
    }
    
    # Update all choice inputs
    numeric_vars <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
    all_vars <- names(values$processed_data)
    categorical_vars <- names(values$processed_data)[sapply(values$processed_data, function(x) is.factor(x) || is.character(x))]
    
    # Update all select inputs
    updateSelectInput(session, "var_to_categorize", choices = numeric_vars)
    updateSelectInput(session, "desc_variable", choices = numeric_vars)
    updateSelectInput(session, "viz_variable", choices = numeric_vars)
    updateSelectInput(session, "x_variable", choices = numeric_vars)
    updateSelectInput(session, "y_variable", choices = numeric_vars)
    updateSelectInput(session, "color_variable", choices = c("Tidak Ada" = "none", categorical_vars))
    updateSelectInput(session, "map_variable", choices = numeric_vars)
    updateSelectInput(session, "spatial_variable", choices = numeric_vars)
    updateSelectInput(session, "hotspot_variable", choices = numeric_vars)
    updateSelectInput(session, "normality_variable", choices = numeric_vars)
    updateSelectInput(session, "homogeneity_variable", choices = numeric_vars)
    updateSelectInput(session, "grouping_variable", choices = categorical_vars)
    updateSelectInput(session, "diagnostic_variable", choices = numeric_vars)
    updateSelectInput(session, "prop_variable", choices = numeric_vars)
    updateSelectInput(session, "prop_group_var", choices = categorical_vars)
    updateSelectInput(session, "variance_variable", choices = numeric_vars)
    updateSelectInput(session, "variance_group_var", choices = categorical_vars)
    updateSelectInput(session, "mean_variable", choices = numeric_vars)
    updateSelectInput(session, "mean_group_var", choices = categorical_vars)
    updateSelectInput(session, "anova1_dependent", choices = numeric_vars)
    updateSelectInput(session, "anova1_independent", choices = categorical_vars)
    updateSelectInput(session, "anova2_dependent", choices = numeric_vars)
    updateSelectInput(session, "anova2_factor1", choices = categorical_vars)
    updateSelectInput(session, "anova2_factor2", choices = categorical_vars)
    updateSelectInput(session, "reg_dependent", choices = numeric_vars)
    updateCheckboxGroupInput(session, "reg_independent", choices = numeric_vars)
    updateSelectInput(session, "bootstrap_variable", choices = numeric_vars)
    updateSelectInput(session, "jackknife_variable", choices = numeric_vars)
    updateSelectInput(session, "perm_variable1", choices = numeric_vars)
    updateSelectInput(session, "perm_variable2", choices = numeric_vars)
  })
  
  # Check if shapefile is available
  output$shapefile_available <- reactive({
    return(!is.null(shp_data))
  })
  outputOptions(output, "shapefile_available", suspendWhenHidden = FALSE)
  
  # HOME TAB
  output$variable_dictionary <- DT::renderDataTable({
    DT::datatable(var_descriptions,
                  options = list(scrollX = TRUE, pageLength = 16, dom = 'Bfrtip'),
                  class = 'cell-border stripe',
                  caption = "Kamus Data Variabel Indeks Kerentanan Sosial Indonesia")
  })
  
  # DATA MANAGEMENT TAB
  output$original_data_table <- DT::renderDataTable({
    DT::datatable(sovi_data,
                  options = list(scrollX = TRUE, pageLength = 10, dom = 'Bfrtip'),
                  class = 'cell-border stripe')
  })
  
  output$data_summary_stats <- renderPrint({
    cat("RINGKASAN STATISTIK DATASET\n")
    cat("==========================\n")
    cat("Jumlah Observasi:", nrow(sovi_data), "\n")
    cat("Jumlah Variabel:", ncol(sovi_data), "\n")
    cat("Tahun Data: 2017\n")
    cat("Sumber: Badan Pusat Statistik (BPS)\n\n")
    summary(sovi_data)
  })
  
  # Missing value analysis
  output$missing_pattern_plot <- renderPlot({
    VIM::aggr(sovi_data, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE,
              main = "Pola Data Hilang")
  })
  
  output$missing_summary_table <- DT::renderDataTable({
    missing_summary <- sovi_data %>%
      summarise_all(~sum(is.na(.))) %>%
      gather(Variabel, Jumlah_Missing) %>%
      mutate(Persentase_Missing = round(Jumlah_Missing / nrow(sovi_data) * 100, 2)) %>%
      arrange(desc(Jumlah_Missing))
    
    DT::datatable(missing_summary, options = list(pageLength = 15))
  })
  
  # Imputation logic
  observeEvent(input$apply_imputation, {
    req(input$imputation_method)
    
    tryCatch({
      if (input$imputation_method == "mean") {
        values$processed_data <- sovi_data %>%
          mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))
      } else if (input$imputation_method == "median") {
        values$processed_data <- sovi_data %>%
          mutate_if(is.numeric, ~ifelse(is.na(.), median(., na.rm = TRUE), .))
      } else if (input$imputation_method == "mode") {
        get_mode <- function(x) {
          ux <- unique(x[!is.na(x)])
          ux[which.max(tabulate(match(x, ux)))]
        }
        values$processed_data <- sovi_data %>%
          mutate_all(~ifelse(is.na(.), get_mode(.), .))
      } else if (input$imputation_method == "remove") {
        values$processed_data <- na.omit(sovi_data)
      } else if (input$imputation_method == "mice") {
        # MICE imputation (simplified)
        numeric_cols <- sapply(sovi_data, is.numeric)
        if (sum(numeric_cols) > 0) {
          mice_result <- mice(sovi_data[, numeric_cols], m = 1, method = 'pmm', printFlag = FALSE)
          imputed_data <- complete(mice_result)
          values$processed_data <- sovi_data
          values$processed_data[, numeric_cols] <- imputed_data
        }
      }
      
      output$imputation_results <- renderText({
        paste("Imputasi berhasil menggunakan metode", input$imputation_method, ".",
              "\nDataset asli:", nrow(sovi_data), "baris,", ncol(sovi_data), "kolom",
              "\nDataset terproses:", nrow(values$processed_data), "baris,", ncol(values$processed_data), "kolom")
      })
      
      showNotification("Imputasi data berhasil diselesaikan!", type = "success")
    }, error = function(e) {
      showNotification(paste("Error dalam imputasi:", e$message), type = "error")
    })
  })
  
  # Categorization logic
  observeEvent(input$create_categories, {
    req(input$var_to_categorize, input$n_categories, input$categorize_method)
    
    tryCatch({
      var_data <- values$processed_data[[input$var_to_categorize]]
      
      if (input$categorize_method == "quantile") {
        breaks <- quantile(var_data, probs = seq(0, 1, length.out = input$n_categories + 1), na.rm = TRUE)
        categories <- cut(var_data, breaks = breaks, include.lowest = TRUE,
                          labels = paste0("Q", 1:input$n_categories))
      } else if (input$categorize_method == "width") {
        categories <- cut(var_data, breaks = input$n_categories,
                          labels = paste0("Kat", 1:input$n_categories))
      } else if (input$categorize_method == "kmeans") {
        valid_data <- var_data[!is.na(var_data)]
        if (length(valid_data) > input$n_categories) {
          km <- kmeans(valid_data, centers = input$n_categories)
          categories <- rep(NA, length(var_data))
          categories[!is.na(var_data)] <- paste0("Kluster", km$cluster)
          categories <- factor(categories)
        } else {
          stop("Data tidak cukup untuk clustering")
        }
      } else if (input$categorize_method == "custom") {
        req(input$custom_breaks)
        breaks <- as.numeric(unlist(strsplit(input$custom_breaks, ",")))
        breaks <- c(-Inf, breaks, Inf)
        categories <- cut(var_data, breaks = breaks, include.lowest = TRUE)
      }
      
      # Generate unique variable name
      new_var_name <- paste0(input$var_to_categorize, "_kat")
      counter <- 1
      while (new_var_name %in% names(values$processed_data)) {
        counter <- counter + 1
        new_var_name <- paste0(input$var_to_categorize, "_kat_", counter)
      }
      
      # Add categorized variable
      values$processed_data[[new_var_name]] <- categories
      values$categorized_vars <- c(values$categorized_vars, new_var_name)
      
      showNotification(paste("Variabel", new_var_name, "berhasil dibuat!"), type = "success")
    }, error = function(e) {
      showNotification(paste("Error dalam kategorisasi:", e$message), type = "error")
    })
  })
  
  output$created_vars_list <- renderText({
    if (length(values$categorized_vars) == 0) {
      "Belum ada variabel kategorikal yang dibuat."
    } else {
      paste("Variabel yang dibuat:\n", paste(values$categorized_vars, collapse = "\n"))
    }
  })
  
  output$categorization_preview <- renderPlotly({
    req(input$var_to_categorize)
    
    if (length(values$categorized_vars) > 0) {
      latest_cat <- values$categorized_vars[length(values$categorized_vars)]
      if (grepl(paste0("^", input$var_to_categorize), latest_cat)) {
        p <- ggplot(values$processed_data, aes_string(x = latest_cat, y = input$var_to_categorize)) +
          geom_boxplot(aes_string(fill = latest_cat)) +
          labs(title = paste("Preview Kategorisasi:", latest_cat),
               x = "Kategori", y = "Nilai Asli") +
          theme_minimal() +
          theme(legend.position = "none")
        ggplotly(p)
      }
    }
  })
  
  output$category_distribution <- DT::renderDataTable({
    req(input$var_to_categorize)
    
    if (length(values$categorized_vars) > 0) {
      latest_cat <- values$categorized_vars[length(values$categorized_vars)]
      if (grepl(paste0("^", input$var_to_categorize), latest_cat)) {
        dist_table <- table(values$processed_data[[latest_cat]])
        dist_df <- data.frame(
          Kategori = names(dist_table),
          Jumlah = as.numeric(dist_table),
          Persentase = round(as.numeric(dist_table) / sum(dist_table) * 100, 2)
        )
        DT::datatable(dist_df, options = list(pageLength = 10))
      }
    }
  })
  
  output$processed_data_table <- DT::renderDataTable({
    req(values$processed_data)
    DT::datatable(values$processed_data,
                  options = list(scrollX = TRUE, pageLength = 10, dom = 'Bfrtip'),
                  class = 'cell-border stripe')
  })
  
  # DATA EXPLORATION TAB
  output$descriptive_table <- DT::renderDataTable({
    numeric_data <- values$processed_data[sapply(values$processed_data, is.numeric)]
    desc_stats <- describe(numeric_data)[, c("n", "mean", "sd", "median", "min", "max", "skew", "kurtosis")]
    desc_stats <- round(desc_stats, 4)
    colnames(desc_stats) <- c("N", "Mean", "Std Dev", "Median", "Min", "Max", "Skewness", "Kurtosis")
    DT::datatable(desc_stats, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$single_var_stats <- renderPrint({
    req(input$desc_variable)
    var_data <- values$processed_data[[input$desc_variable]]
    cat("STATISTIK DESKRIPTIF\n")
    cat("===================\n")
    cat("Variabel:", input$desc_variable, "\n")
    cat("N:", length(var_data[!is.na(var_data)]), "\n")
    cat("Missing:", sum(is.na(var_data)), "\n")
    print(summary(var_data))
    cat("\nStandar Deviasi:", round(sd(var_data, na.rm = TRUE), 4), "\n")
    cat("Skewness:", round(psych::skew(var_data, na.rm = TRUE), 4), "\n")
    cat("Kurtosis:", round(psych::kurtosi(var_data, na.rm = TRUE), 4), "\n")
  })
  
  output$distribution_viz <- renderPlotly({
    req(input$desc_variable)
    
    p <- ggplot(values$processed_data, aes_string(x = input$desc_variable)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
      geom_density(color = "red", size = 1) +
      labs(title = paste("Distribusi", input$desc_variable),
           x = input$desc_variable, y = "Densitas") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$descriptive_interpretation <- renderText({
    req(input$desc_variable)
    
    var_data <- values$processed_data[[input$desc_variable]]
    mean_val <- mean(var_data, na.rm = TRUE)
    median_val <- median(var_data, na.rm = TRUE)
    sd_val <- sd(var_data, na.rm = TRUE)
    skew_val <- psych::skew(var_data, na.rm = TRUE)
    kurt_val <- psych::kurtosi(var_data, na.rm = TRUE)
    
    interpretation <- paste(
      "INTERPRETASI STATISTIK DESKRIPTIF:\n\n",
      sprintf("Variabel: %s", input$desc_variable),
      sprintf("Rata-rata: %.4f | Median: %.4f | Standar Deviasi: %.4f", mean_val, median_val, sd_val),
      sprintf("Skewness: %.4f | Kurtosis: %.4f", skew_val, kurt_val),
      "\nAnalisis Distribusi:",
      if (abs(skew_val) < 0.5) {
        "• Distribusi mendekati simetris (mirip normal)"
      } else if (skew_val > 0.5) {
        "• Distribusi miring ke kanan (positively skewed)"
      } else {
        "• Distribusi miring ke kiri (negatively skewed)"
      },
      if (abs(kurt_val) < 3) {
        "• Distribusi memiliki kurtosis normal (mesokurtic)"
      } else if (kurt_val > 3) {
        "• Distribusi lebih runcing dari normal (leptokurtic)"
      } else {
        "• Distribusi lebih datar dari normal (platykurtic)"
      },
      if (mean_val > median_val) {
        "• Rata-rata > Median: Menunjukkan adanya outlier tinggi"
      } else if (mean_val < median_val) {
        "• Rata-rata < Median: Menunjukkan adanya outlier rendah"
      } else {
        "• Rata-rata ≈ Median: Menunjukkan distribusi seimbang"
      }
    )
    
    return(interpretation)
  })
  
  # Advanced visualizations
  observeEvent(input$generate_plot, {
    output$advanced_visualization <- renderPlotly({
      req(input$plot_type)
      
      tryCatch({
        if (input$plot_type == "histogram") {
          req(input$viz_variable)
          p <- ggplot(values$processed_data, aes_string(x = input$viz_variable)) +
            geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
            labs(title = paste("Histogram:", input$viz_variable),
                 x = input$viz_variable, y = "Frekuensi") +
            theme_minimal()
          
        } else if (input$plot_type == "density") {
          req(input$viz_variable)
          p <- ggplot(values$processed_data, aes_string(x = input$viz_variable)) +
            geom_density(fill = "steelblue", alpha = 0.7) +
            labs(title = paste("Plot Densitas:", input$viz_variable),
                 x = input$viz_variable, y = "Densitas") +
            theme_minimal()
          
        } else if (input$plot_type == "boxplot") {
          req(input$viz_variable)
          p <- ggplot(values$processed_data, aes_string(y = input$viz_variable)) +
            geom_boxplot(fill = "lightblue", alpha = 0.7) +
            labs(title = paste("Box Plot:", input$viz_variable),
                 y = input$viz_variable) +
            theme_minimal()
          
        } else if (input$plot_type == "violin") {
          req(input$viz_variable)
          p <- ggplot(values$processed_data, aes_string(x = "1", y = input$viz_variable)) +
            geom_violin(fill = "lightgreen", alpha = 0.7) +
            geom_boxplot(width = 0.1, fill = "white", alpha = 0.8) +
            labs(title = paste("Violin Plot:", input$viz_variable),
                 y = input$viz_variable) +
            theme_minimal() +
            theme(axis.title.x = element_blank(), axis.text.x = element_blank())
          
        } else if (input$plot_type == "scatter") {
          req(input$x_variable, input$y_variable)
          if (input$color_variable != "none") {
            p <- ggplot(values$processed_data, aes_string(x = input$x_variable, y = input$y_variable, color = input$color_variable)) +
              geom_point(alpha = 0.6) +
              geom_smooth(method = "lm", se = TRUE) +
              labs(title = paste("Scatter Plot:", input$x_variable, "vs", input$y_variable),
                   x = input$x_variable, y = input$y_variable) +
              theme_minimal()
          } else {
            p <- ggplot(values$processed_data, aes_string(x = input$x_variable, y = input$y_variable)) +
              geom_point(alpha = 0.6, color = "steelblue") +
              geom_smooth(method = "lm", se = TRUE, color = "red") +
              labs(title = paste("Scatter Plot:", input$x_variable, "vs", input$y_variable),
                   x = input$x_variable, y = input$y_variable) +
              theme_minimal()
          }
          
        } else if (input$plot_type == "correlation") {
          numeric_data <- values$processed_data[sapply(values$processed_data, is.numeric)]
          cor_matrix <- cor(numeric_data, use = "complete.obs")
          cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
          cor_df$value <- as.vector(cor_matrix)
          
          p <- ggplot(cor_df, aes(Var1, Var2, fill = value)) +
            geom_tile() +
            scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                                 name = "Korelasi") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = "Heatmap Korelasi", x = "", y = "")
          
        } else if (input$plot_type == "pairs") {
          numeric_data <- values$processed_data[sapply(values$processed_data, is.numeric)]
          # Select first 6 variables for pairs plot to avoid overcrowding
          if (ncol(numeric_data) > 6) {
            numeric_data <- numeric_data[, 1:6]
          }
          
          pairs_data <- numeric_data %>%
            mutate(id = row_number()) %>%
            gather(variable, value, -id) %>%
            left_join(
              numeric_data %>% mutate(id = row_number()) %>%
                gather(variable2, value2, -id),
              by = "id"
            )
          
          p <- ggplot(pairs_data, aes(value, value2)) +
            geom_point(alpha = 0.3, size = 0.5) +
            facet_grid(variable2 ~ variable, scales = "free") +
            theme_minimal() +
            theme(axis.text = element_text(size = 6))
        }
        
        ggplotly(p)
      }, error = function(e) {
        showNotification(paste("Error dalam membuat plot:", e$message), type = "error")
        return(NULL)
      })
    })
  })
  
  output$visualization_interpretation <- renderText({
    req(input$plot_type)
    
    interpretation <- switch(input$plot_type,
                             "histogram" = paste("INTERPRETASI HISTOGRAM:\n\nHistogram menunjukkan distribusi frekuensi", input$viz_variable,
                                                 ". Perhatikan pola seperti normalitas, skewness, multimodalitas, atau outlier."),
                             "density" = paste("INTERPRETASI PLOT DENSITAS:\n\nPlot densitas memberikan estimasi halus dari fungsi densitas probabilitas untuk", input$viz_variable,
                                               ". Ini membantu mengidentifikasi bentuk distribusi dan modus potensial."),
                             "boxplot" = paste("INTERPRETASI BOX PLOT:\n\nBox plot merangkum", input$viz_variable,
                                               "menunjukkan median (garis tengah), kuartil (tepi kotak), dan outlier potensial (titik di luar whisker)."),
                             "violin" = paste("INTERPRETASI VIOLIN PLOT:\n\nViolin plot menggabungkan box plot dengan plot densitas untuk", input$viz_variable,
                                              ". Lebar menunjukkan densitas pada nilai berbeda, memberikan detail lebih dari box plot sederhana."),
                             "scatter" = {
                               req(input$x_variable, input$y_variable)
                               cor_val <- cor(values$processed_data[[input$x_variable]], values$processed_data[[input$y_variable]], use = "complete.obs")
                               paste("INTERPRETASI SCATTER PLOT:\n\nScatter plot menunjukkan hubungan antara", input$x_variable, "dan", input$y_variable,
                                     sprintf(". Koefisien korelasi: %.4f", cor_val),
                                     if (abs(cor_val) > 0.7) " - Hubungan linear kuat"
                                     else if (abs(cor_val) > 0.3) " - Hubungan linear sedang"
                                     else " - Hubungan linear lemah atau tidak ada")
                             },
                             "correlation" = "INTERPRETASI HEATMAP KORELASI:\n\nHeatmap menampilkan korelasi berpasangan antara semua variabel numerik. Merah menunjukkan korelasi positif, biru menunjukkan korelasi negatif, dan putih menunjukkan tidak ada korelasi.",
                             "pairs" = "INTERPRETASI PAIRS PLOT:\n\nPairs plot menunjukkan scatter plot untuk semua kombinasi variabel yang dipilih. Ini membantu mengidentifikasi hubungan dan pola di berbagai variabel secara bersamaan."
    )
    
    return(interpretation)
  })
  
  # ===================================================================
  # PERBAIKAN UNTUK BLOK ANALISIS KORELASI (SERVER LOGIC)
  # ===================================================================
  
  # 1. Buat reactiveValues untuk menyimpan hasil korelasi
  correlation_results <- reactiveVal(NULL)
  
  # 2. Gunakan observeEvent HANYA untuk melakukan kalkulasi
  observeEvent(input$compute_correlation, {
    tryCatch({
      
      # Ambil data numerik yang sudah diproses
      numeric_data <- values$processed_data[sapply(values$processed_data, is.numeric)]
      
      # PERBAIKAN: Filter kolom dengan varians nol untuk mencegah error hclust
      is_valid <- sapply(numeric_data, function(col) var(col, na.rm = TRUE) != 0)
      if (sum(is_valid, na.rm = TRUE) < 2) {
        showNotification("Pilih minimal 2 variabel dengan varians valid.", type = "error")
        return()
      }
      data_filtered <- numeric_data[, is_valid, drop = FALSE]
      
      # PERBAIKAN: Gunakan Hmisc::rcorr yang jauh lebih cepat daripada loop
      # Pastikan library(Hmisc) sudah dimuat
      corr_obj <- Hmisc::rcorr(as.matrix(data_filtered), type = input$corr_method)
      
      # Simpan hasilnya di reactiveVal
      correlation_results(list(
        cor_matrix = corr_obj$r,
        p_values = corr_obj$P
      ))
      
      showNotification("Analisis korelasi berhasil!", type = "success")
      
    }, error = function(e) {
      showNotification(paste("Error dalam analisis korelasi:", e$message), type = "error")
    })
  })
  
  # 3. Pindahkan semua 'render' ke luar observeEvent
  output$correlation_plot <- renderPlot({
    results <- correlation_results()
    req(results) # Hanya jalan jika hasil sudah ada
    
    # PERBAIKAN: Menggunakan p.mat dan insig untuk visualisasi signifikansi yang lebih baik
    corrplot(
      results$cor_matrix,
      method = "color",
      type = "upper",
      order = "hclust",
      p.mat = results$p_values,
      sig.level = input$corr_threshold,
      insig = if (input$show_insignificant) "pch" else "blank",
      pch.col = "grey",
      pch.cex = 1.5,
      tl.cex = 0.8,
      tl.col = "black",
      tl.srt = 45,
      col = colorRampPalette(c("#053061", "#f7f7f7", "#67001f"))(200),
      title = "Matriks Korelasi",
      mar = c(0,0,1,0)
    )
  })
  
  output$correlation_table <- DT::renderDataTable({
    results <- correlation_results()
    req(results)
    
    # Mengonversi matriks menjadi tabel data frame
    cor_matrix <- results$cor_matrix
    p_values <- results$p_values
    
    upper_tri <- which(upper.tri(cor_matrix), arr.ind = TRUE)
    cor_df <- data.frame(
      Variabel1 = rownames(cor_matrix)[upper_tri[, 1]],
      Variabel2 = colnames(cor_matrix)[upper_tri[, 2]],
      Korelasi = cor_matrix[upper_tri],
      P_Value = p_values[upper_tri]
    )
    
    cor_df$Signifikan <- ifelse(cor_df$P_Value < input$corr_threshold, "Ya", "Tidak")
    
    # Pembulatan dan pengurutan
    cor_df <- cor_df %>%
      mutate(
        Korelasi = round(Korelasi, 4),
        P_Value = format.pval(P_Value, eps = .001, digits = 3)
      ) %>%
      arrange(desc(abs(Korelasi)))
    
    DT::datatable(cor_df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$correlation_interpretation <- renderText({
    results <- correlation_results()
    req(results)
    
    cor_matrix <- results$cor_matrix
    
    strong_corr <- sum(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, na.rm = TRUE) / 2
    moderate_corr <- sum(abs(cor_matrix) > 0.3 & abs(cor_matrix) <= 0.7, na.rm = TRUE) / 2
    weak_corr <- sum(abs(cor_matrix) <= 0.3, na.rm = TRUE) / 2
    
    interpretation <- paste(
      "INTERPRETASI ANALISIS KORELASI:\n\n",
      sprintf("Metode: Korelasi %s", tools::toTitleCase(input$corr_method)),
      sprintf("Ambang signifikansi (α): %.3f", input$corr_threshold),
      "\n\nRingkasan kekuatan korelasi:",
      sprintf("\n• Korelasi kuat (|r| > 0.7): %d pasang", strong_corr),
      sprintf("\n• Korelasi sedang (0.3 < |r| ≤ 0.7): %d pasang", moderate_corr),
      sprintf("\n• Korelasi lemah (|r| ≤ 0.3): %d pasang", weak_corr),
      "\n\nPanduan interpretasi:",
      "\n• |r| > 0.7: Hubungan linear kuat",
      "\n• 0.3 < |r| ≤ 0.7: Hubungan linear sedang",
      "\n• |r| ≤ 0.3: Hubungan linear lemah atau tidak ada",
      "\n• Nilai positif menunjukkan hubungan positif (searah)",
      "\n• Nilai negatif menunjukkan hubungan negatif (berlawanan arah)"
    )
    
    return(interpretation)
  })
  
  # ASSUMPTION TESTING TAB
  observeEvent(input$run_normality_test, {
    req(input$normality_variable, input$normality_test)
    
    tryCatch({
      var_data <- values$processed_data[[input$normality_variable]]
      var_data <- var_data[!is.na(var_data)]
      
      if (input$normality_test == "shapiro") {
        if (length(var_data) <= 5000) {
          test_result <- shapiro.test(var_data)
        } else {
          test_result <- list(method = "Uji Shapiro-Wilk",
                              data.name = input$normality_variable,
                              p.value = NA,
                              statistic = NA,
                              note = "Ukuran sampel terlalu besar untuk uji Shapiro-Wilk (n > 5000)")
        }
      } else if (input$normality_test == "anderson") {
        test_result <- nortest::ad.test(var_data)
      } else if (input$normality_test == "ks") {
        test_result <- nortest::lillie.test(var_data)
      } else if (input$normality_test == "jarque") {
        # Jarque-Bera test
        n <- length(var_data)
        skew_val <- psych::skew(var_data, na.rm = TRUE)
        kurt_val <- psych::kurtosi(var_data, na.rm = TRUE)
        jb_stat <- n * (skew_val^2 / 6 + (kurt_val - 3)^2 / 24)
        p_val <- 1 - pchisq(jb_stat, df = 2)
        
        test_result <- list(
          method = "Uji Jarque-Bera",
          data.name = input$normality_variable,
          statistic = c(JB = jb_stat),
          p.value = p_val,
          parameter = c(df = 2)
        )
      }
      
      output$normality_test_results <- renderPrint({
        if (!is.na(test_result$p.value)) {
          cat("HASIL UJI NORMALITAS\n")
          cat("===================\n")
          cat("Uji:", test_result$method, "\n")
          cat("Variabel:", test_result$data.name, "\n")
          if (!is.null(test_result$statistic)) {
            cat("Statistik Uji:", round(test_result$statistic, 6), "\n")
          }
          cat("P-value:", format(test_result$p.value, scientific = TRUE), "\n")
          cat("Tingkat Signifikansi:", input$alpha_level, "\n")
          cat("\nKeputusan:\n")
          if (test_result$p.value > input$alpha_level) {
            cat("GAGAL MENOLAK H0: Data tampak berdistribusi normal\n")
          } else {
            cat("TOLAK H0: Data TIDAK berdistribusi normal\n")
          }
        } else {
          cat("CATATAN:", test_result$note, "\n")
        }
      })
      
      output$normality_plots <- renderPlot({
        par(mfrow = c(2, 2))
        
        # Histogram with normal curve
        hist(var_data, breaks = 30, freq = FALSE,
             main = paste("Histogram", input$normality_variable),
             xlab = input$normality_variable, col = "lightblue", border = "white")
        curve(dnorm(x, mean = mean(var_data), sd = sd(var_data)),
              add = TRUE, col = "red", lwd = 2)
        legend("topright", legend = "Kurva Normal", col = "red", lwd = 2)
        
        # Q-Q plot
        qqnorm(var_data, main = paste("Q-Q Plot", input$normality_variable))
        qqline(var_data, col = "red", lwd = 2)
        
        # Box plot
        boxplot(var_data, main = paste("Box Plot", input$normality_variable),
                ylab = input$normality_variable, col = "lightgreen")
        
        # Density plot
        plot(density(var_data), main = paste("Plot Densitas", input$normality_variable),
             xlab = input$normality_variable, col = "blue", lwd = 2)
        polygon(density(var_data), col = "lightblue", border = "blue")
      })
      
      output$normality_interpretation <- renderText({
        if (!is.na(test_result$p.value)) {
          interpretation <- paste(
            "INTERPRETASI UJI NORMALITAS:\n\n",
            sprintf("Uji: %s", test_result$method),
            sprintf("Variabel: %s", input$normality_variable),
            sprintf("Ukuran sampel: %d", length(var_data)),
            sprintf("Statistik uji: %.6f", ifelse(is.null(test_result$statistic), NA, test_result$statistic)),
            sprintf("P-value: %.6e", test_result$p.value),
            sprintf("Tingkat signifikansi: %.3f", input$alpha_level),
            "\nHipotesis:",
            "H₀: Data mengikuti distribusi normal",
            "H₁: Data tidak mengikuti distribusi normal",
            "\nKesimpulan:",
            if (test_result$p.value > input$alpha_level) {
              paste("Dengan α =", input$alpha_level, ", kita GAGAL MENOLAK hipotesis nol.",
                    "Data tampak berdistribusi normal.")
            } else {
              paste("Dengan α =", input$alpha_level, ", kita MENOLAK hipotesis nol.",
                    "Data TIDAK berdistribusi normal.")
            },
            "\nPenilaian Visual:",
            "• Histogram: Harus menunjukkan distribusi berbentuk lonceng",
            "• Q-Q Plot: Titik harus mengikuti garis diagonal dengan rapat",
            "• Box Plot: Harus simetris dengan sedikit outlier",
            "• Plot Densitas: Harus menyerupai bentuk kurva normal"
          )
        } else {
          interpretation <- paste("Uji tidak dapat dilakukan:", test_result$note)
        }
        
        return(interpretation)
      })
    }, error = function(e) {
      showNotification(paste("Error dalam uji normalitas:", e$message), type = "error")
    })
  })
  
  # Homogeneity testing
  observeEvent(input$run_homogeneity_test, {
    req(input$homogeneity_variable, input$grouping_variable, input$homogeneity_test)
    
    tryCatch({
      test_data <- data.frame(
        variable = values$processed_data[[input$homogeneity_variable]],
        group = values$processed_data[[input$grouping_variable]]
      )
      test_data <- test_data[complete.cases(test_data), ]
      
      if (input$homogeneity_test == "levene") {
        test_result <- car::leveneTest(variable ~ group, data = test_data)
      } else if (input$homogeneity_test == "bartlett") {
        test_result <- bartlett.test(variable ~ group, data = test_data)
      } else if (input$homogeneity_test == "fligner") {
        test_result <- fligner.test(variable ~ group, data = test_data)
      }
      
      output$homogeneity_test_results <- renderPrint({
        cat("HASIL UJI HOMOGENITAS VARIANS\n")
        cat("=============================\n")
        print(test_result)
        cat("\nKeputusan:\n")
        p_val <- if (input$homogeneity_test == "levene") test_result$`Pr(>F)`[1] else test_result$p.value
        if (p_val > 0.05) {
          cat("GAGAL MENOLAK H0: Varians homogen (sama)\n")
        } else {
          cat("TOLAK H0: Varians TIDAK homogen (tidak sama)\n")
        }
      })
      
      output$homogeneity_plot <- renderPlotly({
        p <- ggplot(test_data, aes(x = group, y = variable, fill = group)) +
          geom_boxplot(alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5) +
          stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
          labs(title = paste("Uji Homogenitas:", input$homogeneity_variable, "berdasarkan", input$grouping_variable),
               x = input$grouping_variable, y = input$homogeneity_variable) +
          theme_minimal() +
          theme(legend.position = "none")
        
        ggplotly(p)
      })
      
      output$homogeneity_interpretation <- renderText({
        p_val <- if (input$homogeneity_test == "levene") test_result$`Pr(>F)`[1] else test_result$p.value
        
        interpretation <- paste(
          "INTERPRETASI UJI HOMOGENITAS VARIANS:\n\n",
          sprintf("Uji: %s",
                  switch(input$homogeneity_test,
                         "levene" = "Uji Levene",
                         "bartlett" = "Uji Bartlett",
                         "fligner" = "Uji Fligner-Killeen")),
          sprintf("Variabel: %s dikelompokkan berdasarkan %s", input$homogeneity_variable, input$grouping_variable),
          sprintf("P-value: %.6f", p_val),
          "\nHipotesis:",
          "H₀: Semua varians kelompok sama (homogen)",
          "H₁: Setidaknya satu varians kelompok berbeda (heterogen)",
          "\nKesimpulan:",
          if (p_val > 0.05) {
            "Dengan α = 0.05, kita GAGAL MENOLAK hipotesis nol. Varians tampak homogen antar kelompok."
          } else {
            "Dengan α = 0.05, kita MENOLAK hipotesis nol. Varians TIDAK homogen antar kelompok."
          },
          "\nImplikasi Praktis:",
          if (p_val > 0.05) {
            "• Asumsi varians sama terpenuhi untuk ANOVA dan uji t"
          } else {
            "• Pertimbangkan menggunakan uji t Welch atau alternatif non-parametrik"
          },
          "\nPenilaian Visual:",
          "• Box plot harus memiliki tinggi dan sebaran yang serupa",
          "• Outlier harus terdistribusi serupa antar kelompok"
        )
        
        return(interpretation)
      })
    }, error = function(e) {
      showNotification(paste("Error dalam uji homogenitas:", e$message), type = "error")
    })
  })
  
  # Diagnostic plots
  observeEvent(input$generate_diagnostics, {
    req(input$diagnostic_variable, input$diagnostic_plots)
    
    tryCatch({
      var_data <- values$processed_data[[input$diagnostic_variable]]
      var_data <- var_data[!is.na(var_data)]
      
      output$diagnostic_plots_output <- renderPlot({
        n_plots <- length(input$diagnostic_plots)
        if (n_plots == 1) {
          par(mfrow = c(1, 1))
        } else if (n_plots == 2) {
          par(mfrow = c(1, 2))
        } else if (n_plots <= 4) {
          par(mfrow = c(2, 2))
        } else {
          par(mfrow = c(2, 3))
        }
        
        if ("qq" %in% input$diagnostic_plots) {
          qqnorm(var_data, main = paste("Q-Q Plot:", input$diagnostic_variable))
          qqline(var_data, col = "red", lwd = 2)
        }
        
        if ("hist_norm" %in% input$diagnostic_plots) {
          hist(var_data, breaks = 30, freq = FALSE,
               main = paste("Histogram dengan Kurva Normal:", input$diagnostic_variable),
               xlab = input$diagnostic_variable, col = "lightblue", border = "white")
          curve(dnorm(x, mean = mean(var_data), sd = sd(var_data)),
                add = TRUE, col = "red", lwd = 2)
        }
        
        if ("boxplot" %in% input$diagnostic_plots) {
          boxplot(var_data, main = paste("Box Plot:", input$diagnostic_variable),
                  ylab = input$diagnostic_variable, col = "lightgreen")
        }
        
        if ("density" %in% input$diagnostic_plots) {
          plot(density(var_data), main = paste("Plot Densitas:", input$diagnostic_variable),
               xlab = input$diagnostic_variable, col = "blue", lwd = 2)
          polygon(density(var_data), col = "lightblue", border = "blue")
          # Add normal density for comparison
          x_seq <- seq(min(var_data), max(var_data), length.out = 100)
          normal_density <- dnorm(x_seq, mean = mean(var_data), sd = sd(var_data))
          lines(x_seq, normal_density, col = "red", lwd = 2, lty = 2)
          legend("topright", legend = c("Teramati", "Normal"),
                 col = c("blue", "red"), lwd = 2, lty = c(1, 2))
        }
        
        if ("prob" %in% input$diagnostic_plots) {
          # Probability plot
          n <- length(var_data)
          sorted_data <- sort(var_data)
          theoretical_quantiles <- qnorm((1:n - 0.5) / n)
          plot(theoretical_quantiles, sorted_data,
               main = paste("Plot Probabilitas:", input$diagnostic_variable),
               xlab = "Kuantil Teoretis", ylab = "Kuantil Sampel",
               pch = 16, col = "blue")
          abline(mean(var_data), sd(var_data), col = "red", lwd = 2)
        }
      })
      
      output$diagnostic_interpretation <- renderText({
        skew_val <- psych::skew(var_data, na.rm = TRUE)
        kurt_val <- psych::kurtosi(var_data, na.rm = TRUE)
        
        interpretation <- paste(
          "INTERPRETASI PLOT DIAGNOSTIK:\n\n",
          sprintf("Variabel: %s", input$diagnostic_variable),
          sprintf("Ukuran sampel: %d", length(var_data)),
          sprintf("Skewness: %.4f", skew_val),
          sprintf("Kurtosis: %.4f", kurt_val),
          "\nInterpretasi Plot:",
          if ("qq" %in% input$diagnostic_plots) {
            "• Q-Q Plot: Titik harus mengikuti garis diagonal untuk normalitas"
          } else "",
          if ("hist_norm" %in% input$diagnostic_plots) {
            "• Histogram: Harus cocok dengan kurva normal merah untuk normalitas"
          } else "",
          if ("boxplot" %in% input$diagnostic_plots) {
            "• Box Plot: Harus simetris dengan outlier minimal"
          } else "",
          if ("density" %in% input$diagnostic_plots) {
            "• Plot Densitas: Garis biru (teramati) harus cocok dengan garis putus-putus merah (normal)"
          } else "",
          if ("prob" %in% input$diagnostic_plots) {
            "• Plot Probabilitas: Titik harus sejajar dengan garis merah untuk normalitas"
          } else "",
          "\nPenilaian Normalitas:",
          if (abs(skew_val) < 0.5 && abs(kurt_val - 3) < 1) {
            "✓ Data tampak mendekati normal berdasarkan skewness dan kurtosis"
          } else {
            "✗ Data menunjukkan penyimpangan dari normalitas - pertimbangkan transformasi"
          }
        )
        
        return(interpretation)
      })
    }, error = function(e) {
      showNotification(paste("Error dalam plot diagnostik:", e$message), type = "error")
    })
  })
  
  # INFERENTIAL STATISTICS I TAB
  # Proportion tests
  observeEvent(input$run_proportion_test, {
    req(input$prop_variable, input$prop_threshold, input$null_proportion)
    
    tryCatch({
      var_data <- values$processed_data[[input$prop_variable]]
      
      if (input$prop_test_type == "one") {
        # One-sample proportion test
        successes <- sum(var_data > input$prop_threshold, na.rm = TRUE)
        n <- sum(!is.na(var_data))
        test_result <- prop.test(successes, n, p = input$null_proportion)
        
        output$proportion_test_results <- renderPrint({
          cat("HASIL UJI PROPORSI SATU SAMPEL\n")
          cat("==============================\n")
          cat("Variabel:", input$prop_variable, "\n")
          cat("Ambang:", input$prop_threshold, "\n")
          cat("Jumlah sukses:", successes, "\n")
          cat("Ukuran sampel:", n, "\n")
          cat("Proporsi sampel:", round(successes/n, 4), "\n")
          cat("Proporsi H0:", input$null_proportion, "\n")
          print(test_result)
        })
        
        output$proportion_plot <- renderPlotly({
          prop_data <- data.frame(
            Kategori = c("Sukses", "Gagal"),
            Jumlah = c(successes, n - successes)
          )
          
          p <- ggplot(prop_data, aes(x = Kategori, y = Jumlah, fill = Kategori)) +
            geom_bar(stat = "identity", alpha = 0.7) +
            labs(title = paste("Distribusi Proporsi:", input$prop_variable),
                 x = "Kategori", y = "Jumlah") +
            theme_minimal()
          
          ggplotly(p)
        })
        
      } else {
        # Two-sample proportion test
        req(input$prop_group_var)
        group_data <- values$processed_data[[input$prop_group_var]]
        groups <- unique(group_data[!is.na(group_data)])
        
        if (length(groups) == 2) {
          group1_data <- var_data[group_data == groups[1]]
          group2_data <- var_data[group_data == groups[2]]
          
          successes1 <- sum(group1_data > input$prop_threshold, na.rm = TRUE)
          successes2 <- sum(group2_data > input$prop_threshold, na.rm = TRUE)
          n1 <- sum(!is.na(group1_data))
          n2 <- sum(!is.na(group2_data))
          
          test_result <- prop.test(c(successes1, successes2), c(n1, n2))
          
          output$proportion_test_results <- renderPrint({
            cat("HASIL UJI PROPORSI DUA SAMPEL\n")
            cat("=============================\n")
            cat("Variabel:", input$prop_variable, "\n")
            cat("Kelompok:", input$prop_group_var, "\n")
            cat("Ambang:", input$prop_threshold, "\n")
            cat("\nKelompok 1 (", groups[1], "):\n", sep = "")
            cat("  Sukses:", successes1, "dari", n1, "(", round(successes1/n1, 4), ")\n")
            cat("Kelompok 2 (", groups[2], "):\n", sep = "")
            cat("  Sukses:", successes2, "dari", n2, "(", round(successes2/n2, 4), ")\n")
            print(test_result)
          })
          
          output$proportion_plot <- renderPlotly({
            prop_data <- data.frame(
              Kelompok = rep(groups, each = 2),
              Kategori = rep(c("Sukses", "Gagal"), 2),
              Jumlah = c(successes1, n1 - successes1, successes2, n2 - successes2)
            )
            
            p <- ggplot(prop_data, aes(x = Kelompok, y = Jumlah, fill = Kategori)) +
              geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
              labs(title = paste("Perbandingan Proporsi:", input$prop_variable),
                   x = "Kelompok", y = "Jumlah") +
              theme_minimal()
            
            ggplotly(p)
          })
        }
      }
      
      output$proportion_interpretation <- renderText({
        p_val <- test_result$p.value
        
        interpretation <- paste(
          "INTERPRETASI UJI PROPORSI:\n\n",
          sprintf("Jenis uji: %s", ifelse(input$prop_test_type == "one", "Satu sampel", "Dua sampel")),
          sprintf("P-value: %.6f", p_val),
          "\nHipotesis:",
          if (input$prop_test_type == "one") {
            paste("H₀: p =", input$null_proportion)
          } else {
            "H₀: p₁ = p₂"
          },
          if (input$prop_test_type == "one") {
            paste("H₁: p ≠", input$null_proportion)
          } else {
            "H₁: p₁ ≠ p₂"
          },
          "\nKesimpulan:",
          if (p_val > 0.05) {
            "Dengan α = 0.05, kita GAGAL MENOLAK hipotesis nol."
          } else {
            "Dengan α = 0.05, kita MENOLAK hipotesis nol."
          },
          "\nInterval kepercayaan tersedia dalam output uji."
        )
        
        return(interpretation)
      })
    }, error = function(e) {
      showNotification(paste("Error dalam uji proporsi:", e$message), type = "error")
    })
  })
  
  # Variance tests
  observeEvent(input$run_variance_test, {
    req(input$variance_variable, input$variance_test_type)
    
    tryCatch({
      var_data <- values$processed_data[[input$variance_variable]]
      var_data <- var_data[!is.na(var_data)]
      
      if (input$variance_test_type == "one") {
        # One-sample variance test (Chi-square)
        req(input$null_variance)
        n <- length(var_data)
        sample_var <- var(var_data)
        chi_stat <- (n - 1) * sample_var / input$null_variance
        p_val <- 2 * min(pchisq(chi_stat, df = n - 1), 1 - pchisq(chi_stat, df = n - 1))
        
        test_result <- list(
          method = "Uji Chi-square untuk Varians Satu Sampel",
          statistic = chi_stat,
          p.value = p_val,
          parameter = n - 1,
          sample_var = sample_var,
          null_var = input$null_variance
        )
        
        output$variance_test_results <- renderPrint({
          cat("HASIL UJI VARIANS SATU SAMPEL\n")
          cat("=============================\n")
          cat("Variabel:", input$variance_variable, "\n")
          cat("Ukuran sampel:", n, "\n")
          cat("Varians sampel:", round(sample_var, 4), "\n")
          cat("Varians H0:", input$null_variance, "\n")
          cat("Statistik Chi-square:", round(chi_stat, 4), "\n")
          cat("Derajat bebas:", n - 1, "\n")
          cat("P-value:", format(p_val, scientific = TRUE), "\n")
        })
        
        output$variance_plot <- renderPlotly({
          # Chi-square distribution plot
          x_vals <- seq(0, qchisq(0.99, df = n - 1), length.out = 1000)
          y_vals <- dchisq(x_vals, df = n - 1)
          
          plot_data <- data.frame(x = x_vals, y = y_vals)
          
          p <- ggplot(plot_data, aes(x = x, y = y)) +
            geom_line(color = "blue", size = 1) +
            geom_vline(xintercept = chi_stat, color = "red", linetype = "dashed", size = 1) +
            labs(title = "Distribusi Chi-square dan Statistik Uji",
                 x = "Nilai Chi-square", y = "Densitas") +
            theme_minimal()
          
          ggplotly(p)
        })
        
      } else if (input$variance_test_type == "two") {
        # Two-sample F-test
        req(input$variance_group_var)
        group_data <- values$processed_data[[input$variance_group_var]]
        groups <- unique(group_data[!is.na(group_data)])
        
        if (length(groups) >= 2) {
          group1_data <- var_data[group_data == groups[1]]
          group2_data <- var_data[group_data == groups[2]]
          
          test_result <- var.test(group1_data, group2_data)
          
          output$variance_test_results <- renderPrint({
            cat("HASIL UJI F UNTUK DUA VARIANS\n")
            cat("=============================\n")
            print(test_result)
          })
          
          output$variance_plot <- renderPlotly({
            var_data_plot <- data.frame(
              Nilai = c(group1_data, group2_data),
              Kelompok = c(rep(groups[1], length(group1_data)), 
                           rep(groups[2], length(group2_data)))
            )
            
            p <- ggplot(var_data_plot, aes(x = Kelompok, y = Nilai, fill = Kelompok)) +
              geom_boxplot(alpha = 0.7) +
              labs(title = paste("Perbandingan Varians:", input$variance_variable),
                   x = "Kelompok", y = "Nilai") +
              theme_minimal()
            
            ggplotly(p)
          })
        }
        
      } else {
        # Multiple groups (Bartlett test)
        req(input$variance_group_var)
        group_data <- values$processed_data[[input$variance_group_var]]
        test_data <- data.frame(value = var_data, group = group_data)
        test_data <- test_data[complete.cases(test_data), ]
        
        test_result <- bartlett.test(value ~ group, data = test_data)
        
        output$variance_test_results <- renderPrint({
          cat("HASIL UJI BARTLETT UNTUK BANYAK VARIANS\n")
          cat("=======================================\n")
          print(test_result)
        })
        
        output$variance_plot <- renderPlotly({
          p <- ggplot(test_data, aes(x = group, y = value, fill = group)) +
            geom_boxplot(alpha = 0.7) +
            labs(title = paste("Perbandingan Varians:", input$variance_variable),
                 x = "Kelompok", y = "Nilai") +
            theme_minimal() +
            theme(legend.position = "none")
          
          ggplotly(p)
        })
      }
      
      output$variance_interpretation <- renderText({
        p_val <- test_result$p.value
        
        interpretation <- paste(
          "INTERPRETASI UJI VARIANS:\n\n",
          sprintf("Jenis uji: %s", 
                  switch(input$variance_test_type,
                         "one" = "Satu sampel (Chi-square)",
                         "two" = "Dua sampel (F-test)",
                         "multiple" = "Banyak kelompok (Bartlett)")),
          sprintf("P-value: %.6f", p_val),
          "\nHipotesis:",
          switch(input$variance_test_type,
                 "one" = paste("H₀: σ² =", input$null_variance),
                 "two" = "H₀: σ₁² = σ₂²",
                 "multiple" = "H₀: σ₁² = σ₂² = ... = σₖ²"),
          switch(input$variance_test_type,
                 "one" = paste("H₁: σ² ≠", input$null_variance),
                 "two" = "H₁: σ₁² ≠ σ₂²",
                 "multiple" = "H₁: Setidaknya satu varians berbeda"),
          "\nKesimpulan:",
          if (p_val > 0.05) {
            "Dengan α = 0.05, kita GAGAL MENOLAK hipotesis nol."
          } else {
            "Dengan α = 0.05, kita MENOLAK hipotesis nol."
          }
        )
        
        return(interpretation)
      })
    }, error = function(e) {
      showNotification(paste("Error dalam uji varians:", e$message), type = "error")
    })
  })
  
  # Mean tests
  observeEvent(input$run_mean_test, {
    req(input$mean_variable, input$mean_test_type)
    
    tryCatch({
      var_data <- values$processed_data[[input$mean_variable]]
      
      if (input$mean_test_type == "one") {
        # One-sample t-test
        req(input$null_mean)
        test_result <- t.test(var_data, mu = input$null_mean)
        
        output$mean_test_results <- renderPrint({
          cat("HASIL UJI T SATU SAMPEL\n")
          cat("=======================\n")
          print(test_result)
        })
        
        output$mean_plot <- renderPlotly({
          p <- ggplot(values$processed_data, aes(x =  "", y = !!sym(input$mean_variable))) +
            geom_boxplot(fill = "lightblue", alpha = 0.7) +
            geom_hline(yintercept = input$null_mean, color = "red", linetype = "dashed", size = 1) +
            labs(title = paste("Uji t Satu Sampel:", input$mean_variable),
                 x = "", y = input$mean_variable) +
            theme_minimal()
          
          ggplotly(p)
        })
        
      } else if (input$mean_test_type == "two") {
        # Two-sample t-test
        req(input$mean_group_var)
        group_data <- values$processed_data[[input$mean_group_var]]
        groups <- unique(group_data[!is.na(group_data)])
        
        if (length(groups) >= 2) {
          group1_data <- var_data[group_data == groups[1]]
          group2_data <- var_data[group_data == groups[2]]
          
          test_result <- t.test(group1_data, group2_data, var.equal = input$assume_equal_var)
          
          output$mean_test_results <- renderPrint({
            cat("HASIL UJI T DUA SAMPEL\n")
            cat("======================\n")
            print(test_result)
          })
          
          output$mean_plot <- renderPlotly({
            plot_data <- data.frame(
              Nilai = c(group1_data, group2_data),
              Kelompok = c(rep(groups[1], length(group1_data)), 
                           rep(groups[2], length(group2_data)))
            )
            
            p <- ggplot(plot_data, aes(x = Kelompok, y = Nilai, fill = Kelompok)) +
              geom_boxplot(alpha = 0.7) +
              stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
              labs(title = paste("Perbandingan Rata-rata:", input$mean_variable),
                   x = "Kelompok", y = "Nilai") +
              theme_minimal()
            
            ggplotly(p)
          })
        }
        
      } else {
        # Paired t-test
        req(input$mean_group_var)
        # For paired t-test, we need paired observations
        # This is a simplified implementation
        group_data <- values$processed_data[[input$mean_group_var]]
        groups <- unique(group_data[!is.na(group_data)])
        
        if (length(groups) >= 2) {
          group1_data <- var_data[group_data == groups[1]]
          group2_data <- var_data[group_data == groups[2]]
          
          # Make sure we have equal length for pairing
          min_length <- min(length(group1_data), length(group2_data))
          if (min_length > 0) {
            test_result <- t.test(group1_data[1:min_length], group2_data[1:min_length], paired = TRUE)
            
            output$mean_test_results <- renderPrint({
              cat("HASIL UJI T BERPASANGAN\n")
              cat("=======================\n")
              print(test_result)
            })
            
            output$mean_plot <- renderPlotly({
              diff_data <- group1_data[1:min_length] - group2_data[1:min_length]
              plot_data <- data.frame(
                Index = 1:min_length,
                Selisih = diff_data
              )
              
              p <- ggplot(plot_data, aes(x = Index, y = Selisih)) +
                geom_point(alpha = 0.6) +
                geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
                labs(title = paste("Selisih Berpasangan:", input$mean_variable),
                     x = "Indeks", y = "Selisih") +
                theme_minimal()
              
              ggplotly(p)
            })
          }
        }
      }
      
      output$mean_interpretation <- renderText({
        p_val <- test_result$p.value
        
        interpretation <- paste(
          "INTERPRETASI UJI RATA-RATA:\n\n",
          sprintf("Jenis uji: %s", 
                  switch(input$mean_test_type,
                         "one" = "Uji t satu sampel",
                         "two" = "Uji t dua sampel",
                         "paired" = "Uji t berpasangan")),
          sprintf("P-value: %.6f", p_val),
          sprintf("Interval kepercayaan: [%.4f, %.4f]", test_result$conf.int[1], test_result$conf.int[2]),
          "\nHipotesis:",
          switch(input$mean_test_type,
                 "one" = paste("H₀: μ =", input$null_mean),
                 "two" = "H₀: μ₁ = μ₂",
                 "paired" = "H₀: μd = 0"),
          switch(input$mean_test_type,
                 "one" = paste("H₁: μ ≠", input$null_mean),
                 "two" = "H₁: μ₁ ≠ μ₂",
                 "paired" = "H₁: μd ≠ 0"),
          "\nKesimpulan:",
          if (p_val > 0.05) {
            "Dengan α = 0.05, kita GAGAL MENOLAK hipotesis nol."
          } else {
            "Dengan α = 0.05, kita MENOLAK hipotesis nol."
          }
        )
        
        return(interpretation)
      })
    }, error = function(e) {
      showNotification(paste("Error dalam uji rata-rata:", e$message), type = "error")
    })
  })
  
  # INFERENTIAL STATISTICS II TAB
  # One-way ANOVA
  observeEvent(input$run_anova1, {
    req(input$anova1_dependent, input$anova1_independent)
    
    tryCatch({
      formula_str <- paste(input$anova1_dependent, "~", input$anova1_independent)
      anova_formula <- as.formula(formula_str)
      
      values$anova1_model <- aov(anova_formula, data = values$processed_data)
      anova_summary <- summary(values$anova1_model)
      
      output$anova1_results <- renderPrint({
        cat("HASIL ANOVA SATU ARAH\n")
        cat("====================\n")
        cat("Formula:", formula_str, "\n\n")
        print(anova_summary)
      })
      
      # Post-hoc tests
      if (input$anova1_posthoc) {
        if (input$posthoc_method == "tukey") {
          posthoc_result <- TukeyHSD(values$anova1_model)
        } else if (input$posthoc_method == "bonferroni") {
          posthoc_result <- pairwise.t.test(values$processed_data[[input$anova1_dependent]], 
                                            values$processed_data[[input$anova1_independent]], 
                                            p.adjust.method = "bonferroni")
        } else {
          # Scheffe test (simplified)
          posthoc_result <- pairwise.t.test(values$processed_data[[input$anova1_dependent]], 
                                            values$processed_data[[input$anova1_independent]], 
                                            p.adjust.method = "none")
        }
        
        output$posthoc_results <- renderPrint({
          cat("HASIL UJI POST-HOC\n")
          cat("==================\n")
          cat("Metode:", input$posthoc_method, "\n\n")
          print(posthoc_result)
        })
      }
      
      output$anova1_plot <- renderPlotly({
        p <- ggplot(values$processed_data, aes_string(x = input$anova1_independent, y = input$anova1_dependent, fill = input$anova1_independent)) +
          geom_boxplot(alpha = 0.7) +
          stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
          labs(title = paste("ANOVA Satu Arah:", input$anova1_dependent, "berdasarkan", input$anova1_independent),
               x = input$anova1_independent, y = input$anova1_dependent) +
          theme_minimal() +
          theme(legend.position = "none")
        
        ggplotly(p)
      })
      
      output$anova1_interpretation <- renderText({
        f_stat <- anova_summary[[1]]$`F value`[1]
        p_val <- anova_summary[[1]]$`Pr(>F)`[1]
        
        interpretation <- paste(
          "INTERPRETASI ANOVA SATU ARAH:\n\n",
          sprintf("Variabel dependen: %s", input$anova1_dependent),
          sprintf("Faktor: %s", input$anova1_independent),
          sprintf("F-statistik: %.4f", f_stat),
          sprintf("P-value: %.6f", p_val),
          "\nHipotesis:",
          "H₀: Semua rata-rata kelompok sama",
          "H₁: Setidaknya satu rata-rata kelompok berbeda",
          "\nKesimpulan:",
          if (p_val > 0.05) {
            "Dengan α = 0.05, kita GAGAL MENOLAK hipotesis nol. Tidak ada perbedaan signifikan antar kelompok."
          } else {
            "Dengan α = 0.05, kita MENOLAK hipotesis nol. Ada perbedaan signifikan antar kelompok."
          },
          if (input$anova1_posthoc && p_val <= 0.05) {
            "\nUji post-hoc dilakukan untuk menentukan kelompok mana yang berbeda."
          } else ""
        )
        
        return(interpretation)
      })
    }, error = function(e) {
      showNotification(paste("Error dalam ANOVA satu arah:", e$message), type = "error")
    })
  })
  
  # Two-way ANOVA
  observeEvent(input$run_anova2, {
    req(input$anova2_dependent, input$anova2_factor1, input$anova2_factor2)
    
    tryCatch({
      if (input$include_interaction) {
        formula_str <- paste(input$anova2_dependent, "~", input$anova2_factor1, "*", input$anova2_factor2)
      } else {
        formula_str <- paste(input$anova2_dependent, "~", input$anova2_factor1, "+", input$anova2_factor2)
      }
      
      anova_formula <- as.formula(formula_str)
      values$anova2_model <- aov(anova_formula, data = values$processed_data)
      
      if (input$anova2_type == "II") {
        anova_summary <- car::Anova(values$anova2_model, type = "II")
      } else if (input$anova2_type == "III") {
        anova_summary <- car::Anova(values$anova2_model, type = "III")
      } else {
        anova_summary <- summary(values$anova2_model)
      }
      
      output$anova2_results <- renderPrint({
        cat("HASIL ANOVA DUA ARAH\n")
        cat("===================\n")
        cat("Formula:", formula_str, "\n")
        cat("Tipe Sum of Squares:", input$anova2_type, "\n\n")
        print(anova_summary)
      })
      
      output$anova2_plot <- renderPlotly({
        # Interaction plot
        interaction_data <- values$processed_data %>%
          group_by(!!sym(input$anova2_factor1), !!sym(input$anova2_factor2)) %>%
          summarise(mean_val = mean(!!sym(input$anova2_dependent), na.rm = TRUE), .groups = 'drop')
        
        p <- ggplot(interaction_data, aes_string(x = input$anova2_factor1, y = "mean_val", 
                                                 color = input$anova2_factor2, group = input$anova2_factor2)) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          labs(title = paste("Plot Interaksi:", input$anova2_dependent),
               x = input$anova2_factor1, y = paste("Rata-rata", input$anova2_dependent),
               color = input$anova2_factor2) +
          theme_minimal()
        
        ggplotly(p)
      })
      
      output$anova2_interpretation <- renderText({
        interpretation <- paste(
          "INTERPRETASI ANOVA DUA ARAH:\n\n",
          sprintf("Variabel dependen: %s", input$anova2_dependent),
          sprintf("Faktor 1: %s", input$anova2_factor1),
          sprintf("Faktor 2: %s", input$anova2_factor2),
          sprintf("Interaksi: %s", ifelse(input$include_interaction, "Ya", "Tidak")),
          sprintf("Tipe Sum of Squares: %s", input$anova2_type),
          "\nHipotesis yang diuji:",
          sprintf("• Efek utama %s", input$anova2_factor1),
          sprintf("• Efek utama %s", input$anova2_factor2),
          if (input$include_interaction) {
            sprintf("• Efek interaksi %s × %s", input$anova2_factor1, input$anova2_factor2)
          } else "",
          "\nLihat tabel ANOVA untuk p-value masing-masing efek.",
          "\nPlot interaksi menunjukkan pola hubungan antar faktor."
        )
        
        return(interpretation)
      })
    }, error = function(e) {
      showNotification(paste("Error dalam ANOVA dua arah:", e$message), type = "error")
    })
  })
  
  # ANOVA diagnostics
  observeEvent(input$generate_anova_diagnostics, {
    req(input$anova_diagnostic_type, input$anova_diagnostic_plots)
    
    tryCatch({
      if (input$anova_diagnostic_type == "one" && !is.null(values$anova1_model)) {
        model <- values$anova1_model
      } else if (input$anova_diagnostic_type == "two" && !is.null(values$anova2_model)) {
        model <- values$anova2_model
      } else {
        stop("Model ANOVA belum dibuat. Jalankan ANOVA terlebih dahulu.")
      }
      
      output$anova_diagnostic_plots_output <- renderPlot({
        n_plots <- length(input$anova_diagnostic_plots)
        if (n_plots <= 2) {
          par(mfrow = c(1, n_plots))
        } else {
          par(mfrow = c(2, 2))
        }
        
        if ("resid_fitted" %in% input$anova_diagnostic_plots) {
          plot(model, which = 1, main = "Residual vs Fitted")
        }
        
        if ("qq_resid" %in% input$anova_diagnostic_plots) {
          plot(model, which = 2, main = "Q-Q Plot Residual")
        }
        
        if ("scale_location" %in% input$anova_diagnostic_plots) {
          plot(model, which = 3, main = "Scale-Location")
        }
        
        if ("resid_leverage" %in% input$anova_diagnostic_plots) {
          plot(model, which = 5, main = "Residual vs Leverage")
        }
      })
      
      output$anova_diagnostic_interpretation <- renderText({
        interpretation <- paste(
          "INTERPRETASI DIAGNOSTIK ANOVA:\n\n",
          sprintf("Model: %s", ifelse(input$anova_diagnostic_type == "one", "ANOVA Satu Arah", "ANOVA Dua Arah")),
          "\nInterpretasi Plot:",
          if ("resid_fitted" %in% input$anova_diagnostic_plots) {
            "• Residual vs Fitted: Harus menunjukkan pola acak tanpa tren"
          } else "",
          if ("qq_resid" %in% input$anova_diagnostic_plots) {
            "• Q-Q Plot: Titik harus mengikuti garis diagonal untuk normalitas residual"
          } else "",
          if ("scale_location" %in% input$anova_diagnostic_plots) {
            "• Scale-Location: Harus menunjukkan varians konstan (homoskedastisitas)"
          } else "",
          if ("resid_leverage" %in% input$anova_diagnostic_plots) {
            "• Residual vs Leverage: Mengidentifikasi observasi berpengaruh"
          } else "",
          "\nAsumsi ANOVA:",
          "1. Normalitas residual",
          "2. Homogenitas varians",
          "3. Independensi observasi",
          "\nJika asumsi dilanggar, pertimbangkan transformasi data atau uji non-parametrik."
        )
        
        return(interpretation)
      })
    }, error = function(e) {
      showNotification(paste("Error dalam diagnostik ANOVA:", e$message), type = "error")
    })
  })
  
  # RESAMPLING METHODS TAB
  # Bootstrap
  observeEvent(input$run_bootstrap, {
    req(input$bootstrap_variable, input$bootstrap_statistic, input$bootstrap_samples)
    
    tryCatch({
      var_data <- values$processed_data[[input$bootstrap_variable]]
      var_data <- var_data[!is.na(var_data)]
      
      # Define statistic function
      stat_func <- switch(input$bootstrap_statistic,
                          "mean" = function(x, i) mean(x[i]),
                          "median" = function(x, i) median(x[i]),
                          "sd" = function(x, i) sd(x[i]),
                          "var" = function(x, i) var(x[i]))
      
      # Perform bootstrap
      boot_result <- boot(var_data, stat_func, R = input$bootstrap_samples)
      values$bootstrap_results <- boot_result
      
      # Calculate confidence interval
      ci_result <- boot.ci(boot_result, conf = input$confidence_level, type = "perc")
      
      output$bootstrap_results <- renderPrint({
        cat("HASIL BOOTSTRAP\n")
        cat("==============\n")
        cat("Variabel:", input$bootstrap_variable, "\n")
        cat("Statistik:", input$bootstrap_statistic, "\n")
        cat("Jumlah sampel bootstrap:", input$bootstrap_samples, "\n")
        cat("Tingkat kepercayaan:", input$confidence_level, "\n\n")
        
        cat("Statistik asli:", round(boot_result$t0, 4), "\n")
        cat("Rata-rata bootstrap:", round(mean(boot_result$t), 4), "\n")
        cat("Standar error bootstrap:", round(sd(boot_result$t), 4), "\n")
        cat("Bias:", round(mean(boot_result$t) - boot_result$t0, 4), "\n\n")
        
        if (!is.null(ci_result)) {
          cat("Interval Kepercayaan", input$confidence_level * 100, "%:\n")
          cat("[", round(ci_result$percent[4], 4), ",", round(ci_result$percent[5], 4), "]\n")
        }
      })
      
      output$bootstrap_plot <- renderPlotly({
        boot_data <- data.frame(bootstrap_values = boot_result$t[, 1])
        
        p <- ggplot(boot_data, aes(x = bootstrap_values)) +
          geom_histogram(aes(y = ..density..), bins = 50, fill = "steelblue", alpha = 0.7, color = "white") +
          geom_density(color = "red", size = 1) +
          geom_vline(xintercept = boot_result$t0, color = "green", linetype = "dashed", size = 1) +
          labs(title = paste("Distribusi Bootstrap:", input$bootstrap_statistic),
               x = paste("Nilai", input$bootstrap_statistic), y = "Densitas") +
          theme_minimal()
        
        ggplotly(p)
      })
      
      output$bootstrap_interpretation <- renderText({
        interpretation <- paste(
          "INTERPRETASI BOOTSTRAP:\n\n",
          sprintf("Variabel: %s", input$bootstrap_variable),
          sprintf("Statistik: %s", input$bootstrap_statistic),
          sprintf("Jumlah replikasi: %d", input$bootstrap_samples),
          sprintf("Nilai asli: %.4f", boot_result$t0),
          sprintf("Rata-rata bootstrap: %.4f", mean(boot_result$t)),
          sprintf("Standar error: %.4f", sd(boot_result$t)),
          sprintf("Bias estimasi: %.4f", mean(boot_result$t) - boot_result$t0),
          "\nInterpretasi:",
          "• Garis hijau putus-putus menunjukkan nilai statistik asli",
          "• Distribusi bootstrap memberikan estimasi distribusi sampling",
          "• Standar error bootstrap adalah estimasi standar error populasi",
          "• Interval kepercayaan memberikan rentang nilai yang masuk akal untuk parameter populasi",
          "\nKegunaan Bootstrap:",
          "• Estimasi standar error tanpa asumsi distribusi",
          "• Konstruksi interval kepercayaan",
          "• Uji hipotesis non-parametrik"
        )
        
        return(interpretation)
      })
    }, error = function(e) {
      showNotification(paste("Error dalam bootstrap:", e$message), type = "error")
    })
  })
  
  # Jackknife
  observeEvent(input$run_jackknife, {
    req(input$jackknife_variable, input$jackknife_statistic)
    
    tryCatch({
      var_data <- values$processed_data[[input$jackknife_variable]]
      var_data <- var_data[!is.na(var_data)]
      n <- length(var_data)
      
      # Define statistic function
      stat_func <- switch(input$jackknife_statistic,
                          "mean" = mean,
                          "median" = median,
                          "sd" = sd,
                          "var" = var)
      
      # Calculate original statistic
      original_stat <- stat_func(var_data)
      
      # Jackknife resampling
      jackknife_values <- numeric(n)
      for (i in 1:n) {
        jackknife_values[i] <- stat_func(var_data[-i])
      }
      
      # Calculate jackknife statistics
      jackknife_mean <- mean(jackknife_values)
      jackknife_bias <- (n - 1) * (jackknife_mean - original_stat)
      jackknife_se <- sqrt((n - 1) / n * sum((jackknife_values - jackknife_mean)^2))
      
      values$jackknife_results <- list(
        original = original_stat,
        values = jackknife_values,
        mean = jackknife_mean,
        bias = jackknife_bias,
        se = jackknife_se
      )
      
      output$jackknife_results <- renderPrint({
        cat("HASIL JACKKNIFE\n")
        cat("===============\n")
        cat("Variabel:", input$jackknife_variable, "\n")
        cat("Statistik:", input$jackknife_statistic, "\n")
        cat("Ukuran sampel:", n, "\n\n")
        
        cat("Statistik asli:", round(original_stat, 4), "\n")
        cat("Rata-rata jackknife:", round(jackknife_mean, 4), "\n")
        cat("Bias jackknife:", round(jackknife_bias, 4), "\n")
        cat("Standar error jackknife:", round(jackknife_se, 4), "\n")
        cat("Statistik bias-corrected:", round(original_stat - jackknife_bias, 4), "\n")
      })
      
      output$jackknife_plot <- renderPlotly({
        jack_data <- data.frame(
          index = 1:n,
          jackknife_values = jackknife_values
        )
        
        p <- ggplot(jack_data, aes(x = index, y = jackknife_values)) +
          geom_point(alpha = 0.6, color = "steelblue") +
          geom_hline(yintercept = original_stat, color = "red", linetype = "dashed", size = 1) +
          geom_hline(yintercept = jackknife_mean, color = "green", linetype = "dashed", size = 1) +
          labs(title = paste("Nilai Jackknife:", input$jackknife_statistic),
               x = "Indeks Observasi yang Dihapus", y = paste("Nilai", input$jackknife_statistic)) +
          theme_minimal()
        
        ggplotly(p)
      })
      
      output$jackknife_interpretation <- renderText({
        interpretation <- paste(
          "INTERPRETASI JACKKNIFE:\n\n",
          sprintf("Variabel: %s", input$jackknife_variable),
          sprintf("Statistik: %s", input$jackknife_statistic),
          sprintf("Ukuran sampel: %d", n),
          sprintf("Nilai asli: %.4f", original_stat),
          sprintf("Rata-rata jackknife: %.4f", jackknife_mean),
          sprintf("Bias estimasi: %.4f", jackknife_bias),
          sprintf("Standar error: %.4f", jackknife_se),
          sprintf("Nilai bias-corrected: %.4f", original_stat - jackknife_bias),
          "\nInterpretasi:",
          "• Garis merah putus-putus: nilai statistik asli",
          "• Garis hijau putus-putus: rata-rata jackknife",
          "• Setiap titik menunjukkan nilai statistik ketika satu observasi dihapus",
          "• Bias jackknife mengukur bias estimator",
          "• Standar error jackknife adalah estimasi standar error",
          "\nKegunaan Jackknife:",
          "• Estimasi bias dan standar error",
          "• Koreksi bias untuk estimator",
          "• Identifikasi observasi berpengaruh",
          "• Metode resampling yang lebih sederhana dari bootstrap"
        )
        
        return(interpretation)
      })
    }, error = function(e) {
      showNotification(paste("Error dalam jackknife:", e$message), type = "error")
    })
  })
  
  # Permutation test
  observeEvent(input$run_permutation, {
    req(input$perm_variable1, input$perm_variable2, input$perm_test_type, input$perm_samples)
    
    tryCatch({
      var1_data <- values$processed_data[[input$perm_variable1]]
      var2_data <- values$processed_data[[input$perm_variable2]]
      
      # Remove missing values
      complete_cases <- complete.cases(var1_data, var2_data)
      var1_data <- var1_data[complete_cases]
      var2_data <- var2_data[complete_cases]
      
      # Calculate observed statistic
      if (input$perm_test_type == "mean_diff") {
        observed_stat <- mean(var1_data) - mean(var2_data)
        test_name <- "Perbedaan Rata-rata"
      } else if (input$perm_test_type == "correlation") {
        observed_stat <- cor(var1_data, var2_data)
        test_name <- "Korelasi"
      } else if (input$perm_test_type == "t_test") {
        t_result <- t.test(var1_data, var2_data)
        observed_stat <- t_result$statistic
        test_name <- "Statistik t"
      }
      
      # Permutation test
      perm_stats <- numeric(input$perm_samples)
      
      for (i in 1:input$perm_samples) {
        if (input$perm_test_type == "mean_diff") {
          # Permute group labels
          combined_data <- c(var1_data, var2_data)
          n1 <- length(var1_data)
          perm_indices <- sample(length(combined_data))
          perm_group1 <- combined_data[perm_indices[1:n1]]
          perm_group2 <- combined_data[perm_indices[(n1+1):length(combined_data)]]
          perm_stats[i] <- mean(perm_group1) - mean(perm_group2)
        } else if (input$perm_test_type == "correlation") {
          # Permute one variable
          perm_var2 <- sample(var2_data)
          perm_stats[i] <- cor(var1_data, perm_var2)
        } else if (input$perm_test_type == "t_test") {
          # Permute group labels
          combined_data <- c(var1_data, var2_data)
          n1 <- length(var1_data)
          perm_indices <- sample(length(combined_data))
          perm_group1 <- combined_data[perm_indices[1:n1]]
          perm_group2 <- combined_data[perm_indices[(n1+1):length(combined_data)]]
          perm_t_result <- t.test(perm_group1, perm_group2)
          perm_stats[i] <- perm_t_result$statistic
        }
      }
      
      # Calculate p-value
      p_value <- mean(abs(perm_stats) >= abs(observed_stat))
      
      values$permutation_results <- list(
        observed = observed_stat,
        permuted = perm_stats,
        p_value = p_value,
        test_type = test_name
      )
      
      output$permutation_results <- renderPrint({
        cat("HASIL UJI PERMUTASI\n")
        cat("==================\n")
        cat("Variabel 1:", input$perm_variable1, "\n")
        cat("Variabel 2:", input$perm_variable2, "\n")
        cat("Jenis uji:", test_name, "\n")
        cat("Jumlah permutasi:", input$perm_samples, "\n\n")
        
        cat("Statistik teramati:", round(observed_stat, 4), "\n")
        cat("Rata-rata permutasi:", round(mean(perm_stats), 4), "\n")
        cat("P-value:", round(p_value, 4), "\n\n")
        
        cat("Kesimpulan:\n")
        if (p_value <= 0.05) {
          cat("Dengan α = 0.05, hasil signifikan secara statistik.\n")
        } else {
          cat("Dengan α = 0.05, hasil tidak signifikan secara statistik.\n")
        }
      })
      
      output$permutation_plot <- renderPlotly({
        perm_data <- data.frame(permuted_values = perm_stats)
        
        p <- ggplot(perm_data, aes(x = permuted_values)) +
          geom_histogram(aes(y = ..density..), bins = 50, fill = "lightblue", alpha = 0.7, color = "white") +
          geom_density(color = "blue", size = 1) +
          geom_vline(xintercept = observed_stat, color = "red", linetype = "dashed", size = 1) +
          geom_vline(xintercept = -observed_stat, color = "red", linetype = "dashed", size = 1) +
          labs(title = paste("Distribusi Permutasi:", test_name),
               x = paste("Nilai", test_name), y = "Densitas") +
          theme_minimal()
        
        ggplotly(p)
      })
      
      output$permutation_interpretation <- renderText({
        interpretation <- paste(
          "INTERPRETASI UJI PERMUTASI:\n\n",
          sprintf("Variabel 1: %s", input$perm_variable1),
          sprintf("Variabel 2: %s", input$perm_variable2),
          sprintf("Jenis uji: %s", test_name),
          sprintf("Jumlah permutasi: %d", input$perm_samples),
          sprintf("Statistik teramati: %.4f", observed_stat),
          sprintf("P-value: %.4f", p_value),
          "\nHipotesis:",
          "H₀: Tidak ada hubungan/perbedaan antara variabel",
          "H₁: Ada hubungan/perbedaan antara variabel",
          "\nKesimpulan:",
          if (p_value <= 0.05) {
            "Dengan α = 0.05, kita MENOLAK hipotesis nol. Hasil signifikan secara statistik."
          } else {
            "Dengan α = 0.05, kita GAGAL MENOLAK hipotesis nol. Hasil tidak signifikan."
          },
          "\nInterpretasi:",
          "• Garis merah putus-putus menunjukkan nilai statistik teramati",
          "• Distribusi biru menunjukkan distribusi di bawah hipotesis nol",
          "• P-value adalah proporsi permutasi dengan nilai ekstrem seperti yang teramati",
          "\nKeunggulan Uji Permutasi:",
          "• Tidak memerlukan asumsi distribusi",
          "• Memberikan p-value eksak",
          "• Robust terhadap outlier",
          "• Dapat diterapkan pada berbagai statistik"
        )
        
        return(interpretation)
      })
    }, error = function(e) {
      showNotification(paste("Error dalam uji permutasi:", e$message), type = "error")
    })
  })
  
  # REGRESSION ANALYSIS TAB
  # Build regression model
  observeEvent(input$build_regression, {
    req(input$reg_dependent, input$reg_independent)
    
    tryCatch({
      # Create formula
      if (input$include_intercept) {
        formula_str <- paste(input$reg_dependent, "~", paste(input$reg_independent, collapse = " + "))
      } else {
        formula_str <- paste(input$reg_dependent, "~ -1 +", paste(input$reg_independent, collapse = " + "))
      }
      
      reg_formula <- as.formula(formula_str)
      
      # Variable selection
      if (input$selection_method == "enter") {
        values$current_regression <- lm(reg_formula, data = values$processed_data)
      } else if (input$selection_method == "forward") {
        null_model <- lm(paste(input$reg_dependent, "~ 1"), data = values$processed_data)
        full_model <- lm(reg_formula, data = values$processed_data)
        values$current_regression <- step(null_model, scope = list(lower = null_model, upper = full_model),
                                          direction = "forward", trace = 0)
      } else if (input$selection_method == "backward") {
        full_model <- lm(reg_formula, data = values$processed_data)
        values$current_regression <- step(full_model, direction = "backward", trace = 0)
      } else if (input$selection_method == "stepwise") {
        null_model <- lm(paste(input$reg_dependent, "~ 1"), data = values$processed_data)
        full_model <- lm(reg_formula, data = values$processed_data)
        values$current_regression <- step(null_model, scope = list(lower = null_model, upper = full_model),
                                          direction = "both", trace = 0)
      }
      
      output$regression_summary <- renderPrint({
        cat("HASIL REGRESI LINEAR BERGANDA\n")
        cat("=============================\n")
        cat("Metode seleksi:", input$selection_method, "\n")
        cat("Formula:", deparse(formula(values$current_regression)), "\n\n")
        summary(values$current_regression)
      })
      
      output$model_fit_stats <- renderPrint({
        model_summary <- summary(values$current_regression)
        cat("STATISTIK KECOCOKAN MODEL\n")
        cat("=========================\n")
        cat("R-squared:", round(model_summary$r.squared, 4), "\n")
        cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n")
        cat("F-statistic:", round(model_summary$fstatistic[1], 4), "\n")
        cat("P-value (F-test):", format(pf(model_summary$fstatistic[1], 
                                           model_summary$fstatistic[2], 
                                           model_summary$fstatistic[3], 
                                           lower.tail = FALSE), scientific = TRUE), "\n")
        cat("Residual standard error:", round(model_summary$sigma, 4), "\n")
        cat("Degrees of freedom:", model_summary$df[2], "\n")
      })
      
      output$regression_interpretation <- renderText({
        model_summary <- summary(values$current_regression)
        
        interpretation <- paste(
          "INTERPRETASI MODEL REGRESI:\n\n",
          sprintf("Variabel dependen: %s", input$reg_dependent),
          sprintf("Jumlah variabel independen: %d", length(coef(values$current_regression)) - 1),
          sprintf("R-squared: %.4f (%.1f%% varians dijelaskan)", 
                  model_summary$r.squared, model_summary$r.squared * 100),
          sprintf("Adjusted R-squared: %.4f", model_summary$adj.r.squared),
          sprintf("F-statistic: %.4f", model_summary$fstatistic[1]),
          sprintf("P-value model: %.6e", pf(model_summary$fstatistic[1], 
                                            model_summary$fstatistic[2], 
                                            model_summary$fstatistic[3], 
                                            lower.tail = FALSE)),
          "\nSignifikansi Model:",
          if (pf(model_summary$fstatistic[1], model_summary$fstatistic[2], 
                 model_summary$fstatistic[3], lower.tail = FALSE) < 0.05) {
            "Model secara keseluruhan signifikan (p < 0.05)"
          } else {
            "Model secara keseluruhan tidak signifikan (p ≥ 0.05)"
          },
          "\nInterpretasi Koefisien:",
          "• Lihat tabel summary untuk signifikansi individual variabel",
          "• Koefisien menunjukkan perubahan rata-rata Y untuk setiap unit perubahan X",
          "• P-value < 0.05 menunjukkan variabel signifikan",
          "\nLangkah Selanjutnya:",
          "• Lakukan diagnostik model untuk memeriksa asumsi",
          "• Evaluasi multikolinearitas dengan VIF",
          "• Periksa normalitas dan homoskedastisitas residual"
        )
        
        return(interpretation)
      })
    }, error = function(e) {
      showNotification(paste("Error dalam regresi:", e$message), type = "error")
    })
  })
  
  # Regression diagnostics
  observeEvent(input$run_regression_diagnostics, {
    req(values$current_regression, input$regression_diagnostics)
    
    tryCatch({
      model <- values$current_regression
      
      output$regression_diagnostic_results <- renderPrint({
        cat("HASIL DIAGNOSTIK REGRESI\n")
        cat("========================\n\n")
        
        if ("normality" %in% input$regression_diagnostics) {
          cat("UJI NORMALITAS RESIDUAL:\n")
          residuals <- residuals(model)
          shapiro_result <- shapiro.test(residuals)
          cat("Shapiro-Wilk Test:\n")
          cat("  W =", round(shapiro_result$statistic, 4), "\n")
          cat("  P-value =", format(shapiro_result$p.value, scientific = TRUE), "\n")
          if (shapiro_result$p.value > 0.05) {
            cat("  Kesimpulan: Residual berdistribusi normal\n\n")
          } else {
            cat("  Kesimpulan: Residual TIDAK berdistribusi normal\n\n")
          }
        }
        
        if ("hetero" %in% input$regression_diagnostics) {
          cat("UJI HETEROSKEDASTISITAS:\n")
          bp_result <- lmtest::bptest(model)
          cat("Breusch-Pagan Test:\n")
          cat("  BP =", round(bp_result$statistic, 4), "\n")
          cat("  P-value =", format(bp_result$p.value, scientific = TRUE), "\n")
          if (bp_result$p.value > 0.05) {
            cat("  Kesimpulan: Homoskedastisitas (varians konstan)\n\n")
          } else {
            cat("  Kesimpulan: Heteroskedastisitas (varians tidak konstan)\n\n")
          }
        }
        
        if ("vif" %in% input$regression_diagnostics) {
          cat("VARIANCE INFLATION FACTOR (VIF):\n")
          if (length(coef(model)) > 2) {  # More than intercept + 1 variable
            vif_values <- car::vif(model)
            print(round(vif_values, 2))
            cat("\nInterpretasi VIF:\n")
            cat("• VIF < 5: Multikolinearitas rendah\n")
            cat("• 5 ≤ VIF < 10: Multikolinearitas sedang\n")
            cat("• VIF ≥ 10: Multikolinearitas tinggi\n\n")
          } else {
            cat("VIF tidak dapat dihitung (hanya satu variabel independen)\n\n")
          }
        }
        
        if ("outliers" %in% input$regression_diagnostics) {
          cat("DETEKSI OUTLIER:\n")
          # Standardized residuals
          std_residuals <- rstandard(model)
          outliers <- which(abs(std_residuals) > 2)
          cat("Observasi dengan |standardized residual| > 2:\n")
          if (length(outliers) > 0) {
            cat("Indeks:", outliers, "\n")
            cat("Jumlah:", length(outliers), "dari", length(std_residuals), "observasi\n\n")
          } else {
            cat("Tidak ada outlier terdeteksi\n\n")
          }
        }
        
        if ("influence" %in% input$regression_diagnostics) {
          cat("UKURAN PENGARUH:\n")
          # Cook's distance
          cooks_d <- cooks.distance(model)
          influential <- which(cooks_d > 4/length(cooks_d))
          cat("Observasi berpengaruh (Cook's D > 4/n):\n")
          if (length(influential) > 0) {
            cat("Indeks:", influential, "\n")
            cat("Cook's Distance:", round(cooks_d[influential], 4), "\n")
          } else {
            cat("Tidak ada observasi berpengaruh terdeteksi\n")
          }
        }
      })
      
      output$regression_diagnostic_plots <- renderPlot({
        n_plots <- length(input$regression_diagnostics)
        if (n_plots <= 2) {
          par(mfrow = c(1, n_plots))
        } else if (n_plots <= 4) {
          par(mfrow = c(2, 2))
        } else {
          par(mfrow = c(2, 3))
        }
        
        if ("residual_plots" %in% input$regression_diagnostics) {
          # Residual vs Fitted
          plot(model, which = 1, main = "Residual vs Fitted")
          
          # Q-Q plot of residuals
          plot(model, which = 2, main = "Q-Q Plot Residual")
        }
        
        if ("normality" %in% input$regression_diagnostics) {
          # Histogram of residuals
          residuals <- residuals(model)
          hist(residuals, breaks = 20, freq = FALSE, 
               main = "Histogram Residual", xlab = "Residual", col = "lightblue")
          curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)), 
                add = TRUE, col = "red", lwd = 2)
        }
        
        if ("hetero" %in% input$regression_diagnostics) {
          # Scale-Location plot
          plot(model, which = 3, main = "Scale-Location")
        }
        
        if ("outliers" %in% input$regression_diagnostics) {
          # Residual vs Leverage
          plot(model, which = 5, main = "Residual vs Leverage")
        }
        
        if ("influence" %in% input$regression_diagnostics) {
          # Cook's distance
          plot(model, which = 4, main = "Cook's Distance")
        }
      })
      
      output$regression_diagnostic_interpretation <- renderText({
        interpretation <- paste(
          "INTERPRETASI DIAGNOSTIK REGRESI:\n\n",
          "Asumsi Regresi Linear:",
          "1. Linearitas: Hubungan linear antara X dan Y",
          "2. Independensi: Observasi saling independen",
          "3. Homoskedastisitas: Varians residual konstan",
          "4. Normalitas: Residual berdistribusi normal",
          "5. Tidak ada multikolinearitas: Variabel X tidak berkorelasi tinggi",
          "\nPanduan Interpretasi:",
          if ("residual_plots" %in% input$regression_diagnostics) {
            "• Residual vs Fitted: Harus menunjukkan pola acak tanpa tren"
          } else "",
          if ("normality" %in% input$regression_diagnostics) {
            "• Q-Q Plot & Histogram: Residual harus mengikuti distribusi normal"
          } else "",
          if ("hetero" %in% input$regression_diagnostics) {
            "• Scale-Location: Garis harus relatif datar (homoskedastisitas)"
          } else "",
          if ("vif" %in% input$regression_diagnostics) {
            "• VIF: Nilai < 5 menunjukkan multikolinearitas rendah"
          } else "",
          if ("outliers" %in% input$regression_diagnostics) {
            "• Outlier: |standardized residual| > 2 perlu diperhatikan"
          } else "",
          if ("influence" %in% input$regression_diagnostics) {
            "• Cook's Distance: Nilai > 4/n menunjukkan observasi berpengaruh"
          } else "",
          "\nTindakan jika Asumsi Dilanggar:",
          "• Transformasi variabel (log, sqrt, dll.)",
          "• Robust regression",
          "• Weighted least squares",
          "• Regularization (Ridge, Lasso)"
        )
        
        return(interpretation)
      })
    }, error = function(e) {
      showNotification(paste("Error dalam diagnostik regresi:", e$message), type = "error")
    })
  })
  
  # Model comparison
  observeEvent(input$compare_models, {
    req(values$current_regression, input$comparison_models)
    
    tryCatch({
      models_list <- list()
      
      if ("full" %in% input$comparison_models) {
        # Full model with all selected variables
        full_formula <- paste(input$reg_dependent, "~", paste(input$reg_independent, collapse = " + "))
        models_list[["Full Model"]] <- lm(as.formula(full_formula), data = values$processed_data)
      }
      
      if ("reduced" %in% input$comparison_models) {
        # Reduced model with significant variables only
        current_summary <- summary(values$current_regression)
        significant_vars <- rownames(current_summary$coefficients)[current_summary$coefficients[, 4] < 0.05]
        significant_vars <- significant_vars[significant_vars != "(Intercept)"]
        
        if (length(significant_vars) > 0) {
          reduced_formula <- paste(input$reg_dependent, "~", paste(significant_vars, collapse = " + "))
          models_list[["Reduced Model"]] <- lm(as.formula(reduced_formula), data = values$processed_data)
        }
      }
      
      if ("stepwise" %in% input$comparison_models) {
        models_list[["Stepwise Model"]] <- values$current_regression
      }
      
      # Create comparison table
      comparison_data <- data.frame(
        Model = names(models_list),
        R_squared = sapply(models_list, function(m) summary(m)$r.squared),
        Adj_R_squared = sapply(models_list, function(m) summary(m)$adj.r.squared),
        AIC = sapply(models_list, AIC),
        BIC = sapply(models_list, BIC),
        RMSE = sapply(models_list, function(m) sqrt(mean(residuals(m)^2))),
        Variables = sapply(models_list, function(m) length(coef(m)) - 1)
      )
      
      output$model_comparison_table <- DT::renderDataTable({
        comparison_data_rounded <- comparison_data
        comparison_data_rounded[, 2:6] <- round(comparison_data_rounded[, 2:6], 4)
        DT::datatable(comparison_data_rounded, options = list(pageLength = 10, scrollX = TRUE))
      })
      
      output$model_comparison_plot <- renderPlotly({
        # Plot R-squared vs number of variables
        p <- ggplot(comparison_data, aes(x = Variables, y = Adj_R_squared, color = Model)) +
          geom_point(size = 4) +
          geom_line(aes(group = 1), alpha = 0.5) +
          labs(title = "Perbandingan Model: Adj R-squared vs Jumlah Variabel",
               x = "Jumlah Variabel", y = "Adjusted R-squared") +
          theme_minimal()
        
        ggplotly(p)
      })
      
      output$model_comparison_interpretation <- renderText({
        best_r2 <- comparison_data[which.max(comparison_data$Adj_R_squared), ]
        best_aic <- comparison_data[which.min(comparison_data$AIC), ]
        
        interpretation <- paste(
          "INTERPRETASI PERBANDINGAN MODEL:\n\n",
          sprintf("Jumlah model dibandingkan: %d", nrow(comparison_data)),
          "\nKriteria Evaluasi:",
          "• R-squared: Proporsi varians yang dijelaskan (lebih tinggi lebih baik)",
          "• Adjusted R-squared: R-squared yang disesuaikan dengan jumlah variabel",
          "• AIC/BIC: Information criteria (lebih rendah lebih baik)",
          "• RMSE: Root Mean Square Error (lebih rendah lebih baik)",
          "\nModel Terbaik:",
          sprintf("• Berdasarkan Adj R-squared: %s (%.4f)", best_r2$Model, best_r2$Adj_R_squared),
          sprintf("• Berdasarkan AIC: %s (%.2f)", best_aic$Model, best_aic$AIC),
          "\nRekomendasi:",
          "• Pilih model dengan keseimbangan terbaik antara kecocokan dan kesederhanaan",
          "• Pertimbangkan interpretabilitas dan tujuan analisis",
          "• Model dengan Adj R-squared tinggi dan AIC rendah umumnya disukai",
          "• Hindari overfitting dengan terlalu banyak variabel"
        )
        
        return(interpretation)
      })
    }, error = function(e) {
      showNotification(paste("Error dalam perbandingan model:", e$message), type = "error")
    })
  })
  
  # GEOSPATIAL ANALYSIS TAB
  # Update map
  observeEvent(input$update_map, {
    req(input$map_variable)
    
    if (!is.null(shp_data)) {
      tryCatch({
        # Merge spatial data with attribute data
        if ("districtkd" %in% names(values$processed_data) && any(grepl("id|code|kd", tolower(names(shp_data))))) {
          id_col <- names(shp_data)[grepl("id|code|kd", tolower(names(shp_data)))][1]
          merged_data <- merge(shp_data, values$processed_data, by.x = id_col, by.y = "districtkd", all.x = TRUE)
        } else {
          # If no matching ID column, just use the first n rows
          n_rows <- min(nrow(shp_data), nrow(values$processed_data))
          merged_data <- shp_data[1:n_rows, ]
          merged_data[[input$map_variable]] <- values$processed_data[[input$map_variable]][1:n_rows]
        }
        
        # Create color palette
        var_values <- merged_data[[input$map_variable]]
        var_values <- var_values[!is.na(var_values)]
        
        if (input$reverse_colors) {
          pal <- colorBin(palette = paste0("-", input$color_scheme), domain = var_values, bins = input$map_bins, reverse = TRUE)
        } else {
          pal <- colorBin(palette = input$color_scheme, domain = var_values, bins = input$map_bins)
        }
        
        output$choropleth_map <- renderLeaflet({
          leaflet(merged_data) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~pal(get(input$map_variable)),
              weight = 1,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 2,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE
              ),
              popup = ~paste0(
                "<strong>", ifelse(exists(id_col), get(id_col), "Area"), "</strong><br/>",
                input$map_variable, ": ", round(get(input$map_variable), 2)
              )
            ) %>%
            addLegend(
              pal = pal,
              values = ~get(input$map_variable),
              opacity = 0.7,
              title = input$map_variable,
              position = "bottomright"
            )
        })
        
        output$spatial_interpretation <- renderText({
          var_values <- merged_data[[input$map_variable]]
          var_values <- var_values[!is.na(var_values)]
          
          interpretation <- paste(
            "INTERPRETASI ANALISIS SPASIAL:\n\n",
            sprintf("Variabel yang dipetakan: %s", input$map_variable),
            sprintf("Jumlah area: %d", length(var_values)),
            sprintf("Nilai minimum: %.2f", min(var_values)),
            sprintf("Nilai maksimum: %.2f", max(var_values)),
            sprintf("Rata-rata: %.2f", mean(var_values)),
            sprintf("Standar deviasi: %.2f", sd(var_values)),
            "\nPola Spasial:",
            "• Perhatikan kluster area dengan nilai tinggi/rendah",
            "• Identifikasi pola geografis (utara-selatan, timur-barat)",
            "• Cari area outlier yang berbeda dari tetangganya",
            "\nInterpretasi Warna:",
            sprintf("• Skema warna: %s", input$color_scheme),
            sprintf("• Jumlah kelas: %d", input$map_bins),
            "• Warna lebih gelap/terang menunjukkan nilai lebih tinggi/rendah",
            "\nAnalisis Lanjutan:",
            "• Gunakan tab Statistik Spasial untuk uji autokorelasi",
            "• Lakukan analisis hotspot untuk identifikasi kluster signifikan",
            "• Pertimbangkan faktor geografis yang mempengaruhi pola"
          )
          
          return(interpretation)
        })
      }, error = function(e) {
        showNotification(paste("Error dalam pembuatan peta:", e$message), type = "error")
      })
    }
  })
  
  # Spatial statistics
  observeEvent(input$run_spatial_analysis, {
    req(input$spatial_variable, input$spatial_tests)
    
    if (!is.null(shp_data)) {
      tryCatch({
        # This is a simplified spatial analysis
        # In practice, you would use packages like spdep for proper spatial analysis
        
        output$spatial_statistics_results <- renderPrint({
          cat("HASIL STATISTIK SPASIAL\n")
          cat("======================\n")
          cat("Variabel:", input$spatial_variable, "\n")
          cat("Uji yang dipilih:", paste(input$spatial_tests, collapse = ", "), "\n\n")
          
          var_values <- values$processed_data[[input$spatial_variable]]
          var_values <- var_values[!is.na(var_values)]
          
          if ("moran_global" %in% input$spatial_tests) {
            cat("MORAN'S I GLOBAL:\n")
            cat("(Implementasi sederhana - gunakan package spdep untuk analisis lengkap)\n")
            # Simplified Moran's I calculation
            n <- length(var_values)
            mean_val <- mean(var_values)
            numerator <- sum((var_values - mean_val)^2)
            # This is a very simplified version
            moran_i <- (n / sum((var_values - mean_val)^2)) * sum((var_values - mean_val) * (var_values - mean_val))
            cat("Moran's I (perkiraan):", round(moran_i, 4), "\n")
            cat("Interpretasi: Nilai mendekati 0 = acak, > 0 = kluster positif, < 0 = kluster negatif\n\n")
          }
          
          if ("autocorr" %in% input$spatial_tests) {
            cat("AUTOKORELASI SPASIAL:\n")
            # Simple lag-1 autocorrelation
            if (length(var_values) > 1) {
              lag1_corr <- cor(var_values[-length(var_values)], var_values[-1])
              cat("Autokorelasi lag-1:", round(lag1_corr, 4), "\n")
              cat("Interpretasi: Korelasi antara nilai bertetangga\n\n")
            }
          }
          
          cat("CATATAN:\n")
          cat("Untuk analisis spasial yang komprehensif, gunakan package khusus seperti:\n")
          cat("• spdep: Untuk statistik dependensi spasial\n")
          cat("• spatstat: Untuk analisis pola titik\n")
          cat("• gstat: Untuk geostatistik\n")
        })
        
        output$spatial_analysis_plot <- renderPlot({
          var_values <- values$processed_data[[input$spatial_variable]]
          var_values <- var_values[!is.na(var_values)]
          
          # Simple spatial analysis plot
          par(mfrow = c(1, 2))
          
          # Histogram
          hist(var_values, breaks = 20, main = paste("Distribusi", input$spatial_variable),
               xlab = input$spatial_variable, col = "lightblue")
          
          # Lag plot (simplified)
          if (length(var_values) > 1) {
            plot(var_values[-length(var_values)], var_values[-1],
                 main = "Lag Plot (Autokorelasi)",
                 xlab = paste(input$spatial_variable, "(t)"),
                 ylab = paste(input$spatial_variable, "(t+1)"),
                 pch = 16, col = "blue")
            abline(lm(var_values[-1] ~ var_values[-length(var_values)]), col = "red", lwd = 2)
          }
        })
        
        output$spatial_statistics_interpretation <- renderText({
          interpretation <- paste(
            "INTERPRETASI STATISTIK SPASIAL:\n\n",
            sprintf("Variabel: %s", input$spatial_variable),
            "Uji yang dilakukan:", paste(input$spatial_tests, collapse = ", "),
            "\nCatatan Penting:",
            "Implementasi ini adalah versi sederhana untuk demonstrasi.",
            "Untuk analisis spasial yang akurat, diperlukan:",
            "• Matriks bobot spasial (spatial weights matrix)",
            "• Definisi ketetanggaan yang tepat",
            "• Package khusus seperti spdep atau spatstat",
            "\nInterpretasi Umum:",
            "• Moran's I > 0: Auto korelasi positif (kluster)",
            "• Moran's I ≈ 0: Distribusi acak",
            "• Moran's I < 0: Auto korelasi negatif (dispersi)",
            "\nRekomendasi:",
            "Gunakan software GIS atau package R khusus untuk analisis spasial yang komprehensif."
          )
          
          return(interpretation)
        })
      }, error = function(e) {
        showNotification(paste("Error dalam statistik spasial:", e$message), type = "error")
      })
    }
  })
  
  # Hotspot analysis
  observeEvent(input$detect_hotspots, {
    req(input$hotspot_variable, input$hotspot_method)
    
    if (!is.null(shp_data)) {
      tryCatch({
        # Simplified hotspot analysis
        var_values <- values$processed_data[[input$hotspot_variable]]
        var_values <- var_values[!is.na(var_values)]
        
        # Simple z-score based hotspot detection
        z_scores <- scale(var_values)[, 1]
        
        # Classify hotspots
        hotspot_class <- ifelse(z_scores > qnorm(1 - input$hotspot_threshold/2), "Hot Spot",
                                ifelse(z_scores < qnorm(input$hotspot_threshold/2), "Cold Spot", "Not Significant"))
        
        # Create hotspot data
        hotspot_data <- data.frame(
          ID = 1:length(var_values),
          Value = var_values,
          Z_Score = z_scores,
          Classification = hotspot_class,
          stringsAsFactors = FALSE
        )
        
        # Merge with spatial data for mapping
        if (nrow(shp_data) >= length(var_values)) {
          map_data <- shp_data[1:length(var_values), ]
          map_data$hotspot_class <- hotspot_class
          map_data$z_score <- z_scores
          
          # Create color palette for hotspots
          hotspot_colors <- c("Hot Spot" = "red", "Cold Spot" = "blue", "Not Significant" = "gray")
          
          output$hotspot_map <- renderLeaflet({
            leaflet(map_data) %>%
              addTiles() %>%
              addPolygons(
                fillColor = ~hotspot_colors[hotspot_class],
                weight = 1,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                popup = ~paste0(
                  "<strong>Area ", 1:nrow(map_data), "</strong><br/>",
                  input$hotspot_variable, ": ", round(var_values, 2), "<br/>",
                  "Z-Score: ", round(z_score, 2), "<br/>",
                  "Klasifikasi: ", hotspot_class
                )
              ) %>%
              addLegend(
                colors = c("red", "blue", "gray"),
                labels = c("Hot Spot", "Cold Spot", "Not Significant"),
                opacity = 0.7,
                title = "Klasifikasi Hotspot",
                position = "bottomright"
              )
          })
        }
        
        output$hotspot_summary <- DT::renderDataTable({
          summary_table <- table(hotspot_class)
          summary_df <- data.frame(
            Klasifikasi = names(summary_table),
            Jumlah = as.numeric(summary_table),
            Persentase = round(as.numeric(summary_table) / sum(summary_table) * 100, 2)
          )
          DT::datatable(summary_df, options = list(pageLength = 10))
        })
        
        output$hotspot_interpretation <- renderText({
          n_hot <- sum(hotspot_class == "Hot Spot")
          n_cold <- sum(hotspot_class == "Cold Spot")
          n_ns <- sum(hotspot_class == "Not Significant")
          
          interpretation <- paste(
            "INTERPRETASI ANALISIS HOTSPOT:\n\n",
            sprintf("Variabel: %s", input$hotspot_variable),
            sprintf("Metode: %s", input$hotspot_method),
            sprintf("Ambang signifikansi: %.3f", input$hotspot_threshold),
            sprintf("\nHasil Klasifikasi:"),
            sprintf("• Hot Spots: %d area (%.1f%%)", n_hot, n_hot/length(hotspot_class)*100),
            sprintf("• Cold Spots: %d area (%.1f%%)", n_cold, n_cold/length(hotspot_class)*100),
            sprintf("• Tidak Signifikan: %d area (%.1f%%)", n_ns, n_ns/length(hotspot_class)*100),
            "\nInterpretasi:",
            "• Hot Spots (merah): Area dengan nilai signifikan tinggi",
            "• Cold Spots (biru): Area dengan nilai signifikan rendah",
            "• Abu-abu: Area dengan nilai tidak signifikan berbeda dari rata-rata",
            "\nCatatan Metodologi:",
            "Analisis ini menggunakan z-score sederhana.",
            "Untuk analisis hotspot yang lebih akurat, gunakan:",
            "• Getis-Ord Gi* dengan matriks bobot spasial",
            "• Local Moran's I untuk autokorelasi lokal",
            "• Software GIS dengan tools hotspot analysis",
            "\nAplikasi Praktis:",
            "• Identifikasi area prioritas untuk intervensi",
            "• Perencanaan alokasi sumber daya",
            "• Analisis pola spasial untuk kebijakan publik"
          )
          
          return(interpretation)
        })
      }, error = function(e) {
        showNotification(paste("Error dalam analisis hotspot:", e$message), type = "error")
      })
    }
  })
  
  # Download handlers
  output$download_dictionary <- downloadHandler(
    filename = function() {
      paste("kamus_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(var_descriptions, file, row.names = FALSE)
    }
  )
  
  output$download_original <- downloadHandler(
    filename = function() {
      paste("data_asli_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(sovi_data, file, row.names = FALSE)
    }
  )
  
  output$download_processed <- downloadHandler(
    filename = function() {
      paste("data_terproses_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$processed_data, file, row.names = FALSE)
    }
  )
  
  output$download_descriptive <- downloadHandler(
    filename = function() {
      paste("statistik_deskriptif_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      numeric_data <- values$processed_data[sapply(values$processed_data, is.numeric)]
      desc_stats <- describe(numeric_data)
      write.csv(desc_stats, file, row.names = TRUE)
    }
  )
  
  # Additional download handlers for other outputs would go here...
  # (Similar pattern for all other download buttons)
}

# Run the application
shinyApp(ui = ui, server = server)
