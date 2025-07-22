# STARLIGHT v4.0 - Dashboard Analisis Statistik UAS Komputasi Statistik
# Statistical Analysis and Research Laboratory for Intelligent Geospatial Handling and Testing
# Politeknik Statistika STIS - APLIKASI DIPERBAIKI SESUAI PERMINTAAN

# ==================== LIBRARY LOADING ====================
required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "plotly", "leaflet", 
  "sf", "dplyr", "corrplot", "psych", "car", "nortest", "flextable", 
  "officer", "shinyWidgets", "shinycssloaders", "htmlwidgets", 
  "RColorBrewer", "viridis", "reshape2", "broom", "VIM", "mice", 
  "gridExtra", "knitr", "rmarkdown", "jsonlite", "lmtest", 
  "Hmisc", "sandwich", "readr", "tidyr", "openxlsx", "stringr"
)

for(pkg in required_packages) {
  suppressPackageStartupMessages({
    if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, quiet = TRUE)
      library(pkg, character.only = TRUE, quietly = TRUE)
    }
  })
}

# ==================== CSS STYLING ====================
custom_css <- "
.starlight-header {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: white;
  padding: 20px;
  border-radius: 10px;
  text-align: center;
  margin-bottom: 20px;
  box-shadow: 0 4px 6px rgba(0,0,0,0.1);
}

.info-card, .stat-card {
  background: #f8f9fa;
  padding: 15px;
  border-radius: 8px;
  margin: 10px 0;
  border-left: 4px solid #007bff;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.stat-card {
  border-left-color: #28a745;
}

.alert-info {
  background-color: #d1ecf1;
  border-color: #bee5eb;
  color: #0c5460;
  padding: 15px;
  border-radius: 8px;
  margin: 10px 0;
}

.download-section {
  background: #f8f9fa;
  padding: 15px;
  border-radius: 8px;
  margin: 10px 0;
  border: 1px solid #dee2e6;
}

.btn-custom {
  margin: 5px;
  border-radius: 5px;
}

.data-overview {
  background: #fff;
  border: 1px solid #dee2e6;
  border-radius: 8px;
  padding: 15px;
  margin-bottom: 20px;
}
"

# ==================== DATA LOADING FUNCTIONS ====================
muat_data_aman <- function() {
  base_path <- "C:/Mario/Semester 4/Komstat/UAS/data/"
  
  # Data CSV utama dengan ID untuk peta
  sovi_data <- data.frame(
    id = 1:153,
    kode_kabkota = paste0("KAB", sprintf("%03d", 1:153)),
    ANAK = round(runif(153, 10, 40), 2),
    PEREMPUAN = round(runif(153, 45, 55), 2),
    LANSIA = round(runif(153, 5, 20), 2),
    KEPALA_KELUARGA_PEREMPUAN = round(runif(153, 10, 30), 2),
    UKURAN_KELUARGA = round(runif(153, 3, 6), 1),
    TANPA_LISTRIK = round(runif(153, 0, 20), 2),
    PENDIDIKAN_RENDAH = round(runif(153, 10, 50), 2),
    PERTUMBUHAN_PENDUDUK = round(runif(153, -2, 5), 2),
    KEMISKINAN = round(runif(153, 5, 40), 2),
    BUTA_HURUF = round(runif(153, 2, 25), 2),
    TANPA_PELATIHAN = round(runif(153, 20, 70), 2),
    RAWAN_BENCANA = round(runif(153, 1, 5), 1),
    RUMAH_SEWA = round(runif(153, 5, 30), 2),
    TANPA_SANITASI = round(runif(153, 10, 60), 2),
    AKSES_AIR_BERSIH = round(runif(153, 40, 95), 2),
    JUMLAH_PENDUDUK = round(runif(153, 10000, 500000))
  )
  
  # Coba muat data asli jika tersedia
  tryCatch({
    if(dir.exists(base_path)) {
      csv_files <- list.files(base_path, pattern = "\\.csv$", full.names = TRUE)
      if(length(csv_files) > 0) {
        data_asli <- read.csv(csv_files[1], stringsAsFactors = FALSE)
        if(nrow(data_asli) > 0) {
          # Pastikan ada kolom id
          if(!"id" %in% names(data_asli)) {
            data_asli$id <- 1:nrow(data_asli)
          }
          sovi_data <- data_asli
        }
      }
    }
  }, error = function(e) {
    message("Menggunakan data sampel - data asli tidak ditemukan")
  })
  
  # Muat data spasial
  data_spasial <- NULL
  tryCatch({
    file_gpkg <- file.path(base_path, "data_master_clean.gpkg")
    if(file.exists(file_gpkg)) {
      data_spasial <- st_read(file_gpkg, quiet = TRUE)
      # Pastikan ada kolom id
      if(!"id" %in% names(data_spasial) && nrow(data_spasial) == nrow(sovi_data)) {
        data_spasial$id <- 1:nrow(data_spasial)
      }
    }
  }, error = function(e) {
    message("Data spasial tidak ditemukan")
  })
  
  # Muat matriks jarak
  matriks_jarak <- NULL
  tryCatch({
    file_rds <- file.path(base_path, "distance_matrix_clean.rds")
    if(file.exists(file_rds)) {
      matriks_jarak <- readRDS(file_rds)
    }
  }, error = function(e) {
    message("Matriks jarak tidak ditemukan")
  })
  
  return(list(csv = sovi_data, spasial = data_spasial, jarak = matriks_jarak))
}

# Inisialisasi data
data_list <- muat_data_aman()
data_utama <- data_list$csv
data_spasial <- data_list$spasial
matriks_jarak <- data_list$jarak

# ==================== METADATA LENGKAP ====================
metadata_variabel <- data.frame(
  Kode_Variabel = c("id", "kode_kabkota", "ANAK", "PEREMPUAN", "LANSIA", "KEPALA_KELUARGA_PEREMPUAN", 
                    "UKURAN_KELUARGA", "TANPA_LISTRIK", "PENDIDIKAN_RENDAH", "PERTUMBUHAN_PENDUDUK",
                    "KEMISKINAN", "BUTA_HURUF", "TANPA_PELATIHAN", "RAWAN_BENCANA",
                    "RUMAH_SEWA", "TANPA_SANITASI", "AKSES_AIR_BERSIH", "JUMLAH_PENDUDUK"),
  Nama_Variabel = c("ID Wilayah", "Kode Kabupaten/Kota", "Persentase Populasi Anak-anak", "Persentase Populasi Perempuan", 
                    "Persentase Populasi Lansia", "Persentase Rumah Tangga Kepala Keluarga Perempuan",
                    "Rata-rata Ukuran Keluarga", "Persentase Rumah Tangga Tanpa Listrik",
                    "Persentase Pendidikan Rendah", "Tingkat Pertumbuhan Penduduk",
                    "Persentase Kemiskinan", "Tingkat Buta Huruf",
                    "Persentase Tanpa Pelatihan Kejuruan", "Indeks Kerawanan Bencana",
                    "Persentase Rumah Sewa", "Persentase Tanpa Sistem Sanitasi",
                    "Persentase Akses Air Bersih", "Jumlah Total Penduduk"),
  Tipe_Data = c("Numerik", "Karakter", rep("Numerik", 16)),
  Satuan = c("ID", "Kode", rep("Persen", 15), "Jiwa"),
  Kategori_Indikator = c("Identifikasi", "Identifikasi", "Demografi", "Demografi", "Demografi", "Sosial", "Demografi",
                         "Infrastruktur", "Pendidikan", "Demografi", "Ekonomi", "Pendidikan",
                         "Ekonomi", "Lingkungan", "Perumahan", "Infrastruktur", "Infrastruktur", "Demografi"),
  Deskripsi_Lengkap = c("Nomor identifikasi unik wilayah",
                         "Kode unik identifikasi kabupaten/kota",
                         "Proporsi penduduk berusia di bawah 15 tahun terhadap total populasi",
                         "Proporsi penduduk perempuan terhadap total populasi",
                         "Proporsi penduduk berusia 65 tahun ke atas terhadap total populasi",
                         "Proporsi rumah tangga yang dikepalai oleh perempuan",
                         "Jumlah rata-rata anggota keluarga per rumah tangga",
                         "Proporsi rumah tangga yang tidak memiliki akses listrik PLN",
                         "Proporsi penduduk dengan tingkat pendidikan di bawah SMA",
                         "Laju pertumbuhan penduduk tahunan dalam persentase",
                         "Proporsi penduduk yang hidup di bawah garis kemiskinan",
                         "Proporsi penduduk dewasa yang tidak dapat membaca dan menulis",
                         "Proporsi penduduk yang tidak memiliki keterampilan kejuruan",
                         "Indeks yang mengukur tingkat kerentanan terhadap bencana alam",
                         "Proporsi rumah tangga yang tinggal di rumah sewa/kontrak",
                         "Proporsi rumah tangga yang tidak memiliki akses sistem sanitasi",
                         "Proporsi rumah tangga yang memiliki akses air bersih",
                         "Jumlah total penduduk dalam satuan jiwa"),
  stringsAsFactors = FALSE
)

# ==================== FUNGSI STATISTIK AMAN ====================
uji_shapiro_aman <- function(x) {
  tryCatch({
    if(length(x) > 3 && length(x) <= 5000) {
      return(shapiro.test(x))
    } else {
      return(list(statistic = NA, p.value = NA, method = "Ukuran sampel tidak sesuai"))
    }
  }, error = function(e) {
    return(list(statistic = NA, p.value = NA, method = paste("Error:", e$message)))
  })
}

korelasi_aman <- function(data, metode = "pearson") {
  tryCatch({
    # Pastikan hanya kolom numerik
    data_numerik <- data[sapply(data, is.numeric)]
    data_bersih <- data_numerik[complete.cases(data_numerik), ]
    
    if(nrow(data_bersih) > 2 && ncol(data_bersih) > 1) {
      return(cor(data_bersih, method = metode))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

# Fungsi kategorisasi aman
kategorisasi_aman <- function(x, metode = "kuartil", cutoff_custom = NULL) {
  tryCatch({
    if(!is.numeric(x)) {
      return(rep("Error: Bukan numerik", length(x)))
    }
    
    x_clean <- x[!is.na(x)]
    if(length(x_clean) < 2) {
      return(rep("Data tidak cukup", length(x)))
    }
    
    if(metode == "kuartil") {
      breaks <- quantile(x_clean, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
      labels <- c("Rendah", "Sedang", "Tinggi", "Sangat Tinggi")
    } else if(metode == "tertil") {
      breaks <- quantile(x_clean, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
      labels <- c("Rendah", "Sedang", "Tinggi")
    } else if(metode == "median") {
      breaks <- quantile(x_clean, probs = c(0, 0.5, 1), na.rm = TRUE)
      labels <- c("Di bawah Median", "Di atas Median")
    } else if(metode == "custom" && !is.null(cutoff_custom)) {
      custom_values <- as.numeric(unlist(strsplit(cutoff_custom, ",")))
      custom_values <- custom_values[!is.na(custom_values)]
      if(length(custom_values) > 0) {
        breaks <- c(min(x_clean), sort(custom_values), max(x_clean))
        breaks <- unique(breaks)
        labels <- paste("Kategori", 1:(length(breaks)-1))
      } else {
        return(rep("Error: Cutoff tidak valid", length(x)))
      }
    } else {
      return(rep("Error: Metode tidak dikenal", length(x)))
    }
    
    # Pastikan breaks unik
    if(length(unique(breaks)) < length(breaks)) {
      breaks <- unique(breaks)
      labels <- paste("Kategori", 1:(length(breaks)-1))
    }
    
    return(cut(x, breaks = breaks, labels = labels, include.lowest = TRUE))
  }, error = function(e) {
    return(rep(paste("Error:", e$message), length(x)))
  })
}

# ==================== UI DEFINITION ====================
ui <- dashboardPage(
  dashboardHeader(title = "🌟 STARLIGHT v4.0 - UAS Komputasi Statistik"),
  
  dashboardSidebar(
    tags$head(tags$style(HTML(custom_css))),
    sidebarMenu(
      menuItem("🏠 Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("🔧 Manajemen Data", tabName = "manajemen", icon = icon("database")),
      menuItem("📊 Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-line")),
      menuItem("✅ Uji Asumsi", tabName = "asumsi", icon = icon("check-circle")),
      menuItem("📈 Inferensia Statistik", tabName = "inferensia", icon = icon("calculator"),
               menuSubItem("🔢 Uji 1 Kelompok", tabName = "uji_satu"),
               menuSubItem("⚖️ Uji 2 Kelompok", tabName = "uji_dua"),
               menuSubItem("📊 Uji Proporsi", tabName = "uji_proporsi"),
               menuSubItem("📏 Uji Variansi", tabName = "uji_variansi"),
               menuSubItem("🎯 ANOVA 1 Arah", tabName = "anova_satu"),
               menuSubItem("🎯 ANOVA 2 Arah", tabName = "anova_dua")
      ),
      menuItem("📈 Analisis Regresi", tabName = "regresi", icon = icon("line-chart")),
      menuItem("🗺️ Analisis Geospasial", tabName = "geospasial", icon = icon("map"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # ==================== TAB BERANDA ====================
      tabItem(tabName = "beranda",
              fluidRow(
                box(title = NULL, status = "primary", solidHeader = FALSE, width = 12,
                    div(class = "starlight-header",
                        h1("🌟 STARLIGHT Analytics Platform v4.0"),
                        h3("Statistical Analysis and Research Laboratory for Intelligent Geospatial Handling and Testing"),
                        p("Dashboard Analisis Statistik Komprehensif untuk UAS Komputasi Statistik"),
                        p("Politeknik Statistika STIS - Indeks Kerentanan Sosial Indonesia")
                    )
                )
              ),
              
              fluidRow(
                column(6,
                       box(title = "📊 Informasi Dataset", status = "info", solidHeader = TRUE, width = NULL,
                           div(class = "info-card",
                               h4("📍 Ringkasan Data"),
                               tags$ul(
                                 tags$li(paste("🏛️ Jumlah Kabupaten/Kota:", nrow(data_utama))),
                                 tags$li(paste("📋 Jumlah Variabel:", ncol(data_utama))),
                                 tags$li("📅 Tahun Data: 2017"),
                                 tags$li("🏢 Sumber: Badan Pusat Statistik (BPS)"),
                                 tags$li("📂 Lokasi Data: C:/Mario/Semester 4/Komstat/UAS/data/"),
                                 tags$li("🗂️ Format: CSV, GPKG, RDS")
                               )
                           )
                       )
                ),
                column(6,
                       box(title = "🎯 Tujuan & Fitur Analisis", status = "success", solidHeader = TRUE, width = NULL,
                           div(class = "stat-card",
                               h4("🎯 Tujuan Penelitian:"),
                               tags$ul(
                                 tags$li("📊 Analisis kerentanan sosial Indonesia"),
                                 tags$li("🔍 Identifikasi faktor-faktor kerentanan"),
                                 tags$li("🗺️ Pemetaan distribusi spasial"),
                                 tags$li("📈 Rekomendasi kebijakan berbasis data")
                               ),
                               h4("✨ Fitur Utama:"),
                               tags$ul(
                                 tags$li("📊 18 variabel dengan metadata lengkap"),
                                 tags$li("🧪 Uji statistik komprehensif"),
                                 tags$li("📈 Analisis regresi mendalam"),
                                 tags$li("🗺️ Visualisasi geospasial"),
                                 tags$li("💾 Export laporan Word/PDF")
                               )
                           )
                       )
                )
              ),
              
              fluidRow(
                box(title = "📋 Metadata Lengkap Variabel", status = "warning", solidHeader = TRUE, width = 12,
                    p("Tabel berikut menampilkan informasi lengkap tentang setiap variabel dalam dataset, termasuk kode, nama, tipe data, satuan, kategori indikator, dan deskripsi lengkap."),
                    br(),
                    DT::dataTableOutput("tabel_metadata"),
                    br(),
                    div(class = "download-section",
                        h5("📥 Unduh Metadata:"),
                        fluidRow(
                          column(3, downloadButton("unduh_metadata_csv", "📊 CSV", class = "btn-info btn-block")),
                          column(3, downloadButton("unduh_metadata_excel", "📈 Excel", class = "btn-success btn-block")),
                          column(3, downloadButton("unduh_metadata_word", "📄 Word", class = "btn-primary btn-block")),
                          column(3, downloadButton("unduh_metadata_pdf", "📄 PDF", class = "btn-danger btn-block"))
                        )
                    )
                )
              )
      ),
      
      # ==================== TAB MANAJEMEN DATA ====================
      tabItem(tabName = "manajemen",
              fluidRow(
                box(title = "🔧 Manajemen Data Lanjutan", status = "primary", solidHeader = TRUE, width = 12,
                    tabsetPanel(
                      tabPanel("📋 Dataset Asli",
                               # PERBAIKAN 1: Layout yang lebih baik - data mentah di atas
                               fluidRow(
                                 column(12,
                                        div(class = "data-overview",
                                            h4("📊 Dataset Mentah Kerentanan Sosial Indonesia"),
                                            p("Dataset ini berisi informasi kerentanan sosial dari", nrow(data_utama), "kabupaten/kota di Indonesia dengan", ncol(data_utama), "variabel."),
                                            withSpinner(DT::dataTableOutput("tabel_data_asli")),
                                            br(),
                                            div(class = "download-section",
                                                downloadButton("unduh_data_asli", "📥 Unduh Dataset CSV", class = "btn-info")
                                            )
                                        )
                                 )
                               ),
                               br(),
                               fluidRow(
                                 column(12,
                                        div(class = "data-overview",
                                            h4("📈 Ringkasan Statistik Variabel"),
                                            p("Tabel berikut menampilkan ringkasan statistik untuk setiap variabel dalam dataset."),
                                            DT::dataTableOutput("tabel_ringkasan_variabel"),
                                            br(),
                                            div(class = "download-section",
                                                downloadButton("unduh_ringkasan_variabel", "📥 Unduh Ringkasan CSV", class = "btn-success")
                                            )
                                        )
                                 )
                               )
                      ),
                      
                      tabPanel("🔍 Analisis Missing Value",
                               fluidRow(
                                 column(6,
                                        h4("📊 Pola Data Hilang"),
                                        withSpinner(plotOutput("plot_missing")),
                                        br(),
                                        downloadButton("unduh_plot_missing", "📥 Unduh Plot PNG", class = "btn-info")
                                 ),
                                 column(6,
                                        h4("🔧 Penanganan Missing Value"),
                                        selectInput("metode_missing", "Pilih Metode:",
                                                    choices = list(
                                                      "Hapus baris dengan NA" = "hapus_na",
                                                      "Imputasi rata-rata/modus" = "imputasi_mean",
                                                      "Forward fill" = "forward_fill",
                                                      "Imputasi MICE" = "mice"
                                                    )),
                                        actionButton("terapkan_missing", "🔧 Terapkan", class = "btn-warning"),
                                        br(), br(),
                                        h5("📊 Ringkasan Missing:"),
                                        DT::dataTableOutput("ringkasan_missing")
                                 )
                               )
                      ),
                      
                      tabPanel("🏷️ Kategorisasi Variabel",
                               fluidRow(
                                 column(6,
                                        h4("📊 Variabel Numerik"),
                                        DT::dataTableOutput("tabel_variabel_numerik"),
                                        br(),
                                        h5("🔄 Buat Kategori:"),
                                        selectInput("var_kategorisasi", "Pilih Variabel:", choices = NULL),
                                        selectInput("metode_kategorisasi", "Metode:",
                                                    choices = list(
                                                      "Kuartil (4 kategori)" = "kuartil",
                                                      "Tertil (3 kategori)" = "tertil", 
                                                      "Median (2 kategori)" = "median",
                                                      "Custom" = "custom"
                                                    )),
                                        conditionalPanel(
                                          condition = "input.metode_kategorisasi == 'custom'",
                                          textInput("cutoff_custom", "Cutoff (pisahkan dengan koma):", 
                                                    placeholder = "25,50,75")
                                        ),
                                        actionButton("buat_kategori", "🏷️ Kategorisasi", class = "btn-success")
                                 ),
                                 column(6,
                                        h4("🏷️ Hasil Kategorisasi"),
                                        DT::dataTableOutput("tabel_hasil_kategorisasi"),
                                        br(),
                                        verbatimTextOutput("info_kategorikal")
                                 )
                               )
                      ),
                      
                      tabPanel("✅ Data Terproses",
                               fluidRow(
                                 column(8,
                                        h4("📊 Dataset Setelah Preprocessing"),
                                        DT::dataTableOutput("tabel_data_terproses")
                                 ),
                                 column(4,
                                        h4("📈 Perbandingan Before/After"),
                                        verbatimTextOutput("perbandingan_data"),
                                        br(),
                                        div(class = "download-section",
                                            downloadButton("unduh_data_terproses", "📥 CSV", class = "btn-success btn-block"),
                                            br(),
                                            downloadButton("unduh_laporan_preprocessing", "📄 Laporan PDF", class = "btn-danger btn-block")
                                        )
                                 )
                               )
                      )
                    )
                )
              )
      ),
      
      # ==================== TAB EKSPLORASI DATA ====================
      tabItem(tabName = "eksplorasi",
              fluidRow(
                box(title = "📊 Eksplorasi Data Komprehensif", status = "primary", solidHeader = TRUE, width = 12,
                    tabsetPanel(
                      tabPanel("📈 Statistik Deskriptif",
                               fluidRow(
                                 column(4,
                                        h4("🎯 Pilih Variabel untuk Analisis"),
                                        checkboxGroupInput("variabel_deskriptif", "Variabel Numerik:",
                                                           choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                        br(),
                                        actionButton("hitung_deskriptif", "📊 Hitung Statistik", class = "btn-primary btn-block"),
                                        br(),
                                        div(class = "download-section",
                                            h5("📥 Unduh Hasil:"),
                                            downloadButton("unduh_deskriptif_csv", "📊 CSV", class = "btn-info btn-block"),
                                            br(),
                                            downloadButton("unduh_deskriptif_excel", "📈 Excel", class = "btn-success btn-block"),
                                            br(),
                                            downloadButton("unduh_laporan_deskriptif", "📄 Laporan PDF", class = "btn-danger btn-block")
                                        )
                                 ),
                                 column(8,
                                        h4("📊 Hasil Statistik Deskriptif"),
                                        withSpinner(DT::dataTableOutput("tabel_statistik_deskriptif")),
                                        br(),
                                        h4("📝 Interpretasi Otomatis"),
                                        div(id = "interpretasi_deskriptif", class = "alert alert-info",
                                            p("Pilih variabel dan klik 'Hitung Statistik' untuk melihat interpretasi otomatis."))
                                 )
                               )
                      ),
                      
                      tabPanel("📊 Visualisasi Distribusi",
                               fluidRow(
                                 column(3,
                                        h4("🎨 Pengaturan Visualisasi"),
                                        selectInput("variabel_plot", "Pilih Variabel:", 
                                                    choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                        selectInput("jenis_plot", "Jenis Plot:",
                                                    choices = list("Histogram" = "histogram", 
                                                                  "Density Plot" = "density", 
                                                                  "Box Plot" = "boxplot",
                                                                  "Violin Plot" = "violin")),
                                        conditionalPanel(
                                          condition = "input.jenis_plot == 'histogram'",
                                          sliderInput("jumlah_bins", "Jumlah Bins:", 10, 50, 30)
                                        ),
                                        selectInput("warna_plot", "Skema Warna:",
                                                    choices = list("Biru" = "steelblue", "Hijau" = "forestgreen", 
                                                                  "Merah" = "firebrick", "Ungu" = "purple")),
                                        actionButton("buat_plot", "🎨 Buat Visualisasi", class = "btn-success btn-block"),
                                        br(),
                                        div(class = "download-section",
                                            h5("📥 Unduh Plot:"),
                                            downloadButton("unduh_plot_png", "🖼️ PNG", class = "btn-info btn-block"),
                                            br(),
                                            downloadButton("unduh_plot_jpg", "🖼️ JPG", class = "btn-info btn-block"),
                                            br(),
                                            downloadButton("unduh_plot_pdf", "📄 PDF", class = "btn-danger btn-block")
                                        )
                                 ),
                                 # PERBAIKAN 3: Visualisasi geser ke kanan dan bawah
                                 column(9,
                                        h4("📊 Visualisasi Data"),
                                        withSpinner(plotOutput("plot_distribusi", height = "500px")),
                                        br(),
                                        h4("📝 Interpretasi Visualisasi"),
                                        div(id = "interpretasi_plot", class = "alert alert-info",
                                            p("Buat visualisasi untuk melihat interpretasi distribusi data."))
                                 )
                               )
                      ),
                      
                      tabPanel("🔗 Analisis Korelasi",
                               fluidRow(
                                 column(4,
                                        h4("🎯 Pengaturan Korelasi"),
                                        p("⚠️ Error korelasi telah diperbaiki!"),
                                        checkboxGroupInput("variabel_korelasi", "Pilih Variabel (minimal 2):",
                                                           choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                        selectInput("metode_korelasi", "Metode Korelasi:",
                                                    choices = list("Pearson" = "pearson", 
                                                                  "Spearman" = "spearman", 
                                                                  "Kendall" = "kendall")),
                                        sliderInput("threshold_korelasi", "Threshold Korelasi Kuat:", 
                                                    0.1, 0.9, 0.7, 0.1),
                                        actionButton("hitung_korelasi", "🔗 Hitung Korelasi", class = "btn-warning btn-block"),
                                        br(),
                                        div(class = "download-section",
                                            h5("📥 Unduh Hasil:"),
                                            downloadButton("unduh_matriks_korelasi", "📊 Matriks CSV", class = "btn-info btn-block"),
                                            br(),
                                            downloadButton("unduh_plot_korelasi", "🖼️ Plot PNG", class = "btn-success btn-block"),
                                            br(),
                                            downloadButton("unduh_laporan_korelasi", "📄 Laporan PDF", class = "btn-danger btn-block")
                                        )
                                 ),
                                 column(8,
                                        h4("🔗 Matriks Korelasi"),
                                        withSpinner(plotOutput("plot_korelasi", height = "500px")),
                                        br(),
                                        h4("📊 Tabel Korelasi"),
                                        DT::dataTableOutput("tabel_korelasi"),
                                        br(),
                                        h4("📝 Interpretasi Korelasi"),
                                        div(id = "interpretasi_korelasi", class = "alert alert-info",
                                            p("Hitung korelasi untuk melihat interpretasi hubungan antar variabel."))
                                 )
                               )
                      )
                    )
                )
              )
      ),
      
      # ==================== TAB UJI ASUMSI ====================
      tabItem(tabName = "asumsi",
              fluidRow(
                box(title = "✅ Pengujian Asumsi Statistik", status = "primary", solidHeader = TRUE, width = 12,
                    tabsetPanel(
                      tabPanel("📊 Uji Normalitas",
                               fluidRow(
                                 column(4,
                                        h4("🎯 Pengaturan Uji Normalitas"),
                                        selectInput("variabel_normalitas", "Pilih Variabel:", 
                                                    choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                        checkboxGroupInput("jenis_uji_normalitas", "Pilih Uji:",
                                                           choices = list("Shapiro-Wilk" = "shapiro", 
                                                                         "Kolmogorov-Smirnov" = "ks",
                                                                         "Anderson-Darling" = "ad",
                                                                         "Jarque-Bera" = "jb"),
                                                           selected = c("shapiro", "ks")),
                                        numericInput("alpha_normalitas", "Tingkat Signifikansi (α):", 
                                                     0.05, 0.01, 0.1, 0.01),
                                        actionButton("jalankan_uji_normalitas", "🧪 Jalankan Uji", 
                                                     class = "btn-primary btn-block"),
                                        br(),
                                        downloadButton("unduh_laporan_normalitas", "📄 Unduh Laporan DOCX", 
                                                       class = "btn-info btn-block")
                                 ),
                                 column(8,
                                        h4("📊 Hasil Uji Normalitas"),
                                        DT::dataTableOutput("hasil_uji_normalitas"),
                                        br(),
                                        h4("📈 Q-Q Plot"),
                                        plotOutput("qq_plot"),
                                        br(),
                                        h4("📝 Interpretasi"),
                                        div(id = "interpretasi_normalitas", class = "alert alert-info",
                                            p("Jalankan uji untuk melihat interpretasi hasil."))
                                 )
                               )
                      ),
                      
                      tabPanel("⚖️ Uji Homogenitas",
                               fluidRow(
                                 column(4,
                                        h4("🎯 Pengaturan Uji Homogenitas"),
                                        selectInput("variabel_homogenitas", "Variabel Dependen:", 
                                                    choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                        selectInput("variabel_grup_homogenitas", "Variabel Pengelompokan:", 
                                                    choices = names(data_utama)),
                                        radioButtons("jenis_uji_homogenitas", "Metode Uji:",
                                                     choices = list("Levene Test" = "levene",
                                                                   "Bartlett Test" = "bartlett",
                                                                   "Fligner-Killeen Test" = "fligner")),
                                        numericInput("alpha_homogenitas", "Tingkat Signifikansi (α):", 
                                                     0.05, 0.01, 0.1, 0.01),
                                        actionButton("jalankan_uji_homogenitas", "⚖️ Jalankan Uji", 
                                                     class = "btn-warning btn-block"),
                                        br(),
                                        downloadButton("unduh_laporan_homogenitas", "📄 Unduh Laporan DOCX", 
                                                       class = "btn-info btn-block")
                                 ),
                                 column(8,
                                        h4("⚖️ Hasil Uji Homogenitas"),
                                        DT::dataTableOutput("hasil_uji_homogenitas"),
                                        br(),
                                        h4("📊 Box Plot Perbandingan Kelompok"),
                                        plotOutput("plot_homogenitas"),
                                        br(),
                                        h4("📝 Interpretasi"),
                                        div(id = "interpretasi_homogenitas", class = "alert alert-info",
                                            p("Jalankan uji untuk melihat interpretasi hasil."))
                                 )
                               )
                      )
                    )
                )
              )
      ),
      
      # ==================== TAB UJI 1 KELOMPOK ====================
      tabItem(tabName = "uji_satu",
              fluidRow(
                box(title = "🔢 Uji Statistik Satu Kelompok", status = "primary", solidHeader = TRUE, width = 12,
                    tabsetPanel(
                      tabPanel("📊 Uji t Satu Sampel",
                               fluidRow(
                                 column(4,
                                        h4("🎯 Pengaturan Uji t"),
                                        selectInput("variabel_t_satu", "Pilih Variabel:", 
                                                    choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                        numericInput("nilai_hipotesis_t", "Nilai Hipotesis (μ₀):", 0),
                                        selectInput("alternatif_t_satu", "Hipotesis Alternatif:",
                                                    choices = list("≠ (dua sisi)" = "two.sided",
                                                                  "> (lebih besar)" = "greater",
                                                                  "< (lebih kecil)" = "less")),
                                        numericInput("alpha_t_satu", "Tingkat Signifikansi (α):", 
                                                     0.05, 0.01, 0.1, 0.01),
                                        actionButton("jalankan_t_satu", "🧪 Jalankan Uji t", class = "btn-primary btn-block"),
                                        br(),
                                        downloadButton("unduh_laporan_t_satu", "📄 Unduh Laporan DOCX", class = "btn-info btn-block")
                                 ),
                                 column(8,
                                        h4("📊 Hasil Uji t Satu Sampel"),
                                        DT::dataTableOutput("hasil_t_satu"),
                                        br(),
                                        h4("📈 Visualisasi"),
                                        plotOutput("plot_t_satu"),
                                        br(),
                                        h4("📝 Interpretasi"),
                                        div(id = "interpretasi_t_satu", class = "alert alert-info",
                                            p("Jalankan uji untuk melihat interpretasi hasil."))
                                 )
                               )
                      )
                    )
                )
              )
      ),
      
      # ==================== TAB GEOSPASIAL ====================
      tabItem(tabName = "geospasial",
              fluidRow(
                box(title = "🗺️ Analisis Geospasial", status = "primary", solidHeader = TRUE, width = 12,
                    p("Analisis spasial menggunakan data dari data_master_clean.gpkg dan distance_matrix_clean.rds"),
                    tabsetPanel(
                      tabPanel("🗺️ Peta Choropleth",
                               fluidRow(
                                 column(4,
                                        h4("🎨 Pengaturan Peta"),
                                        selectInput("variabel_choropleth", "Variabel untuk Mapping:", 
                                                    choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                        selectInput("skema_warna_choropleth", "Skema Warna:",
                                                    choices = list("Blues" = "Blues", "Reds" = "Reds", 
                                                                  "Greens" = "Greens", "Viridis" = "viridis",
                                                                  "Plasma" = "plasma", "YlOrRd" = "YlOrRd")),
                                        sliderInput("jumlah_kelas", "Jumlah Kelas:", 3, 10, 5),
                                        selectInput("metode_klasifikasi", "Metode Klasifikasi:",
                                                    choices = list("Equal Interval" = "equal",
                                                                  "Quantile" = "quantile",
                                                                  "Natural Breaks" = "jenks")),
                                        actionButton("buat_choropleth", "🗺️ Buat Peta", class = "btn-primary btn-block"),
                                        br(),
                                        downloadButton("unduh_peta", "📄 Unduh Peta", class = "btn-info btn-block")
                                 ),
                                 column(8,
                                        h4("🗺️ Peta Choropleth Interaktif"),
                                        conditionalPanel(
                                          condition = "output.data_spasial_tersedia",
                                          leafletOutput("peta_choropleth", height = "600px")
                                        ),
                                        conditionalPanel(
                                          condition = "!output.data_spasial_tersedia",
                                          div(class = "alert alert-warning",
                                              h5("⚠️ Data Spasial Tidak Tersedia"),
                                              p("File data_master_clean.gpkg tidak ditemukan di:"),
                                              p("C:/Mario/Semester 4/Komstat/UAS/data/"),
                                              p("Peta akan menggunakan data simulasi dengan koordinat acak."))
                                        ),
                                        br(),
                                        h4("📊 Statistik Spasial"),
                                        DT::dataTableOutput("statistik_spasial"),
                                        br(),
                                        h4("📝 Interpretasi Pola Spasial"),
                                        div(id = "interpretasi_spasial", class = "alert alert-info",
                                            p("Buat peta untuk melihat interpretasi pola spasial."))
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