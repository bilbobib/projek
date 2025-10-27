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
library(readr)
library(tidyr)
library(stringr)

# Data loading and preparation function
load_data <- function() {
  # Load CSV data from URL or local file
  tryCatch({
    sovi_data <- read.csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv",
                          stringsAsFactors = FALSE)
  }, error = function(e) {
    # Fallback to local file if URL fails
    data_path <- "C:/Mario/Semester 4/Komstat/UAS/data"
    csv_files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)
    if (length(csv_files) > 0) {
      sovi_data <- read.csv(csv_files[1], stringsAsFactors = FALSE)
    } else {
      # Create sample data if no file found
      sovi_data <- data.frame(
        districtkd = paste0("ID", sprintf("%03d", 1:100)),
        CHILDREN = round(runif(100, 10, 40), 2),
        FEMALE = round(runif(100, 45, 55), 2),
        ELDERLY = round(runif(100, 5, 20), 2),
        FHEAD = round(runif(100, 10, 30), 2),
        FAMILYSIZE = round(runif(100, 3, 6), 1),
        NOELECTRIC = round(runif(100, 0, 20), 2),
        LOWEDU = round(runif(100, 10, 50), 2),
        GROWTH = round(runif(100, -2, 5), 2),
        POVERTY = round(runif(100, 5, 40), 2),
        ILLITERATE = round(runif(100, 2, 25), 2),
        NOTRAINING = round(runif(100, 20, 70), 2),
        DPRONE = round(runif(100, 1, 5), 1),
        RENTED = round(runif(100, 5, 30), 2),
        NOSEWER = round(runif(100, 10, 60), 2),
        TAPWATER = round(runif(100, 40, 95), 2),
        POPULATION = round(runif(100, 10000, 500000))
      )
    }
  })
  
  # Load spatial data (shapefile or geojson)
  shp_data <- NULL
  tryCatch({
    data_path <- "C:/Mario/Semester 4/Komstat/UAS/data"
    
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
    # Create dummy spatial data if loading fails
    coords <- data.frame(
      lng = runif(100, 106.5, 107.0),
      lat = runif(100, -6.5, -6.0)
    )
    shp_data <- st_as_sf(coords, coords = c("lng", "lat"), crs = 4326)
    shp_data$districtkd <- paste0("ID", sprintf("%03d", 1:100))
  })
  
  return(list(data = sovi_data, spatial = shp_data))
}

# Safe categorization function
kategorisasi_aman <- function(data, col_name, n_bins = 5) {
  tryCatch({
    if (!is.numeric(data[[col_name]]) || length(data[[col_name]]) == 0) {
      return(rep("Tidak Valid", nrow(data)))
    }
    
    values <- data[[col_name]][!is.na(data[[col_name]])]
    if (length(unique(values)) <= 1) {
      return(rep("Kategori Tunggal", nrow(data)))
    }
    
    breaks <- quantile(values, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
    breaks <- unique(breaks)
    
    if (length(breaks) <= 2) {
      return(ifelse(data[[col_name]] <= median(values, na.rm = TRUE), "Rendah", "Tinggi"))
    }
    
    labels <- paste0("Kategori ", 1:(length(breaks) - 1))
    cut(data[[col_name]], breaks = breaks, labels = labels, include.lowest = TRUE)
  }, error = function(e) {
    return(rep("Error", nrow(data)))
  })
}

# Safe correlation function
korelasi_aman <- function(data) {
  tryCatch({
    numeric_data <- data[sapply(data, is.numeric)]
    if (ncol(numeric_data) < 2) {
      return(NULL)
    }
    
    # Remove columns with all NA or constant values
    numeric_data <- numeric_data[sapply(numeric_data, function(x) {
      length(unique(x[!is.na(x)])) > 1
    })]
    
    if (ncol(numeric_data) < 2) {
      return(NULL)
    }
    
    cor(numeric_data, use = "complete.obs")
  }, error = function(e) {
    return(NULL)
  })
}

# Statistical interpretation functions
interpret_descriptive <- function(variable, stats) {
  mean_val <- round(stats$mean, 2)
  median_val <- round(stats$median, 2)
  sd_val <- round(stats$sd, 2)
  cv <- round((sd_val / mean_val) * 100, 2)
  
  skew_val <- round(stats$skew, 2)
  kurt_val <- round(stats$kurtosis, 2)
  
  interpretation <- paste0(
    "Variabel ", variable, " memiliki:\n",
    "• Rata-rata: ", mean_val, "\n",
    "• Median: ", median_val, "\n",
    "• Standar deviasi: ", sd_val, "\n",
    "• Koefisien variasi: ", cv, "%\n\n",
    
    "Interpretasi distribusi:\n",
    if (abs(skew_val) < 0.5) {
      "• Data terdistribusi normal (skewness mendekati 0)\n"
    } else if (skew_val > 0.5) {
      "• Data miring ke kanan (right-skewed)\n"
    } else {
      "• Data miring ke kiri (left-skewed)\n"
    },
    
    if (abs(kurt_val) < 0.5) {
      "• Distribusi mesokurtik (kurtosis normal)\n"
    } else if (kurt_val > 0.5) {
      "• Distribusi leptokurtik (lebih runcing)\n"
    } else {
      "• Distribusi platykurtik (lebih datar)\n"
    },
    
    if (cv < 15) {
      "• Variabilitas data rendah (homogen)\n"
    } else if (cv < 35) {
      "• Variabilitas data sedang\n"
    } else {
      "• Variabilitas data tinggi (heterogen)\n"
    }
  )
  
  return(interpretation)
}

# Load initial data
initial_data <- load_data()

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "STARLIGHT v3.0 - Dashboard Analisis Statistik",
    titleWidth = 400
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("🏠 Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("📊 Manajemen Data", tabName = "data", icon = icon("database")),
      menuItem("🔍 Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-line")),
      menuItem("📈 Analisis Inferensial", tabName = "inferensial", icon = icon("calculator")),
      menuItem("🗺️ Visualisasi Spasial", tabName = "spasial", icon = icon("map")),
      menuItem("📋 Laporan", tabName = "laporan", icon = icon("file-alt")),
      menuItem("ℹ️ Tentang", tabName = "tentang", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .box-header {
          border-bottom: 1px solid #e0e0e0;
        }
        .main-header .navbar {
          background-color: #3c8dbc !important;
        }
        .skin-blue .main-sidebar {
          background-color: #222d32;
        }
        .plot-container {
          margin-top: 20px;
          margin-left: 20px;
        }
      "))
    ),
    
    tabItems(
      # Beranda Tab
      tabItem(tabName = "beranda",
        fluidRow(
          box(
            title = "Selamat Datang di STARLIGHT v3.0", status = "primary", solidHeader = TRUE,
            width = 12, height = "200px",
            div(
              style = "text-align: center; padding: 20px;",
              h3("Statistical Analysis and Research Laboratory for Intelligent Geospatial Handling and Testing"),
              p("Dashboard Analisis Statistik Komprehensif untuk Indeks Kerentanan Sosial"),
              p("Politeknik Statistika STIS - UAS Komputasi Statistik")
            )
          )
        ),
        
        fluidRow(
          valueBox(
            value = textOutput("total_observations"),
            subtitle = "Total Observasi",
            icon = icon("list"),
            color = "blue",
            width = 3
          ),
          valueBox(
            value = textOutput("total_variables"),
            subtitle = "Total Variabel",
            icon = icon("table"),
            color = "green",
            width = 3
          ),
          valueBox(
            value = textOutput("numeric_variables"),
            subtitle = "Variabel Numerik",
            icon = icon("calculator"),
            color = "yellow",
            width = 3
          ),
          valueBox(
            value = textOutput("missing_percentage"),
            subtitle = "% Data Hilang",
            icon = icon("exclamation-triangle"),
            color = "red",
            width = 3
          )
        ),
        
        fluidRow(
          box(
            title = "Informasi Dataset", status = "info", solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("dataset_info")
          ),
          box(
            title = "Statistik Ringkas", status = "success", solidHeader = TRUE,
            width = 6,
            DT::dataTableOutput("summary_stats")
          )
        )
      ),
      
      # Manajemen Data Tab
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Upload Data", status = "primary", solidHeader = TRUE,
            width = 4,
            fileInput("file_upload", "Pilih File CSV:",
                     accept = c(".csv", ".txt")),
            checkboxInput("header", "Header", TRUE),
            checkboxInput("stringsAsFactors", "Strings as factors", FALSE),
            radioButtons("sep", "Separator:",
                        choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                        selected = ","),
            radioButtons("quote", "Quote:",
                        choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                        selected = '"'),
            actionButton("load_sample", "Muat Data Contoh", class = "btn-info"),
            br(), br(),
            downloadButton("download_data", "Unduh Data", class = "btn-success")
          ),
          
          box(
            title = "Transformasi Data", status = "warning", solidHeader = TRUE,
            width = 4,
            selectInput("transform_var", "Pilih Variabel:", choices = NULL),
            selectInput("transform_type", "Jenis Transformasi:",
                       choices = list(
                         "Log Natural" = "log",
                         "Log10" = "log10",
                         "Akar Kuadrat" = "sqrt",
                         "Kuadrat" = "square",
                         "Standardisasi" = "scale",
                         "Normalisasi" = "normalize"
                       )),
            actionButton("apply_transform", "Terapkan Transformasi", class = "btn-warning"),
            br(), br(),
            h5("Kategorisasi Data:"),
            selectInput("cat_var", "Variabel untuk Dikategorisasi:", choices = NULL),
            numericInput("n_categories", "Jumlah Kategori:", value = 5, min = 2, max = 10),
            actionButton("categorize", "Kategorisasi", class = "btn-info")
          ),
          
          box(
            title = "Penanganan Missing Values", status = "danger", solidHeader = TRUE,
            width = 4,
            h5("Metode Imputasi:"),
            radioButtons("impute_method", "Pilih Metode:",
                        choices = list(
                          "Mean/Mode" = "mean",
                          "Median" = "median",
                          "Forward Fill" = "ffill",
                          "Backward Fill" = "bfill",
                          "Linear Interpolation" = "linear"
                        )),
            actionButton("impute_data", "Terapkan Imputasi", class = "btn-danger"),
            br(), br(),
            verbatimTextOutput("missing_info")
          )
        ),
        
        fluidRow(
          # Data mentah di atas
          box(
            title = "Data Mentah", status = "primary", solidHeader = TRUE,
            width = 12,
            div(
              style = "max-height: 400px; overflow-y: auto;",
              DT::dataTableOutput("tabel_data_asli")
            )
          )
        ),
        
        fluidRow(
          # Ringkasan variabel di bawah
          box(
            title = "Ringkasan Variabel", status = "info", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("tabel_ringkasan_variabel")
          )
        )
      ),
      
      # Eksplorasi Data Tab
      tabItem(tabName = "eksplorasi",
        fluidRow(
          box(
            title = "Pengaturan Analisis", status = "primary", solidHeader = TRUE,
            width = 3,
            selectInput("selected_vars", "Pilih Variabel:",
                       choices = NULL, multiple = TRUE),
            selectInput("plot_type", "Jenis Plot:",
                       choices = list(
                         "Histogram" = "histogram",
                         "Box Plot" = "boxplot",
                         "Density Plot" = "density",
                         "Scatter Plot" = "scatter",
                         "Correlation Plot" = "correlation"
                       )),
            conditionalPanel(
              condition = "input.plot_type == 'scatter'",
              selectInput("x_var", "Variabel X:", choices = NULL),
              selectInput("y_var", "Variabel Y:", choices = NULL)
            ),
            actionButton("generate_plot", "Buat Plot", class = "btn-primary"),
            br(), br(),
            actionButton("hitung_deskriptif", "Hitung Statistik Deskriptif", class = "btn-info"),
            br(), br(),
            downloadButton("unduh_plot_png", "Unduh PNG", class = "btn-success"),
            br(),
            downloadButton("unduh_plot_jpg", "Unduh JPG", class = "btn-success")
          ),
          
          box(
            title = "Visualisasi", status = "success", solidHeader = TRUE,
            width = 9,
            div(
              class = "plot-container",
              withSpinner(plotOutput("main_plot", height = "500px"))
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Statistik Deskriptif", status = "info", solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("descriptive_stats")
          ),
          box(
            title = "Interpretasi", status = "warning", solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("interpretation_text")
          )
        ),
        
        fluidRow(
          box(
            title = "Matriks Korelasi", status = "primary", solidHeader = TRUE,
            width = 12,
            withSpinner(plotOutput("correlation_plot", height = "400px"))
          )
        )
      ),
      
      # Analisis Inferensial Tab
      tabItem(tabName = "inferensial",
        fluidRow(
          box(
            title = "Pengaturan Uji Statistik", status = "primary", solidHeader = TRUE,
            width = 4,
            selectInput("test_type", "Jenis Uji:",
                       choices = list(
                         "Uji t Satu Sampel" = "t_one",
                         "Uji t Dua Sampel" = "t_two",
                         "Uji Normalitas" = "normality",
                         "Uji Homogenitas" = "homogeneity",
                         "Regresi Linear" = "regression",
                         "ANOVA" = "anova"
                       )),
            
            conditionalPanel(
              condition = "input.test_type == 't_one'",
              selectInput("t_one_var", "Variabel:", choices = NULL),
              numericInput("mu0", "Nilai Hipotesis (μ₀):", value = 0),
              numericInput("alpha", "Tingkat Signifikansi (α):", value = 0.05, min = 0.01, max = 0.1, step = 0.01)
            ),
            
            conditionalPanel(
              condition = "input.test_type == 't_two'",
              selectInput("t_two_var1", "Variabel 1:", choices = NULL),
              selectInput("t_two_var2", "Variabel 2:", choices = NULL),
              checkboxInput("equal_var", "Asumsi Varians Sama", TRUE)
            ),
            
            conditionalPanel(
              condition = "input.test_type == 'normality'",
              selectInput("norm_var", "Variabel:", choices = NULL),
              selectInput("norm_test", "Jenis Uji:",
                         choices = list(
                           "Shapiro-Wilk" = "shapiro",
                           "Kolmogorov-Smirnov" = "ks",
                           "Anderson-Darling" = "ad"
                         ))
            ),
            
            conditionalPanel(
              condition = "input.test_type == 'regression'",
              selectInput("reg_y", "Variabel Dependen (Y):", choices = NULL),
              selectInput("reg_x", "Variabel Independen (X):", choices = NULL, multiple = TRUE)
            ),
            
            actionButton("run_test", "Jalankan Uji", class = "btn-primary"),
            br(), br(),
            downloadButton("unduh_laporan_inferensial", "Unduh Laporan", class = "btn-success")
          ),
          
          box(
            title = "Hasil Uji Statistik", status = "success", solidHeader = TRUE,
            width = 8,
            verbatimTextOutput("test_results")
          )
        ),
        
        fluidRow(
          box(
            title = "Interpretasi Hasil", status = "info", solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("test_interpretation")
          )
        )
      ),
      
      # Visualisasi Spasial Tab
      tabItem(tabName = "spasial",
        fluidRow(
          box(
            title = "Pengaturan Peta", status = "primary", solidHeader = TRUE,
            width = 3,
            selectInput("map_variable", "Variabel untuk Peta:", choices = NULL),
            selectInput("color_palette", "Palet Warna:",
                       choices = list(
                         "Viridis" = "viridis",
                         "Plasma" = "plasma",
                         "Blues" = "Blues",
                         "Reds" = "Reds",
                         "Greens" = "Greens"
                       )),
            numericInput("n_breaks", "Jumlah Kelas:", value = 5, min = 3, max = 10),
            actionButton("create_map", "Buat Peta", class = "btn-primary"),
            br(), br(),
            h5("Pengaturan Tampilan:"),
            checkboxInput("show_labels", "Tampilkan Label", FALSE),
            checkboxInput("show_legend", "Tampilkan Legenda", TRUE),
            sliderInput("map_opacity", "Transparansi:", min = 0.3, max = 1, value = 0.7, step = 0.1)
          ),
          
          box(
            title = "Peta Choropleth", status = "success", solidHeader = TRUE,
            width = 9,
            withSpinner(leafletOutput("choropleth_map", height = "500px"))
          )
        ),
        
        fluidRow(
          box(
            title = "Statistik Spasial", status = "info", solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("spatial_stats")
          ),
          box(
            title = "Interpretasi Peta", status = "warning", solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("map_interpretation")
          )
        )
      ),
      
      # Laporan Tab
      tabItem(tabName = "laporan",
        fluidRow(
          box(
            title = "Generator Laporan", status = "primary", solidHeader = TRUE,
            width = 4,
            h4("Pilih Komponen Laporan:"),
            checkboxInput("include_summary", "Ringkasan Data", TRUE),
            checkboxInput("include_descriptive", "Statistik Deskriptif", TRUE),
            checkboxInput("include_plots", "Visualisasi", TRUE),
            checkboxInput("include_correlation", "Analisis Korelasi", TRUE),
            checkboxInput("include_tests", "Uji Statistik", FALSE),
            checkboxInput("include_spatial", "Analisis Spasial", FALSE),
            br(),
            
            h4("Format Output:"),
            radioButtons("report_format", "Pilih Format:",
                        choices = list(
                          "Word Document (.docx)" = "docx",
                          "PDF Document (.pdf)" = "pdf",
                          "HTML Report (.html)" = "html"
                        )),
            
            textInput("report_title", "Judul Laporan:", 
                     value = "Laporan Analisis Statistik STARLIGHT"),
            textInput("author_name", "Nama Penulis:", value = ""),
            
            br(),
            actionButton("preview_report", "Preview Laporan", class = "btn-info"),
            br(), br(),
            downloadButton("download_report", "Unduh Laporan", class = "btn-success")
          ),
          
          box(
            title = "Preview Laporan", status = "success", solidHeader = TRUE,
            width = 8,
            div(
              style = "max-height: 600px; overflow-y: auto; border: 1px solid #ddd; padding: 15px;",
              htmlOutput("report_preview")
            )
          )
        )
      ),
      
      # Tentang Tab
      tabItem(tabName = "tentang",
        fluidRow(
          box(
            title = "Tentang STARLIGHT v3.0", status = "primary", solidHeader = TRUE,
            width = 12,
            div(
              style = "padding: 20px;",
              h3("Statistical Analysis and Research Laboratory for Intelligent Geospatial Handling and Testing"),
              h4("Dashboard Analisis Statistik Komprehensif"),
              p("Versi 3.0 - UAS Komputasi Statistik"),
              hr(),
              
              h4("Fitur Utama:"),
              tags$ul(
                tags$li("📊 Manajemen dan eksplorasi data interaktif"),
                tags$li("📈 Visualisasi data yang komprehensif"),
                tags$li("🔍 Analisis statistik deskriptif dan inferensial"),
                tags$li("🗺️ Visualisasi spasial dengan peta choropleth"),
                tags$li("📋 Generator laporan otomatis"),
                tags$li("💾 Export data dan visualisasi")
              ),
              
              h4("Teknologi yang Digunakan:"),
              tags$ul(
                tags$li("R Shiny untuk web application framework"),
                tags$li("Leaflet untuk visualisasi peta interaktif"),
                tags$li("ggplot2 untuk visualisasi data"),
                tags$li("DT untuk tabel interaktif"),
                tags$li("sf untuk analisis data spasial")
              ),
              
              hr(),
              p("Dikembangkan untuk UAS Komputasi Statistik"),
              p("Politeknik Statistika STIS - 2024")
            )
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    data = initial_data$data,
    spatial_data = initial_data$spatial,
    current_plot = NULL,
    test_results = NULL
  )
  
  # Update choices when data changes
  observe({
    if (!is.null(values$data)) {
      numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
      all_vars <- names(values$data)
      
      updateSelectInput(session, "selected_vars", choices = all_vars)
      updateSelectInput(session, "transform_var", choices = numeric_vars)
      updateSelectInput(session, "cat_var", choices = numeric_vars)
      updateSelectInput(session, "x_var", choices = numeric_vars)
      updateSelectInput(session, "y_var", choices = numeric_vars)
      updateSelectInput(session, "t_one_var", choices = numeric_vars)
      updateSelectInput(session, "t_two_var1", choices = numeric_vars)
      updateSelectInput(session, "t_two_var2", choices = numeric_vars)
      updateSelectInput(session, "norm_var", choices = numeric_vars)
      updateSelectInput(session, "reg_y", choices = numeric_vars)
      updateSelectInput(session, "reg_x", choices = numeric_vars)
      updateSelectInput(session, "map_variable", choices = numeric_vars)
    }
  })
  
  # Beranda outputs
  output$total_observations <- renderText({
    if (!is.null(values$data)) nrow(values$data) else 0
  })
  
  output$total_variables <- renderText({
    if (!is.null(values$data)) ncol(values$data) else 0
  })
  
  output$numeric_variables <- renderText({
    if (!is.null(values$data)) {
      sum(sapply(values$data, is.numeric))
    } else 0
  })
  
  output$missing_percentage <- renderText({
    if (!is.null(values$data)) {
      missing_count <- sum(is.na(values$data))
      total_count <- nrow(values$data) * ncol(values$data)
      paste0(round((missing_count / total_count) * 100, 1), "%")
    } else "0%"
  })
  
  output$dataset_info <- renderText({
    if (!is.null(values$data)) {
      paste0(
        "Dataset Information:\n",
        "- Jumlah observasi: ", nrow(values$data), "\n",
        "- Jumlah variabel: ", ncol(values$data), "\n",
        "- Variabel numerik: ", sum(sapply(values$data, is.numeric)), "\n",
        "- Variabel karakter: ", sum(sapply(values$data, is.character)), "\n",
        "- Total missing values: ", sum(is.na(values$data)), "\n",
        "- Ukuran data: ", format(object.size(values$data), units = "Kb")
      )
    }
  })
  
  output$summary_stats <- DT::renderDataTable({
    if (!is.null(values$data)) {
      numeric_data <- values$data[sapply(values$data, is.numeric)]
      if (ncol(numeric_data) > 0) {
        summary_df <- data.frame(
          Variable = names(numeric_data),
          Mean = round(sapply(numeric_data, mean, na.rm = TRUE), 2),
          Median = round(sapply(numeric_data, median, na.rm = TRUE), 2),
          SD = round(sapply(numeric_data, sd, na.rm = TRUE), 2),
          Min = round(sapply(numeric_data, min, na.rm = TRUE), 2),
          Max = round(sapply(numeric_data, max, na.rm = TRUE), 2)
        )
        DT::datatable(summary_df, options = list(pageLength = 5, dom = 't'))
      }
    }
  })
  
  # Data Management
  output$tabel_data_asli <- DT::renderDataTable({
    if (!is.null(values$data)) {
      DT::datatable(
        values$data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "300px",
          dom = 'Bfrtip'
        ),
        class = 'cell-border stripe'
      )
    }
  })
  
  output$tabel_ringkasan_variabel <- DT::renderDataTable({
    if (!is.null(values$data)) {
      var_info <- data.frame(
        Variabel = names(values$data),
        Tipe = sapply(values$data, function(x) class(x)[1]),
        `Missing Values` = sapply(values$data, function(x) sum(is.na(x))),
        `% Missing` = round(sapply(values$data, function(x) sum(is.na(x)) / length(x) * 100), 2),
        `Unique Values` = sapply(values$data, function(x) length(unique(x[!is.na(x)]))),
        check.names = FALSE
      )
      
      DT::datatable(
        var_info,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Bfrtip'
        ),
        class = 'cell-border stripe'
      ) %>%
        DT::formatStyle(
          "% Missing",
          backgroundColor = DT::styleInterval(c(5, 20), c("white", "yellow", "red")),
          color = DT::styleInterval(c(5, 20), c("black", "black", "white"))
        )
    }
  })
  
  # File upload
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    tryCatch({
      df <- read.csv(input$file_upload$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote,
                     stringsAsFactors = input$stringsAsFactors)
      values$data <- df
      showNotification("Data berhasil dimuat!", type = "success")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Load sample data
  observeEvent(input$load_sample, {
    new_data <- load_data()
    values$data <- new_data$data
    values$spatial_data <- new_data$spatial
    showNotification("Data contoh berhasil dimuat!", type = "success")
  })
  
  # Data transformation
  observeEvent(input$apply_transform, {
    req(input$transform_var, input$transform_type)
    
    tryCatch({
      var_name <- input$transform_var
      transform_type <- input$transform_type
      
      if (is.numeric(values$data[[var_name]])) {
        original_data <- values$data[[var_name]]
        
        transformed_data <- switch(transform_type,
          "log" = log(original_data + abs(min(original_data, na.rm = TRUE)) + 1),
          "log10" = log10(original_data + abs(min(original_data, na.rm = TRUE)) + 1),
          "sqrt" = sqrt(original_data + abs(min(original_data, na.rm = TRUE))),
          "square" = original_data^2,
          "scale" = as.numeric(scale(original_data)),
          "normalize" = (original_data - min(original_data, na.rm = TRUE)) / 
                       (max(original_data, na.rm = TRUE) - min(original_data, na.rm = TRUE))
        )
        
        new_var_name <- paste0(var_name, "_", transform_type)
        values$data[[new_var_name]] <- transformed_data
        
        showNotification(paste("Transformasi", transform_type, "berhasil diterapkan!"), type = "success")
      } else {
        showNotification("Variabel harus numerik untuk transformasi!", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error transformasi:", e$message), type = "error")
    })
  })
  
  # Categorization
  observeEvent(input$categorize, {
    req(input$cat_var, input$n_categories)
    
    tryCatch({
      var_name <- input$cat_var
      n_cats <- input$n_categories
      
      categorized <- kategorisasi_aman(values$data, var_name, n_cats)
      new_var_name <- paste0(var_name, "_cat")
      values$data[[new_var_name]] <- categorized
      
      showNotification("Kategorisasi berhasil!", type = "success")
    }, error = function(e) {
      showNotification(paste("Error kategorisasi:", e$message), type = "error")
    })
  })
  
  # Missing values info
  output$missing_info <- renderText({
    if (!is.null(values$data)) {
      missing_summary <- values$data %>%
        summarise_all(~sum(is.na(.))) %>%
        gather(Variable, Missing_Count) %>%
        filter(Missing_Count > 0) %>%
        arrange(desc(Missing_Count))
      
      if (nrow(missing_summary) > 0) {
        paste0(
          "Variabel dengan Missing Values:\n",
          paste(paste0(missing_summary$Variable, ": ", missing_summary$Missing_Count), 
                collapse = "\n")
        )
      } else {
        "Tidak ada missing values dalam dataset."
      }
    }
  })
  
  # Data imputation
  observeEvent(input$impute_data, {
    req(input$impute_method)
    
    tryCatch({
      method <- input$impute_method
      data_imputed <- values$data
      
      for (col in names(data_imputed)) {
        if (any(is.na(data_imputed[[col]]))) {
          if (is.numeric(data_imputed[[col]])) {
            if (method == "mean") {
              data_imputed[[col]][is.na(data_imputed[[col]])] <- mean(data_imputed[[col]], na.rm = TRUE)
            } else if (method == "median") {
              data_imputed[[col]][is.na(data_imputed[[col]])] <- median(data_imputed[[col]], na.rm = TRUE)
            }
          } else {
            if (method == "mean") {  # mode for categorical
              mode_val <- names(sort(table(data_imputed[[col]]), decreasing = TRUE))[1]
              data_imputed[[col]][is.na(data_imputed[[col]])] <- mode_val
            }
          }
        }
      }
      
      values$data <- data_imputed
      showNotification("Imputasi berhasil diterapkan!", type = "success")
    }, error = function(e) {
      showNotification(paste("Error imputasi:", e$message), type = "error")
    })
  })
  
  # Exploratory Data Analysis
  observeEvent(input$generate_plot, {
    req(input$plot_type)
    
    tryCatch({
      plot_type <- input$plot_type
      data_to_plot <- values$data
      
      if (plot_type == "histogram" && length(input$selected_vars) > 0) {
        var_name <- input$selected_vars[1]
        if (is.numeric(data_to_plot[[var_name]])) {
          p <- ggplot(data_to_plot, aes_string(x = var_name)) +
            geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
            theme_minimal() +
            labs(title = paste("Histogram of", var_name),
                 x = var_name, y = "Frequency") +
            theme(plot.title = element_text(hjust = 0.5))
        }
      } else if (plot_type == "boxplot" && length(input$selected_vars) > 0) {
        var_name <- input$selected_vars[1]
        if (is.numeric(data_to_plot[[var_name]])) {
          p <- ggplot(data_to_plot, aes_string(y = var_name)) +
            geom_boxplot(fill = "lightblue", alpha = 0.7) +
            theme_minimal() +
            labs(title = paste("Box Plot of", var_name),
                 y = var_name) +
            theme(plot.title = element_text(hjust = 0.5))
        }
      } else if (plot_type == "density" && length(input$selected_vars) > 0) {
        var_name <- input$selected_vars[1]
        if (is.numeric(data_to_plot[[var_name]])) {
          p <- ggplot(data_to_plot, aes_string(x = var_name)) +
            geom_density(fill = "orange", alpha = 0.7) +
            theme_minimal() +
            labs(title = paste("Density Plot of", var_name),
                 x = var_name, y = "Density") +
            theme(plot.title = element_text(hjust = 0.5))
        }
      } else if (plot_type == "scatter" && !is.null(input$x_var) && !is.null(input$y_var)) {
        p <- ggplot(data_to_plot, aes_string(x = input$x_var, y = input$y_var)) +
          geom_point(alpha = 0.6, color = "darkblue") +
          geom_smooth(method = "lm", se = TRUE, color = "red") +
          theme_minimal() +
          labs(title = paste("Scatter Plot:", input$y_var, "vs", input$x_var),
               x = input$x_var, y = input$y_var) +
          theme(plot.title = element_text(hjust = 0.5))
      }
      
      values$current_plot <- p
    }, error = function(e) {
      showNotification(paste("Error membuat plot:", e$message), type = "error")
    })
  })
  
  output$main_plot <- renderPlot({
    if (!is.null(values$current_plot)) {
      values$current_plot
    }
  })
  
  # Descriptive Statistics
  observeEvent(input$hitung_deskriptif, {
    req(input$selected_vars)
    
    tryCatch({
      selected_data <- values$data[input$selected_vars]
      numeric_data <- selected_data[sapply(selected_data, is.numeric)]
      
      if (ncol(numeric_data) > 0) {
        desc_stats <- describe(numeric_data)
        values$desc_stats <- desc_stats
        
        # Generate interpretation for first variable
        if (ncol(numeric_data) >= 1) {
          first_var <- names(numeric_data)[1]
          interpretation <- interpret_descriptive(first_var, desc_stats[1, ])
          values$interpretation <- interpretation
        }
      } else {
        showNotification("Tidak ada variabel numerik yang dipilih!", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("Error menghitung statistik:", e$message), type = "error")
    })
  })
  
  output$descriptive_stats <- renderText({
    if (!is.null(values$desc_stats)) {
      capture.output(print(values$desc_stats))
    }
  })
  
  output$interpretation_text <- renderText({
    if (!is.null(values$interpretation)) {
      values$interpretation
    }
  })
  
  # Correlation Analysis
  output$correlation_plot <- renderPlot({
    if (!is.null(values$data)) {
      cor_matrix <- korelasi_aman(values$data)
      if (!is.null(cor_matrix)) {
        corrplot(cor_matrix, method = "color", type = "upper", 
                order = "hclust", tl.cex = 0.8, tl.col = "black")
      }
    }
  })
  
  # Download handlers for plots
  output$unduh_plot_png <- downloadHandler(
    filename = function() {
      paste0("plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      if (!is.null(values$current_plot)) {
        ggsave(file, values$current_plot, device = "png", 
               width = 10, height = 6, dpi = 300)
      }
    }
  )
  
  output$unduh_plot_jpg <- downloadHandler(
    filename = function() {
      paste0("plot_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      if (!is.null(values$current_plot)) {
        ggsave(file, values$current_plot, device = "jpg", 
               width = 10, height = 6, dpi = 300)
      }
    }
  )
  
  # Inferential Analysis
  observeEvent(input$run_test, {
    req(input$test_type)
    
    tryCatch({
      test_type <- input$test_type
      result <- NULL
      
      if (test_type == "t_one") {
        req(input$t_one_var, input$mu0)
        var_data <- values$data[[input$t_one_var]]
        var_data <- var_data[!is.na(var_data)]
        
        if (length(var_data) > 0) {
          test_result <- t.test(var_data, mu = input$mu0)
          result <- list(
            test = "One Sample t-test",
            statistic = test_result$statistic,
            p_value = test_result$p.value,
            confidence_interval = test_result$conf.int,
            mean = test_result$estimate,
            interpretation = ifelse(test_result$p.value < input$alpha, 
                                   "Tolak H0: Ada perbedaan signifikan", 
                                   "Gagal tolak H0: Tidak ada perbedaan signifikan")
          )
        }
      } else if (test_type == "normality") {
        req(input$norm_var)
        var_data <- values$data[[input$norm_var]]
        var_data <- var_data[!is.na(var_data)]
        
        if (length(var_data) > 2) {
          if (input$norm_test == "shapiro") {
            test_result <- shapiro.test(var_data)
          } else if (input$norm_test == "ks") {
            test_result <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
          }
          
          result <- list(
            test = paste(input$norm_test, "Normality Test"),
            statistic = test_result$statistic,
            p_value = test_result$p.value,
            interpretation = ifelse(test_result$p.value < 0.05, 
                                   "Data tidak berdistribusi normal", 
                                   "Data berdistribusi normal")
          )
        }
      } else if (test_type == "regression") {
        req(input$reg_y, input$reg_x)
        
        formula_str <- paste(input$reg_y, "~", paste(input$reg_x, collapse = " + "))
        model <- lm(as.formula(formula_str), data = values$data)
        
        result <- list(
          test = "Linear Regression",
          model_summary = summary(model),
          r_squared = summary(model)$r.squared,
          f_statistic = summary(model)$fstatistic,
          interpretation = paste("Model menjelaskan", 
                               round(summary(model)$r.squared * 100, 2), 
                               "% variasi dalam", input$reg_y)
        )
      }
      
      values$test_results <- result
    }, error = function(e) {
      showNotification(paste("Error menjalankan uji:", e$message), type = "error")
    })
  })
  
  output$test_results <- renderText({
    if (!is.null(values$test_results)) {
      if (values$test_results$test == "Linear Regression") {
        capture.output(print(values$test_results$model_summary))
      } else {
        paste0(
          "Test: ", values$test_results$test, "\n",
          "Statistic: ", round(values$test_results$statistic, 4), "\n",
          "P-value: ", format(values$test_results$p_value, scientific = TRUE), "\n",
          if (!is.null(values$test_results$confidence_interval)) {
            paste0("95% CI: [", round(values$test_results$confidence_interval[1], 4), 
                   ", ", round(values$test_results$confidence_interval[2], 4), "]\n")
          }
        )
      }
    }
  })
  
  output$test_interpretation <- renderText({
    if (!is.null(values$test_results)) {
      values$test_results$interpretation
    }
  })
  
  # Spatial Visualization
  observeEvent(input$create_map, {
    req(input$map_variable)
    
    tryCatch({
      if (!is.null(values$spatial_data) && !is.null(values$data)) {
        # Ensure spatial data has id column
        if (!"districtkd" %in% names(values$spatial_data)) {
          values$spatial_data$districtkd <- paste0("ID", sprintf("%03d", 1:nrow(values$spatial_data)))
        }
        
        # Merge data
        map_data <- merge(values$spatial_data, values$data, by = "districtkd", all.x = TRUE)
        
        if (nrow(map_data) > 0 && input$map_variable %in% names(map_data)) {
          values$map_data <- map_data
        } else {
          showNotification("Error: Tidak dapat menggabungkan data spasial dan atribut", type = "error")
        }
      }
    }, error = function(e) {
      showNotification(paste("Error membuat peta:", e$message), type = "error")
    })
  })
  
  output$choropleth_map <- renderLeaflet({
    if (!is.null(values$map_data) && !is.null(input$map_variable)) {
      tryCatch({
        map_var <- input$map_variable
        
        if (map_var %in% names(values$map_data)) {
          # Create color palette
          pal <- colorNumeric(
            palette = input$color_palette,
            domain = values$map_data[[map_var]],
            na.color = "transparent"
          )
          
          # Create leaflet map
          leaflet(values$map_data) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~pal(get(map_var)),
              weight = 1,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = input$map_opacity,
              popup = ~paste0(
                "<b>ID: </b>", districtkd, "<br>",
                "<b>", map_var, ": </b>", round(get(map_var), 2)
              ),
              label = if (input$show_labels) ~paste0(districtkd, ": ", round(get(map_var), 2)) else NULL,
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE
              )
            ) %>%
            {if (input$show_legend) addLegend(., pal = pal, values = ~get(map_var), 
                                            title = map_var, position = "bottomright") else .}
        }
      }, error = function(e) {
        leaflet() %>% addTiles() %>% 
          addMarkers(lng = 106.8, lat = -6.2, popup = paste("Error:", e$message))
      })
    } else {
      leaflet() %>% addTiles() %>% setView(lng = 106.8, lat = -6.2, zoom = 10)
    }
  })
  
  # Spatial statistics
  output$spatial_stats <- renderText({
    if (!is.null(values$map_data) && !is.null(input$map_variable)) {
      var_data <- values$map_data[[input$map_variable]]
      if (is.numeric(var_data)) {
        paste0(
          "Statistik Spasial untuk ", input$map_variable, ":\n",
          "Mean: ", round(mean(var_data, na.rm = TRUE), 2), "\n",
          "Median: ", round(median(var_data, na.rm = TRUE), 2), "\n",
          "Min: ", round(min(var_data, na.rm = TRUE), 2), "\n",
          "Max: ", round(max(var_data, na.rm = TRUE), 2), "\n",
          "Std Dev: ", round(sd(var_data, na.rm = TRUE), 2)
        )
      }
    }
  })
  
  output$map_interpretation <- renderText({
    if (!is.null(values$map_data) && !is.null(input$map_variable)) {
      var_data <- values$map_data[[input$map_variable]]
      if (is.numeric(var_data)) {
        q75 <- quantile(var_data, 0.75, na.rm = TRUE)
        q25 <- quantile(var_data, 0.25, na.rm = TRUE)
        
        paste0(
          "Interpretasi Peta:\n",
          "• Daerah dengan nilai tinggi (>Q3=", round(q75, 2), ") ditunjukkan dengan warna gelap\n",
          "• Daerah dengan nilai rendah (<Q1=", round(q25, 2), ") ditunjukkan dengan warna terang\n",
          "• Pola spasial menunjukkan distribusi ", input$map_variable, " di seluruh wilayah\n",
          "• Hover pada peta untuk melihat detail nilai setiap daerah"
        )
      }
    }
  })
  
  # Report Generation
  observeEvent(input$preview_report, {
    values$report_content <- generate_report_content()
  })
  
  output$report_preview <- renderUI({
    if (!is.null(values$report_content)) {
      HTML(values$report_content)
    } else {
      p("Klik 'Preview Laporan' untuk melihat isi laporan.")
    }
  })
  
  generate_report_content <- function() {
    content <- paste0(
      "<h1>", input$report_title, "</h1>",
      if (input$author_name != "") paste0("<p><b>Penulis:</b> ", input$author_name, "</p>"),
      "<p><b>Tanggal:</b> ", Sys.Date(), "</p>",
      "<hr>"
    )
    
    if (input$include_summary && !is.null(values$data)) {
      content <- paste0(content,
        "<h2>1. Ringkasan Data</h2>",
        "<p>Dataset memiliki ", nrow(values$data), " observasi dan ", 
        ncol(values$data), " variabel.</p>",
        "<p>Variabel numerik: ", sum(sapply(values$data, is.numeric)), "</p>",
        "<p>Missing values: ", sum(is.na(values$data)), " (", 
        round(sum(is.na(values$data)) / (nrow(values$data) * ncol(values$data)) * 100, 1), "%)</p>"
      )
    }
    
    if (input$include_descriptive && !is.null(values$desc_stats)) {
      content <- paste0(content,
        "<h2>2. Statistik Deskriptif</h2>",
        "<p>Analisis statistik deskriptif telah dilakukan untuk variabel numerik dalam dataset.</p>"
      )
    }
    
    if (input$include_correlation) {
      content <- paste0(content,
        "<h2>3. Analisis Korelasi</h2>",
        "<p>Matriks korelasi menunjukkan hubungan antar variabel numerik dalam dataset.</p>"
      )
    }
    
    return(content)
  }
  
  # Download report
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("STARLIGHT_Report_", Sys.Date(), ".", input$report_format)
    },
    content = function(file) {
      if (input$report_format == "docx") {
        # Create Word document
        doc <- read_docx()
        doc <- body_add_par(doc, input$report_title, style = "heading 1")
        doc <- body_add_par(doc, paste("Tanggal:", Sys.Date()))
        
        if (input$include_summary && !is.null(values$data)) {
          doc <- body_add_par(doc, "Ringkasan Data", style = "heading 2")
          doc <- body_add_par(doc, paste("Jumlah observasi:", nrow(values$data)))
          doc <- body_add_par(doc, paste("Jumlah variabel:", ncol(values$data)))
        }
        
        print(doc, target = file)
      }
    }
  )
  
  # Download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("STARLIGHT_Data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$data)) {
        write.csv(values$data, file, row.names = FALSE)
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)