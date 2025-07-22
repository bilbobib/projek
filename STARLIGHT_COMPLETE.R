# STARLIGHT v4.0 - COMPLETE VERSION
# Statistical Analysis and Research Laboratory for Intelligent Geospatial Handling and Testing
# Politeknik Statistika STIS - APLIKASI LENGKAP UNTUK UAS KOMPUTASI STATISTIK

# Load semua library yang diperlukan
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
library(lmtest)
library(Hmisc)
library(sandwich)
library(readr)
library(tidyr)
library(openxlsx)

# Sumber file utama
source("APP_STARLIGHT_FIXED.R")

# Tambahan tab untuk inferensia statistik
additional_tabs <- list(
  # TAB UJI 1 KELOMPOK
  tabItem(tabName = "uji_satu",
          fluidRow(
            box(
              title = "🔢 Uji Statistik Satu Kelompok", status = "primary", solidHeader = TRUE, width = 12,
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
                                  numericInput("alpha_t_satu", "Tingkat Signifikansi (α):", 0.05, 0.01, 0.1, 0.01),
                                  actionButton("jalankan_t_satu", "🧪 Jalankan Uji t", class = "btn-primary btn-block"),
                                  br(),
                                  downloadButton("unduh_laporan_t_satu", "📄 Unduh Laporan", class = "btn-info btn-block")
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
  
  # TAB UJI 2 KELOMPOK
  tabItem(tabName = "uji_dua",
          fluidRow(
            box(
              title = "⚖️ Uji Statistik Dua Kelompok", status = "primary", solidHeader = TRUE, width = 12,
              tabsetPanel(
                tabPanel("📊 Uji t Dua Sampel",
                         fluidRow(
                           column(4,
                                  h4("🎯 Pengaturan Uji t"),
                                  selectInput("variabel_t_dua", "Variabel Dependen:", 
                                              choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                  selectInput("variabel_grup_t", "Variabel Pengelompokan:", 
                                              choices = names(data_utama)),
                                  checkboxInput("paired_t", "Sampel Berpasangan", FALSE),
                                  checkboxInput("equal_var_t", "Varians Sama", TRUE),
                                  selectInput("alternatif_t_dua", "Hipotesis Alternatif:",
                                              choices = list("≠ (dua sisi)" = "two.sided",
                                                            "> (lebih besar)" = "greater",
                                                            "< (lebih kecil)" = "less")),
                                  actionButton("jalankan_t_dua", "⚖️ Jalankan Uji t", class = "btn-primary btn-block"),
                                  br(),
                                  downloadButton("unduh_laporan_t_dua", "📄 Unduh Laporan", class = "btn-info btn-block")
                           ),
                           column(8,
                                  h4("⚖️ Hasil Uji t Dua Sampel"),
                                  DT::dataTableOutput("hasil_t_dua"),
                                  br(),
                                  h4("📊 Visualisasi Perbandingan"),
                                  plotOutput("plot_t_dua"),
                                  br(),
                                  h4("📝 Interpretasi"),
                                  div(id = "interpretasi_t_dua", class = "alert alert-info",
                                      p("Jalankan uji untuk melihat interpretasi hasil."))
                           )
                         )
                )
              )
            )
          )
  ),
  
  # TAB ANALISIS REGRESI
  tabItem(tabName = "regresi",
          fluidRow(
            box(
              title = "📈 Analisis Regresi Komprehensif", status = "primary", solidHeader = TRUE, width = 12,
              tabsetPanel(
                tabPanel("📊 Regresi Linear Sederhana",
                         fluidRow(
                           column(4,
                                  h4("🎯 Pengaturan Model"),
                                  selectInput("y_regresi", "Variabel Dependen (Y):", 
                                              choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                  selectInput("x_regresi", "Variabel Independen (X):", 
                                              choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                  numericInput("alpha_regresi", "Tingkat Signifikansi (α):", 0.05, 0.01, 0.1, 0.01),
                                  actionButton("jalankan_regresi", "📊 Jalankan Regresi", class = "btn-primary btn-block"),
                                  br(),
                                  h5("🔍 Diagnostik Model:"),
                                  checkboxGroupInput("diagnostik_regresi", "Pilih Diagnostik:",
                                                     choices = list("Residual Plot" = "residual",
                                                                   "Q-Q Plot" = "qq",
                                                                   "Scale-Location" = "scale",
                                                                   "Cook's Distance" = "cook"),
                                                     selected = c("residual", "qq")),
                                  br(),
                                  downloadButton("unduh_laporan_regresi", "📄 Laporan Lengkap", class = "btn-danger btn-block")
                           ),
                           column(8,
                                  h4("📊 Hasil Regresi Linear"),
                                  DT::dataTableOutput("hasil_regresi"),
                                  br(),
                                  h4("📈 Plot Regresi (Sumbu Y Dinamis)"),
                                  plotOutput("plot_regresi"),
                                  br(),
                                  h4("🔍 Plot Diagnostik"),
                                  plotOutput("plot_diagnostik_regresi"),
                                  br(),
                                  h4("📝 Interpretasi Mendalam & Rekomendasi"),
                                  div(id = "interpretasi_regresi", class = "alert alert-info",
                                      p("Jalankan regresi untuk interpretasi mendalam dan saran pemodelan."))
                           )
                         )
                ),
                
                tabPanel("📈 Regresi Berganda",
                         fluidRow(
                           column(4,
                                  h4("🎯 Model Berganda"),
                                  selectInput("y_berganda", "Variabel Dependen (Y):", 
                                              choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                  checkboxGroupInput("x_berganda", "Variabel Independen (X):",
                                                     choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                  actionButton("jalankan_berganda", "📈 Jalankan Regresi", class = "btn-success btn-block"),
                                  br(),
                                  h5("🧪 Uji Asumsi:"),
                                  checkboxInput("cek_multikolinearitas", "Multikolinearitas (VIF)", TRUE),
                                  checkboxInput("cek_normalitas_residual", "Normalitas Residual", TRUE),
                                  checkboxInput("cek_homoskedastisitas", "Homoskedastisitas", TRUE),
                                  br(),
                                  downloadButton("unduh_laporan_berganda", "📄 Laporan Lengkap", class = "btn-danger btn-block")
                           ),
                           column(8,
                                  h4("📈 Hasil Regresi Berganda"),
                                  DT::dataTableOutput("hasil_berganda"),
                                  br(),
                                  h4("🧪 Hasil Uji Asumsi"),
                                  DT::dataTableOutput("asumsi_berganda"),
                                  br(),
                                  h4("📊 Plot Diagnostik"),
                                  plotOutput("plot_diagnostik_berganda"),
                                  br(),
                                  h4("📝 Interpretasi & Saran Pemodelan"),
                                  div(id = "interpretasi_berganda", class = "alert alert-info",
                                      p("Jalankan regresi untuk interpretasi dan saran jika asumsi tidak terpenuhi."))
                           )
                         )
                ),
                
                tabPanel("⚖️ Perbandingan Model",
                         fluidRow(
                           column(4,
                                  h4("🎯 Bandingkan Model"),
                                  p("Perbandingan model dengan sumbu Y yang disesuaikan"),
                                  selectInput("y_perbandingan", "Variabel Dependen:", 
                                              choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                  selectInput("x1_perbandingan", "Model 1 - X:", 
                                              choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                  checkboxGroupInput("x2_perbandingan", "Model 2 - X (multiple):",
                                                     choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                  actionButton("jalankan_perbandingan", "⚖️ Bandingkan", class = "btn-warning btn-block"),
                                  br(),
                                  downloadButton("unduh_laporan_perbandingan", "📄 Laporan", class = "btn-danger btn-block")
                           ),
                           column(8,
                                  h4("⚖️ Perbandingan Model"),
                                  DT::dataTableOutput("tabel_perbandingan"),
                                  br(),
                                  h4("📊 Visualisasi Perbandingan"),
                                  plotOutput("plot_perbandingan"),
                                  br(),
                                  h4("📝 Rekomendasi Model Terbaik"),
                                  div(id = "interpretasi_perbandingan", class = "alert alert-info",
                                      p("Bandingkan model untuk rekomendasi."))
                           )
                         )
                )
              )
            )
          )
  ),
  
  # TAB GEOSPASIAL
  tabItem(tabName = "geospasial",
          fluidRow(
            box(
              title = "🗺️ Analisis Geospasial", status = "primary", solidHeader = TRUE, width = 12,
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
                                        p("Pastikan file tersedia untuk analisis geospasial."))
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
                ),
                
                tabPanel("📏 Analisis Jarak",
                         fluidRow(
                           column(4,
                                  h4("🎯 Pengaturan Analisis Jarak"),
                                  selectInput("variabel_jarak", "Variabel untuk Analisis:", 
                                              choices = names(data_utama)[sapply(data_utama, is.numeric)]),
                                  numericInput("threshold_jarak", "Threshold Jarak (km):", 100, 10, 1000),
                                  selectInput("metode_jarak", "Metode Jarak:",
                                              choices = list("Euclidean" = "euclidean",
                                                            "Manhattan" = "manhattan",
                                                            "Haversine" = "haversine")),
                                  actionButton("jalankan_analisis_jarak", "📏 Analisis Jarak", class = "btn-success btn-block"),
                                  br(),
                                  downloadButton("unduh_laporan_jarak", "📄 Unduh Laporan", class = "btn-info btn-block")
                           ),
                           column(8,
                                  h4("📏 Hasil Analisis Jarak"),
                                  DT::dataTableOutput("hasil_jarak"),
                                  br(),
                                  h4("🗺️ Visualisasi Jarak"),
                                  plotOutput("plot_jarak"),
                                  br(),
                                  h4("📝 Interpretasi Analisis Jarak"),
                                  div(id = "interpretasi_jarak", class = "alert alert-info",
                                      p("Jalankan analisis untuk interpretasi hasil."))
                           )
                         )
                )
              )
            )
          )
  )
)

# Fungsi untuk menambahkan server logic tambahan
tambahan_server <- function(input, output, session) {
  
  # ==================== UJI T SATU SAMPEL ====================
  observeEvent(input$jalankan_t_satu, {
    tryCatch({
      if(!is.null(input$variabel_t_satu) && input$variabel_t_satu != "") {
        data_var <- data_utama[[input$variabel_t_satu]]
        data_var <- data_var[!is.na(data_var)]
        
        if(length(data_var) > 1) {
          hasil_t <- t.test(data_var, mu = input$nilai_hipotesis_t, 
                           alternative = input$alternatif_t_satu)
          
          hasil <- data.frame(
            Statistik = "t-statistik",
            Nilai = round(hasil_t$statistic, 4),
            df = hasil_t$parameter,
            P_Value = round(hasil_t$p.value, 4),
            CI_Bawah = round(hasil_t$conf.int[1], 4),
            CI_Atas = round(hasil_t$conf.int[2], 4),
            Keputusan = ifelse(hasil_t$p.value < input$alpha_t_satu, "Tolak H0", "Terima H0")
          )
          
          output$hasil_t_satu <- DT::renderDataTable({
            DT::datatable(hasil, options = list(pageLength = 10), rownames = FALSE,
                          caption = "Hasil Uji t Satu Sampel")
          })
          
          output$plot_t_satu <- renderPlot({
            hist(data_var, main = paste("Distribusi", input$variabel_t_satu), 
                 xlab = input$variabel_t_satu, col = "lightblue", border = "white")
            abline(v = mean(data_var), col = "red", lwd = 2, lty = 2)
            abline(v = input$nilai_hipotesis_t, col = "blue", lwd = 2)
            legend("topright", c("Rata-rata Sampel", "Nilai Hipotesis"), 
                   col = c("red", "blue"), lty = c(2, 1), lwd = 2)
          })
          
          output$interpretasi_t_satu <- renderUI({
            keputusan_teks <- ifelse(hasil_t$p.value < input$alpha_t_satu,
                                    paste("Kita menolak H0. Ada bukti signifikan bahwa rata-rata", 
                                          ifelse(input$alternatif_t_satu == "greater", "lebih besar dari", 
                                                ifelse(input$alternatif_t_satu == "less", "lebih kecil dari", "berbeda dari")),
                                          input$nilai_hipotesis_t),
                                    paste("Kita gagal menolak H0. Tidak ada bukti yang cukup bahwa rata-rata",
                                          ifelse(input$alternatif_t_satu == "greater", "lebih besar dari", 
                                                ifelse(input$alternatif_t_satu == "less", "lebih kecil dari", "berbeda dari")),
                                          input$nilai_hipotesis_t))
            
            div(class = "alert alert-info",
                h5("📝 Interpretasi:"),
                p(keputusan_teks),
                p(paste("Rata-rata sampel:", round(mean(data_var), 4))),
                p(paste("Interval Kepercayaan 95%: [", round(hasil_t$conf.int[1], 4), 
                        ",", round(hasil_t$conf.int[2], 4), "]")),
                p(paste("t-statistik =", round(hasil_t$statistic, 4), 
                        ", p-value =", round(hasil_t$p.value, 4))))
          })
        }
      }
    }, error = function(e) {
      showNotification(paste("Error dalam uji t:", e$message), type = "error", duration = 5)
    })
  })
  
  # ==================== ANALISIS REGRESI ====================
  observeEvent(input$jalankan_regresi, {
    tryCatch({
      if(!is.null(input$y_regresi) && !is.null(input$x_regresi) && 
         input$y_regresi != input$x_regresi) {
        
        y_data <- data_utama[[input$y_regresi]]
        x_data <- data_utama[[input$x_regresi]]
        
        # Hapus missing values
        complete_cases <- complete.cases(y_data, x_data)
        y_data <- y_data[complete_cases]
        x_data <- x_data[complete_cases]
        
        if(length(y_data) > 2) {
          # Fit model regresi
          model <- lm(y_data ~ x_data)
          ringkasan_model <- summary(model)
          
          # Buat tabel hasil
          hasil <- data.frame(
            Term = c("Intercept", input$x_regresi),
            Estimate = round(ringkasan_model$coefficients[, "Estimate"], 4),
            Std_Error = round(ringkasan_model$coefficients[, "Std. Error"], 4),
            t_value = round(ringkasan_model$coefficients[, "t value"], 4),
            P_value = round(ringkasan_model$coefficients[, "Pr(>|t|)"], 4),
            Signifikansi = ifelse(ringkasan_model$coefficients[, "Pr(>|t|)"] < 0.001, "***",
                                 ifelse(ringkasan_model$coefficients[, "Pr(>|t|)"] < 0.01, "**",
                                       ifelse(ringkasan_model$coefficients[, "Pr(>|t|)"] < 0.05, "*", "")))
          )
          
          # Tambah statistik model
          hasil <- rbind(hasil, data.frame(
            Term = c("R-squared", "Adj R-squared", "F-statistic", "p-value (model)"),
            Estimate = c(round(ringkasan_model$r.squared, 4),
                        round(ringkasan_model$adj.r.squared, 4),
                        round(ringkasan_model$fstatistic[1], 4),
                        round(pf(ringkasan_model$fstatistic[1], 
                               ringkasan_model$fstatistic[2], 
                               ringkasan_model$fstatistic[3], lower.tail = FALSE), 4)),
            Std_Error = rep(NA, 4),
            t_value = rep(NA, 4),
            P_value = rep(NA, 4),
            Signifikansi = rep("", 4)
          ))
          
          output$hasil_regresi <- DT::renderDataTable({
            DT::datatable(hasil, options = list(pageLength = 10), rownames = FALSE,
                          caption = "Hasil Analisis Regresi Linear")
          })
          
          # PLOT REGRESI DENGAN SUMBU Y DINAMIS
          output$plot_regresi <- renderPlot({
            # Hitung batas sumbu Y dinamis
            y_range <- range(y_data, na.rm = TRUE)
            y_margin <- diff(y_range) * 0.1
            y_limits <- c(y_range[1] - y_margin, y_range[2] + y_margin)
            
            plot(x_data, y_data, 
                 xlab = input$x_regresi, ylab = input$y_regresi,
                 main = paste("Regresi:", input$y_regresi, "vs", input$x_regresi),
                 pch = 16, col = "steelblue", alpha = 0.6,
                 ylim = y_limits)
            abline(model, col = "red", lwd = 2)
            
            # Tambah R-squared ke plot
            text(min(x_data, na.rm = TRUE), max(y_limits) * 0.95, 
                 paste("R² =", round(ringkasan_model$r.squared, 3)), 
                 adj = 0, cex = 1.2, col = "red")
          })
          
          # Plot diagnostik
          output$plot_diagnostik_regresi <- renderPlot({
            par(mfrow = c(2, 2))
            
            if("residual" %in% input$diagnostik_regresi) {
              plot(fitted(model), residuals(model),
                   main = "Residuals vs Fitted",
                   xlab = "Fitted Values", ylab = "Residuals",
                   pch = 16, col = "steelblue")
              abline(h = 0, col = "red", lty = 2)
            }
            
            if("qq" %in% input$diagnostik_regresi) {
              qqnorm(residuals(model), main = "Q-Q Plot Residuals", pch = 16, col = "steelblue")
              qqline(residuals(model), col = "red")
            }
            
            if("scale" %in% input$diagnostik_regresi) {
              plot(fitted(model), sqrt(abs(residuals(model))),
                   main = "Scale-Location Plot",
                   xlab = "Fitted Values", ylab = "√|Residuals|",
                   pch = 16, col = "steelblue")
            }
            
            if("cook" %in% input$diagnostik_regresi) {
              plot(cooks.distance(model), main = "Cook's Distance",
                   ylab = "Cook's Distance", pch = 16, col = "steelblue")
              abline(h = 4/length(y_data), col = "red", lty = 2)
            }
            
            par(mfrow = c(1, 1))
          })
          
          # INTERPRETASI MENDALAM
          output$interpretasi_regresi <- renderUI({
            r_squared <- ringkasan_model$r.squared
            adj_r_squared <- ringkasan_model$adj.r.squared
            p_value <- pf(ringkasan_model$fstatistic[1], 
                         ringkasan_model$fstatistic[2], 
                         ringkasan_model$fstatistic[3], lower.tail = FALSE)
            
            slope <- ringkasan_model$coefficients[2, "Estimate"]
            slope_p <- ringkasan_model$coefficients[2, "Pr(>|t|)"]
            
            # Signifikansi model
            model_sig <- ifelse(p_value < 0.05, "signifikan", "tidak signifikan")
            
            # Interpretasi slope
            slope_direction <- ifelse(slope > 0, "positif", "negatif")
            slope_sig <- ifelse(slope_p < 0.05, "signifikan", "tidak signifikan")
            
            # Interpretasi R-squared
            r_sq_interpretation <- ifelse(r_squared > 0.7, "kuat",
                                         ifelse(r_squared > 0.5, "sedang",
                                               ifelse(r_squared > 0.3, "lemah", "sangat lemah")))
            
            # Rekomendasi
            rekomendasi <- c()
            if(r_squared < 0.3) {
              rekomendasi <- c(rekomendasi, "• Pertimbangkan menambah prediktor untuk meningkatkan fit model")
            }
            if(p_value >= 0.05) {
              rekomendasi <- c(rekomendasi, "• Model tidak signifikan secara statistik - pertimbangkan ulang pemilihan variabel")
            }
            if(slope_p >= 0.05) {
              rekomendasi <- c(rekomendasi, "• Variabel prediktor tidak berpengaruh signifikan terhadap outcome")
            }
            if(length(rekomendasi) == 0) {
              rekomendasi <- c("• Model sudah baik dan signifikan secara statistik")
            }
            
            div(class = "alert alert-info",
                h5("📝 Interpretasi Mendalam & Rekomendasi:"),
                h6("Performa Model:"),
                p(paste("• Model regresi", model_sig, "(p =", round(p_value, 4), ")")),
                p(paste("• R² =", round(r_squared, 3), "menunjukkan hubungan", r_sq_interpretation)),
                p(paste("• Model menjelaskan", round(r_squared * 100, 1), "% varians dalam", input$y_regresi)),
                h6("Hubungan Variabel:"),
                p(paste("• Terdapat hubungan", slope_direction, "dan", slope_sig)),
                p(paste("• Setiap kenaikan 1 unit", input$x_regresi, ",", input$y_regresi, 
                        ifelse(slope > 0, "naik", "turun"), "sebesar", round(abs(slope), 4), "unit")),
                h6("Rekomendasi:"),
                HTML(paste(rekomendasi, collapse = "<br>")))
          })
        }
      }
    }, error = function(e) {
      showNotification(paste("Error dalam analisis regresi:", e$message), type = "error", duration = 5)
    })
  })
  
  # ==================== GEOSPASIAL ====================
  output$data_spasial_tersedia <- reactive({
    !is.null(data_spasial)
  })
  outputOptions(output, "data_spasial_tersedia", suspendWhenHidden = FALSE)
  
  observeEvent(input$buat_choropleth, {
    if(!is.null(data_spasial) && !is.null(input$variabel_choropleth)) {
      tryCatch({
        # Buat peta choropleth
        output$peta_choropleth <- renderLeaflet({
          leaflet(data_spasial) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~colorQuantile(input$skema_warna_choropleth, 
                                        data_spasial[[input$variabel_choropleth]], 
                                        n = input$jumlah_kelas)(data_spasial[[input$variabel_choropleth]]),
              weight = 1,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              popup = ~paste(input$variabel_choropleth, ":", data_spasial[[input$variabel_choropleth]])
            ) %>%
            addLegend(
              pal = colorQuantile(input$skema_warna_choropleth, 
                                 data_spasial[[input$variabel_choropleth]], 
                                 n = input$jumlah_kelas),
              values = data_spasial[[input$variabel_choropleth]],
              opacity = 0.7,
              title = input$variabel_choropleth,
              position = "bottomright"
            )
        })
        
        output$interpretasi_spasial <- renderUI({
          div(class = "alert alert-info",
              h5("📝 Interpretasi Pola Spasial:"),
              p(paste("Peta choropleth menunjukkan distribusi spasial", input$variabel_choropleth, 
                      "di seluruh wilayah kajian. Warna yang lebih gelap menunjukkan nilai yang lebih tinggi.")),
              p("Pola spasial dapat mengindikasikan adanya clustering atau dispersi geografis dari fenomena yang diamati."))
        })
      }, error = function(e) {
        showNotification(paste("Error membuat peta choropleth:", e$message), type = "error", duration = 5)
      })
    }
  })
}

# Gabungkan dengan server utama
server_lengkap <- function(input, output, session) {
  # Panggil server utama
  server(input, output, session)
  
  # Panggil server tambahan
  tambahan_server(input, output, session)
}

# Gabungkan UI dengan tab tambahan
ui_lengkap <- ui
ui_lengkap$children[[3]]$children[[1]]$children <- c(
  ui_lengkap$children[[3]]$children[[1]]$children,
  additional_tabs
)

# Jalankan aplikasi lengkap
shinyApp(ui = ui_lengkap, server = server_lengkap)