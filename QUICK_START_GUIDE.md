# 🚀 STARLIGHT v3.0 - Quick Start Guide

## Cara Cepat Menjalankan Aplikasi

### 1. 📦 Install Required Packages
Jalankan kode berikut di R/RStudio untuk menginstall semua dependencies:

```r
# List semua packages yang dibutuhkan
required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "plotly", "leaflet",
  "sf", "dplyr", "corrplot", "psych", "car", "nortest", "flextable",
  "officer", "downloadthis", "shinyWidgets", "shinycssloaders", 
  "htmlwidgets", "RColorBrewer", "viridis", "reshape2", "broom", 
  "VIM", "mice", "gridExtra", "knitr", "rmarkdown", "jsonlite", 
  "boot", "lmtest", "Hmisc", "sandwich", "readr", "tidyr", "stringr"
)

# Install packages yang belum tersedia
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(missing_packages)) {
  install.packages(missing_packages, dependencies = TRUE)
}

# Verify installation
cat("✅ Semua packages berhasil diinstall!\n")
```

### 2. 🏃‍♂️ Menjalankan Aplikasi

#### Option A: Direct Run (Recommended)
```r
# Run aplikasi langsung
shiny::runApp("STARLIGHT_COMPLETE_V3.R")
```

#### Option B: Custom Port
```r
# Jika port default sudah digunakan
shiny::runApp("STARLIGHT_COMPLETE_V3.R", port = 8080)
```

#### Option C: Source dan Run
```r
# Source file terlebih dahulu
source("STARLIGHT_COMPLETE_V3.R")
# Aplikasi akan otomatis berjalan
```

### 3. 🌐 Akses Aplikasi
- Buka browser dan akses: `http://localhost:3838`
- Atau jika menggunakan custom port: `http://localhost:8080`

### 4. 📊 Mulai Menggunakan

#### Upload Data
1. Klik tab **"📊 Manajemen Data"**
2. Klik **"Muat Data Contoh"** untuk data sample
3. Atau upload file CSV Anda sendiri

#### Eksplorasi Data
1. Klik tab **"🔍 Eksplorasi Data"**
2. Pilih variabel yang ingin dianalisis
3. Pilih jenis plot (Histogram, Box Plot, dll.)
4. Klik **"Buat Plot"**

#### Analisis Statistik
1. Klik **"Hitung Statistik Deskriptif"**
2. Lihat interpretasi otomatis
3. Download plot dalam format PNG/JPG

#### Visualisasi Peta
1. Klik tab **"🗺️ Visualisasi Spasial"**
2. Pilih variabel untuk peta
3. Klik **"Buat Peta"**
4. Explore peta interaktif

## 🔧 Troubleshooting

### Error: Package tidak ditemukan
```r
# Install package secara manual
install.packages("nama_package")
```

### Error: Port sudah digunakan
```r
# Gunakan port lain
shiny::runApp("STARLIGHT_COMPLETE_V3.R", port = 8080)
```

### Error: Memory insufficient
- Gunakan data yang lebih kecil untuk testing
- Restart R session: `Ctrl+Shift+F10` (RStudio)

## 📈 Fitur Utama yang Bisa Langsung Dicoba

✅ **Dashboard Overview**: Lihat statistik dataset  
✅ **Data Upload**: Upload CSV atau gunakan sample data  
✅ **Visualisasi**: Histogram, Box Plot, Scatter Plot  
✅ **Statistik Deskriptif**: Dengan interpretasi otomatis  
✅ **Peta Choropleth**: Visualisasi spasial interaktif  
✅ **Download**: Export plot dan laporan  
✅ **Transformasi Data**: Log, standardisasi, kategorisasi  
✅ **Analisis Korelasi**: Correlation matrix  
✅ **Uji Statistik**: t-test, normalitas, regresi  

## 🎯 Tips Penggunaan

1. **Mulai dengan Sample Data**: Klik "Muat Data Contoh" untuk familiarisasi
2. **Pilih Variabel Numerik**: Untuk analisis statistik yang optimal
3. **Gunakan Interpretasi**: Baca panel interpretasi untuk memahami hasil
4. **Download Results**: Simpan plot dan laporan untuk dokumentasi
5. **Explore Interactive Maps**: Hover dan click pada peta untuk detail

## 📞 Bantuan
Jika mengalami masalah, periksa:
- Semua packages sudah terinstall
- File `STARLIGHT_COMPLETE_V3.R` ada di working directory
- R version minimal 4.0.0
- RStudio version terbaru (recommended)

---
**Happy Analyzing! 📊✨**