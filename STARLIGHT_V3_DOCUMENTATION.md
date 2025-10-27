# STARLIGHT v3.0 - Dashboard Analisis Statistik Komprehensif
## Statistical Analysis and Research Laboratory for Intelligent Geospatial Handling and Testing

### 📋 Deskripsi Aplikasi
STARLIGHT v3.0 adalah dashboard analisis statistik komprehensif yang dirancang khusus untuk analisis Indeks Kerentanan Sosial (Social Vulnerability Index). Aplikasi ini dikembangkan menggunakan R Shiny dengan antarmuka yang user-friendly dan fitur-fitur analisis statistik yang lengkap.

---

## 🚀 Fitur Utama

### 1. 🏠 **Beranda (Dashboard)**
- **Value Boxes**: Menampilkan ringkasan data secara real-time
  - Total observasi
  - Total variabel
  - Jumlah variabel numerik
  - Persentase data hilang
- **Informasi Dataset**: Detail teknis dataset
- **Statistik Ringkas**: Tabel interaktif dengan statistik deskriptif dasar

### 2. 📊 **Manajemen Data**
#### Upload dan Import Data
- Upload file CSV dengan pengaturan separator dan quote
- Auto-detection format file
- Fallback ke data lokal atau sample data
- Download data yang telah diproses

#### Transformasi Data
- **Transformasi Matematika**:
  - Log Natural
  - Log10
  - Akar Kuadrat
  - Kuadrat
  - Standardisasi (Z-score)
  - Normalisasi (Min-Max)

#### Kategorisasi Data
- Fungsi `kategorisasi_aman()` dengan error handling
- Pemilihan jumlah kategori (2-10)
- Otomatis menangani data non-numerik dan edge cases

#### Penanganan Missing Values
- **Metode Imputasi**:
  - Mean/Mode
  - Median
  - Forward Fill
  - Backward Fill
  - Linear Interpolation
- Visualisasi missing values pattern

#### Layout Data yang Diperbaiki
- **Data Mentah**: Ditampilkan di bagian atas dengan scrolling
- **Ringkasan Variabel**: Tabel informasi variabel di bagian bawah
- Styling yang konsisten dengan color coding untuk missing values

### 3. 🔍 **Eksplorasi Data**
#### Visualisasi Interaktif
- **Jenis Plot**:
  - Histogram dengan binning otomatis
  - Box Plot untuk deteksi outlier
  - Density Plot untuk distribusi
  - Scatter Plot dengan regression line
  - Correlation Plot dengan clustering

#### Layout yang Diperbaiki
- **Panel Kontrol**: Width 3 (kiri)
- **Panel Visualisasi**: Width 9 (kanan dan bawah)
- Margin dan spacing yang optimal

#### Statistik Deskriptif
- Fungsi `interpret_descriptive()` untuk interpretasi otomatis
- Analisis skewness dan kurtosis
- Koefisien variasi
- Error handling untuk data non-numerik

#### Download Plots
- Export PNG (high resolution)
- Export JPG (compressed)
- Custom filename dengan timestamp

### 4. 📈 **Analisis Inferensial**
#### Uji Statistik Tersedia
- **Uji t Satu Sampel**: Dengan pengaturan α dan μ₀
- **Uji t Dua Sampel**: Independent dan paired
- **Uji Normalitas**:
  - Shapiro-Wilk Test
  - Kolmogorov-Smirnov Test
  - Anderson-Darling Test
- **Regresi Linear**: Multiple regression dengan R²
- **ANOVA**: One-way dan two-way (framework)

#### Interpretasi Otomatis
- Keputusan hipotesis berdasarkan p-value
- Confidence intervals
- Effect size interpretation

### 5. 🗺️ **Visualisasi Spasial**
#### Peta Choropleth Interaktif
- **Error Handling yang Diperbaiki**: Mengatasi "object 'id_col' not found"
- **ID Column Management**: Otomatis menambahkan kolom ID jika tidak ada
- **Fallback Spatial Data**: Membuat dummy coordinates jika data spasial tidak tersedia

#### Fitur Peta
- **Color Palettes**: Viridis, Plasma, Blues, Reds, Greens
- **Interactive Features**:
  - Hover labels
  - Click popups
  - Zoom dan pan
  - Legend dengan positioning
- **Customization**:
  - Opacity control
  - Number of breaks
  - Show/hide labels dan legend

#### Statistik Spasial
- Descriptive statistics untuk variabel peta
- Quartile-based interpretation
- Spatial pattern analysis

### 6. 📋 **Generator Laporan**
#### Format Output
- **Word Document (.docx)**: Menggunakan `officer` package
- **PDF Document (.pdf)**: Framework siap
- **HTML Report (.html)**: Framework siap

#### Komponen Laporan
- Ringkasan data
- Statistik deskriptif
- Visualisasi
- Analisis korelasi
- Uji statistik
- Analisis spasial

#### Preview dan Customization
- Live preview dalam aplikasi
- Custom title dan author
- Modular content selection

### 7. ℹ️ **Tentang**
- Informasi aplikasi lengkap
- Teknologi yang digunakan
- Fitur-fitur utama
- Credits dan acknowledgments

---

## 🔧 Perbaikan dan Peningkatan

### 1. **Layout Manajemen Data**
✅ **DIPERBAIKI**: Data mentah sekarang ditampilkan di atas, ringkasan variabel di bawah
- Menggunakan `fluidRow` terpisah untuk setiap section
- Scrolling container untuk data mentah
- Styling yang konsisten

### 2. **Error Kategorisasi Data**
✅ **DIPERBAIKI**: Fungsi `kategorisasi_aman()` dengan comprehensive error handling
```r
kategorisasi_aman <- function(data, col_name, n_bins = 5) {
  tryCatch({
    # Validasi input
    if (!is.numeric(data[[col_name]]) || length(data[[col_name]]) == 0) {
      return(rep("Tidak Valid", nrow(data)))
    }
    
    # Handle edge cases
    values <- data[[col_name]][!is.na(data[[col_name]])]
    if (length(unique(values)) <= 1) {
      return(rep("Kategori Tunggal", nrow(data)))
    }
    
    # Safe quantile calculation
    breaks <- quantile(values, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
    breaks <- unique(breaks)
    
    # Fallback untuk data dengan variabilitas rendah
    if (length(breaks) <= 2) {
      return(ifelse(data[[col_name]] <= median(values, na.rm = TRUE), "Rendah", "Tinggi"))
    }
    
    labels <- paste0("Kategori ", 1:(length(breaks) - 1))
    cut(data[[col_name]], breaks = breaks, labels = labels, include.lowest = TRUE)
  }, error = function(e) {
    return(rep("Error", nrow(data)))
  })
}
```

### 3. **Non-numeric Argument Error**
✅ **DIPERBAIKI**: Statistik deskriptif sekarang hanya memproses variabel numerik
```r
observeEvent(input$hitung_deskriptif, {
  req(input$selected_vars)
  
  tryCatch({
    selected_data <- values$data[input$selected_vars]
    numeric_data <- selected_data[sapply(selected_data, is.numeric)]
    
    if (ncol(numeric_data) > 0) {
      desc_stats <- describe(numeric_data)
      # ... processing
    } else {
      showNotification("Tidak ada variabel numerik yang dipilih!", type = "warning")
    }
  }, error = function(e) {
    showNotification(paste("Error menghitung statistik:", e$message), type = "error")
  })
})
```

### 4. **Layout Visualisasi**
✅ **DIPERBAIKI**: Plot sekarang menggunakan layout yang optimal
- Control panel: `width = 3` (kiri)
- Visualization panel: `width = 9` (kanan dan bawah)
- CSS styling untuk positioning yang tepat

### 5. **Error Korelasi**
✅ **DIPERBAIKI**: Fungsi `korelasi_aman()` dengan validation
```r
korelasi_aman <- function(data) {
  tryCatch({
    numeric_data <- data[sapply(data, is.numeric)]
    if (ncol(numeric_data) < 2) {
      return(NULL)
    }
    
    # Remove columns dengan semua NA atau nilai konstan
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
```

### 6. **Choropleth Map Error**
✅ **DIPERBAIKI**: "object 'id_col' not found" error resolved
```r
observeEvent(input$create_map, {
  req(input$map_variable)
  
  tryCatch({
    if (!is.null(values$spatial_data) && !is.null(values$data)) {
      # Ensure spatial data has id column
      if (!"districtkd" %in% names(values$spatial_data)) {
        values$spatial_data$districtkd <- paste0("ID", sprintf("%03d", 1:nrow(values$spatial_data)))
      }
      
      # Safe merge dengan error handling
      map_data <- merge(values$spatial_data, values$data, by = "districtkd", all.x = TRUE)
      # ... rest of processing
    }
  }, error = function(e) {
    showNotification(paste("Error membuat peta:", e$message), type = "error")
  })
})
```

### 7. **Interpretasi yang Diperbaiki**
✅ **DIPERBAIKI**: Fungsi `interpret_descriptive()` memberikan interpretasi yang comprehensive
- Analisis distribusi (skewness, kurtosis)
- Variabilitas data (coefficient of variation)
- Interpretasi dalam Bahasa Indonesia yang mudah dipahami

### 8. **Download Event Handling**
✅ **DIPERBAIKI**: Implementasi download handlers untuk semua format
```r
# PNG Download
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

# DOCX Report Download
output$download_report <- downloadHandler(
  filename = function() {
    paste0("STARLIGHT_Report_", Sys.Date(), ".", input$report_format)
  },
  content = function(file) {
    if (input$report_format == "docx") {
      doc <- read_docx()
      # ... document creation
      print(doc, target = file)
    }
  }
)
```

---

## 📦 Dependencies dan Libraries

### Core Shiny Libraries
```r
library(shiny)           # Web application framework
library(shinydashboard)  # Dashboard layout
library(DT)              # Interactive tables
library(shinyWidgets)    # Enhanced UI widgets
library(shinycssloaders) # Loading spinners
```

### Data Visualization
```r
library(ggplot2)         # Static plots
library(plotly)          # Interactive plots
library(leaflet)         # Interactive maps
library(corrplot)        # Correlation matrices
library(RColorBrewer)    # Color palettes
library(viridis)         # Perceptually uniform colors
```

### Data Processing
```r
library(dplyr)           # Data manipulation
library(tidyr)           # Data reshaping
library(readr)           # Fast data reading
library(stringr)         # String manipulation
```

### Statistical Analysis
```r
library(psych)           # Descriptive statistics
library(car)             # Regression diagnostics
library(nortest)         # Normality tests
library(boot)            # Bootstrap methods
library(lmtest)          # Linear model tests
library(Hmisc)           # Miscellaneous functions
library(sandwich)        # Robust standard errors
```

### Spatial Analysis
```r
library(sf)              # Simple features for spatial data
```

### Missing Values
```r
library(VIM)             # Visualization of missing values
library(mice)            # Multiple imputation
```

### Report Generation
```r
library(officer)         # Word document creation
library(flextable)       # Flexible tables
library(knitr)           # Dynamic report generation
library(rmarkdown)       # R Markdown processing
```

### Utilities
```r
library(htmlwidgets)     # HTML widgets
library(reshape2)        # Data reshaping
library(broom)           # Tidy model outputs
library(gridExtra)       # Arrange multiple plots
library(jsonlite)        # JSON processing
```

---

## 🚀 Cara Menjalankan Aplikasi

### 1. **Persiapan Environment**
```r
# Install required packages jika belum tersedia
required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "plotly", "leaflet",
  "sf", "dplyr", "corrplot", "psych", "car", "nortest", "flextable",
  "officer", "downloadthis", "shinyWidgets", "shinycssloaders", 
  "htmlwidgets", "RColorBrewer", "viridis", "reshape2", "broom", 
  "VIM", "mice", "gridExtra", "knitr", "rmarkdown", "jsonlite", 
  "boot", "lmtest", "Hmisc", "sandwich", "readr", "tidyr", "stringr"
)

# Check dan install packages yang missing
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(missing_packages)) install.packages(missing_packages)
```

### 2. **Menjalankan Aplikasi**
```r
# Method 1: Source file dan run
source("STARLIGHT_COMPLETE_V3.R")

# Method 2: Run dari file
shiny::runApp("STARLIGHT_COMPLETE_V3.R")

# Method 3: Run dengan custom port
shiny::runApp("STARLIGHT_COMPLETE_V3.R", port = 8080)
```

### 3. **Akses Aplikasi**
- **Local**: `http://localhost:3838` (default port)
- **Custom Port**: `http://localhost:[PORT]`
- **Network**: `http://[IP_ADDRESS]:[PORT]`

---

## 📁 Struktur Data

### Data Format yang Didukung
- **CSV Files**: Dengan berbagai separator (comma, semicolon, tab)
- **Online Data**: URL ke raw CSV files
- **Local Files**: Path ke file lokal

### Sample Data Structure
```
districtkd    CHILDREN  FEMALE  ELDERLY  FHEAD     FAMILYSIZE
ID001         25.30     52.1    12.5     18.2      4.2
ID002         30.15     48.7    8.9      22.1      3.8
ID003         22.80     54.3    15.2     16.5      4.6
...
```

### Spatial Data Support
- **GeoJSON**: Preferred format
- **Shapefile**: .shp dengan supporting files
- **Auto-generated**: Dummy coordinates jika data spasial tidak tersedia

---

## 🎯 Target Pengguna

### 1. **Akademisi dan Peneliti**
- Analisis data survei sosial
- Penelitian indeks kerentanan
- Publikasi ilmiah

### 2. **Pemerintah dan NGO**
- Perencanaan kebijakan publik
- Monitoring program sosial
- Evaluasi dampak program

### 3. **Mahasiswa**
- Tugas akhir dan skripsi
- Pembelajaran statistik
- Praktikum analisis data

### 4. **Data Analyst**
- Exploratory data analysis
- Report generation
- Data visualization

---

## 🔮 Future Enhancements

### Planned Features
- [ ] Machine Learning integration
- [ ] Time series analysis
- [ ] Advanced spatial statistics
- [ ] Real-time data streaming
- [ ] Multi-language support
- [ ] Cloud deployment options

### Performance Improvements
- [ ] Lazy loading untuk dataset besar
- [ ] Caching untuk visualisasi
- [ ] Parallel processing untuk analisis
- [ ] Memory optimization

---

## 📞 Support dan Dokumentasi

### Troubleshooting
- **Memory Issues**: Gunakan data sampling untuk dataset besar
- **Package Conflicts**: Update ke versi terbaru semua packages
- **Spatial Data**: Pastikan CRS consistency

### Best Practices
- **Data Preparation**: Clean data sebelum upload
- **Variable Selection**: Pilih variabel yang relevan
- **Interpretation**: Selalu validasi hasil statistik

---

## 📄 License dan Credits

**Developed for**: UAS Komputasi Statistik  
**Institution**: Politeknik Statistika STIS  
**Year**: 2024  
**Version**: 3.0  

**Framework**: R Shiny  
**Visualization**: ggplot2, leaflet, plotly  
**Statistics**: psych, car, nortest  
**Spatial**: sf package  

---

*STARLIGHT v3.0 - Statistical Analysis and Research Laboratory for Intelligent Geospatial Handling and Testing*