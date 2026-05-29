# ==============================================================================
# SCRIPT OTOMATISASI TERPADU: DOWNLOAD, REKAP, DAN VISUALISASI CURAH HUJAN BMKG
# ==============================================================================

# --- 1. LOAD SEMUA LIBRARIES ---
suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(magick)
  library(readxl)   
  library(writexl)  
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(tools)
  library(gridExtra) 
})

# --- 2. IDENTITAS PENYAMARAN BROWSER ---
identitas_browser <- add_headers(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
  `Referer` = "https://awscenter.bmkg.go.id/",
  `Accept` = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet, text/html, */*"
)

# ==========================================
# TAHAP 1: PROSES LOGIN & LEWATI CAPTCHA
# ==========================================
url_base <- "https://awscenter.bmkg.go.id/base"
url_login <- "https://awscenter.bmkg.go.id/base/verify"
url_captcha <- "https://awscenter.bmkg.go.id/public/assets/img/captcha.png"

print("Memulai sesi koneksi ke web AWSCenter BMKG...")
session <- handle(url_base)
GET(url_base, handle = session, identitas_browser) 

captcha_img <- GET(url_captcha, handle = session, identitas_browser)
img <- image_read(content(captcha_img, "raw"))
print(img) 

# ---> STOP SEMENTARA DI SINI: Masukkan Captcha <---
jawaban_captcha <- "-4" # GANTI ANGKA INI DENGAN HASIL DARI VIEWER!

payload <- list(
  username = "balai1",              
  password = "balai1@2020", 
  captcha = jawaban_captcha          
)

login_attempt <- POST(url_login, body = payload, encode = "form", handle = session, identitas_browser)

if (status_code(login_attempt) != 200) {
  stop("Login gagal! Pastikan password dan Captcha benar. Skrip dihentikan.")
}

print("Login sukses! Memulai proses unduh dan ekstraksi data...")

# ==========================================
# TAHAP 2: PERSIAPAN TABEL, TANGGAL & FOLDER
# ==========================================
daftar_stasiun <- data.frame(
  tipe = c("arg", "aaws", "aws", "aaws", "arg", "arg", "arg", "arg", "arg", "aws", "arg", "arg", "arg", "aws", "arg", "arg", "aws", "arg", "arg", "arg", "aaws", "arg", "aws", "arg", "aaws", "aws", "aws", "aws", "aws", "aws"),
  id = c("150260", "STA3212", "STA2068", "STS1001", "150106", "150109", "STA0259", "150262", "150259", "160044", "STA0178", "150108", "150110", "STA2295", "150107", "14032795", "160051", "150111", "150115", "150113", "STA3209", "STA0203", "STW1052", "STG1014", "STA3032", "STA5051", "STA5001", "STW1002", "STA2238", "STW1072"),
  nama = c("Sunggal", "Hinai", "Deliserdang", "Sei_Rejo", "Sinabung", "Salak", "Bahorok", "Merek", "Raya", "Parapat", "Balige", "Harian", "Pakkat", "Dolok Sanggul", "Sipoholon", "Lubuk Barumun", "Sosa", "Arse", "Kualuh Selatan", "Mompang", "Batubara", "Kota Pinang", "Bah Jambi", "Teluk Dalam", "Deli Serdang", "Aek Godang", "Kualanamu", "Belawan", "Tigaras", "Silangit"),
  stringsAsFactors = FALSE
)

# Masukkan tanggal Anda di sini
tgl_otomatis <- "2026-05-29" 
tgl_awal <- tgl_otomatis    
tgl_akhir <- tgl_otomatis

nama_folder <- format(as.Date(tgl_otomatis), "%d%b%y")
folder_simpan <- paste0("D:/Cuhar/", nama_folder, "/")
folder_plot <- paste0(folder_simpan, "Plot/") # Folder untuk hasil PDF

if (!dir.exists(folder_simpan)) dir.create(folder_simpan, recursive = TRUE)
if (!dir.exists(folder_plot)) dir.create(folder_plot, recursive = TRUE)

daftar_stasiun$RR_Maks_Harian <- NA 

# ==========================================
# TAHAP 3: PROSES UNDUH & FILTER MAKSIMUM
# ==========================================
for(i in 1:nrow(daftar_stasiun)) {
  
  tipe_alat <- daftar_stasiun$tipe[i]
  id_stasiun <- daftar_stasiun$id[i]
  nama_stasiun <- daftar_stasiun$nama[i]
  
  url_download <- paste0("https://apiaws.bmkg.go.id/rawdata/downloadaccesdata/", tipe_alat, "/", id_stasiun, "/", tgl_awal, "/", tgl_akhir)
  
  # PENYEMPURNAAN: Awalan "RAW_" dihapus agar nyambung ke proses visualisasi
  nama_file_excel <- paste0(folder_simpan, nama_stasiun, ".xlsx") 
  
  print(sprintf("[%02d/30] Memproses %s ...", i, nama_stasiun))
  
  unduhan <- GET(url_download, 
                 handle = session, 
                 identitas_browser, 
                 write_disk(nama_file_excel, overwrite = TRUE), 
                 config(http_version = 0))
  
  if(status_code(unduhan) == 200){
    tryCatch({
      raw_data <- read_excel(nama_file_excel)
      
      # PAKSA semua nama kolom menjadi huruf kecil agar R tidak tertipu
      names(raw_data) <- tolower(names(raw_data))
      
      # FILTER TANGGAL & WAKTU
      if ("tanggal" %in% names(raw_data)) {
        waktu_aktual <- as.POSIXct(raw_data$tanggal, tz = "UTC")
        jam_aktual <- format(waktu_aktual, "%H")
        tanggal_aktual <- format(waktu_aktual, "%Y-%m-%d")
        
        # EKSEKUSI: Ambil HANYA tanggal yang diinput, DAN buang jam 00
        filter_kondisi <- (tanggal_aktual == tgl_otomatis) & (jam_aktual != "00")
        raw_data <- raw_data[which(filter_kondisi), ]
      }
      
      # CARI MAKSIMUM
      if ("rr" %in% names(raw_data)) {
        kolom_rr <- as.numeric(raw_data$rr) 
        
        if(length(kolom_rr) == 0 || all(is.na(kolom_rr))) {
          daftar_stasiun$RR_Maks_Harian[i] <- 0
        } else {
          rr_maks <- max(kolom_rr, na.rm = TRUE)
          daftar_stasiun$RR_Maks_Harian[i] <- ifelse(is.infinite(rr_maks), 0, rr_maks)
        }
      } else {
        daftar_stasiun$RR_Maks_Harian[i] <- 0
      }
      
      print(sprintf("        -> Berhasil! RR Maks: %.1f mm", daftar_stasiun$RR_Maks_Harian[i]))
      
    }, error = function(e){
      print(paste("        -> Error R:", e$message))
    })
  } else { 
    print("        -> GAGAL diunduh dari server AWS.")
  }
  
  Sys.sleep(2) 
} 

# SIMPAN REKAPITULASI
nama_file_rekap <- paste0(folder_simpan, "REKAP_Curah_Hujan_", tgl_otomatis, ".xlsx")
write_xlsx(daftar_stasiun, nama_file_rekap)

print("=========================================================")
print("DOWNLOAD & REKAP SELESAI! Lanjut ke Visualisasi...")
print("=========================================================")

# ==============================================================================
# TAHAP 4: VISUALISASI CURAH HUJAN & EXPORT PDF (OTOMATIS TERSAMBUNG)
# ==============================================================================

# PENYEMPURNAAN: Sumbu waktu dihitung otomatis berdasarkan variabel tgl_otomatis
tgl_besok <- as.Date(tgl_otomatis) + 1
start_time <- ymd_hms(paste0(tgl_otomatis, " 00:01:00"), tz = "UTC")
end_time   <- ymd_hms(paste0(tgl_besok, " 00:00:00"), tz = "UTC")

zoom_start <- ymd_hms(paste0(tgl_otomatis, " 07:00:00"), tz = "Asia/Jakarta")
zoom_end   <- ymd_hms(paste0(tgl_besok, " 07:00:00"), tz = "Asia/Jakarta") 

# A. Parameter Peringatan Dini PERTAMA (Sesuaikan jam aktual manual setiap harinya)
row_peringatan1 <- data.frame(
  waktu_rilis  = ymd_hms(paste0(tgl_otomatis, " 14:00:00"), tz = "Asia/Jakarta"), 
  mulai_pred   = ymd_hms(paste0(tgl_otomatis, " 14:10:00"), tz = "Asia/Jakarta"), 
  akhir_pred   = ymd_hms(paste0(tgl_otomatis, " 17:10:00"), tz = "Asia/Jakarta"), 
  label_rilis  = "Peringatan Dini\n14:00 WIB" 
)

# B. Parameter Peringatan Dini UPDATE 1
row_peringatan2 <- data.frame(
  waktu_rilis  = ymd_hms(paste0(tgl_otomatis, " 16:30:00"), tz = "Asia/Jakarta"),
  mulai_pred   = ymd_hms(paste0(tgl_otomatis, " 17:00:00"), tz = "Asia/Jakarta"),
  akhir_pred   = ymd_hms(paste0(tgl_otomatis, " 20:00:00"), tz = "Asia/Jakarta"), 
  label_rilis  = "Update Peringatan\n16:30 WIB"
)

# C. Parameter Peringatan Dini UPDATE 2 
row_peringatan3 <- data.frame(
  waktu_rilis  = ymd_hms(paste0(tgl_otomatis, " 19:15:00"), tz = "Asia/Jakarta"),
  mulai_pred   = ymd_hms(paste0(tgl_otomatis, " 19:30:00"), tz = "Asia/Jakarta"),
  akhir_pred   = ymd_hms(paste0(tgl_otomatis, " 22:30:00"), tz = "Asia/Jakarta"), 
  label_rilis  = "Update Ke-2\n19:15 WIB"
)

# Membaca Otomatis 3 File Teks Peringatan dari folder yang sama
file_teks1 <- paste0(folder_simpan, "peringatan.txt")
if (file.exists(file_teks1)) { teks_peringatan1 <- paste(readLines(file_teks1, warn = FALSE), collapse = " ")
} else { teks_peringatan1 <- ""; cat("Warning: peringatan.txt tidak ditemukan.\n") }

file_teks2 <- paste0(folder_simpan, "update.txt")
if (file.exists(file_teks2)) { teks_peringatan2 <- paste(readLines(file_teks2, warn = FALSE), collapse = " ")
} else { teks_peringatan2 <- ""; cat("Warning: update.txt tidak ditemukan.\n") }

file_teks3 <- paste0(folder_simpan, "update2.txt")
if (file.exists(file_teks3)) { teks_peringatan3 <- paste(readLines(file_teks3, warn = FALSE), collapse = " ")
} else { teks_peringatan3 <- ""; cat("Warning: update2.txt tidak ditemukan.\n") }

# Fungsi Pembuat Grafik
buat_grafik_stasiun <- function(file_path, nama_stasiun) {
  
  ada_1 <- str_detect(tolower(teks_peringatan1), tolower(nama_stasiun))
  ada_2 <- str_detect(tolower(teks_peringatan2), tolower(nama_stasiun))
  ada_3 <- str_detect(tolower(teks_peringatan3), tolower(nama_stasiun))
  
  tabel_plot <- bind_rows(
    if(ada_1) row_peringatan1 else NULL,
    if(ada_2) row_peringatan2 else NULL,
    if(ada_3) row_peringatan3 else NULL
  )
  
  if (nrow(tabel_plot) == 0) { tabel_plot <- row_peringatan1[0, ] }
  
  data <- suppressMessages(read_excel(file_path))
  
  if (!is.POSIXct(data$Tanggal)) { data$Tanggal <- ymd_hms(data$Tanggal, tz = "UTC")
  } else { attr(data$Tanggal, "tzone") <- "UTC" }
  
  data$rr <- as.numeric(data$rr)
  
  data_filtered <- data %>% 
    filter(!is.na(rr)) %>%
    filter(Tanggal >= start_time & Tanggal <= end_time) %>%
    arrange(Tanggal)
  
  if(nrow(data_filtered) == 0) {
    data_filtered <- data.frame(Tanggal = seq(ymd_hms(paste0(tgl_otomatis, " 00:10:00"), tz="UTC"), end_time, by = "10 mins"), rr = 0)
  }
  
  data_minute <- data_filtered %>%
    mutate(rr = replace_na(rr, 0)) %>%
    mutate(Cumulative_Smooth = cummax(rr))
  
  data_10min <- data_minute %>%
    mutate(Waktu_10Min = ceiling_date(Tanggal, unit = "10 mins")) %>%
    group_by(Waktu_10Min) %>%
    summarise(max_rr_10min = max(Cumulative_Smooth, na.rm = TRUE), .groups = "drop") %>%
    mutate(Curah_Hujan_10Min = max_rr_10min - lag(max_rr_10min, default = 0)) %>%
    mutate(Curah_Hujan_10Min = ifelse(Curah_Hujan_10Min < 0, 0, Curah_Hujan_10Min))
  
  max_col  <- max(data_10min$Curah_Hujan_10Min, na.rm = TRUE)
  max_line <- max(data_minute$Cumulative_Smooth, na.rm = TRUE)
  
  if(max_col == 0 || is.infinite(max_col)) max_col <- 1
  if(max_line == 0 || is.infinite(max_line)) max_line <- 1
  
  batas_kiri   <- ceiling(max_col / 5) * 5     
  batas_kanan  <- ceiling(max_line / 20) * 20  
  scale_factor <- batas_kanan / batas_kiri
  grid_kiri  <- seq(0, batas_kiri, length.out = 6)
  grid_kanan <- seq(0, batas_kanan, length.out = 6)
  
  waktu_terakhir   <- max(data_minute$Tanggal, na.rm = TRUE)
  total_akumulatif <- max(data_minute$Cumulative_Smooth, na.rm = TRUE)
  if(is.infinite(total_akumulatif)) total_akumulatif <- 0
  
  plot_final <- ggplot() +
    geom_rect(data = tabel_plot, aes(xmin = mulai_pred, xmax = akhir_pred, ymin = 0, ymax = Inf), fill = "#FFD700", alpha = 0.15) +
    geom_text(data = tabel_plot, aes(x = mulai_pred + (akhir_pred - mulai_pred)/2, y = Inf, label = "Rentang Potensi"), color = "orange4", size = 2.2, fontface = "italic", vjust = 1.8) +
    geom_segment(data = data_10min, aes(x = with_tz(Waktu_10Min, "Asia/Jakarta"), xend = with_tz(Waktu_10Min, "Asia/Jakarta"), y = 0, yend = Curah_Hujan_10Min, color = "Curah Hujan Per 10 Menit"), linewidth = 0.8, lineend = "round", alpha = 0.9) +
    geom_line(data = data_minute, aes(x = with_tz(Tanggal, "Asia/Jakarta"), y = Cumulative_Smooth / scale_factor, color = "Akumulatif Rainfall"), linewidth = 0.9) +
    geom_vline(data = tabel_plot, aes(xintercept = waktu_rilis), linetype = "dashed", color = "#228B22", linewidth = 0.7) +
    geom_label(data = tabel_plot, aes(x = waktu_rilis, y = Inf, label = label_rilis), fill = "#228B22", color = "white", fontface = "bold", size = 2, vjust = -0.3, alpha = 0.85) +
    geom_point(aes(x = with_tz(waktu_terakhir, "Asia/Jakarta"), y = total_akumulatif / scale_factor), color = "#d62728", size = 1.5) +
    geom_text(aes(x = with_tz(waktu_terakhir, "Asia/Jakarta"), y = total_akumulatif / scale_factor, label = paste0(round(total_akumulatif, 1), " mm")), color = "#d62728", fontface = "bold", size = 2.5, hjust = 1.1, vjust = -1, show.legend = FALSE) +
    scale_y_continuous(name = "CH /10 Menit (mm)", limits = c(0, batas_kiri), breaks = grid_kiri, expand = expansion(mult = c(0, 0.08)), sec.axis = sec_axis(~ . * scale_factor, name = "Akumulasi (mm)", breaks = grid_kanan)) +
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M", expand = expansion(mult = c(0.02, 0.05)), timezone = "Asia/Jakarta") +
    labs(title = toupper(nama_stasiun), x = "Waktu (WIB)", subtitle = NULL) +
    scale_color_manual(name = "", values = c("Curah Hujan Per 10 Menit" = "#1f77b4", "Akumulatif Rainfall" = "#d62728"), drop = FALSE, guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), shape = c(NA, NA), linewidth = c(1.5, 1.2)))) +
    coord_cartesian(xlim = c(zoom_start, zoom_end), clip = "off") + 
    theme_minimal() +
    theme(
      legend.position = c(0.12, 0.80), 
      legend.direction = "vertical", 
      legend.background = element_rect(fill = alpha("white", 0.85), color = "grey70", linewidth = 0.4), 
      legend.margin = margin(t = 2, r = 5, b = 2, l = 5), 
      legend.text = element_text(size = 7), 
      panel.grid.major = element_line(color = "grey85", linewidth = 0.3), 
      panel.grid.minor = element_blank(), 
      axis.text.x = element_text(size = 7, angle = 0, hjust = 0.5), 
      axis.text.y = element_text(size = 7), 
      axis.title = element_text(size = 8), 
      axis.title.y.left = element_text(color = "#1f77b4", face = "bold"), 
      axis.text.y.left = element_text(color = "#1f77b4"), 
      axis.title.y.right = element_text(color = "#d62728", face = "bold"), 
      axis.text.y.right = element_text(color = "#d62728"), 
      panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.6), 
      plot.title = element_text(face = "bold", size = 10, hjust = 0, margin = margin(b=16)), 
      plot.margin = margin(t = 15, r = 15, b = 5, l = 5)
    )
  
  return(plot_final)
}

# Eksekusi Pengumpulan Grafik
# PERHATIKAN: Kita memfilter file Excel, dan mengecualikan file Rekap
daftar_file <- list.files(path = folder_simpan, pattern = "\\.xlsx$", full.names = TRUE)
daftar_file <- daftar_file[!grepl("REKAP", daftar_file)] 

list_semua_grafik <- list()

cat("\nMulai memproses pembuatan grafik dari file Excel...\n")
for (file_path in daftar_file) {
  nama_stasiun_bersih <- file_path_sans_ext(basename(file_path)) 
  
  plot_stasiun <- buat_grafik_stasiun(file_path = file_path, nama_stasiun = nama_stasiun_bersih)
  list_semua_grafik[[nama_stasiun_bersih]] <- plot_stasiun
  
  cat("Memproses Plot:", nama_stasiun_bersih, "\n")
}

# Eksport menjadi Laporan PDF A4
cat("\nMenyusun tata letak PDF. Harap tunggu sebentar...\n")

layout_pdf <- marrangeGrob(
  grobs = list_semua_grafik, 
  nrow = 4, 
  ncol = 1, 
  top = quote(paste("Verifikasi Peringatan DINI Tanggal", tgl_otomatis, "- Halaman", g, "dari", npages))
)

nama_pdf <- paste0(folder_plot, "Laporan_Curah_Hujan_", tgl_otomatis, ".pdf")
suppressWarnings(ggsave(
  filename = nama_pdf, 
  plot = layout_pdf, 
  width = 8.27,  
  height = 11.69, 
  units = "in",
  dpi = 300
))

cat("\n=======================================================\n")
cat("SELESAI TOTAL! File Raw, Rekap Excel, dan PDF Laporan \nberhasil tersimpan di folder:\n")
cat(folder_simpan, "\n")
cat("=======================================================\n")