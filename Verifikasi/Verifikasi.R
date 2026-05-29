# ==============================================================================
# SCRIPT OTOMATISASI VISUALISASI CURAH HUJAN (ARG, AWS, AAWS SUMUT)
# VERSI FINAL - 3 LAPIS PERINGATAN DINI (.TXT) - 28 MEI 2026
# ==============================================================================

# --- 1. Load Libraries ---
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readxl)
  library(scales)
  library(tools)
  library(gridExtra) 
})

# --- 2. Parameter Global ---
tanggal_analisis <- "28Mei26"
folder_data   <- paste0("D:/Cuhar/", tanggal_analisis, "/") 
folder_output <- "D:/Cuhar/Plot/"

# A. Parameter Peringatan Dini PERTAMA (Sesuaikan jam aktual jika perlu)
row_peringatan1 <- data.frame(
  waktu_rilis  = ymd_hms("2026-05-28 14:00:00", tz = "Asia/Jakarta"), 
  mulai_pred   = ymd_hms("2026-05-28 14:10:00", tz = "Asia/Jakarta"), 
  akhir_pred   = ymd_hms("2026-05-28 17:10:00", tz = "Asia/Jakarta"), 
  label_rilis  = "Peringatan Dini\n14:00 WIB" 
)

# B. Parameter Peringatan Dini UPDATE 1
row_peringatan2 <- data.frame(
  waktu_rilis  = ymd_hms("2026-05-28 16:30:00", tz = "Asia/Jakarta"),
  mulai_pred   = ymd_hms("2026-05-28 17:00:00", tz = "Asia/Jakarta"),
  akhir_pred   = ymd_hms("2026-05-28 20:00:00", tz = "Asia/Jakarta"), 
  label_rilis  = "Update Peringatan\n16:30 WIB"
)

# C. Parameter Peringatan Dini UPDATE 2 
row_peringatan3 <- data.frame(
  waktu_rilis  = ymd_hms("2026-05-28 19:15:00", tz = "Asia/Jakarta"),
  mulai_pred   = ymd_hms("2026-05-28 19:30:00", tz = "Asia/Jakarta"),
  akhir_pred   = ymd_hms("2026-05-28 22:30:00", tz = "Asia/Jakarta"), # Berlangsung hingga dini hari 29 Mei
  label_rilis  = "Update Ke-2\n19:15 WIB"
)

# Batas Waktu Filter Data (28 Mei - Mulai 00:01 UTC)
start_time <- ymd_hms("2026-05-28 00:01:00", tz = "UTC")
end_time   <- ymd_hms("2026-05-29 00:00:00", tz = "UTC")

# Sumbu tampilan grafik 24 Jam (07:00 WIB - 07:00 WIB besoknya)
zoom_start <- ymd_hms("2026-05-28 07:00:00", tz = "Asia/Jakarta")
zoom_end   <- ymd_hms("2026-05-29 07:00:00", tz = "Asia/Jakarta") 

# --- 3. Membaca Otomatis 3 File Teks Peringatan ---
file_teks1 <- paste0(folder_data, "peringatan.txt")
if (file.exists(file_teks1)) {
  teks_peringatan1 <- paste(readLines(file_teks1, warn = FALSE), collapse = " ")
} else { teks_peringatan1 <- ""; cat("Warning: peringatan.txt tidak ditemukan.\n") }

file_teks2 <- paste0(folder_data, "update.txt")
if (file.exists(file_teks2)) {
  teks_peringatan2 <- paste(readLines(file_teks2, warn = FALSE), collapse = " ")
} else { teks_peringatan2 <- ""; cat("Warning: update.txt tidak ditemukan.\n") }

file_teks3 <- paste0(folder_data, "update2.txt")
if (file.exists(file_teks3)) {
  teks_peringatan3 <- paste(readLines(file_teks3, warn = FALSE), collapse = " ")
} else { teks_peringatan3 <- ""; cat("Warning: update2.txt tidak ditemukan.\n") }

# --- 4. Membangun Fungsi (Mengembalikan Grafik) ---
buat_grafik_stasiun <- function(file_path, nama_stasiun) {
  
  # a. Logika Filter Peringatan 3 Lapis (Triple-Check)
  ada_1 <- str_detect(tolower(teks_peringatan1), tolower(nama_stasiun))
  ada_2 <- str_detect(tolower(teks_peringatan2), tolower(nama_stasiun))
  ada_3 <- str_detect(tolower(teks_peringatan3), tolower(nama_stasiun))
  
  tabel_plot <- bind_rows(
    if(ada_1) row_peringatan1 else NULL,
    if(ada_2) row_peringatan2 else NULL,
    if(ada_3) row_peringatan3 else NULL
  )
  
  if (nrow(tabel_plot) == 0) {
    tabel_plot <- row_peringatan1[0, ]
  }
  
  # b. Memproses Data Excel
  data <- suppressMessages(read_excel(file_path))
  
  if (!is.POSIXct(data$Tanggal)) {
    data$Tanggal <- ymd_hms(data$Tanggal, tz = "UTC")
  } else {
    attr(data$Tanggal, "tzone") <- "UTC"
  }
  data$rr <- as.numeric(data$rr)
  
  data_filtered <- data %>% 
    filter(!is.na(rr)) %>%
    filter(Tanggal >= start_time & Tanggal <= end_time) %>%
    arrange(Tanggal)
  
  if(nrow(data_filtered) == 0) {
    # Data dummy menggunakan format 10 menitan jika kosong
    data_filtered <- data.frame(Tanggal = seq(ymd_hms("2026-05-28 00:10:00", tz="UTC"), end_time, by = "10 mins"), rr = 0)
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
  
  # c. Membuat Plot
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
    
    labs(
      title = toupper(nama_stasiun), 
      x = "Waktu (WIB)",
      subtitle = NULL
    ) +
    
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

# --- 5. Eksekusi Pengumpulan Grafik (Looping) ---
daftar_file <- list.files(path = folder_data, pattern = "\\.xlsx$", full.names = TRUE)
if(!dir.exists(folder_output)) dir.create(folder_output)

list_semua_grafik <- list()

cat("\nMulai memproses file Excel...\n")
for (file_path in daftar_file) {
  
  # Karena nama file sudah bersih (misal: "Kualanamu.xlsx"),
  # kita cukup mengekstrak nama dasarnya saja.
  nama_stasiun_bersih <- file_path_sans_ext(basename(file_path)) 
  
  plot_stasiun <- buat_grafik_stasiun(file_path = file_path, nama_stasiun = nama_stasiun_bersih)
  list_semua_grafik[[nama_stasiun_bersih]] <- plot_stasiun
  
  cat("Memproses:", nama_stasiun_bersih, "\n")
}

# --- 6. Eksport menjadi Laporan PDF A4 ---
cat("\nMenyusun tata letak PDF. Harap tunggu sebentar...\n")

layout_pdf <- marrangeGrob(
  grobs = list_semua_grafik, 
  nrow = 4, 
  ncol = 1, 
  top = quote(paste("Verifikasi Peringatan DINI Tanggal 28 Mei 2026 - Halaman", g, "dari", npages))
)

nama_pdf <- paste0(folder_output, "Laporan_Curah_Hujan_28Mei2026.pdf")
suppressWarnings(ggsave(
  filename = nama_pdf, 
  plot = layout_pdf, 
  width = 8.27,  
  height = 11.69, 
  units = "in",
  dpi = 300
))

cat("\n=======================================================\n")
cat("BERHASIL! Cek file PDF Laporan di folder:", folder_output, "\n")
cat("=======================================================\n")