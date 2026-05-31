# =====================================================================
# SCRIPT OTOMATISASI WINDROSE (8 MATA ANGIN) -> 1 PDF MULTI-HALAMAN
# =====================================================================

# 1. Load library
library(readxl)    
library(dplyr)     
library(ggplot2)   
library(lubridate) 
library(RColorBrewer) 
library(patchwork) 
library(stringr)   

# 2. Pengaturan Parameter Global
folder_path <- "D:/Cuhar/28May26"
tanggal_teks <- "28 Mei 2026" 

# Membuat direktori 'plot' di dalam folder jika belum ada
folder_plot <- file.path(folder_path, "plot")
if (!dir.exists(folder_plot)) {
  dir.create(folder_plot)
  message("Folder 'plot' berhasil dibuat di: ", folder_plot)
}

daftar_file_excel <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

urutan_blok <- c("07:00 - 10:00", "10:00 - 13:00", "13:00 - 16:00", 
                 "16:00 - 19:00", "19:00 - 22:00", "22:00 - 01:00", 
                 "01:00 - 04:00", "04:00 - 07:00")

# Level 8 mata angin
level_arah <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
level_kecepatan <- c("< 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10", 
                     "10 - 12", "12 - 14", "14 - 16", "16 - 18", 
                     "18 - 20", "> 20")

warna_windrose <- colorRampPalette(rev(brewer.pal(11, "Spectral")))(11)
names(warna_windrose) <- level_kecepatan 

# 3. Fungsi Pembuat 1 Kotak Windrose (Dengan Perbaikan Logika Skala & Label)
buat_windrose_premium <- function(df_blok, nama_blok) {
  
  if(nrow(df_blok) == 0 || all(is.na(df_blok$Persentase) | df_blok$Persentase == 0)) {
    batas_maks_dinamis <- 10
  } else {
    # PERBAIKAN 1: Menghitung total tumpukan persentase di setiap arah angin
    akumulasi_arah <- df_blok %>%
      group_by(Arah_Angin) %>%
      summarise(Total_Persen = sum(Persentase, na.rm = TRUE))
    
    nilai_max_persen <- max(akumulasi_arah$Total_Persen, na.rm = TRUE)
    
    if (nilai_max_persen == 0 || is.na(nilai_max_persen)) {
      batas_maks_dinamis <- 10
    } else {
      batas_maks_dinamis <- (floor(nilai_max_persen / 10) + 1) * 10
    }
  }
  
  ggplot(df_blok, aes(x = Arah_Angin, y = Persentase, fill = Kecepatan_Angin)) +
    geom_bar(stat = "identity", width = 0.95, color = "black", linewidth = 0.15) + 
    coord_polar(theta = "x", start = -pi/8) + 
    scale_fill_manual(values = warna_windrose, drop = FALSE) + 
    scale_y_continuous(limits = c(0, batas_maks_dinamis), breaks = seq(0, batas_maks_dinamis, by = 10)) +
    
    # PERBAIKAN 2: Menggeser Label ke x = 1.5 (di antara N dan NE) agar tidak menutupi grafik arah Utara
    geom_label(data = data.frame(y = seq(10, batas_maks_dinamis, by = 10)), 
               aes(x = 1.5, y = y, label = paste0(y, "%")), 
               inherit.aes = FALSE, size = 2.8, fontface = "bold", color = "black",
               fill = alpha("white", 0.5), label.size = 0, label.padding = unit(0.1, "lines")) +
    
    facet_wrap(~ Blok_3_Jam) + 
    theme_minimal() +
    labs(x = NULL, y = NULL, fill = "Kecepatan\n(m/s)") +
    theme(
      axis.text.x = element_text(size = 9, face = "bold", color = "black"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_line(color = "gray70", linetype = "solid", linewidth = 0.35), 
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(size = 11, face = "bold", color = "white"),
      strip.background = element_rect(fill = "#2C3E50", color = NA) 
    )
}

# =====================================================================
# 4. MEMBUKA KONEKSI PDF & LOOP UTAMA
# =====================================================================
total_sukses <- 0
nama_file_pdf <- file.path(folder_plot, paste0("Kompilasi_Windrose_8Arah_", str_replace_all(tanggal_teks, " ", "_"), ".pdf"))

pdf(file = nama_file_pdf, width = 11.69, height = 8.27)

for (file_excel in daftar_file_excel) {
  data_mentah <- tryCatch(read_excel(file_excel), error = function(e) NULL)
  
  if (!is.null(data_mentah) && "Nama Stasiun" %in% names(data_mentah)) {
    data_target <- data_mentah %>%
      filter(str_detect(`Nama Stasiun`, regex("AWS|AAWS", ignore_case = TRUE)))
    
    stasiun_unik <- unique(data_target$`Nama Stasiun`)
    
    for (nama_stasiun in stasiun_unik) {
      message("Menambahkan stasiun ke PDF: ", nama_stasiun, " ...")
      
      data_stasiun_ini <- data_target %>%
        filter(`Nama Stasiun` == nama_stasiun) %>%
        filter(!is.na(wd_avg) & !is.na(ws_avg))
      
      if (nrow(data_stasiun_ini) == 0) {
        message(" -> Data angin kosong, dilewati.")
        next
      }
      
      data_bersih <- data_stasiun_ini %>%
        mutate(
          Waktu_UTC = ymd_hms(Tanggal, tz = "UTC"),
          Waktu_WIB = with_tz(Waktu_UTC, tzone = "Asia/Jakarta"),
          Jam = hour(Waktu_WIB),
          Blok_Hitung = (((Jam - 7) %% 24) %/% 3) * 3 + 7,
          Blok_Mulai = Blok_Hitung %% 24, 
          Blok_Selesai = (Blok_Mulai + 3) %% 24, 
          Label_Blok = sprintf("%02d:00 - %02d:00", Blok_Mulai, Blok_Selesai),
          Blok_3_Jam = factor(Label_Blok, levels = urutan_blok)
        )
      
      data_summary_3jam <- data_bersih %>%
        mutate(
          WD_shifted = (wd_avg + 22.5) %% 360,
          Arah_Angin = factor(cut(WD_shifted, breaks = seq(0, 360, by = 45), right = FALSE, labels = level_arah), levels = level_arah),
          Kecepatan_Angin = factor(cut(ws_avg, breaks = c(-Inf, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, Inf), labels = level_kecepatan), levels = level_kecepatan)
        ) %>%
        count(Blok_3_Jam, Arah_Angin, Kecepatan_Angin, .drop = FALSE) %>%
        group_by(Blok_3_Jam) %>%
        mutate(Persentase = (n / sum(n)) * 100) %>%
        mutate(Persentase = ifelse(is.na(Persentase), 0, Persentase)) %>% 
        ungroup()
      
      list_data <- split(data_summary_3jam, data_summary_3jam$Blok_3_Jam)
      
      daftar_plot <- lapply(names(list_data), function(nama) {
        buat_windrose_premium(list_data[[nama]], nama)
      })
      
      plot_gabungan <- wrap_plots(daftar_plot, ncol = 4) + 
        plot_layout(guides = "collect") + 
        plot_annotation(
          title = paste("Windrose Diurnal Tanggal", tanggal_teks),
          subtitle = paste("Stasiun:", nama_stasiun),
          theme = theme(
            plot.title = element_text(face = "bold", hjust = 0.5, size = 18, margin = margin(b = 5)),
            plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 14, color = "#2C3E50", margin = margin(b = 15)),
            plot.background = element_rect(fill = "white", color = NA)
          )
        )
      
      print(plot_gabungan)
      total_sukses <- total_sukses + 1
    }
  }
}

dev.off()

message("=========================================================")
message("PROSES SELESAI! Total stasiun direkam: ", total_sukses)
message("File PDF (8 Arah + Fix Skala) tersimpan di: ", nama_file_pdf)