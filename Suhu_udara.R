# =====================================================================
# SCRIPT VISUALISASI SUHU UDARA: 3 HARI (10 MENIT) - SORTING SUHU
# (VERSI OTOMATIS LINTAS BULAN & LINTAS TAHUN)
# =====================================================================

# 1. Load Library
library(readxl)    
library(dplyr)     
library(ggplot2)   
library(lubridate) 
library(stringr)

# =====================================================================
# 2. PENGATURAN TANGGAL DINAMIS (ANTI-ERROR LINTAS BULAN)
# =====================================================================

# Tentukan tanggal TERAKHIR dari rentang 3 hari (Bisa ganti jadi Sys.Date() jika mau otomatis)
# Contoh: Jika Anda ingin menarik data 30 Mei, 31 Mei, dan 1 Juni
tgl_terakhir <- as.Date("2026-06-02") 

# R otomatis menghitung 3 hari berurutan ke belakang dengan sangat akurat
tgl_vektor <- seq(tgl_terakhir - 2, tgl_terakhir, by = "days")

# MENGUNCI bahasa kalender ke "English" SEMENTARA agar format %b konsisten jadi "May", "Jun", dsb.
Sys.setlocale("LC_TIME", "English") 

# Membuat array 3 folder sumber secara otomatis (misal: "D:/Cuhar/30May26", "D:/Cuhar/31May26", "D:/Cuhar/01Jun26")
folder_sumber <- paste0("D:/Cuhar/", format(tgl_vektor, "%d%b%y"))

# Folder simpan dikunci HANYA di folder tanggal yang terakhir
folder_simpan <- paste0("D:/Cuhar/", format(tgl_terakhir, "%d%b%y"), "/plot")
dir.create(folder_simpan, recursive = TRUE, showWarnings = FALSE)

# Kembalikan kalender ke Bahasa Indonesia untuk penulisan Subtitle Grafik nanti
Sys.setlocale("LC_TIME", "Indonesian")

target_stasiun_awal <- c("KNO", "Tigaras", "Silangit", "Aek Godang", "Sosa", "Hinai", "Bah Jambi")


# =====================================================================
# 3. MEMBACA SELURUH DATA DARI 3 FOLDER
# =====================================================================
message("Mengumpulkan data suhu udara beresolusi 10 menit lintas bulan...")
daftar_file <- unlist(lapply(folder_sumber, function(dir) {
  list.files(path = dir, pattern = "\\.xlsx$", full.names = TRUE)
}))

if(length(daftar_file) == 0) stop("GAGAL: Tidak ada file Excel di ketiga folder tersebut.")

all_temp_data <- data.frame()

for (f in daftar_file) {
  tmp <- tryCatch({
    df <- read_excel(f)
    if ("Nama Stasiun" %in% names(df) && "tt_air_avg" %in% names(df)) {
      df_subset <- df[, c("Nama Stasiun", "Tanggal", "tt_air_avg")]
      df_subset$`Nama Stasiun` <- as.character(df_subset$`Nama Stasiun`)
      df_subset$Tanggal <- as.character(df_subset$Tanggal)
      df_subset$tt_air_avg <- as.numeric(df_subset$tt_air_avg)
      df_subset
    } else { NULL }
  }, error = function(e) NULL)
  
  if (!is.null(tmp)) all_temp_data <- bind_rows(all_temp_data, tmp)
}

if(nrow(all_temp_data) == 0) stop("GAGAL: Tidak ada data valid yang berhasil dibaca.")


# =====================================================================
# 4. PRA-PEMROSESAN & PENGURUTAN SUHU
# =====================================================================
data_suhu_kontinyu <- all_temp_data %>%
  filter(!is.na(tt_air_avg)) %>%
  mutate(
    Tanggal_Clean = str_remove(Tanggal, "\\+00"),
    Waktu_UTC = ymd_hms(Tanggal_Clean, tz = "UTC"),
    Waktu_WIB = with_tz(Waktu_UTC, tzone = "Asia/Jakarta"),
    Waktu_10m = round_date(Waktu_WIB, "10 mins")
  ) %>%
  group_by(`Nama Stasiun`, Waktu_10m) %>%
  summarise(Suhu_Rata = mean(tt_air_avg, na.rm = TRUE), .groups = 'drop')

data_suhu_kontinyu <- data_suhu_kontinyu %>%
  mutate(
    Nama_Clean = case_when(
      grepl("KNO|Kualanamu", `Nama Stasiun`, ignore.case = TRUE) ~ "KNO",
      grepl("Tigaras", `Nama Stasiun`, ignore.case = TRUE) ~ "Tigaras",
      grepl("Silangit", `Nama Stasiun`, ignore.case = TRUE) ~ "Silangit",
      grepl("Aek Godang", `Nama Stasiun`, ignore.case = TRUE) ~ "Aek Godang",
      grepl("Sosa", `Nama Stasiun`, ignore.case = TRUE) ~ "Sosa",
      grepl("Hinai", `Nama Stasiun`, ignore.case = TRUE) ~ "Hinai",
      grepl("Bah Jambi", `Nama Stasiun`, ignore.case = TRUE) ~ "Bah Jambi",
      TRUE ~ NA_character_ 
    )
  ) %>%
  filter(!is.na(Nama_Clean)) %>%
  mutate(`Nama Stasiun` = Nama_Clean) 

urutan_suhu <- data_suhu_kontinyu %>%
  group_by(`Nama Stasiun`) %>%
  summarise(Suhu_Keseluruhan = mean(Suhu_Rata, na.rm = TRUE)) %>%
  arrange(desc(Suhu_Keseluruhan)) %>%
  pull(`Nama Stasiun`)

data_suhu_kontinyu <- data_suhu_kontinyu %>%
  mutate(`Nama Stasiun` = factor(`Nama Stasiun`, levels = urutan_suhu))

target_stasiun_urut <- as.character(urutan_suhu)
message(">> Info: Urutan stasiun terpanas: ", paste(target_stasiun_urut, collapse = ", "))


# =====================================================================
# 5. PENGATURAN BATAS WAKTU METEOROLOGI (DINAMIS LINTAS BULAN)
# =====================================================================
# Menghitung jam 07:00 pada tanggal awal (H-2) dan 07:00 pada tanggal akhir + 1 (besoknya)
waktu_mulai   <- ymd_hms(paste0(tgl_vektor[1], " 07:00:00"), tz = "Asia/Jakarta")
waktu_selesai <- ymd_hms(paste0(tgl_terakhir + 1, " 07:00:00"), tz = "Asia/Jakarta")

garis_hari_meteorologi <- seq(waktu_mulai, waktu_selesai, by = "24 hours")

# Format string untuk teks subjudul (Contoh: "30 Mei - 01 Juni 2026")
teks_periode <- paste(format(tgl_vektor[1], "%d %B"), "-", format(tgl_terakhir, "%d %B %Y"))

label_waktu_tengah <- function(waktu_vektor) {
  hasil <- character(length(waktu_vektor))
  for (i in seq_along(waktu_vektor)) {
    if (is.na(waktu_vektor[i])) {
      hasil[i] <- ""
    } else {
      jam_teks <- format(waktu_vektor[i], "%H:00")
      if (hour(waktu_vektor[i]) == 19) {
        bulan_indo <- c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun", "Jul", "Ags", "Sep", "Okt", "Nov", "Des")
        tgl_teks <- paste0(format(waktu_vektor[i], "%d"), " ", bulan_indo[month(waktu_vektor[i])], " ", format(waktu_vektor[i], "%Y"))
        hasil[i] <- paste0(jam_teks, "\n", tgl_teks) 
      } else {
        hasil[i] <- jam_teks 
      }
    }
  }
  return(hasil)
}


# =====================================================================
# 6. PEMBUATAN PDF MULTI-HALAMAN
# =====================================================================
lokasi_pdf <- file.path(folder_simpan, "Grafik_Suhu_3Hari_PerStasiun.pdf")
pdf(file = lokasi_pdf, width = 11.69, height = 8.27)

stasiun_chunks <- split(target_stasiun_urut, ceiling(seq_along(target_stasiun_urut) / 4))

for (i in seq_along(stasiun_chunks)) {
  
  stasiun_di_halaman_ini <- stasiun_chunks[[i]]
  data_subset <- data_suhu_kontinyu %>% filter(`Nama Stasiun` %in% stasiun_di_halaman_ini)
  
  if (nrow(data_subset) == 0) next 
  
  plot_halaman <- ggplot(data_subset, aes(x = Waktu_10m, y = Suhu_Rata, color = `Nama Stasiun`)) +
    
    geom_vline(xintercept = garis_hari_meteorologi, linetype = "dashed", color = "gray40", linewidth = 0.7) +
    
    geom_line(linewidth = 0.6, alpha = 0.9) +
    geom_point(size = 0.3, alpha = 0.7) +
    
    facet_wrap(~ `Nama Stasiun`, ncol = 1, scales = "free_y") +
    scale_color_brewer(palette = "Set1") +
    
    scale_x_datetime(
      breaks = seq(waktu_mulai, waktu_selesai, by = "3 hours"), 
      date_minor_breaks = "1 hour",
      labels = label_waktu_tengah,                              
      expand = c(0.01, 0)
    ) +
    
    scale_y_continuous(breaks = seq(10, 40, by = 2), minor_breaks = seq(10, 40, by = 1)) +
    
    labs(
      title = "Suhu Udara Berdasarkan Stasiun ARG, AWS dan AAWS SUMUT (Resolusi 10 Menit)",
      subtitle = paste("Halaman", i, "| Periode:", teks_periode, "| Garis Vertikal: 07:00 WIB | Diurutkan dari Terpanas ke Terdingin"),
      x = "Waktu Pengamatan (WIB)",
      y = "Suhu Udara (°C)"
    ) +
    
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#2C3E50", margin = margin(b = 10)),
      
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9, color = "black"),
      axis.text.y = element_text(size = 9, color = "black"),
      axis.title.x = element_text(face = "bold", size = 10, margin = margin(t = 8)),
      axis.title.y = element_text(face = "bold", size = 10, margin = margin(r = 8)),
      
      strip.background = element_rect(fill = "#34495E"),
      strip.text = element_text(color = "white", face = "bold", size = 11, margin = margin(t = 5, b = 5)),
      
      panel.grid.major.y = element_line(color = "gray80", linewidth = 0.4),
      panel.grid.major.x = element_line(color = "gray80", linewidth = 0.4),
      panel.grid.minor.y = element_line(color = "gray92", linewidth = 0.2),
      panel.grid.minor.x = element_line(color = "gray92", linewidth = 0.2),
      
      legend.position = "none"
    )
  
  print(plot_halaman)
}

invisible(dev.off())

message("=========================================================")
message("BERHASIL! Stasiun diurutkan otomatis, PDF tersimpan di:\n", lokasi_pdf)