# =====================================================================
# SCRIPT SPATIAL WINDROSE MULTI-STASIUN BATCH (8 BLOK WAKTU HARIAN)
# =====================================================================

# 1. Load Library
library(readxl)    
library(dplyr)     
library(ggplot2)   
library(lubridate) 
library(RColorBrewer) 
library(sf)        
library(grid)      
library(stringr)
library(ggrepel)   

# 2. Pengaturan Direktori & Data Spasial
file_shp_sumut <- "D:/Cuhar/SHP_Master/kabupaten_sumut.shp"
file_shp_toba  <- "D:/Cuhar/SHP_Master/danau_toba.shp"
file_stasiun   <- "D:/Cuhar/Stasiun1.xlsx"
folder_angin   <- "D:/Cuhar/30May26"
folder_simpan  <- "D:/Cuhar/30May26/plot"

dir.create(folder_simpan, recursive = TRUE, showWarnings = FALSE)

# VARIABEL TEKS TANGGAL UTAMA
tanggal_teks <- "30 Mei 2026"

# Membaca shapefile dan menetapkan CRS (WGS 84)
shp_sumut <- st_read(file_shp_sumut, quiet = TRUE)
shp_toba  <- st_read(file_shp_toba, quiet = TRUE)
st_crs(shp_sumut) <- 4326
st_crs(shp_toba) <- 4326
data_stasiun <- read_excel(file_stasiun)

# 3. Parameter Stasiun dan Estetika
target_stasiun <- c("KNO", "Tigaras", "Silangit", "Aek Godang", "Sosa", "Hinai", "Bah Jambi")
level_arah <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
level_kecepatan <- c("< 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10", 
                     "10 - 12", "12 - 14", "14 - 16", "16 - 18", "18 - 20", "> 20")
warna_windrose <- colorRampPalette(rev(brewer.pal(11, "Spectral")))(11)
names(warna_windrose) <- level_kecepatan 

# Urutan 8 Blok Waktu Harian
urutan_blok <- c("07:00 - 10:00", "10:00 - 13:00", "13:00 - 16:00", 
                 "16:00 - 19:00", "19:00 - 22:00", "22:00 - 01:00", 
                 "01:00 - 04:00", "04:00 - 07:00")

# 4. Membaca & Pra-Pemrosesan Seluruh Data Angin Sekaligus
message("Mengumpulkan dan memproses data angin dari folder...")
daftar_file <- list.files(path = folder_angin, pattern = "\\.xlsx$", full.names = TRUE)
all_wind_data <- data.frame()

for (f in daftar_file) {
  tmp <- tryCatch({
    df <- read_excel(f)
    if ("Nama Stasiun" %in% names(df) && "wd_avg" %in% names(df)) {
      df_subset <- df[, c("Nama Stasiun", "Tanggal", "wd_avg", "ws_avg")]
      df_subset$`Nama Stasiun` <- as.character(df_subset$`Nama Stasiun`)
      df_subset$Tanggal <- as.character(df_subset$Tanggal)
      df_subset$wd_avg <- as.numeric(df_subset$wd_avg)
      df_subset$ws_avg <- as.numeric(df_subset$ws_avg)
      df_subset
    } else { NULL }
  }, error = function(e) NULL)
  
  if (!is.null(tmp)) all_wind_data <- bind_rows(all_wind_data, tmp)
}

# PRE-CALCULATION: Menghitung Blok Waktu & Faktor Angin di Awal agar Loop Lebih Cepat
all_wind_data <- all_wind_data %>%
  filter(!is.na(wd_avg) & !is.na(ws_avg)) %>%
  mutate(
    Waktu_WIB = with_tz(ymd_hms(Tanggal, tz = "UTC"), tzone = "Asia/Jakarta"),
    Jam = hour(Waktu_WIB),
    Blok_Hitung = (((Jam - 7) %% 24) %/% 3) * 3 + 7,
    Blok_Mulai = Blok_Hitung %% 24,
    Blok_Selesai = (Blok_Mulai + 3) %% 24,
    Label_Blok = sprintf("%02d:00 - %02d:00", Blok_Mulai, Blok_Selesai),
    WD_shifted = (wd_avg + 22.5) %% 360,
    Arah_Angin = factor(cut(WD_shifted, breaks = seq(0, 360, by = 45), right = FALSE, labels = level_arah), levels = level_arah),
    Kecepatan_Angin = factor(cut(ws_avg, breaks = c(-Inf, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, Inf), labels = level_kecepatan), levels = level_kecepatan)
  )

# 5. Fungsi Pembuat Grob Windrose Transparan
buat_windrose_transparan <- function(df_blok) {
  if(nrow(df_blok) == 0 || all(is.na(df_blok$Persentase) | df_blok$Persentase == 0)) {
    batas_maks_dinamis <- 10
  } else {
    akumulasi <- df_blok %>% group_by(Arah_Angin) %>% summarise(Total = sum(Persentase, na.rm=TRUE))
    nilai_max <- max(akumulasi$Total, na.rm = TRUE)
    batas_maks_dinamis <- ifelse(nilai_max == 0 || is.na(nilai_max), 10, (floor(nilai_max / 10) + 1) * 10)
  }
  
  p <- ggplot(df_blok, aes(x = Arah_Angin, y = Persentase, fill = Kecepatan_Angin)) +
    geom_bar(stat = "identity", width = 0.95, color = "black", linewidth = 0.1) + 
    coord_polar(theta = "x", start = -pi/8) + 
    scale_fill_manual(values = warna_windrose, drop = FALSE) + 
    scale_y_continuous(limits = c(0, batas_maks_dinamis), breaks = seq(0, batas_maks_dinamis, by = 10)) +
    geom_label(data = data.frame(y = seq(10, batas_maks_dinamis, by = 10)), 
               aes(x = 1.5, y = y, label = paste0(y, "%")), 
               inherit.aes = FALSE, size = 1.8, fontface = "bold", color = "black",
               fill = alpha("white", 0.6), label.size = 0, label.padding = unit(0.05, "lines")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 6, face = "bold", color = "black"),
      axis.text.y = element_blank(), axis.title = element_blank(),
      panel.grid.major = element_line(color = "gray40", linetype = "solid", linewidth = 0.2), 
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.position = "none" 
    )
  return(ggplotGrob(p))
}

# =====================================================================
# 6. LOOPING UTAMA: MEMBUAT PETA UNTUK SETIAP BLOK WAKTU
# =====================================================================

offset_ukuran <- 0.35 
jarak_label <- 0.45 
df_legend <- data.frame(longitude = 97, latitude = 0, Kecepatan = factor(level_kecepatan, levels = level_kecepatan))

for (blok_jam in urutan_blok) {
  message(">> Sedang memproses peta untuk blok waktu: ", blok_jam, " WIB ...")
  
  list_layers_windrose <- list()
  list_titik_stasiun <- list()
  
  for (kunci in target_stasiun) {
    koordinat <- data_stasiun %>% filter(grepl(kunci, Stasiun, ignore.case = TRUE)) %>% head(1)
    if(nrow(koordinat) == 0) next
    
    lon <- as.numeric(koordinat$longitude)
    lat <- as.numeric(koordinat$latitude)
    geser_x <- ifelse(grepl("Hinai|Tigaras|Aek", kunci, ignore.case = TRUE), -jarak_label, jarak_label)
    
    list_titik_stasiun[[kunci]] <- data.frame(Stasiun = kunci, longitude = lon, latitude = lat, nudge_x_val = geser_x)
    
    kunci_angin <- ifelse(toupper(kunci) == "KNO", "Kualanamu", kunci)
    
    # Memfilter data hanya untuk stasiun ini DAN blok jam ini
    data_filter <- all_wind_data %>%
      filter(grepl(kunci_angin, `Nama Stasiun`, ignore.case = TRUE)) %>%
      filter(Label_Blok == blok_jam) %>%
      count(Arah_Angin, Kecepatan_Angin, .drop = FALSE) %>%
      mutate(Persentase = ifelse(is.na(n/sum(n)), 0, (n/sum(n))*100))
    
    layer_grob <- annotation_custom(
      grob = buat_windrose_transparan(data_filter), 
      xmin = lon - offset_ukuran, xmax = lon + offset_ukuran, 
      ymin = lat - offset_ukuran, ymax = lat + offset_ukuran
    )
    list_layers_windrose[[kunci]] <- layer_grob
  }
  
  df_titik_all <- bind_rows(list_titik_stasiun)
  
  # Membuat Peta Final untuk blok waktu saat ini
  peta_sumut <- ggplot() +
    geom_sf(data = shp_sumut, fill = "#F2F2F2", color = "white", linewidth = 0.3) +
    geom_sf(data = shp_toba, fill = alpha("#87CEFA", 0.6), color = "#5DADE2", linewidth = 0.5) +
    
    geom_point(data = df_legend, aes(x = longitude, y = latitude, fill = Kecepatan), shape = 22, size = 0, color = "transparent") +
    scale_fill_manual(values = warna_windrose, name = "Kecepatan\nAngin (m/s)", drop = FALSE) +
    guides(fill = guide_legend(override.aes = list(size = 8, color = "black"))) +
    
    list_layers_windrose +
    geom_point(data = df_titik_all, aes(x = longitude, y = latitude), color = "red", size = 2, shape = 16) +
    
    geom_text_repel(
      data = df_titik_all, aes(x = longitude, y = latitude, label = Stasiun),
      nudge_x = df_titik_all$nudge_x_val, nudge_y = 0, direction = "y",                    
      color = "black", fontface = "bold", size = 4, segment.color = NA 
    ) +
    
    coord_sf(xlim = c(97.0, 100.8), ylim = c(0.5, 4.2)) + 
    
    # Judul otomatis menyesuaikan blok jam
    labs(title = paste("Wind Rose Sumatera Utara -", tanggal_teks),
         subtitle = paste("Periode Waktu:", blok_jam, "WIB"),
         x = "Longitude", y = "Latitude") +
    
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#2C3E50", margin = margin(b=15)),
      panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
      panel.background = element_rect(fill = "#E0F3F8", color = NA),
      legend.position = c(0.15, 0.65), 
      legend.background = element_rect(fill = alpha("white", 0.9), color = "gray50", linewidth = 0.5),
      legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
      legend.text = element_text(size = 12),
      legend.key.size = unit(0.7, "cm")
    )
  
  # Format penamaan file otomatis (Contoh: Peta_Windrose_0700_1000.png)
  nama_file_bersih <- str_replace_all(blok_jam, " - ", "_")
  nama_file_bersih <- str_replace_all(nama_file_bersih, ":", "")
  lokasi_simpan <- file.path(folder_simpan, paste0("Peta_Windrose_", nama_file_bersih, ".png"))
  
  ggsave(lokasi_simpan, plot = peta_sumut, width = 12, height = 12, dpi = 300)
}

message("=========================================================")
message("BERHASIL! 8 Peta Windrose untuk seluruh blok waktu telah diekspor ke folder plot.")