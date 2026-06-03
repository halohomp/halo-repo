# ==============================================================================
# SCRIPT MONITORING CURAH HUJAN HARIAN (SINGLE MAP - A4 SIZE + STREET MAP)
# TANGGAL: 3 JUNI 2026
# Diolah Oleh: Observer Stasiun 96041 Medan
# ==============================================================================

# --- 1. LOAD LIBRARIES ---
library(sf)
library(readxl)
library(ggplot2)
library(ggspatial)
library(dplyr)
library(tidyr)
library(stringr)
library(ggrepel)
library(forcats)

# --- 2. PENGATURAN TANGGAL & PATH FILE ---
tgl_plot    <- "3" 
bulan_tahun <- "Juni 2026"

path_shp    <- "D:/Cuhar/SHP_Master/"
path_excel  <- "D:/Cuhar/Stasiun.xlsx"

# --- 3. LOAD DATA SPASIAL & EXCEL ---
# Validasi ketersediaan folder dan file sebelum running
if (!dir.exists(path_shp)) stop("Error: Folder SHP_Master tidak ditemukan! Periksa kembali path Anda.")
if (!file.exists(path_excel)) stop("Error: File Stasiun.xlsx tidak ditemukan! Periksa kembali path Anda.")

# Membaca Data SHP dan menetapkan CRS ke WGS 84 (EPSG:4326)
peta_sumut  <- read_sf(paste0(path_shp, "DAS Sumut.shp")) %>% st_set_crs(4326)
danau_toba  <- read_sf(paste0(path_shp, "danau_toba.shp")) %>% st_set_crs(4326)
sungai      <- read_sf(paste0(path_shp, "RIV_SUMUT_CLIP.shp")) %>% st_set_crs(4326)

# Membaca Data Excel
data_mentah <- read_excel(path_excel)

# --- 4. FUNGSI PENGHITUNGAN & PEMBERSIHAN DATA ---
clean_and_classify <- function(df, col_name) {
  df %>%
    mutate(across(all_of(col_name), ~ {
      x <- as.character(.)             
      x <- str_trim(x)                 
      x <- str_replace(x, ",", ".")  
      
      val <- suppressWarnings(as.numeric(x)) 
      ifelse(is.na(val), 0, val)     
    })) %>%
    rowwise() %>%
    mutate(total = sum(c_across(all_of(col_name)), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(kriteria = cut(total,
                          breaks = c(-Inf, 0, 1, 5, 10, 25, 50, 100, Inf),
                          labels = c("No Rain", "0-1", "1-5", "5-10", 
                                     "10-25", "25-50", "50-100", ">100"),
                          include.lowest = TRUE)) %>%
    mutate(
      # Normalisasi koordinat agar tidak error jika ada koma di Excel
      latitude  = as.numeric(str_replace(as.character(latitude), ",", ".")),
      longitude = as.numeric(str_replace(as.character(longitude), ",", "."))
    ) %>%
    select(Stasiun, latitude, longitude, total, kriteria)
}

# --- 5. EKSEKUSI DATA & KONVERSI SF ---
label_peta <- paste0("Curah Hujan Harian (", tgl_plot, " ", bulan_tahun, ")")

# Proses data hanya untuk tanggal yang ditetapkan
data_harian <- clean_and_classify(data_mentah, tgl_plot) %>% 
  mutate(Index = label_peta) %>%
  mutate(kriteria = fct_na_value_to_level(kriteria, level = "No Data")) %>%
  filter(!is.na(latitude), !is.na(longitude))

# Konversi tabel ke bentuk spasial (sf)
stasiun_sf <- st_as_sf(data_harian, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# --- 6. PENGATURAN ESTETIKA & WARNA ---
warna_palet <- c(
  "No Rain" = "white", "0-1" = "#C1F3FF", "1-5" = "#85D4FF", 
  "5-10" = "#4E84FF", "10-25" = "#2219FF", "25-50" = "green", 
  "50-100" = "yellow", ">100" = "red", "No Data" = "#E0E0E0"
)

ukuran_legenda <- c(
  "No Rain" = 1.5, "0-1" = 2.0, "1-5" = 2.5, "5-10" = 3.0, 
  "10-25" = 3.5, "25-50" = 4.5, "50-100" = 5.5, ">100" = 7.0, "No Data" = 3.0
)

# --- 7. VISUALISASI SPASIAL (GGPLOT2) ---
peta_final <- ggplot() +
  
  # Layer 0: Latar Belakang Street Map
  annotation_map_tile(type = "osm", zoom = 7, alpha = 0.7) +
  
  # Layer 1: Batas Wilayah (DAS)
  # PERUBAHAN: Dibuat solid, lebih tipis (0.4), dan warna abu-abu sangat gelap agar smooth
  geom_sf(data = peta_sumut, fill = NA, color = "gray20", linewidth = 0.4) +
  
  # Layer 2: Jaringan Sungai
  geom_sf(data = sungai, color = "#2980B9", linewidth = 0.3, alpha = 0.8) +
  
  # Layer 3: Danau Toba
  geom_sf(data = danau_toba, fill = "#A2D2FF", alpha = 0.5, color = "#5FA8D3", linewidth = 0.2) +
  
  # Layer 4: Titik Lokasi Stasiun/ARG/AWS
  geom_sf(data = stasiun_sf, aes(fill = kriteria, size = total), 
          shape = 21, color = "black", stroke = 0.5, alpha = 0.95) +
  
  # Layer 5: Label Otomatis Aman
  geom_text_repel(
    data = subset(stasiun_sf, kriteria == ">100"), 
    aes(x = longitude, y = latitude, label = Stasiun),
    size = 3.0,                  
    fontface = "bold",          
    color = "black",
    bg.color = "white",         
    bg.r = 0.15,
    point.padding = 0.3,        
    segment.color = "black",   
    segment.size = 0.5,         
    force = 5,                  
    max.overlaps = Inf          
  ) +
  
  scale_size_continuous(range = c(2.0, 8.0), guide = "none") +  
  
  scale_fill_manual(
    values = warna_palet,
    name = "Rainfall Amount (mm)",
    drop = FALSE, 
    guide = guide_legend(
      override.aes = list(size = ukuran_legenda, alpha = 1),
      nrow = 1 
    )
  ) +
  
  # Layer 6: Pengunci Rasio & Frame Peta
  coord_sf(
    xlim = c(st_bbox(peta_sumut)["xmin"], st_bbox(peta_sumut)["xmax"]),
    ylim = c(st_bbox(peta_sumut)["ymin"], st_bbox(peta_sumut)["ymax"]),
    crs = 4326, 
    expand = FALSE
  ) +
  
  scale_y_continuous(labels = function(x) paste0(x, "°N")) +
  scale_x_continuous(labels = function(x) paste0(x, "°E")) +
  
  annotation_scale(location = "bl", width_hint = 0.2, text_cex = 0.8) +
  annotation_north_arrow(location = "tr", height = unit(1.0, "cm"), width = unit(1.0, "cm"),
                         style = north_arrow_minimal()) +
  
  labs(
    title = "RAINFALL MONITORING - NORTH SUMATRA",
    subtitle = paste0("Daily Rainfall Intensity (", tgl_plot, " ", bulan_tahun, ")"),
    x = NULL, y = NULL,
    caption = "Sumber: Curah hujan di Stasiun BMKG, ARG, AWS Sumatera Utara\nDiolah: Observer Stasiun 96041 Medan"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, hjust = 0, face = "italic", color = "gray30"),
    axis.text = element_text(size = 9),
    panel.background = element_rect(fill = "#E3F2FD", color = NA),
    panel.grid.major = element_line(color = "gray80", linetype = "dotted")
  )

# --- 8. PENYIMPANAN HASIL GRAFIK (UKURAN A4 LANDSCAPE) ---
# Dimensi A4 Landscape: 11.69 x 8.27 inch
output_file <- "D:/Cuhar/Monitoring_Hujan_Sumut_3Juni2026.png"
ggsave(output_file, plot = peta_final, width = 11.69, height = 8.27, units = "in", dpi = 300)

cat("\n=======================================================\n")
cat("PROSES SELESAI TANPA KENDALA!\n")
cat("File gambar peta (A4 Landscape) telah disimpan di:\n", output_file, "\n")
cat("=======================================================\n")