# ==============================================================================
# SCRIPT 3-IN-1 MONITORING CURAH HUJAN (RR1day, RR3day, & RR5day)
# VERSI FINAL - BEBAS ERROR GEOM_TEXT_REPEL (AUTOMATIC SKIPPING)
# Diolah Oleh: Observer Stasiun 96041 Medan
# ==============================================================================

# --- 1. LOAD LIBRARIES ---
library(sf)
library(readxl)
library(ggplot2)
library(ggspatial)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(ggrepel)

# --- 2. PENGATURAN TANGGAL & PATH FILE ---
tgl_ref     <- 28 
bulan_tahun <- "Mei 2026"

path_shp    <- "D:/Cuhar/SHP_Master/"
path_excel  <- "D:/Cuhar/Stasiun.xlsx"

# Menentukan rentang tanggal analisis
tgl_h1  <- as.character(tgl_ref - 1)
tgl_h3  <- as.character((tgl_ref - 3):(tgl_ref - 1))
tgl_h5  <- as.character((tgl_ref - 5):(tgl_ref - 1))

# --- 3. LOAD DATA SPASIAL & EXCEL ---
# Validasi ketersediaan folder dan file sebelum running
if (!dir.exists(path_shp)) stop("Error: Folder SHP_Master tidak ditemukan! Periksa kembali path Anda.")
if (!file.exists(path_excel)) stop("Error: File Stasiun.xlsx tidak ditemukan! Periksa kembali path Anda.")

peta_sumut  <- read_sf(paste0(path_shp, "kabupaten_sumut.shp")) %>% st_set_crs(4326)
danau_toba  <- read_sf(paste0(path_shp, "danau_toba.shp")) %>% st_set_crs(4326)
data_mentah <- read_excel(path_excel)

# --- 4. FUNGSI PENGHITUNGAN & PEMBERSIHAN DATA ---
clean_and_classify <- function(df, cols) {
  df %>%
    mutate(across(any_of(cols), ~ {
      x <- as.character(.)           # Paksa ke karakter
      x <- str_trim(x)               # Hapus spasi liar
      x <- str_replace(x, ",", ".")  # Normalisasi: Koma menjadi Titik
      
      val <- suppressWarnings(as.numeric(x)) # Konversi ke angka & redam pesan warning coercion
      ifelse(is.na(val), 0, val)     # Jika NA atau "NR", jadikan 0
    })) %>%
    rowwise() %>%
    mutate(total = sum(c_across(any_of(cols)), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(kriteria = cut(total,
                          breaks = c(-Inf, 0, 1, 5, 10, 25, 50, 100, Inf),
                          labels = c("No Rain", "0-1", "1-5", "5-10", 
                                     "10-25", "25-50", "50-100", ">100"),
                          include.lowest = TRUE)) %>%
    # Normalisasi koordinat agar tidak error jika ada koma di Excel
    mutate(
      latitude  = as.numeric(str_replace(as.character(latitude), ",", ".")),
      longitude = as.numeric(str_replace(as.character(longitude), ",", "."))
    ) %>%
    select(Stasiun, latitude, longitude, total, kriteria)
}

# --- 5. PEMBUATAN LABEL HEADER ---
label_rr1 <- paste0("RR1day\n(Curah Hujan 1 Hari ", tgl_h1, " ", bulan_tahun, ")")
label_rr3 <- paste0("RR3day\n(Akumulasi Curah Hujan 3 Hari\n", tgl_h3[1], " s.d ", tgl_h3[length(tgl_h3)], " ", bulan_tahun, ")")
label_rr5 <- paste0("RR5day\n(Akumulasi Curah Hujan 5 Hari\n", tgl_h5[1], " s.d ", tgl_h5[length(tgl_h5)], " ", bulan_tahun, ")")

# Eksekusi Fungsi Pembersihan Data
rr1 <- clean_and_classify(data_mentah, tgl_h1) %>% mutate(Index = label_rr1)
rr3 <- clean_and_classify(data_mentah, tgl_h3) %>% mutate(Index = label_rr3)
rr5 <- clean_and_classify(data_mentah, tgl_h5) %>% mutate(Index = label_rr5)

# Gabungkan Data & Konversi ke format SF
data_combined <- bind_rows(rr1, rr3, rr5) %>%
  mutate(
    kriteria = fct_na_value_to_level(kriteria, level = "No Data"),
    Index    = factor(Index, levels = c(label_rr1, label_rr3, label_rr5))
  ) %>%
  filter(!is.na(latitude), !is.na(longitude))

# PENTING: remove = FALSE agar kolom longitude & latitude asli tidak dihapus oleh sf
stasiun_sf <- st_as_sf(data_combined, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

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
  geom_sf(data = peta_sumut, fill = "#FBFBFB", color = "#D0D0D0", linewidth = 0.3) +
  geom_sf(data = danau_toba, fill = "#A2D2FF", color = "#5FA8D3", linewidth = 0.2) +
  
  # Layer Titik Lokasi Stasiun/ARG/AWS
  geom_sf(data = stasiun_sf, aes(fill = kriteria, size = total), 
          shape = 21, color = "black", stroke = 0.4, alpha = 0.8) +
  
  # Layer Label Otomatis Aman (Membaca kolom numerik asli, kebal error jika data kosong)
  geom_text_repel(
    data = subset(stasiun_sf, kriteria == ">100"), 
    aes(x = longitude, y = latitude, label = Stasiun),
    size = 2.2,                 
    fontface = "bold",          
    color = "black",            
    point.padding = 0.3,        
    segment.color = "gray50",   
    segment.size = 0.2,         
    force = 5,                  
    max.overlaps = Inf          
  ) +
  
  scale_size_continuous(range = c(1.5, 7), guide = "none") +  
  
  scale_fill_manual(
    values = warna_palet,
    name = "Rainfall Amount (mm)",
    drop = FALSE, 
    guide = guide_legend(
      override.aes = list(size = ukuran_legenda, alpha = 1),
      nrow = 2, byrow = TRUE
    )
  ) +
  
  facet_wrap(~Index, ncol = 3) +
  
  scale_y_continuous(labels = function(x) paste0(x, "°N")) +
  scale_x_continuous(labels = function(x) paste0(x, "°E")) +
  
  annotation_scale(location = "bl", width_hint = 0.2, text_cex = 0.7) +
  annotation_north_arrow(location = "tr", height = unit(0.6, "cm"), width = unit(0.6, "cm"),
                         style = north_arrow_minimal()) +
  
  labs(
    title = "RAINFALL MONITORING - NORTH SUMATRA",
    subtitle = "Analysis of Daily and Accumulated Rainfall Intensity",
    x = NULL, y = NULL,
    caption = "Sumber : Curah hujan di Stasiun BMKG, ARG, AWS Sumatera Utara\nDiolah : Observer Stasiun 96041 Medan"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    strip.text = element_text(face = "bold", size = 8.5, lineheight = 1.1),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.caption = element_text(size = 7, hjust = 0, face = "italic", color = "gray30"),
    axis.text = element_text(size = 7),
    panel.background = element_rect(fill = "white", color = NA)
  )

# --- 8. PENYIMPANAN HASIL GRAFIK ---
output_file <- "D:/Cuhar/Monitoring_Hujan_Sumut_Final_Deskriptif.png"
ggsave(output_file, plot = peta_final, width = 13, height = 8.5, dpi = 300)

cat("\n=======================================================\n")
cat("PROSES SELESAI TANPA KENDALA!\n")
cat("File gambar peta telah disimpan di:\n", output_file, "\n")
cat("=======================================================\n")