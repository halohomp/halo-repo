# ==============================================================================
# SCRIPT MASTER: DARI BACA DATA HINGGA CETAK MULTIPAGE PDF PER DAS
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
folder_tanggal <- "03Jun26" # Nama folder untuk menyimpan PDF

path_shp    <- "D:/Cuhar/SHP_Master/"
path_excel  <- "D:/Cuhar/Stasiun.xlsx"

# --- 3. LOAD DATA SPASIAL & EXCEL ---
message("Membaca data spasial dan Excel...")
peta_sumut  <- read_sf(paste0(path_shp, "DAS Sumut.shp")) %>% st_set_crs(4326)
danau_toba  <- read_sf(paste0(path_shp, "danau_toba.shp")) %>% st_set_crs(4326)
sungai      <- read_sf(paste0(path_shp, "RIV_SUMUT_CLIP.shp")) %>% st_set_crs(4326)
data_mentah <- read_excel(path_excel)

# --- 4. FUNGSI PENGHITUNGAN & PEMBERSIHAN DATA ---
clean_and_classify <- function(df, col_name) {
  df %>%
    mutate(across(all_of(col_name), ~ {
      x <- str_replace(str_trim(as.character(.)), ",", ".")  
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
      latitude  = as.numeric(str_replace(as.character(latitude), ",", ".")),
      longitude = as.numeric(str_replace(as.character(longitude), ",", "."))
    ) %>%
    select(Stasiun, latitude, longitude, total, kriteria)
}

# Proses data dan konversi ke sf
label_peta <- paste0("Curah Hujan Harian (", tgl_plot, " ", bulan_tahun, ")")
data_harian <- clean_and_classify(data_mentah, tgl_plot) %>% 
  mutate(Index = label_peta) %>%
  mutate(kriteria = fct_na_value_to_level(kriteria, level = "No Data")) %>%
  filter(!is.na(latitude), !is.na(longitude))

stasiun_sf <- st_as_sf(data_harian, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# --- 5. PENGATURAN WARNA ---
warna_palet <- c("No Rain" = "white", "0-1" = "#C1F3FF", "1-5" = "#85D4FF", 
                 "5-10" = "#4E84FF", "10-25" = "#2219FF", "25-50" = "green", 
                 "50-100" = "yellow", ">100" = "red", "No Data" = "#E0E0E0")
ukuran_legenda <- c("No Rain" = 1.5, "0-1" = 2.0, "1-5" = 2.5, "5-10" = 3.0, 
                    "10-25" = 3.5, "25-50" = 4.5, "50-100" = 5.5, ">100" = 7.0, "No Data" = 3.0)

# --- 6. SPATIAL JOIN (Mencocokkan Titik Stasiun dengan Poligon DAS) ---
message("Melakukan Spatial Join antara Stasiun dan DAS...")
kolom_nama_das <- "nama_da" 
stasiun_das <- st_join(stasiun_sf, peta_sumut, join = st_intersects)

if (!(kolom_nama_das %in% names(stasiun_das))) stop(paste("Error: Kolom", kolom_nama_das, "TIDAK DITEMUKAN!"))

stasiun_das <- stasiun_das[!is.na(stasiun_das[[kolom_nama_das]]), ]
daftar_das_aktif <- unique(stasiun_das[[kolom_nama_das]])
jumlah_das <- length(daftar_das_aktif)

# --- 7. PENGATURAN FOLDER & PDF OUTPUT ---
path_output <- paste0("D:/Cuhar/", folder_tanggal, "/Plot/")
if (!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)

nama_file_pdf <- paste0(path_output, "Monitoring_Hujan_DAS_", folder_tanggal, ".pdf")
pdf(file = nama_file_pdf, width = 11.69, height = 8.27) # A4 Landscape

# --- 8. LOOPING CETAK PDF PER DAS ---
for (i in seq_along(daftar_das_aktif)) {
  nama_das_sekarang <- daftar_das_aktif[i]
  message(paste0("Mencetak halaman ", i, "/", jumlah_das, ": DAS ", nama_das_sekarang))
  
  das_polygon <- peta_sumut[peta_sumut[[kolom_nama_das]] == nama_das_sekarang, ]
  stasiun_plot <- stasiun_das[stasiun_das[[kolom_nama_das]] == nama_das_sekarang, ]
  sungai_clip <- suppressWarnings(st_intersection(sungai, st_geometry(das_polygon)))
  
  p <- ggplot() +
    annotation_map_tile(type = "osm", zoom = 10, alpha = 0.7) +
    geom_sf(data = das_polygon, fill = NA, color = "gray20", linewidth = 0.6) +
    geom_sf(data = sungai_clip, color = "#2980B9", linewidth = 0.3, alpha = 0.8) +
    geom_sf(data = danau_toba, fill = "#A2D2FF", alpha = 0.5, color = "#5FA8D3", linewidth = 0.2) +
    geom_sf(data = stasiun_plot, aes(fill = kriteria, size = total), 
            shape = 21, color = "black", stroke = 0.5, alpha = 0.95) +
    geom_text_repel(data = stasiun_plot, aes(x = longitude, y = latitude, label = Stasiun),
                    size = 3.5, fontface = "bold", color = "black", bg.color = "white", bg.r = 0.15, force = 5) +
    scale_size_continuous(range = c(3.0, 9.0), guide = "none") +  
    scale_fill_manual(values = warna_palet, name = "Rainfall (mm)", drop = FALSE,
                      guide = guide_legend(override.aes = list(size = ukuran_legenda))) +
    coord_sf(xlim = c(st_bbox(das_polygon)["xmin"], st_bbox(das_polygon)["xmax"]),
             ylim = c(st_bbox(das_polygon)["ymin"], st_bbox(das_polygon)["ymax"]), crs = 4326, expand = TRUE) +
    labs(title = paste("MONITORING CURAH HUJAN - DAS", toupper(nama_das_sekarang)),
         subtitle = paste0("Intensitas Curah Hujan Harian (", tgl_plot, " ", bulan_tahun, ")"),
         caption = "Diolah: Observer Stasiun 96041 Medan") +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
          panel.background = element_rect(fill = "#E3F2FD", color = NA))
  
  print(p)
}

dev.off() # Menutup dan menyimpan file PDF

message("\n=======================================================")
message("PROSES SELESAI! File PDF telah berhasil disimpan di:")
message(nama_file_pdf)
message("=======================================================")