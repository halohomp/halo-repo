# ==============================================================================
# SCRIPT OTOMATISASI: KLIP PETA PER DAS YANG MEMILIKI STASIUN
# ==============================================================================

# --- A. SPATIAL JOIN (Mencocokkan Titik Stasiun dengan Poligon DAS) ---
message("Melakukan Spatial Join antara Stasiun dan DAS...")

# Pastikan nama kolom ini sesuai dengan yang ada di atribut DAS Sumut.shp Anda!
# Cek dengan mengetik: names(peta_sumut) di console
kolom_nama_das <- "NAMA_DA" # <--- UBAH BAGIAN INI JIKA NAMA KOLOMNYA BERBEDA

# st_join akan menempelkan atribut poligon DAS ke titik stasiun yang beririsan
stasiun_das <- st_join(stasiun_sf, peta_sumut, join = st_intersects)

# Hapus stasiun yang lokasinya meleset / tidak masuk ke dalam poligon DAS manapun (NA)
stasiun_das <- stasiun_das[!is.na(stasiun_das[[kolom_nama_das]]), ]

# Dapatkan daftar unik nama-nama DAS yang memiliki minimal 1 stasiun
daftar_das_aktif <- unique(stasiun_das[[kolom_nama_das]])
jumlah_das <- length(daftar_das_aktif)

message(paste("Ditemukan", jumlah_das, "DAS yang memiliki stasiun. Memulai proses pembuatan peta..."))

# --- B. LOOPING PEMBUATAN PETA PER DAS ---
# R akan mengulang blok di bawah ini sebanyak jumlah DAS yang ada stasiunnya
for (i in seq_along(daftar_das_aktif)) {
  
  nama_das_sekarang <- daftar_das_aktif[i]
  message(paste0("Memproses peta ", i, "/", jumlah_das, ": DAS ", nama_das_sekarang))
  
  # 1. Filter geometri khusus untuk DAS yang sedang diproses
  das_polygon <- peta_sumut[peta_sumut[[kolom_nama_das]] == nama_das_sekarang, ]
  
  # 2. Filter stasiun khusus yang berada di DAS tersebut
  stasiun_plot <- stasiun_das[stasiun_das[[kolom_nama_das]] == nama_das_sekarang, ]
  
  # 3. Potong (clip) jaringan sungai agar tidak melewati batas DAS ini (opsional agar lebih rapi)
  # suppressWarnings digunakan agar R tidak cerewet dengan peringatan irisan geometri
  sungai_clip <- suppressWarnings(st_intersection(sungai, st_geometry(das_polygon)))
  
  # 4. Membuat Plot (Sama seperti sebelumnya, tapi fokus ke DAS ini)
  p <- ggplot() +
    # Latar OSM dengan zoom 10 atau 11 karena areanya sekarang lebih sempit (skala DAS)
    annotation_map_tile(type = "osm", zoom = 10, alpha = 0.7) +
    
    # Batas wilayah DAS
    geom_sf(data = das_polygon, fill = NA, color = "gray20", linewidth = 0.6) +
    
    # Sungai di dalam DAS
    geom_sf(data = sungai_clip, color = "#2980B9", linewidth = 0.3, alpha = 0.8) +
    
    # Danau Toba (Jika masuk dalam batas DAS, coord_sf akan otomatis memotong tampilannya)
    geom_sf(data = danau_toba, fill = "#A2D2FF", alpha = 0.5, color = "#5FA8D3", linewidth = 0.2) +
    
    # Titik stasiun di dalam DAS
    geom_sf(data = stasiun_plot, aes(fill = kriteria, size = total), 
            shape = 21, color = "black", stroke = 0.5, alpha = 0.95) +
    
    # Label stasiun (Semua stasiun di DAS ini akan diberi label, tidak hanya yang ekstrem)
    geom_text_repel(
      data = stasiun_plot, 
      aes(x = longitude, y = latitude, label = Stasiun),
      size = 3.5, fontface = "bold", color = "black",
      bg.color = "white", bg.r = 0.15, point.padding = 0.3, force = 5
    ) +
    
    scale_size_continuous(range = c(3.0, 9.0), guide = "none") +  
    scale_fill_manual(values = warna_palet, name = "Rainfall (mm)", drop = FALSE,
                      guide = guide_legend(override.aes = list(size = ukuran_legenda))) +
    
    # Mengunci batas peta HANYA sejauh batas kotak imajiner (Bounding Box) poligon DAS ini
    # expand = TRUE memberikan sedikit ruang longgar agar stasiun di pinggir batas tidak terpotong
    coord_sf(
      xlim = c(st_bbox(das_polygon)["xmin"], st_bbox(das_polygon)["xmax"]),
      ylim = c(st_bbox(das_polygon)["ymin"], st_bbox(das_polygon)["ymax"]),
      crs = 4326, expand = TRUE
    ) +
    
    labs(
      title = paste("MONITORING CURAH HUJAN - DAS", toupper(nama_das_sekarang)),
      subtitle = paste0("Intensitas Curah Hujan Harian (", tgl_plot, " ", bulan_tahun, ")"),
      x = NULL, y = NULL,
      caption = "Diolah: Observer Stasiun 96041 Medan"
    ) +
    
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
      panel.background = element_rect(fill = "#E3F2FD", color = NA)
    )
  
  # 5. Menyimpan Plot Otomatis
  # Format nama file: Peta_DAS_Batang_Toru.png (spasi diganti underscore agar aman)
  nama_file <- paste0("D:/Cuhar/Peta_DAS_", gsub(" ", "_", nama_das_sekarang), ".png")
  ggsave(nama_file, plot = p, width = 10, height = 8, units = "in", dpi = 300)
}

message("\n=======================================================")
message("PROSES SELESAI! Semua peta DAS berhasil dibuat dan disimpan di D:/Cuhar/")
message("=======================================================")