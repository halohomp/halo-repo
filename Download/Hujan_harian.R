# ==============================================================================
# SCRIPT OTOMATISASI DOWNLOAD & REKAP CURAH HUJAN MAKSIMUM AWSCENTER BMKG
# ==============================================================================

library(httr)
library(rvest)
library(magick)
library(readxl)   
library(writexl)  

# 1. Identitas Penyamaran
identitas_browser <- add_headers(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
  `Referer` = "https://awscenter.bmkg.go.id/",
  `Accept` = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet, text/html, */*"
)

# ==========================================
# BAGIAN 1: PROSES LOGIN & LEWATI CAPTCHA
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
jawaban_captcha <- "-2" # GANTI ANGKA INI DENGAN HASIL DARI VIEWER!

payload <- list(
  username = "balai1",              
  password = "balai1@2020", 
  captcha = jawaban_captcha          
)

login_attempt <- POST(url_login, body = payload, encode = "form", handle = session, identitas_browser)

# ==========================================
# BAGIAN 2: PERSIAPAN TABEL & TANGGAL
# ==========================================
if (status_code(login_attempt) == 200) {
  print("Login sukses! Memulai proses unduh dan ekstraksi data...")
  
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
  
  if (!dir.exists(folder_simpan)) {
    dir.create(folder_simpan, recursive = TRUE)
  }
  
  daftar_stasiun$RR_Maks_Harian <- NA 
  
  # ==========================================
  # BAGIAN 3: PROSES UNDUH & FILTER MAKSIMUM
  # ==========================================
  for(i in 1:nrow(daftar_stasiun)) {
    
    tipe_alat <- daftar_stasiun$tipe[i]
    id_stasiun <- daftar_stasiun$id[i]
    nama_stasiun <- daftar_stasiun$nama[i]
    
    url_download <- paste0("https://apiaws.bmkg.go.id/rawdata/downloadaccesdata/", tipe_alat, "/", id_stasiun, "/", tgl_awal, "/", tgl_akhir)
    nama_file_raw <- paste0(folder_simpan, "RAW_", nama_stasiun, ".xlsx")
    
    print(sprintf("[%02d/30] Memproses %s ...", i, nama_stasiun))
    
    unduhan <- GET(url_download, 
                   handle = session, 
                   identitas_browser, 
                   write_disk(nama_file_raw, overwrite = TRUE), 
                   config(http_version = 0))
    
    if(status_code(unduhan) == 200){
      tryCatch({
        raw_data <- read_excel(nama_file_raw)
        
        # PAKSA semua nama kolom menjadi huruf kecil agar R tidak tertipu
        names(raw_data) <- tolower(names(raw_data))
        
        # 1. FILTER TANGGAL & WAKTU (Pasti tereksekusi sekarang!)
        if ("tanggal" %in% names(raw_data)) {
          
          waktu_aktual <- as.POSIXct(raw_data$tanggal, tz = "UTC")
          jam_aktual <- format(waktu_aktual, "%H")
          tanggal_aktual <- format(waktu_aktual, "%Y-%m-%d")
          
          # EKSEKUSI: Ambil HANYA tanggal yang diinput, DAN buang jam 00
          filter_kondisi <- (tanggal_aktual == tgl_otomatis) & (jam_aktual != "00")
          
          # Terapkan filter
          raw_data <- raw_data[which(filter_kondisi), ]
        }
        
        # 2. CARI MAKSIMUM
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
    } else { # INI KURUNG PENUTUP UNDUHAN YANG HILANG
      print("        -> GAGAL diunduh dari server AWS.")
    }
    
    Sys.sleep(2) 
  } # INI KURUNG PENUTUP PERULANGAN STASIUN (FOR LOOP) YANG HILANG
  
  # ==========================================
  # BAGIAN 4: SIMPAN REKAPITULASI
  # ==========================================
  nama_file_rekap <- paste0(folder_simpan, "REKAP_Curah_Hujan_", tgl_otomatis, ".xlsx")
  write_xlsx(daftar_stasiun, nama_file_rekap)
  
  print("=========================================================")
  print(paste("SELESAI! File raw dan rekap tersimpan di:", folder_simpan))
  print("=========================================================")
  
} else {
  print("Login gagal! Pastikan password dan Captcha benar.")
}