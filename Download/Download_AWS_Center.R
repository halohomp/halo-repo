# ==============================================================================
# SCRIPT OTOMATISASI DOWNLOAD DATA CURAH HUJAN AWSCENTER BMKG
# ==============================================================================

# Memuat library yang dibutuhkan
library(httr)
library(rvest)
library(magick)

# ==========================================
# BAGIAN 1: PROSES LOGIN & LEWATI CAPTCHA
# ==========================================
url_base <- "https://awscenter.bmkg.go.id/base"
url_login <- "https://awscenter.bmkg.go.id/base/verify"
url_captcha <- "https://awscenter.bmkg.go.id/public/assets/img/captcha.png"

print("Memulai sesi koneksi ke web AWSCenter BMKG...")
session <- handle(url_base)
GET(url_base, handle = session) 

# Menarik dan menampilkan gambar CAPTCHA
captcha_img <- GET(url_captcha, handle = session)
img <- image_read(content(captcha_img, "raw"))
print(img) 

# Mengirim data login
payload <- list(
  username = "balai1",              
  password = "balai1@2020", # GANTI DENGAN PASSWORD ASLI ANDA
  captcha = jawaban_captcha          
)

login_attempt <- POST(url_login, body = payload, encode = "form", handle = session)

# ==========================================
# BAGIAN 2: PROSES UNDUH 30 STASIUN
# ==========================================
if (status_code(login_attempt) == 200) {
  print("Login sukses! Memulai proses unduh data massal...")
  
  # 1. Tabel Master 30 Stasiun Sumatera Utara & Sekitarnya
  daftar_stasiun <- data.frame(
    tipe = c(
      "arg", "aaws", "aws", "aaws", "arg", "arg", "arg", "arg", 
      "arg", "aws", "arg", "arg", "arg", "aws", "arg",
      "arg", "aws", "arg", "arg", "arg", "aaws", "arg", "aws", "arg", "aaws",
      "aws", "aws", "aws", "aws", "aws"
    ),
    id = c(
      "150260", "STA3212", "STA2068", "STS1001", "150106", "150109", "STA0259", "150262",
      "150259", "160044", "STA0178", "150108", "150110", "STA2295", "150107",
      "14032795", "160051", "150111", "150115", "150113", "STA3209", "STA0203", "STW1052", "STG1014", "STA3032",
      "STA5051", "STA5001", "STW1002", "STA2238", "STW1072"
    ),
    nama = c(
      "Sunggal", "Hinai", "Deliserdang", "Sei_Rejo", "Sinabung", "Salak", "Bahorok", "Merek",
      "Raya", "Parapat", "Balige", "Harian", "Pakkat", "Dolok Sanggul", "Sipoholon",
      "Lubuk Baru", "Sosa", "Arse", "Kualuh Selatan", "Mompang", "Batubara", "Kota Pinang", "Bah Jambi", "Teluk Dalam", "Deli Serdang",
      "Aek Godang", "Kualanamu", "Belawan", "Tigaras", "Silangit"
    ),
    stringsAsFactors = FALSE
  )
  
  # 2. Tentukan Parameter Waktu dan Direktori Penyimpanan
  # (Cukup ganti tanggal di bawah ini untuk mengunduh bulan/hari berikutnya)
  tgl_awal <- "2026-05-28"    
  tgl_akhir <- "2026-05-28"
  folder_simpan <- "D:/Cuhar/28Mei26/"
  
  if (!dir.exists(folder_simpan)) {
    dir.create(folder_simpan, recursive = TRUE)
  }
  
  # 3. Eksekusi Perulangan
  for(i in 1:nrow(daftar_stasiun)) {
    
    tipe_alat <- daftar_stasiun$tipe[i]
    id_stasiun <- daftar_stasiun$id[i]
    nama_stasiun <- daftar_stasiun$nama[i]
    
    url_download <- paste0("https://apiaws.bmkg.go.id/rawdata/downloadaccesdata_yesterday/",
                           tipe_alat, "/", id_stasiun, "/", tgl_awal, "/", tgl_akhir)
    
    nama_file <- paste0(folder_simpan,nama_stasiun, ".xlsx")
    
    print(sprintf("[%02d/30] Mendownload %s (Tipe: %s)...", i, nama_stasiun, toupper(tipe_alat)))
    unduhan <- GET(url_download, handle = session, write_disk(nama_file, overwrite = TRUE))
    
    if(status_code(unduhan) == 200){
      print("        -> OK!")
    } else {
      print("        -> GAGAL diunduh.")
    }
    
    # Jeda aman 2 detik untuk server
    Sys.sleep(2) 
  }
  
  print("=========================================================")
  print("SELESAI! Semua file Excel berhasil tersimpan di folder:")
  print(folder_simpan)
  print("=========================================================")
  
} else {
  print("Login gagal! Pastikan password dan hasil hitungan CAPTCHA sudah benar.")
}