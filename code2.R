# DATA

# Data dalam satuan jam
data <- c(
  20, 22, 23, 33, 35, 56, 77, 78, 94, 95, 
  97, 103, 111, 113, 115, 116, 122, 125, 134, 139, 143, 145, 
  146, 151, 154, 155, 157, 162, 165, 166, 175, 178, 
  179, 182, 192, 203, 211, 213, 220, 233, 240, 242, 
  243, 244, 246, 247, 250, 255, 256, 260, 273, 276, 
  277, 279, 281, 292, 295, 296, 314, 319, 331, 332, 
  338, 345, 360, 377, 380, 415, 437, 442, 464, 504, 
  506, 518, 537, 569, 573, 590, 626, 675, 685, 733,
  758, 793, 900, 947, 980, 995, 1013, 1128, 1155, 1156, 
  1193, 1214, 1327, 1340, 1526, 1791, 2456, 2464, 4076)

# Cara lain me-load data dari exel
data2=read.delim("clipboard")
data2

# Jumlah data
n <- length(data)
n

# dataMax dan dataMin
max<- max(data)
min<- min(data)

# MENENTUKAN JUMLAH KELAS (K)
K = 1 + 3.3 * log10(n)
K = round(K)
K

# MENENTUKAN INTERVAL (P)
P = (max-min)/k
p = ceiling(p)
p

frekuensi = function(x, y, z) {
  a=0
  for (i in 1:n) {
    if (x[i] >= y && x[i] < z) {
      a = a+1
    }
  }
  print(a)
}

kelas=c("20-525", "526-1028", "1029-1534", "1535-2040", "2041-2546", "2547-3052", "3053-3558", "3559-4064", "4065-4570")

frekuensi(data, 20, 525)
frekuensi(data, 526, 1028)
frekuensi(data, 1029, 1534)
frekuensi(data, 1535, 2040)
frekuensi(data, 2041, 2546)
frekuensi(data, 2547, 3052)
frekuensi(data, 3053, 3558)
frekuensi(data, 3559, 4064)
frekuensi(data, 4065, 4570)

freq=c(80,15,8,2,1,1,1,1,1)



# MEAN
# 1. Menentukan nilai tengah (xBar)
attach(data)
mean(20:525)
mean(526:1028)
mean(1029:1534)
mean(1535:2040)
mean(2041:2546)
mean(2547:3052)
mean(3053:3558)
mean(3559:4064)
mean(4065:4570)

xBar=c(263, 777, 1281.5, 1787.5, 2293.5, 2799.5, 3305.3, 3811.5, 4317.5)

# 2. Membuat Tabel Dengan Data Batas Akhir Kelas, Titik Tengah, Frekuensi
# xi : nilai titik tengah interval kelas i
# fi : frekuensi kelas i
tabel <- data.frame(
  Kelas = kelas,
  Fi = freq,
  Xi = xBar,
  Fi.Xi = freq*xBar
)

print(tabel)

# 3. Menentukan jumlah banyak data
print(sum(freq))

# 4. Menghitung sigma(xi*fi)
print(sum(freq*xBar))

mean <- (sum(freq*xBar))/sum(freq)
print(mean)



# MEDIAN
# 1. Mencari letak kelas median
letakKelas <- sum(freq)/2
print(letakKelas)
# Letak kelas median berada pada data ke-50, yaitu pada kelas 281-334

# 2. Menentukan tb (batas bawah)
tb <- 0
tb

#3. Menentukan frekuensi kumulatif (fkk) kurang dari kelas median
fkk <- 0+80

#Frekuensi Kelas Median (Fi) dan Panjang Kelas (l)
fi <- 80
l <- p

median <- tb + ((letakKelas-fkk)/fi)*l
median



# MODUS
# 1. Menentukan 
#    -tb(batas bawah), 
#    -d1(selisih freq kelas modus dengan freq sebelum kelas modus), 
#    -d2(selisih freq kelas modus dengan freq setelah kelas modus), dan 
#    - l(panjang kelas)

d1 <- 0
d2 <- 80-15

modus <- tb + (d1/(d1+d2))*l
modus



# RANGE
# 1. Menentukan nilai tengah (xBar)
# range = nilai tengah kelas tertinggi - nilai tengah kelas terendah
range <- max(xBar)-min(xBar)
range



# MEAN DEVIASI
meanDev <- (sum(freq*abs(xBar-mean)))/n
meanDev



# STANDAR DEVIASI
standarDev <- sqrt((sum(freq*(xBar-mean)^2))/n)
standarDev










