#input data
penumpang = read_excel("~/Train passengers in Jabodetabek.xlsx")
View(penumpang)

# cek struktur data ASLI
str(penumpang)
head(penumpang)

# 2. Ambil kolom numerik pertama
jumlah_penumpang <- penumpang[, sapply(penumpang, is.numeric)][[1]]
jumlah_penumpang <- as.numeric(jumlah_penumpang)
length(jumlah_penumpang)

# 3. Ubah ke time series bulanan
penumpang_ts <- ts(jumlah_penumpang,
                   start = c(2017, 1),
                   frequency = 12)

summary(penumpang_ts)

# 4. Holt-Winters Additive
hw_add <- HoltWinters(penumpang_ts, seasonal = "additive")
hw_add

# 5. MAPE
mape_add <- mean(
  abs(penumpang_ts - hw_add$fitted[,1]) / penumpang_ts,
  na.rm = TRUE
) * 100
mape_add

# 6. Forecast 12 bulan 
forecast_hw <- predict(hw_add, n.ahead = 12)
forecast_hw

# 7. Plot 
par(mar = c(5, 4, 4, 8))  

plot(penumpang_ts,
     main = "Jumlah Penumpang KAI Jabodetabek (2017–2024)",
     xlab = "Tahun",
     ylab = "Jumlah Penumpang",
     col = "blue",
     lwd = 2,
     type = "l")

lines(hw_add$fitted[,1],
      col = "red",
      lwd = 2)

lines(forecast_hw,
      col = "green",
      lwd = 2)



