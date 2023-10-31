
# 11 05 2022

#Data dibagi menjadi 2 => kategorik dan numerik
#1. Kategorik => klasifikasi (parametrik dan non parametrik)
#contoh: parametrik (regresi logistik (OR), diskriminan), nonparametik (NBC, SVM, tree based)
#kalau menggunakan nonparametrik hanya akurasi model, kalau parametrik bisa untuk prediksi

#2. Numerik => regresi

############
#Machine learning yg harus diketahui ada 2:
#1. Supervised learning => ada respon
#2. Unsupervised learning => tdk punya target
#3. Reinforcement learning => sampel berulang-ulang (bootstrap)
#   bootstrap ada 2: rata2 dan varians, sigma kuadrat didekati dengan metode bootstrap agar dapat nilai sigma kuadrat

# KEBAIKAN MODEL:
# Rsq => semakin bertambah jika prediktornya bertambah, tdk memperhatikan signifikan dari prediktor
# adj Rsq => semakin bertambah prediktor belum tentu nilsinys bertambah, memperhatikan signifikansi prediktor
# MAPE => dibawah 10%
# MSE => kurang dari varians data
# MAE => 


######################################
#Packages
install.packages('faraway')
library(faraway)

#Read data
data(gala)
hist(gala$Species, xlab = "Species", main = "The Number of Plant Species")

#Parameter estimation
gall <- glm(Species ~ Area+Elevation+Nearest+Scruz+Adjacent,
            family = poisson, data = gala)
summary(gall)