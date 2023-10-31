### Nama Kelompok:
## 1. Salsabila T.W
## 2. Sindy A.A
## 3. Rizke Z.L
#################################################

#install.packages("Kohonen")
install.packages("kohonen")
library(kohonen)
library(dplyr)
library(ggplot2) #plots
library(GGally) #plots
library(RColorBrewer)

#Input Data
library(readr)
Cust_Segmentation <- read_csv("Cust_Segmentation.csv")
head(Cust_Segmentation)
colnames(Cust_Segmentation) # Menampilkan variabel
str(Cust_Segmentation) # Mengetahui struktur data
class(Cust_Segmentation) # Mengetahui kelas data

#select
library(tidyverse)
library(dplyr)
data.cust <- as_tibble(Cust_Segmentation)
data.cust
data <- data.cust %>% 
  select(4:7)%>% # diambil 4 variabel pada kolom 4-7
  slice(1:150) # sampel sebanyak 150
head(data)

#standarisasi
data<-scale((data))
head(data)
class(data)
colnames(data)

# Pengelompokan
library(kohonen)
library(dplyr)
library(ggplot2) #plots
library(GGally) #plots
library(RColorBrewer)

#build grid
sc.grid = somgrid(xdim = 4, ydim=4, topo="hexagonal", toroidal=TRUE) 
## grid adalah tempat untuk meletakkan hasil klasifikasi ##

#build model
set.seed(33)
data.som<- som(data, grid=sc.grid, rlen=700, alpha=c(0.05,0.01), keep.data=TRUE)
summary(data.som)
par(mfrow=c(2,2))
plot(data.som, type="changes")

colour1<- tricolor(data.som$grid)
plot(data.som, "mapping", bg=rgb(colour1))
plot(data.som, type="count", bg=rgb(colour1))
plot(data.som, type="codes", codeRendering="segments") 

hclust(dist(data.som$codes[[1]]))
peta<-cutree(hclust(dist(data.som$codes[[1]])), 3)
plot(data.som, type="codes", bgcol=rainbow(3) [peta])
add.cluster.boundaries(data.som, peta)

tricolor(data.som$grid, phis = c(0, 2 * pi/3, 4 * pi/3), offset = 0)
plot(data.som, "codes", bg=rgb(colour1), shape="straight")

data.som$codes[[1]]

