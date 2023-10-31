#install.packages("Kohonen")
install.packages("kohonen")
library(kohonen)
library(dplyr)

#read
library(readr)
Cust_Segmentation <- read_csv("E:/SEMESTER 7/PENGOLAHAN DATA 2/Cust_Segmentation.csv")
head(Cust_Segmentation)
colnames(Cust_Segmentation)
str(Cust_Segmentation)

#select
data.cust <- Cust_Segmentation %>% select(c("Age","Years Employed","Income","Card Debt","DebtIncomeRatio"))
data.cust

grid =somgrid(xdim=5, ydim=5, topo=c("hexagonal"))
grid

som.Cust.Segmentation<-som(data.cust, grid=somgrid (5,5,"hexagonal"))
som.Cust.Segmentation
plot(som.Cust.Segmentation)
som.Cust.Segmentation$grid$pts

hclust(dist(som.Cust.Segmentation$codes[[1]]))
peta<-cutree(hclust(dist(som.Cust.Segmentation$codes[[1]])), 3)
plot(som.Cust.Segmentation, type="codes", bgcol=rainbow(3) [peta])
add.cluster.boundaries(som.Cust.Segmentation, peta)



