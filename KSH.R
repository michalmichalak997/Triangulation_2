library(dplyr)
library(ggpubr)
library(purrr)
library(ggplot2)


#A function calculating length START
norm_vec <- function(x) sqrt(sum(x^2))
#A function calculating length STOP


#Filtering -  START, 
tab0<-read.csv(file ="KSH_output.txt" , sep=";", dec=".", header = T)
old_size<-nrow(tab0)
old_size
tab0<-dplyr::filter(tab0, DOC<0.90)
new_size<-nrow(tab0)
new_size
new_size/old_size
nrow(tab0)
#Filtering -  STOP

#Setting seed START
set.seed(1)
#Setting seed STOP


#Elbow method (normals) START

tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(x = as.matrix(dplyr::select(tab0, c("X_N","Y_N", "Z_N"))), centers = k, nstart=40, iter.max = 100000, algorithm="Lloyd")
  model$tot.withinss
})
elbow_df <- data.frame(k = 1:10, tot_withinss = tot_withinss)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_point(size = 1.5) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  ggtitle("Relationship between tot_withinss and the number of clusters") +
  theme(plot.title = element_text(hjust = 0.5))

homocline_normals_totwithinss<- ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_point(size = 1.5) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  ggtitle("Relationship between tot_withinss and the number of clusters") +
  theme(plot.title = element_text(hjust = 0.5))

tiff("homocline_normals_totwithinss.tiff", res = 300, units = "in",  width=10, height=7)
homocline_normals_totwithinss
dev.off()

#Elbow method (normals) STOP



#2 clusters (normals) START
grupowanietab0_2normals<-kmeans( as.matrix(dplyr::select(tab0, c("X_N","Y_N", "Z_N"))), centers = 2, nstart=40, iter.max = 100000, algorithm="Lloyd" )
kmeans2normals<- grupowanietab0_2normals$cluster

centers2normals<-grupowanietab0_2normals$centers
centers2normals

centers2normals_1<-as.numeric(centers2normals[1,])
length_centers2normals_1<-norm_vec(centers2normals_1)
normed_centers2normals_1<-centers2normals_1/length_centers2normals_1
centers2normals_1_dip_1 <- acos((normed_centers2normals_1[3]))*(180/pi)
centers2normals_1_dipdir_1 <- atan2(normed_centers2normals_1[2], normed_centers2normals_1[1])*180/pi
centers2normals_1_dipdir_1
centers2normals_1_dip_1

centers2normals_2<-as.numeric(centers2normals[2,])
length_centers2normals_2<-norm_vec(centers2normals_2)
normed_centers2normals_2<-centers2normals_2/length_centers2normals_2
centers2normals_2_dip_2 <- acos((normed_centers2normals_2[3]))*(180/pi)
centers2normals_2_dipdir_2 <- atan2(normed_centers2normals_2[2], normed_centers2normals_2[1])*180/pi
centers2normals_2_dipdir_2
centers2normals_2_dip_2
#2 clusters (normals) STOP


#3 clusters (normals) START
grupowanietab0_3normals<-kmeans(as.matrix(dplyr::select(tab0, c("X_N","Y_N", "Z_N"))), centers = 3, nstart=40,iter.max = 100000, algorithm="Lloyd" )
kmeans3normals<- grupowanietab0_3normals$cluster
centers3normals<-grupowanietab0_3normals$centers

centers3normals_1<-as.numeric(centers3normals[1,])
length_centers3normals_1<-norm_vec(centers3normals_1)
normed_centers3normals_1<-centers3normals_1/length_centers3normals_1
centers3normals_1_dip_1 <- acos((normed_centers3normals_1[3]))*(180/pi)
centers3normals_1_dipdir_1 <- atan2(normed_centers3normals_1[2], normed_centers3normals_1[1])*180/pi
centers3normals_1_dipdir_1<-centers3normals_1_dipdir_1+360
centers3normals_1_dip_1

centers3normals_2<-as.numeric(centers3normals[2,])
length_centers3normals_2<-norm_vec(centers3normals_2)
normed_centers3normals_2<-centers3normals_2/length_centers3normals_2
centers3normals_2_dip_2 <- acos((normed_centers3normals_2[3]))*(180/pi)
centers3normals_2_dipdir_2 <- atan2(normed_centers3normals_2[2], normed_centers3normals_2[1])*180/pi
centers3normals_2_dipdir_2
centers3normals_2_dip_2

centers3normals_3<-as.numeric(centers3normals[3,])
length_centers3normals_3<-norm_vec(centers3normals_3)
normed_centers3normals_3<-centers3normals_3/length_centers3normals_3
centers3normals_3_dip_3 <- acos((normed_centers3normals_3[3]))*(180/pi)
centers3normals_3_dipdir_3 <- atan2(normed_centers3normals_3[2], normed_centers3normals_3[1])*180/pi
centers3normals_3_dipdir_3<-centers3normals_3_dipdir_3
centers3normals_3_dipdir_3
centers3normals_3_dip_3
#3 clusters (normals) STOP


#4 clusters (normals) START
grupowanietab0_4normals<-kmeans(as.matrix(dplyr::select(tab0, c("X_N","Y_N", "Z_N"))), centers = 4 , nstart=40,iter.max = 100000, algorithm="Lloyd" )
kmeans4normals<- grupowanietab0_4normals$cluster
centers4normals<-grupowanietab0_4normals$centers

centers4normals_1<-as.numeric(centers4normals[1,])
length_centers4normals_1<-norm_vec(centers4normals_1)
normed_centers4normals_1<-centers4normals_1/length_centers4normals_1
centers4normals_1_dip_1 <- acos((normed_centers4normals_1[3]))*(180/pi)
centers4normals_1_dipdir_1 <- atan2(normed_centers4normals_1[2], normed_centers4normals_1[1])*180/pi
centers4normals_1_dipdir_1
centers4normals_1_dip_1

centers4normals_2<-as.numeric(centers4normals[2,])
length_centers4normals_2<-norm_vec(centers4normals_2)
normed_centers4normals_2<-centers4normals_2/length_centers4normals_2
centers4normals_2_dip_2 <- acos((normed_centers4normals_2[3]))*(180/pi)
centers4normals_2_dipdir_2 <- atan2(normed_centers4normals_2[2], normed_centers4normals_2[1])*180/pi
centers4normals_2_dipdir_2<-centers4normals_2_dipdir_2
centers4normals_2_dipdir_2
centers4normals_2_dip_2

centers4normals_3<-as.numeric(centers4normals[3,])
length_centers4normals_3<-norm_vec(centers4normals_3)
normed_centers4normals_3<-centers4normals_3/length_centers4normals_3
centers4normals_3_dip_3 <- acos((normed_centers4normals_3[3]))*(180/pi)
centers4normals_3_dipdir_3 <- atan2(normed_centers4normals_3[2], normed_centers4normals_3[1])*180/pi
centers4normals_3_dipdir_3
centers4normals_3_dip_3

centers4normals_4<-as.numeric(centers4normals[4,])
length_centers4normals_4<-norm_vec(centers4normals_4)
normed_centers4normals_4<-centers4normals_4/length_centers4normals_4
centers4normals_4_dip_4 <- acos((normed_centers4normals_4[3]))*(180/pi)
centers4normals_4_dipdir_4 <- atan2(normed_centers4normals_4[2], normed_centers4normals_4[1])*180/pi
centers4normals_4_dipdir_4<-centers4normals_4_dipdir_4+360
centers4normals_4_dipdir_4
centers4normals_4_dip_4
#4 clusters (normals) STOP


#5 clusters (normals) START
grupowanietab0_5normals<-kmeans(as.matrix(dplyr::select(tab0, c("X_N","Y_N", "Z_N"))), centers = 5, nstart=40, iter.max = 100000,algorithm="Lloyd" )
kmeans5normals<- grupowanietab0_5normals$cluster
centers5normals<-grupowanietab0_5normals$centers

centers5normals_1<-as.numeric(centers5normals[1,])
length_centers5normals_1<-norm_vec(centers5normals_1)
normed_centers5normals_1<-centers5normals_1/length_centers5normals_1
centers5normals_1_dip_1 <- acos((normed_centers5normals_1[3]))*(180/pi)
centers5normals_1_dipdir_1 <- atan2(normed_centers5normals_1[2], normed_centers5normals_1[1])*180/pi
centers5normals_1_dipdir_1
centers5normals_1_dip_1

centers5normals_2<-as.numeric(centers5normals[2,])
length_centers5normals_2<-norm_vec(centers5normals_2)
normed_centers5normals_2<-centers5normals_2/length_centers5normals_2
centers5normals_2_dip_2 <- acos((normed_centers5normals_2[3]))*(180/pi)
centers5normals_2_dipdir_2 <- atan2(normed_centers5normals_2[2], normed_centers5normals_2[1])*180/pi
centers5normals_2_dipdir_2<-centers5normals_2_dipdir_2+360
centers5normals_2_dipdir_2
centers5normals_2_dip_2

centers5normals_3<-as.numeric(centers5normals[3,])
length_centers5normals_3<-norm_vec(centers5normals_3)
normed_centers5normals_3<-centers5normals_3/length_centers5normals_3
centers5normals_3_dip_3 <- acos((normed_centers5normals_3[3]))*(180/pi)
centers5normals_3_dipdir_3 <- atan2(normed_centers5normals_3[2], normed_centers5normals_3[1])*180/pi
centers5normals_3_dipdir_3
centers5normals_3_dip_3

centers5normals_4<-as.numeric(centers5normals[4,])
length_centers5normals_4<-norm_vec(centers5normals_4)
normed_centers5normals_4<-centers5normals_4/length_centers5normals_4
centers5normals_4_dip_4 <- acos((normed_centers5normals_4[3]))*(180/pi)
centers5normals_4_dipdir_4 <- atan2(normed_centers5normals_4[2], normed_centers5normals_4[1])*180/pi
centers5normals_4_dipdir_4
centers5normals_4_dip_4

centers5normals_5<-as.numeric(centers5normals[5,])
length_centers5normals_5<-norm_vec(centers5normals_5)
normed_centers5normals_5<-centers5normals_5/length_centers5normals_5
centers5normals_5_dip_5 <- acos((normed_centers5normals_5[3]))*(180/pi)
centers5normals_5_dipdir_5 <- atan2(normed_centers5normals_5[2], normed_centers5normals_5[1])*180/pi
centers5normals_5_dipdir_5
centers5normals_5_dip_5
#5 clusters (normals) STOP


#6 clusters (normals) START
grupowanietab0_6normals<-kmeans(as.matrix(dplyr::select(tab0, c("X_N","Y_N", "Z_N"))), centers = 6, nstart=40,iter.max = 100000, algorithm="Lloyd" )
kmeans6normals<- grupowanietab0_6normals$cluster
centers6normals<-grupowanietab0_6normals$centers

centers6normals_1<-as.numeric(centers6normals[1,])
length_centers6normals_1<-norm_vec(centers6normals_1)
normed_centers6normals_1<-centers6normals_1/length_centers6normals_1
centers6normals_1_dip_1 <- acos((normed_centers6normals_1[3]))*(180/pi)
centers6normals_1_dipdir_1 <- atan2(normed_centers6normals_1[2], normed_centers6normals_1[1])*180/pi
centers6normals_1_dipdir_1
centers6normals_1_dip_1

centers6normals_2<-as.numeric(centers6normals[2,])
length_centers6normals_2<-norm_vec(centers6normals_2)
normed_centers6normals_2<-centers6normals_2/length_centers6normals_2
centers6normals_2_dip_2 <- acos((normed_centers6normals_2[3]))*(180/pi)
centers6normals_2_dipdir_2 <- atan2(normed_centers6normals_2[2], normed_centers6normals_2[1])*180/pi
centers6normals_2_dipdir_2<-centers6normals_2_dipdir_2+360
centers6normals_2_dipdir_2
centers6normals_2_dip_2

centers6normals_3<-as.numeric(centers6normals[3,])
length_centers6normals_3<-norm_vec(centers6normals_3)
normed_centers6normals_3<-centers6normals_3/length_centers6normals_3
centers6normals_3_dip_3 <- acos((normed_centers6normals_3[3]))*(180/pi)
centers6normals_3_dipdir_3 <- atan2(normed_centers6normals_3[2], normed_centers6normals_3[1])*180/pi
centers6normals_3_dipdir_3
centers6normals_3_dip_3

centers6normals_4<-as.numeric(centers6normals[4,])
length_centers6normals_4<-norm_vec(centers6normals_4)
normed_centers6normals_4<-centers6normals_4/length_centers6normals_4
centers6normals_4_dip_4 <- acos((normed_centers6normals_4[3]))*(180/pi)
centers6normals_4_dipdir_4 <- atan2(normed_centers6normals_4[2], normed_centers6normals_4[1])*180/pi
centers6normals_4_dipdir_4
centers6normals_4_dip_4

centers6normals_5<-as.numeric(centers6normals[5,])
length_centers6normals_5<-norm_vec(centers6normals_5)
normed_centers6normals_5<-centers6normals_5/length_centers6normals_5
centers6normals_5_dip_5 <- acos((normed_centers6normals_5[3]))*(180/pi)
centers6normals_5_dipdir_5 <- atan2(normed_centers6normals_5[2], normed_centers6normals_5[1])*180/pi
centers6normals_5_dipdir_5
centers6normals_5_dip_5

centers6normals_6<-as.numeric(centers6normals[6,])
length_centers6normals_6<-norm_vec(centers6normals_6)
normed_centers6normals_6<-centers6normals_6/length_centers6normals_6
centers6normals_6_dip_6 <- acos((normed_centers6normals_6[3]))*(180/pi)
centers6normals_6_dipdir_6 <- atan2(normed_centers6normals_6[2], normed_centers6normals_6[1])*180/pi
centers6normals_6_dipdir_6<-centers6normals_6_dipdir_6
centers6normals_6_dipdir_6
centers6normals_6_dip_6
#6 clusters (normals) STOP


#Elbow method (dips) START

tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(x = as.matrix(dplyr::select(tab0, c("X_D","Y_D", "Z_D"))), centers = k, nstart=40, iter.max = 100000, algorithm="Lloyd")
  model$tot.withinss
})
elbow_df <- data.frame(k = 1:10, tot_withinss = tot_withinss)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_point(size = 1.5) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  ggtitle("Relationship between tot_withinss and the number of clusters") +
  theme(plot.title = element_text(hjust = 0.5))

tiff("homocline_dips_totwithinss.tiff", res = 300, units = "in",  width=10, height=7)
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_point(size = 1.5) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  ggtitle("Relationship between tot_withinss and the number of clusters") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
#Elbow method (dips) START

#2 clusters (dips) START
grupowanietab0_2upadowe<-kmeans(as.matrix(dplyr::select(tab0, c("X_D","Y_D", "Z_D"))), centers = 2, nstart=40,iter.max = 100000, algorithm="Lloyd" )
kmeans2upadowe<- grupowanietab0_2upadowe$cluster
centers2dip<-grupowanietab0_2upadowe$centers

centers2dip_1<-as.numeric(centers2dip[1,])
length_centers2dip_1<-norm_vec(centers2dip_1)
normed_centers2dip_1<-centers2dip_1/length_centers2dip_1
centers2dip_1_dip_1 <- acos((normed_centers2dip_1[3]))*(180/pi)
centers2dip_1_dipdir_1 <- atan2(normed_centers2dip_1[2], normed_centers2dip_1[1])*180/pi
centers2dip_1_dip_1<-centers2dip_1_dip_1-90
centers2dip_1_dipdir_1
centers2dip_1_dip_1

centers2dip_2<-as.numeric(centers2dip[2,])
length_centers2dip_2<-norm_vec(centers2dip_2)
normed_centers2dip_2<-centers2dip_2/length_centers2dip_2
centers2dip_2_dip_2 <- acos((normed_centers2dip_2[3]))*(180/pi)
centers2dip_2_dipdir_2 <- atan2(normed_centers2dip_2[2], normed_centers2dip_2[1])*180/pi
centers2dip_2_dipdir_2<-centers2dip_2_dipdir_2+360
centers2dip_2_dip_2<-centers2dip_2_dip_2-90
centers2dip_2_dipdir_2
centers2dip_2_dip_2
#2 clusters (dips) STOP


#3 clusters (dips) START
grupowanietab0_3upadowe<-kmeans(as.matrix(dplyr::select(tab0, c("X_D","Y_D", "Z_D"))), centers = 3, nstart=40,iter.max = 100000, algorithm="Lloyd" )
kmeans3upadowe<- grupowanietab0_3upadowe$cluster
centers3dip<-grupowanietab0_3upadowe$centers

centers3dip_1<-as.numeric(centers3dip[1,])
length_centers3dip_1<-norm_vec(centers3dip_1)
normed_centers3dip_1<-centers3dip_1/length_centers3dip_1
centers3dip_1_dip_1 <- acos((normed_centers3dip_1[3]))*(180/pi)
centers3dip_1_dipdir_1 <- atan2(normed_centers3dip_1[2], normed_centers3dip_1[1])*180/pi
centers3dip_1_dip_1<-centers3dip_1_dip_1-90
centers3dip_1_dipdir_1
centers3dip_1_dip_1

centers3dip_2<-as.numeric(centers3dip[2,])
length_centers3dip_2<-norm_vec(centers3dip_2)
normed_centers3dip_2<-centers3dip_2/length_centers3dip_2
centers3dip_2_dip_2 <- acos((normed_centers3dip_2[3]))*(180/pi)
centers3dip_2_dipdir_2 <- atan2(normed_centers3dip_2[2], normed_centers3dip_2[1])*180/pi
centers3dip_2_dipdir_2<-centers3dip_2_dipdir_2+360
centers3dip_2_dip_2<-centers3dip_2_dip_2-90
centers3dip_2_dipdir_2
centers3dip_2_dip_2

centers3dip_3<-as.numeric(centers3dip[3,])
length_centers3dip_3<-norm_vec(centers3dip_3)
normed_centers3dip_3<-centers3dip_3/length_centers3dip_3
centers3dip_3_dip_3 <- acos((normed_centers3dip_3[3]))*(180/pi)
centers3dip_3_dipdir_3 <- atan2(normed_centers3dip_3[2], normed_centers3dip_3[1])*180/pi
centers3dip_3_dip_3<-centers3dip_3_dip_3-90
centers3dip_3_dipdir_3
centers3dip_3_dip_3
#3 clusters (dips) STOP

#4 clusters (dips) START
grupowanietab0_4upadowe<-kmeans(as.matrix(dplyr::select(tab0, c("X_D","Y_D", "Z_D"))), centers = 4, nstart=40,iter.max = 100000, algorithm="Lloyd" )
kmeans4upadowe<- grupowanietab0_4upadowe$cluster
centers4dip<-grupowanietab0_4upadowe$centers

centers4dip_1<-as.numeric(centers4dip[1,])
length_centers4dip_1<-norm_vec(centers4dip_1)
normed_centers4dip_1<-centers4dip_1/length_centers4dip_1
centers4dip_1_dip_1 <- acos((normed_centers4dip_1[3]))*(180/pi)
centers4dip_1_dipdir_1 <- atan2(normed_centers4dip_1[2], normed_centers4dip_1[1])*180/pi
centers4dip_1_dip_1<-centers4dip_1_dip_1-90
centers4dip_1_dipdir_1
centers4dip_1_dip_1

centers4dip_2<-as.numeric(centers4dip[2,])
length_centers4dip_2<-norm_vec(centers4dip_2)
normed_centers4dip_2<-centers4dip_2/length_centers4dip_2
centers4dip_2_dip_2 <- acos((normed_centers4dip_2[3]))*(180/pi)
centers4dip_2_dipdir_2 <- atan2(normed_centers4dip_2[2], normed_centers4dip_2[1])*180/pi
centers4dip_2_dipdir_2<-centers4dip_2_dipdir_2+360
centers4dip_2_dip_2<-centers4dip_2_dip_2-90
centers4dip_2_dipdir_2
centers4dip_2_dip_2


centers4dip_3<-as.numeric(centers4dip[3,])
length_centers4dip_3<-norm_vec(centers4dip_3)
normed_centers4dip_3<-centers4dip_3/length_centers4dip_3
centers4dip_3_dip_3 <- acos((normed_centers4dip_3[3]))*(180/pi)
centers4dip_3_dipdir_3 <- atan2(normed_centers4dip_3[2], normed_centers4dip_3[1])*180/pi
centers4dip_3_dip_3<-centers4dip_3_dip_3-90
centers4dip_3_dipdir_3
centers4dip_3_dip_3

centers4dip_4<-as.numeric(centers4dip[4,])
length_centers4dip_4<-norm_vec(centers4dip_4)
normed_centers4dip_4<-centers4dip_4/length_centers4dip_4
centers4dip_4_dip_4 <- acos((normed_centers4dip_4[3]))*(180/pi)
centers4dip_4_dipdir_4 <- atan2(normed_centers4dip_4[2], normed_centers4dip_4[1])*180/pi
centers4dip_4_dip_4<-centers4dip_4_dip_4-90
centers4dip_4_dipdir_4
centers4dip_4_dip_4
#4 clusters (dips) STOP

#5 clusters (dips) START
grupowanietab0_5upadowe<-kmeans(as.matrix(dplyr::select(tab0, c("X_D","Y_D", "Z_D"))), centers = 5, nstart=40,iter.max = 100000, algorithm="Lloyd" )
kmeans5upadowe<- grupowanietab0_5upadowe$cluster
centers5dip<-grupowanietab0_5upadowe$centers

centers5dip_1<-as.numeric(centers5dip[1,])
length_centers5dip_1<-norm_vec(centers5dip_1)
normed_centers5dip_1<-centers5dip_1/length_centers5dip_1
centers5dip_1_dip_1 <- acos((normed_centers5dip_1[3]))*(180/pi)
centers5dip_1_dipdir_1 <- atan2(normed_centers5dip_1[2], normed_centers5dip_1[1])*180/pi
centers5dip_1_dip_1<-centers5dip_1_dip_1-90
centers5dip_1_dipdir_1
centers5dip_1_dip_1

centers5dip_2<-as.numeric(centers5dip[2,])
length_centers5dip_2<-norm_vec(centers5dip_2)
normed_centers5dip_2<-centers5dip_2/length_centers5dip_2
centers5dip_2_dip_2 <- acos((normed_centers5dip_2[3]))*(180/pi)
centers5dip_2_dipdir_2 <- atan2(normed_centers5dip_2[2], normed_centers5dip_2[1])*180/pi
centers5dip_2_dipdir_2<-centers5dip_2_dipdir_2+360
centers5dip_2_dip_2<-centers5dip_2_dip_2-90
centers5dip_2_dipdir_2
centers5dip_2_dip_2


centers5dip_3<-as.numeric(centers5dip[3,])
length_centers5dip_3<-norm_vec(centers5dip_3)
normed_centers5dip_3<-centers5dip_3/length_centers5dip_3
centers5dip_3_dip_3 <- acos((normed_centers5dip_3[3]))*(180/pi)
centers5dip_3_dipdir_3 <- atan2(normed_centers5dip_3[2], normed_centers5dip_3[1])*180/pi
centers5dip_3_dip_3<-centers5dip_3_dip_3-90
centers5dip_3_dipdir_3
centers5dip_3_dip_3

centers5dip_4<-as.numeric(centers5dip[4,])
length_centers5dip_4<-norm_vec(centers5dip_4)
normed_centers5dip_4<-centers5dip_4/length_centers5dip_4
centers5dip_4_dip_4 <- acos((normed_centers5dip_4[3]))*(180/pi)
centers5dip_4_dipdir_4 <- atan2(normed_centers5dip_4[2], normed_centers5dip_4[1])*180/pi
centers5dip_4_dip_4<-centers5dip_4_dip_4-90
centers5dip_4_dipdir_4
centers5dip_4_dip_4

centers5dip_5<-as.numeric(centers5dip[5,])
length_centers5dip_5<-norm_vec(centers5dip_5)
normed_centers5dip_5<-centers5dip_5/length_centers5dip_5
centers5dip_5_dip_5 <- acos((normed_centers5dip_5[3]))*(180/pi)
centers5dip_5_dipdir_5 <- atan2(normed_centers5dip_5[2], normed_centers5dip_5[1])*180/pi
centers5dip_5_dip_5<-centers5dip_5_dip_5-90
centers5dip_5_dipdir_5
centers5dip_5_dip_5
#5 clusters (dips) STOP

#6 clusters (dips) START
grupowanietab0_6upadowe<-kmeans(as.matrix(dplyr::select(tab0, c("X_D","Y_D", "Z_D"))), centers = 6, nstart=40,iter.max = 100000, algorithm="Lloyd" )
kmeans6upadowe<- grupowanietab0_6upadowe$cluster
centers6dip<-grupowanietab0_6upadowe$centers

centers6dip_1<-as.numeric(centers6dip[1,])
length_centers6dip_1<-norm_vec(centers6dip_1)
normed_centers6dip_1<-centers6dip_1/length_centers6dip_1
centers6dip_1_dip_1 <- acos((normed_centers6dip_1[3]))*(180/pi)
centers6dip_1_dipdir_1 <- atan2(normed_centers6dip_1[2], normed_centers6dip_1[1])*180/pi
centers6dip_1_dip_1<-centers6dip_1_dip_1-90
centers6dip_1_dipdir_1
centers6dip_1_dip_1

centers6dip_2<-as.numeric(centers6dip[2,])
length_centers6dip_2<-norm_vec(centers6dip_2)
normed_centers6dip_2<-centers6dip_2/length_centers6dip_2
centers6dip_2_dip_2 <- acos((normed_centers6dip_2[3]))*(180/pi)
centers6dip_2_dipdir_2 <- atan2(normed_centers6dip_2[2], normed_centers6dip_2[1])*180/pi
centers6dip_2_dipdir_2<-centers6dip_2_dipdir_2+360
centers6dip_2_dip_2<-centers6dip_2_dip_2-90
centers6dip_2_dipdir_2
centers6dip_2_dip_2


centers6dip_3<-as.numeric(centers6dip[3,])
length_centers6dip_3<-norm_vec(centers6dip_3)
normed_centers6dip_3<-centers6dip_3/length_centers6dip_3
centers6dip_3_dip_3 <- acos((normed_centers6dip_3[3]))*(180/pi)
centers6dip_3_dipdir_3 <- atan2(normed_centers6dip_3[2], normed_centers6dip_3[1])*180/pi
centers6dip_3_dip_3<-centers6dip_3_dip_3-90
centers6dip_3_dipdir_3
centers6dip_3_dip_3

centers6dip_4<-as.numeric(centers6dip[4,])
length_centers6dip_4<-norm_vec(centers6dip_4)
normed_centers6dip_4<-centers6dip_4/length_centers6dip_4
centers6dip_4_dip_4 <- acos((normed_centers6dip_4[3]))*(180/pi)
centers6dip_4_dipdir_4 <- atan2(normed_centers6dip_4[2], normed_centers6dip_4[1])*180/pi
centers6dip_4_dip_4<-centers6dip_4_dip_4-90
centers6dip_4_dipdir_4
centers6dip_4_dip_4

centers6dip_5<-as.numeric(centers6dip[5,])
length_centers6dip_5<-norm_vec(centers6dip_5)
normed_centers6dip_5<-centers6dip_5/length_centers6dip_5
centers6dip_5_dip_5 <- acos((normed_centers6dip_5[3]))*(180/pi)
centers6dip_5_dipdir_5 <- atan2(normed_centers6dip_5[2], normed_centers6dip_5[1])*180/pi
centers6dip_5_dip_5<-centers6dip_5_dip_5-90
centers6dip_5_dipdir_5
centers6dip_5_dip_5

centers6dip_6<-as.numeric(centers6dip[6,])
length_centers6dip_6<-norm_vec(centers6dip_6)
normed_centers6dip_6<-centers6dip_6/length_centers6dip_6
centers6dip_6_dip_6 <- acos((normed_centers6dip_6[3]))*(180/pi)
centers6dip_6_dipdir_6 <- atan2(normed_centers6dip_6[2], normed_centers6dip_6[1])*180/pi
centers6dip_6_dip_6<-centers6dip_6_dip_6-90
centers6dip_6_dipdir_6<-centers6dip_6_dipdir_6
centers6dip_6_dipdir_6
centers6dip_6_dip_6
#6 clusters (dips) STOP


#Cluster centers data frame START
cluster_centers<-c(
centers2normals_1_dipdir_1,
centers2normals_1_dip_1,

centers2normals_2_dipdir_2,
centers2normals_2_dip_2,



centers3normals_1_dipdir_1,
centers3normals_1_dip_1,

centers3normals_2_dipdir_2,
centers3normals_2_dip_2,

centers3normals_3_dipdir_3,
centers3normals_3_dip_3,


centers4normals_1_dipdir_1,
centers4normals_1_dip_1,

centers4normals_2_dipdir_2,
centers4normals_2_dip_2,

centers4normals_3_dipdir_3,
centers4normals_3_dip_3,

centers4normals_4_dipdir_4,
centers4normals_4_dip_4,





centers5normals_1_dipdir_1,
centers5normals_1_dip_1,

centers5normals_2_dipdir_2,
centers5normals_2_dip_2,

centers5normals_3_dipdir_3,
centers5normals_3_dip_3,

centers5normals_4_dipdir_4,
centers5normals_4_dip_4,

centers5normals_5_dipdir_5,
centers5normals_5_dip_5,


centers6normals_1_dipdir_1,
centers6normals_1_dip_1,

centers6normals_2_dipdir_2,
centers6normals_2_dip_2,

centers6normals_3_dipdir_3,
centers6normals_3_dip_3,

centers6normals_4_dipdir_4,
centers6normals_4_dip_4,

centers6normals_5_dipdir_5,
centers6normals_5_dip_5,

centers6normals_6_dipdir_6,
centers6normals_6_dip_6,









centers2dip_1_dipdir_1,
centers2dip_1_dip_1,

centers2dip_2_dipdir_2,
centers2dip_2_dip_2,



centers3dip_1_dipdir_1,
centers3dip_1_dip_1,

centers3dip_2_dipdir_2,
centers3dip_2_dip_2,

centers3dip_3_dipdir_3,
centers3dip_3_dip_3,


centers4dip_1_dipdir_1,
centers4dip_1_dip_1,

centers4dip_2_dipdir_2,
centers4dip_2_dip_2,

centers4dip_3_dipdir_3,
centers4dip_3_dip_3,

centers4dip_4_dipdir_4,
centers4dip_4_dip_4,





centers5dip_1_dipdir_1,
centers5dip_1_dip_1,

centers5dip_2_dipdir_2,
centers5dip_2_dip_2,

centers5dip_3_dipdir_3,
centers5dip_3_dip_3,

centers5dip_4_dipdir_4,
centers5dip_4_dip_4,

centers5dip_5_dipdir_5,
centers5dip_5_dip_5,


centers6dip_1_dipdir_1,
centers6dip_1_dip_1,

centers6dip_2_dipdir_2,
centers6dip_2_dip_2,

centers6dip_3_dipdir_3,
centers6dip_3_dip_3,

centers6dip_4_dipdir_4,
centers6dip_4_dip_4,

centers6dip_5_dipdir_5,
centers6dip_5_dip_5,

centers6dip_6_dipdir_6,
centers6dip_6_dip_6



)


cluster_names<-c(
  "centers2normals_1",
  "centers2normals_2",

  "centers3normals_1",
  "centers3normals_2",
  "centers3normals_3",
  
  "centers4normals_1",
  "centers4normals_2",
  "centers4normals_3",
  "centers4normals_4",
  
  "centers5normals_1",
  "centers5normals_2",
  "centers5normals_3",
  "centers5normals_4",
  "centers5normals_5",

  "centers6normals_1",
  "centers6normals_2",
  "centers6normals_3",
  "centers6normals_4",
  "centers6normals_5",
  "centers6normals_6",
  
  
  "centers2dip_1",
  "centers2dip_2",
  
  "centers3dip_1",
  "centers3dip_2",
  "centers3dip_3",
  
  "centers4dip_1",
  "centers4dip_2",
  "centers4dip_3",
  "centers4dip_4",
  
  "centers5dip_1",
  "centers5dip_2",
  "centers5dip_3",
  "centers5dip_4",
  "centers5dip_5",
  
  "centers6dip_1",
  "centers6dip_2",
  "centers6dip_3",
  "centers6dip_4",
  "centers6dip_5",
  "centers6dip_6"
  
  
)

cluster_matrix<-matrix(cluster_centers, ncol=2, nrow=length(cluster_centers)/2, byrow = T )
cluster_matrix<-data.frame(cluster_matrix)
cluster_matrix[cluster_matrix$X1 > 360,]$X1=cluster_matrix[cluster_matrix$X1 > 360,]$X1-360
cluster_matrix[cluster_matrix$X1 < 0,]$X1=cluster_matrix[cluster_matrix$X1 < 0,]$X1+360
cluster_matrix<-dplyr::select(cluster_matrix, c(2,1))
colnames(cluster_matrix)<-c("dip_ang", "dip_dir")
cluster_matrix<-data.frame(cluster_matrix, row.names = cluster_names)
cluster_matrix<-dplyr::mutate(cluster_matrix, label=cluster_names)
cluster_matrix

write.table(x=cluster_matrix, file = "cluster_matrix.csv", sep=";", dec=".", col.names = TRUE, row.names=FALSE )

#Cluster centers data frame STOP

#Cluster output START
tab0<-dplyr::mutate(tab0,  
                    kmeans2normals, kmeans3normals, kmeans4normals, kmeans5normals, kmeans6normals,
                    kmeans2upadowe, kmeans3upadowe, kmeans4upadowe, kmeans5upadowe, kmeans6upadowe)

write.table(x=tab0, file = "homoclinetab0.csv", sep=";", dec=".", col.names = TRUE, row.names=FALSE )
#Cluster output STOP



#Palette START
pal2<- c( "green", "magenta")
pal3<- c( "magenta", "green","blue")
pal4<- c("green", "magenta", "blue", "black")
pal5<- c("yellow", "black", "green", "magenta", "blue")
pal6<- c("brown", "yellow", "black", "green", "magenta", "blue")
#Palette STOP

#Saving (normals) START
max_x<-max(max(tab0$X1 ), max(tab0$X2 ), max(tab0$X3 ))
min_x<-min(min(tab0$X1 ), min(tab0$X2 ), min(tab0$X3 ))
max_y<-max(max(tab0$Y1 ), max(tab0$Y2 ), max(tab0$Y3 ))
min_y<-min(min(tab0$Y1 ), min(tab0$Y2 ), min(tab0$Y3 ))
coeff <- (max_x-min_x)/(max_y-min_y)

tiff("homocline_all_output_0_normals2.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab0,aes(x=Y_C, y=X_C, col=factor(kmeans2normals)))+geom_point(size=2)+
  scale_color_manual("Cluster", values =  pal2)+coord_fixed(ratio=1)
dev.off()

tiff("homocline_all_output_0_normals3.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab0,aes(x=Y_C, y=X_C, col=factor(kmeans3normals)))+geom_point(size=2)+
  scale_color_manual("Cluster", values =  pal3)+coord_fixed(ratio=1)
dev.off()
pal4<- c("green", "blue", "magenta", "black")
tiff("homocline_all_output_0_normals4.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab0,aes(x=Y_C, y=X_C, col=factor(kmeans4normals)))+geom_point(size=2)+
  scale_color_manual("Cluster", values =  pal4)+coord_fixed(ratio=1)
dev.off()

tiff("homocline_all_output_0_normals5.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab0,aes(x=Y_C, y=X_C, col=factor(kmeans5normals)))+geom_point(size=2)+
  scale_color_manual("Cluster", values =  pal5)+coord_fixed(ratio=1)
dev.off()

tiff("homocline_all_output_0_normals6.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab0,aes(x=Y_C, y=X_C, col=factor(kmeans6normals)))+geom_point(size=2)+
  scale_color_manual("Cluster", values =  pal6)+coord_fixed(ratio=1)
dev.off()
#Saving (normals) STOP

#Palette START
pal2<- c( "green", "magenta")
pal3<- c( "green", "blue","magenta")
pal4<- c("green", "black", "blue", "magenta")
pal5<- c("yellow", "black", "green", "magenta", "blue")
pal6<- c("brown", "yellow", "black", "green", "magenta", "blue")

#Palette STOP

#Saving (dips) START
tiff("homocline_all_output_0_upadowe2.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab0,aes(x=Y_C, y=X_C, col=factor(kmeans2upadowe)))+geom_point(size=2)+
  scale_color_manual("Cluster", values =  pal2)+coord_fixed(ratio=1)
dev.off()

tiff("homocline_all_output_0_upadowe3.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab0,aes(x=Y_C, y=X_C, col=factor(kmeans3upadowe)))+geom_point(size=2)+
  scale_color_manual("Cluster", values =  pal3)+coord_fixed(ratio=1)
dev.off()
pal4<- c("green", "black", "blue", "magenta")
tiff("homocline_all_output_0_upadowe4.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab0,aes(x=Y_C, y=X_C, col=factor(kmeans4upadowe)))+geom_point(size=2)+
  scale_color_manual("Cluster", values =  pal4)+coord_fixed(ratio=1)
dev.off()

tiff("homocline_all_output_0_upadowe5.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab0,aes(x=Y_C, y=X_C, col=factor(kmeans5upadowe)))+geom_point(size=2)+
  scale_color_manual("Cluster", values =  pal5)+coord_fixed(ratio=1)
dev.off()

tiff("homocline_all_output_0_upadowe6.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab0,aes(x=Y_C, y=X_C, col=factor(kmeans6upadowe)))+geom_point(size=2)+
  scale_color_manual("Cluster", values =  pal6)+coord_fixed(ratio=1)
dev.off()
#Saving (dips) STOP


#Grid maps START

gridpliknazwa<-"KSH_grid_locate_200"
gridded <- read.table(paste0(gridpliknazwa, ".txt"), header=TRUE, sep = ";")
head(gridded)
nbottom <- tab0
table(nbottom$kmeans3upadowe)
head(nbottom)
nmerged <- merge(x = gridded, y = nbottom, by = c("IDT1", "IDT2", "IDT3"), all.y = TRUE)
head(nmerged)


id_vec <- data.frame(nmerged$IDT1,nmerged$IDT2,nmerged$IDT3)
nrow(id_vec)
nl_all <- nrow(unique(id_vec))
nl_all

nmerged_filtered<-dplyr::filter(nmerged, !is.na(px))
id_vec_filtered <- data.frame(nmerged_filtered$IDT1,nmerged_filtered$IDT2,nmerged_filtered$IDT3)
nrow(id_vec_filtered)
nl_some <- nrow(unique(id_vec_filtered))
nl_some

proportion<-nl_some/nl_all*100
proportion
colnames(nmerged)
ekspert_pal<- c("green", "magenta", "blue", "magenta")


max_x<-max(max(nbottom$X1 ), max(nbottom$X2 ), max(nbottom$X3 ))
min_x<-min(min(nbottom$X1 ), min(nbottom$X2 ), min(nbottom$X3 ))
max_y<-max(max(nbottom$Y1 ), max(nbottom$Y2 ), max(nbottom$Y3 ))
min_y<-min(min(nbottom$Y1 ), min(nbottom$Y2 ), min(nbottom$Y3 ))
coeff <- (max_x-min_x)/(max_y-min_y)
coeff

gekspert1<-ggplot(nmerged_filtered, aes(x=py, y=px, col=factor(kmeans3upadowe) ))+
  geom_point(size=1.0)+
  scale_color_manual("Cluster", values =  pal3)+ggtitle(paste0("200 meter spacing. Coverage: ", round(proportion,2), "%."))

gekspert1<-gekspert1+coord_fixed(ratio=1)
gekspert1


gridpliknazwa<-"KSH_grid_locate_150"
gridded <- read.table(paste0(gridpliknazwa, ".txt"), header=TRUE, sep = ";")
head(gridded)
nbottom <- tab0
table(nbottom$kmeans3upadowe)
head(nbottom)
nmerged <- merge(x = gridded, y = nbottom, by = c("IDT1", "IDT2", "IDT3"), all.y = TRUE)
head(nmerged)


id_vec <- data.frame(nmerged$IDT1,nmerged$IDT2,nmerged$IDT3)
nrow(id_vec)
nl_all <- nrow(unique(id_vec))
nl_all

nmerged_filtered<-dplyr::filter(nmerged, !is.na(px))
id_vec_filtered <- data.frame(nmerged_filtered$IDT1,nmerged_filtered$IDT2,nmerged_filtered$IDT3)
nrow(id_vec_filtered)
nl_some <- nrow(unique(id_vec_filtered))
nl_some

proportion<-nl_some/nl_all*100
proportion
colnames(nmerged)
ekspert_pal<- c("green", "magenta", "blue", "magenta")


max_x<-max(max(nbottom$X1 ), max(nbottom$X2 ), max(nbottom$X3 ))
min_x<-min(min(nbottom$X1 ), min(nbottom$X2 ), min(nbottom$X3 ))
max_y<-max(max(nbottom$Y1 ), max(nbottom$Y2 ), max(nbottom$Y3 ))
min_y<-min(min(nbottom$Y1 ), min(nbottom$Y2 ), min(nbottom$Y3 ))
coeff <- (max_x-min_x)/(max_y-min_y)
coeff

gekspert2<-ggplot(nmerged_filtered, aes(x=py, y=px, col=factor(kmeans3upadowe) ))+
  geom_point(size=1.0)+
  scale_color_manual("Cluster", values =  pal3)+ggtitle(paste0("150 meter spacing. Coverage: ", round(proportion,2), "%."))

gekspert2<-gekspert2+coord_fixed(ratio=1)
gekspert2

gridpliknazwa<-"KSH_grid_locate_100"
gridded <- read.table(paste0(gridpliknazwa, ".txt"), header=TRUE, sep = ";")
head(gridded)
nbottom <- tab0
table(nbottom$kmeans3upadowe)
head(nbottom)
nmerged <- merge(x = gridded, y = nbottom, by = c("IDT1", "IDT2", "IDT3"), all.y = TRUE)
head(nmerged)


id_vec <- data.frame(nmerged$IDT1,nmerged$IDT2,nmerged$IDT3)
nrow(id_vec)
nl_all <- nrow(unique(id_vec))
nl_all

nmerged_filtered<-dplyr::filter(nmerged, !is.na(px))
id_vec_filtered <- data.frame(nmerged_filtered$IDT1,nmerged_filtered$IDT2,nmerged_filtered$IDT3)
nrow(id_vec_filtered)
nl_some <- nrow(unique(id_vec_filtered))
nl_some

proportion<-nl_some/nl_all*100

proportion
colnames(nmerged)
ekspert_pal<- c("green", "magenta", "blue", "magenta")


max_x<-max(max(nbottom$X1 ), max(nbottom$X2 ), max(nbottom$X3 ))
min_x<-min(min(nbottom$X1 ), min(nbottom$X2 ), min(nbottom$X3 ))
max_y<-max(max(nbottom$Y1 ), max(nbottom$Y2 ), max(nbottom$Y3 ))
min_y<-min(min(nbottom$Y1 ), min(nbottom$Y2 ), min(nbottom$Y3 ))
coeff <- (max_x-min_x)/(max_y-min_y)
coeff

gekspert3<-ggplot(nmerged_filtered, aes(x=py, y=px, col=factor(kmeans3upadowe) ))+
  geom_point(size=1.0)+
  scale_color_manual("Cluster", values =  pal3)+ggtitle(paste0("100 meter spacing. Coverage: ", round(proportion,2), "%."))

gekspert3<-gekspert3+coord_fixed(ratio=1)
gekspert3

gridpliknazwa<-"KSH_grid_locate_50"
gridded <- read.table(paste0(gridpliknazwa, ".txt"), header=TRUE, sep = ";")
head(gridded)
nbottom <- tab0
table(nbottom$kmeans3upadowe)
head(nbottom)
nmerged <- merge(x = gridded, y = nbottom, by = c("IDT1", "IDT2", "IDT3"), all.y = TRUE)
head(nmerged)


id_vec <- data.frame(nmerged$IDT1,nmerged$IDT2,nmerged$IDT3)
nrow(id_vec)
nl_all <- nrow(unique(id_vec))
nl_all

nmerged_filtered<-dplyr::filter(nmerged, !is.na(px))
id_vec_filtered <- data.frame(nmerged_filtered$IDT1,nmerged_filtered$IDT2,nmerged_filtered$IDT3)
nrow(id_vec_filtered)
nl_some <- nrow(unique(id_vec_filtered))
nl_some

proportion<-nl_some/nl_all*100

proportion
colnames(nmerged)
ekspert_pal<- c("green", "magenta", "blue", "magenta")


max_x<-max(max(nbottom$X1 ), max(nbottom$X2 ), max(nbottom$X3 ))
min_x<-min(min(nbottom$X1 ), min(nbottom$X2 ), min(nbottom$X3 ))
max_y<-max(max(nbottom$Y1 ), max(nbottom$Y2 ), max(nbottom$Y3 ))
min_y<-min(min(nbottom$Y1 ), min(nbottom$Y2 ), min(nbottom$Y3 ))
coeff <- (max_x-min_x)/(max_y-min_y)
coeff

gekspert4<-ggplot(nmerged_filtered, aes(x=py, y=px, col=factor(kmeans3upadowe) ))+
  geom_point(size=0.25)+
  scale_color_manual("Cluster", values =  pal3)+ggtitle(paste0("50 meter spacing. Coverage: ", round(proportion,2), "%."))

gekspert4<-gekspert4+coord_fixed(ratio=1)
gekspert4


max_x<-max(max(nbottom$X_C ), max(nbottom$X_C ), max(nbottom$X_C ))
min_x<-min(min(nbottom$X_C ), min(nbottom$X_C ), min(nbottom$X_C ))
max_y<-max(max(nbottom$Y_C ), max(nbottom$Y_C ), max(nbottom$Y_C ))
min_y<-min(min(nbottom$Y_C ), min(nbottom$Y_C ), min(nbottom$Y_C ))
coeff <- (max_x-min_x)/(max_y-min_y)
coeff


tiff(paste0("Arrange_plot","_regular", Sys.Date(), "_",".tiff"), units="in", width = 10, height=10*coeff, res=300)
ggarrange( gekspert1, gekspert2, gekspert3, gekspert4, ncol=2, nrow=2, labels=c("a", "b", "c", "d"), common.legend = TRUE)
dev.off()

#Grid maps STOP

