library(ggplot2)
library(dplyr)
library(purrr)

#A function calculating length START
norm_vec <- function(x) sqrt(sum(x^2))
#A function calculating length STOP


#Palette START
pal2<- c(   "black", "magenta")
pal3<- c(   "green","black",   "magenta")
pal4<- c( "black", "green", "blue", "magenta")
#Palette STOP

#Opening CEBS file START
tab1<-read.csv(file ="CEBS_output.txt" , sep=";", dec=".", header = T)
nrow(tab1)
#Opening CEBS file STOP


#Setting seed START
set.seed(1)
#Setting seed STOP

#Elbow method (normals) START


tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(x = as.matrix(dplyr::select(tab1, c("X_N","Y_N", "Z_N"))), centers = k, nstart=40, iter.max = 100000, algorithm="Lloyd")
  model$tot.withinss
})

elbow_df <- data.frame(k = 1:10, tot_withinss = tot_withinss)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_point(size = 1.5) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  ggtitle("Relationship between tot_withinss and the number of clusters") +
  theme(plot.title = element_text(hjust = 0.5))

tiff("CEBS_normals_totwithinss.tiff", res = 300, units = "in",  width=10, height=7)
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_point(size = 1.5) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  ggtitle("Relationship between tot_withinss and the number of clusters") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
#Elbow method (normals) STOP

#2 clusters (normals) START
grupowanietab1_2normals<-kmeans(as.matrix(dplyr::select(tab1, c("X_N","Y_N", "Z_N"))), centers = 2, nstart=40,iter.max = 100000, algorithm="Lloyd" )
kmeans2normals<- grupowanietab1_2normals$cluster
centers2normals<-grupowanietab1_2normals$centers

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
grupowanietab1_3normals<-kmeans(as.matrix(dplyr::select(tab1, c("X_N","Y_N", "Z_N"))), centers = 3, nstart=40,iter.max = 100000, algorithm="Lloyd" )
kmeans3normals<- grupowanietab1_3normals$cluster
centers3normals<-grupowanietab1_3normals$centers

centers3normals_1<-as.numeric(centers3normals[1,])
length_centers3normals_1<-norm_vec(centers3normals_1)
normed_centers3normals_1<-centers3normals_1/length_centers3normals_1
centers3normals_1_dip_1 <- acos((normed_centers3normals_1[3]))*(180/pi)
centers3normals_1_dipdir_1 <- atan2(normed_centers3normals_1[2], normed_centers3normals_1[1])*180/pi
centers3normals_1_dipdir_1<-centers3normals_1_dipdir_1
centers3normals_1_dipdir_1
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
grupowanietab1_4normals<-kmeans(as.matrix(dplyr::select(tab1, c("X_N","Y_N", "Z_N"))), centers = 4, nstart=40,iter.max = 100000, algorithm="Lloyd" )
kmeans4normals<- grupowanietab1_4normals$cluster
centers4normals<-grupowanietab1_4normals$centers

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

#Elbow method (dips) START
tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(x = as.matrix(dplyr::select(tab1, c("X_D","Y_D", "Z_D"))), centers = k, nstart=40, iter.max = 100000, algorithm="Lloyd")
  model$tot.withinss
})

elbow_df <- data.frame(k = 1:10, tot_withinss = tot_withinss)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_point(size = 1.5) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  ggtitle("Relationship between tot_withinss and the number of clusters") +
  theme(plot.title = element_text(hjust = 0.5))

tiff("CEBS_dips_totwithinss.tiff", res = 300, units = "in",  width=10, height=7)
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_point(size = 1.5) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  ggtitle("Relationship between tot_withinss and the number of clusters") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
#Elbow method (dips) STOP


#2 clusters (dips) START
grupowanietab1_2dip_version<-kmeans(as.matrix(dplyr::select(tab1, c("X_D","Y_D", "Z_D"))), centers = 2, nstart=40,iter.max = 100000, algorithm="Lloyd" )
kmeans2dip_version<- grupowanietab1_2dip_version$cluster
centers2dip<-grupowanietab1_2dip_version$centers

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
grupowanietab1_3dip_version<-kmeans(as.matrix(dplyr::select(tab1, c("X_D","Y_D", "Z_D"))), centers = 3, nstart=40,iter.max = 100000, algorithm="Lloyd" )
kmeans3dip_version<- grupowanietab1_3dip_version$cluster
centers3dip<-grupowanietab1_3dip_version$centers

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
grupowanietab1_4dip_version<-kmeans(as.matrix(dplyr::select(tab1, c("X_D","Y_D", "Z_D"))), centers = 4, nstart=40,iter.max = 100000, algorithm="Lloyd" )
kmeans4dip_version<- grupowanietab1_4dip_version$cluster
centers4dip<-grupowanietab1_4dip_version$centers

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
  centers4dip_4_dip_4
  
  
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
  
  
  "centers2dip_1",
  "centers2dip_2",
  
  "centers3dip_1",
  "centers3dip_2",
  "centers3dip_3",
  
  "centers4dip_1",
  "centers4dip_2",
  "centers4dip_3",
  "centers4dip_4"
  
  
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

write.table(x=cluster_matrix, file = "cluster_matrix_CEBS.csv", sep=";", dec=".", col.names = TRUE, row.names=FALSE )

#Cluster centers data frame STOP


#Saving output START
tab1<-dplyr::mutate(tab1,  
                    kmeans2normals, kmeans3normals, kmeans4normals, 
                    kmeans2dip_version, kmeans3dip_version, kmeans4dip_version)

write.table(x=tab1, file = "tab1.csv", sep=";", dec=".", col.names = TRUE, row.names=FALSE )
#Saving output STOP

#Saving figures START

max_x<-max(max(tab1$X1 ), max(tab1$X2 ), max(tab1$X3 ))
min_x<-min(min(tab1$X1 ), min(tab1$X2 ), min(tab1$X3 ))
max_y<-max(max(tab1$Y1 ), max(tab1$Y2 ), max(tab1$Y3 ))
min_y<-min(min(tab1$Y1 ), min(tab1$Y2 ), min(tab1$Y3 ))
coeff <- (max_x-min_x)/(max_y-min_y)

tiff("CEBS_normals2.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab1,aes(x=Y_C, y=X_C, col=factor(kmeans2normals)))+geom_point(size=0.005)+
  scale_color_manual("Cluster", values =  pal2)+coord_fixed(ratio=1)
dev.off()

tiff("CEBS_normals3.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab1,aes(x=Y_C, y=X_C, col=factor(kmeans3normals)))+geom_point(size=0.005)+
  scale_color_manual("Cluster", values =  pal3)+coord_fixed(ratio=1)
dev.off()

tiff("CEBS_normals4.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab1,aes(x=Y_C, y=X_C, col=factor(kmeans4normals)))+geom_point(size=0.08)+
  scale_color_manual("Cluster", values =  pal4)+coord_fixed(ratio=1)
dev.off()


tiff("CEBS_dip_version2.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab1,aes(x=Y_C, y=X_C, col=factor(kmeans2dip_version)))+geom_point(size=0.005)+
  scale_color_manual("Cluster", values =  pal2)+coord_fixed(ratio=1)
dev.off()

tiff("CEBS_dip_version3.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab1,aes(x=Y_C, y=X_C, col=factor(kmeans3dip_version)))+geom_point(size=0.005)+
  scale_color_manual("Cluster", values =  pal3)+coord_fixed(ratio=1)
dev.off()

tiff("CEBS_dip_version4.tiff", res = 300, units = "in",  width=10, height=10*coeff)
ggplot(tab1,aes(x=Y_C, y=X_C, col=factor(kmeans4dip_version)))+geom_point(size=0.08)+
  scale_color_manual("Cluster", values =  pal4)+coord_fixed(ratio=1)
dev.off()


#Saving figures STOP


#Dips sample START
tab1<-dplyr::mutate(tab1, kmeans3normals)
tab1_sample<-dplyr::sample_n(tab1, size=10000)
table(tab1_sample$kmeans3normals)
write.table(x=tab1_sample, file = "tab1_sample.csv", sep=";", dec=".", col.names = TRUE, row.names=FALSE )
#Dips sample STOP
