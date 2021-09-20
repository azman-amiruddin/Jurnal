library(readxl)
Jurnal <- read_excel("D:/FILE KULIAH/SEMESTER 8/Jurnal Azman/Data Jurnal.xlsx",
                col_types = c("skip", "numeric","numeric","numeric",
                              "numeric","numeric","numeric","numeric"))

Jurnal
View(Jurnal)
summary(Jurnal)
#Deteksi Outlier
#Jarak Mahalanobis
Mah <- mahalanobis(Jurnal,colMeans(Jurnal),cov(Jurnal))
Mah
View(Mah)
#Uji Kecukupan Sampel
#Uji KMO 
KMO <- function (x)
  { 
  x <- subset(x,complete.cases(x))
  r <- cor(x)
  r2 <- r^2 
  i <- solve(r)
  d <- diag (i)
  p2 <- (-i/sqrt(outer(d,d)))^2
  diag(r2) <- diag(p2) <- 0
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  return(list(KMO=KMO)) 
  }
KMO(Jurnal) #menampilkan nilai KMO
#menghitung korelasi variable 
R<-cor(Jurnal) 
R # menampilkan korelasi variable
View(R)

#Melakukan PCA mengatasi Multikolinearitas 
#PCA
PCA<-prcomp(Jurnal, scale. = FALSE)
PCA
Variansi<-PCA$sdev^2
Variansi
summary(PCA)
PCA_scores<-PCA$x[,1:3]
PCA_scores

#membuat matriks jarak (D)
D<-dist(PCA_scores,method = "canberra")
D
#Analisis Cluster 
library(cluster)
kluster_hirarki<-hclust(D,method = "average")
str(as.dendrogram(kluster_hirarki)) #proses average
dendrogram<-plot(kluster_hirarki) #Menampilkan dendrogram
#menentukan banyaknya kelompok
group<-cutree(kluster_hirarki,3)
kelompok<-cbind(group)
kelompok
plot2 <- plot(kluster_hirarki,hang=-1,col='black',
              main='Cluster Dendrogram Average Linkage',
              sub='',xlab='Indeks Kabupaten/Kota',ylab='Indeks Jarak')
rect.hclust(kluster_hirarki, k=3, border= 2:5)

