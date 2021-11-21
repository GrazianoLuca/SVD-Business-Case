library(Matrix)
library(openxlsx)
library(ggplot2)
library(tidyverse)
library(ggrepel)


#read matrix M' of example 2 from a file
dat<-read.csv('data5.csv')

#transform the dataset into matrix format
M<-as.matrix(dat)
M <- subset(M, select = -c(X))
M

#rank of M:
rankMatrix(M)[1]

#compute SVD
svd_result<-svd(M)
#let us print U D Vtransposed:
U<-svd_result$u
V <- svd_result$v
Vt<-t(svd_result$v)
D<-diag(svd_result$d)
U
D
V
Vt

#Let's compute the total and partial energy of D
energy <- sum(svd_result$d**2)
energy
partial_energy <- sum(svd_result$d[1:2]**2)
partial_energy 

#Let's compute the residual energy if we were to set the three smaller singular values to 0
residual_energy <- partial_energy /energy
residual_energy

#We can see that we can drop the three smaller singular values as we retain 94% of the total energy
#We can compute the rank 2 svd matrices
svd2_result <- svd(M, nu=2, nv=2)
U2<-svd2_result$u
V2 <- svd2_result$v
Vt2<-t(svd2_result$v)
D2<-diag(svd2_result$d)
U2
D2
V2
Vt2

#user1 is now our first user (row 1)
user1 <- c(1,0,1,12,8)

#let's look for his opinions on the concepts expressed in the original dataset:
#let's compute his scores with respect to the "concept space"
score_user1<-user1%*%svd_result$v[,1:2]
score_user1

#let's look for the similarity of all the customers. This is the first way to classify users based on their opinions
Scores_users<-M%*%V[,1:2]
Scores_users
#compute cos(theta), or the similarity coefficient, between user 1 and all other users
similarity_vector<-c()

library(pracma) #needed for dot product
for (i in c(1:dim(Scores_users)[1])){
  similarity_vector[i]<-dot(score_user1,Scores_users[i,])/(sqrt(sum(score_user1^2)*sum((Scores_users[i,])^2)))
}


similarity_vector

#create score users in absolute value
su <- as.data.frame(Scores_users)
abssu = abs(su)

#create a basic scatterplot
plot(abssu)

#seed to replicate geom_text_repel
set.seed = 42

#scatterplot with geom_text_repel to highlight where the various scatter points are
ggplot(abssu, mapping = aes (x = V1, y = V2))+
  geom_point()+
  geom_text_repel(
    label = rownames(su),
    nudge_x = .15,
    box.padding = 0.5,
    nudge_y = 1,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
    )


