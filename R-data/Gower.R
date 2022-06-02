#' Load useful packages
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
setwd("D:/Hubert/Dokumenty/Politechnika Warszawska/IIAD/Semestr IV/Wstep do Uczenia Maszynowego/Projekt/2/GIT/e-shop_clothing")
A <- function(w)
{
  #181
  gower_dist <- daisy(small_df, metric = "gower", weights = w)
  clusters <- pam(gower_dist, 50, variant = "fast")
  ASW_value <- clusters$silinfo$avg.width
  -ASW_value
}

df <- read_csv("./sessions_e_shop.csv")
catergories <- c(1)
df[catergories] <- lapply(df[catergories],factor)
# str(df)  # now look at the classes


size = ceiling(nrow(df)*0.5)
small_df <- df[sample(nrow(df), size), ]


#gower_dist <- daisy(small_df, metric = "gower")
#gower_mat <- as.matrix(gower_dist)
gower_dist <- daisy(small_df, metric = "gower", weights = 1)
#clusters <- pam(gower_dist, 3)
#small_df['cluster'] <- clusters$clustering

#cluster

#' Print most similar clients
#small_df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]
#min(gower_mat[gower_mat != min(gower_mat)])

sil_width <- c()
min_clusters = 10
iterations = 10
step = 10
t = 1
for(i in sequence(iterations, min_clusters, step)){
  start_time <- Sys.time()
  #ones <- rep(1, i)
  #c(1.05521, 1.92192, 1.00234, 1 ,1, 1, 1, 1)
  sil_width <- c(sil_width,A(1, i))
  end_time <- Sys.time()
  print(c(i, end_time - start_time, sil_width[t]))
  t = t + 1
}
graph <- data.frame(sequence(iterations, min_clusters, step), sil_width)
colnames(graph) <- c('k', 'ASW')
graph %>% 
ggplot(aes(x = k, y=ASW)) +  
  geom_point(size = 3) + 
  geom_line()

start_time <- Sys.time()
w <-rep(1,each=8)
controls <- list(6, 100)
names(controls) <- c("trace", "maxit")
optimized_weight <- optim(par = w, fn = A, lower = 0.01, upper = 5, method = "L-BFGS-B", control=controls)
end_time <- Sys.time()
print(end_time - start_time)

