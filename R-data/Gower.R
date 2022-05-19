#' Load useful packages
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)

A <- function(w)
{
  #181
  gower_dist <- daisy(small_df, metric = "gower", weights = w)
  clusters <- pam(gower_dist, 181, variant = "faster")
  ASW_value <- clusters$silinfo$avg.width
  -ASW_value
}

df <- read_csv("../e_shop.csv")
catergories <- c(1,2,3,4,5,6,7,8,9)
df[catergories] <- lapply(df[catergories],factor)
# str(df)  # now look at the classes


size = ceiling(nrow(df)*0.1)
small_df <- df[sample(nrow(df), size), ]


gower_dist <- daisy(small_df, metric = "gower")
gower_mat <- as.matrix(gower_dist)
#' Print most similar clients
#small_df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]
#min(gower_mat[gower_mat != min(gower_mat)])

sil_width <- c()
min_clusters = 150
iterations = 30
step = 1
t = 1
for(i in sequence(iterations, min_clusters, step)){
  start_time <- Sys.time()
  #ones <- rep(1, i)
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
w <-rep(1,each=12)
controls <- list(6, 100)
names(controls) <- c("trace", "maxit")
optimized_weight <- optim(par = w, fn = A, lower = 1, upper = 5, method = "L-BFGS-B", control=controls)
end_time <- Sys.time()
print(start_time - end_time)