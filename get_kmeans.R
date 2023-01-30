get_kmeans<-function(x,k){
  stopifnot(is.numeric(x),  
            length(x)>=k)
  
  m=length(x)
  
  #random assign
  n=ceiling(m/k)
  cluster=sample(rep(1:k, each=n))
  cluster<-cluster[1:m]
  df<-tibble(cluster,x)

  
  #get centroids
  get_centroids <- function(df){
    centroids<-df %>%
      group_by(cluster) %>%
      summarise(centroid = mean(x))
  }

  centroids <- get_centroids(df)
  min.col <- function(m, ...) max.col(-m, ...)
  
  #reassign
  reassign_points <- function(df, centroids){
    d<-matrix(nrow=m,ncol=k)
    for (i in 1:k){
      d[,i]=(df$x-as.numeric(centroids[i,2]))^2
    }
       
    df|>
      ungroup()|>
      mutate(
        cluster= min.col(d))
  }
  df <- reassign_points(df, centroids)
  
  #repeat
  old_cluster <- NULL
  while(!identical(df$cluster, old_cluster)){
    old_cluster <- df$cluster
    centroids <- get_centroids(df)
    df <- reassign_points(df, centroids)
  }
  grps<-with(list(mu = ave(df$x, df$cluster)), match(mu, sort(unique(mu))))
  return(grps)
}