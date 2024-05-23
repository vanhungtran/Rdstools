# stuff to compare algorithms -------------------------------------------------


library(ggthemes)

embed <- function(label, df) {
require(retry)
    times <- bench::mark(
    um <- retry(umapr::umap(df, min_dist = 0.99), until = ~TRUE),
    ts <- retry(Rtsne::Rtsne(df, perplexity = floor((nrow(df) - 1) / 3),
                             dims = 2)$Y, until = ~TRUE),
    ts_no_pca <- retry(Rtsne::Rtsne(df, pca = FALSE, perplexity = floor((nrow(df) - 1) / 3),
                                    dims = 2)$Y, until = ~TRUE),
    sv <- retry(svd(df)$u[,1:2], until = ~TRUE),
    ica <- retry(fastICA::fastICA(df,n.comp=2)$S, until = ~TRUE),
    mds= retry(stats::cmdscale(dist(df)) , until = ~TRUE),
    isomds=retry(MASS::isoMDS(dist(df))$points, until = ~TRUE),
    check = FALSE)








  pca <- prcomp(df)$x[,1:2]

  times$expression <- c( "UMAP", "PCA + t-SNE", "t-SNE", "SDV", "ICA", "MDS", "IsoMDS")
  combo <- function(embedding, name) {
    colnames(embedding) <- c("V1", "V2")
    embedding %>%
      as.data.frame() %>%
      dplyr::mutate(Algorithm = name, Class = label)
  }

  list(times = times,
       results = dplyr::bind_rows(
         combo(pca, "PCA"),
         dplyr::mutate(um, Algorithm = "UMAP", Class = label, V1 = UMAP1, V2 = UMAP2),
         combo(ts, "PCA + t-SNE"),
         combo(ts_no_pca, "t-SNE"),
         combo(sv, "SDV"),
         combo(ica, "ICA"),
         combo(mds, "MDS"),
         combo(isomds, "IsoMDS")

         ))
}



plot_embeddings <- function(embeddings, dataset) {
  require(tidyverse)
  require(ggthemes)
  embeddings %>%
    dplyr::mutate(status = factor(Class)) %>%
    ggplot(aes(V1, V2, color = status)) +
    geom_point() + facet_wrap(~ Algorithm, scales = "free") +
    stat_ellipse(type = "norm", linetype = 1) +
    ggtitle(dataset)
}




# RNA -----------------------------------------------------------------------





iris <- iris[!duplicated(iris), ]
df <- scale(as.matrix(iris[ , 1:4]))
pro_result <- embed(iris$Species, df)
# display results ----------------------------------------------------------
plot_embeddings(pro_result$results, "Iris") + theme_economist()



library(palmerpenguins)
data(package = 'palmerpenguins')



penguins  <- penguins [!duplicated(penguins ), ]
df  <- as.matrix(penguins [ , 3:6])
pro_result <- embed(penguins$species, df)







neighbors <- c(4, 8, 16, 32, 64, 128)
neighbors %>%
  map_df(~ umapr::umap(df, n_neighbors = .x) %>%
           dplyr::mutate(status = factor(iris$Species), Neighbor = .x)) %>%
  dplyr::mutate(Neighbor = as.integer(Neighbor)) %>%
  ggplot(aes(UMAP1, UMAP2, color = status)) +
  geom_point() +
  facet_wrap(~ Neighbor, scales = "free")




dists <- c("euclidean", "manhattan", "canberra", "cosine", "hamming", "dice")
dists %>%
  map_df(~umap(df, metric = .x) %>%
           dplyr::mutate(status = factor(iris$Species), Metric = .x)) %>%
  ggplot(aes(UMAP1, UMAP2, color = status)) +
  stat_ellipse(type = "norm", linetype = 1) +
  geom_point() +
  facet_wrap(~ Metric, scales = "free")



dists <- c(0.001, 0.01, 0.05, 0.1, 0.5, 0.99)
dists %>%
  map_df(~ umapr::umap(df, min_dist = .x) %>%
           dplyr::mutate(status = factor(iris$Species), Distance = .x)) %>%
  ggplot(aes(UMAP1, UMAP2, color = status)) +
  stat_ellipse(type = "norm", linetype = 1) +
  geom_point() + theme_calc()+ scale_colour_calc()+
  facet_wrap(~ Distance, scales = "free")







