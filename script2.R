# library(skimr)
# library(knitr)
# library(tidyverse)
# library(caret)
# library(corrr)
# library(corrplot)
# library(e1071)
# library(ggfortify)
# library(stringr)
# library(tibble)
# library(ggplot2)

pcabrca <- function(df) {
  df$diagnosis[is.na(df$diagnosis)] <- 0
  df = pcabrca(df)
  df$diagnosis <- as.factor(df$diagnosis)
  map_int(df, function(.x) sum(is.na(.x)))
  round(prop.table(table(df$diagnosis)), 2)
  df_corr <- cor(df %>% select(-id, -diagnosis, -X))
  corrplot(df_corr, order = "hclust", tl.cex = 1, addrect = 8)
  
  df2 <- df %>% select(-X, -findCorrelation(df_corr, cutoff = 0.8, verbose=TRUE))
  ncol(df2)
  
  preproc_pca_df <- prcomp(df %>% select(-id, -diagnosis, -X), scale = TRUE, center = TRUE)
  summary(preproc_pca_df)
  pca_df_var <- preproc_pca_df$sdev^2
  pve_df <- pca_df_var / sum(pca_df_var)
  cum_pve <- cumsum(pve_df)
  pve_table <- tibble(comp = seq(1:ncol(df %>% select(-id, -diagnosis, -X))), pve_df, cum_pve)
  ggplot(pve_table, aes(x = comp, y = cum_pve)) + 
    geom_point() + 
    geom_abline(intercept = 0.95, color = "red", slope = 0) + 
    labs(x = "Number of components", y = "Cumulative Variance")
  pca_df <- as_tibble(preproc_pca_df$x)
  ggplot(pca_df, aes(x = PC1, y = PC2, col = df$diagnosis)) + geom_point()
  autoplot(preproc_pca_df, data = df,  colour = 'diagnosis',
           loadings = FALSE, loadings.label = TRUE, loadings.colour = "blue")
  df_pcs <- cbind(as_tibble(df$diagnosis), as_tibble(preproc_pca_df$x))
}