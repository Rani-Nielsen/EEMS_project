# Dependencies
library('ggrepel')
library('ggfortify')
library("factoextra")


# Using reemsplot2
buffalo <- make_eems_plots('final_previuos/buffalo/', longlat = FALSE, add_demes = T)
leopard <- make_eems_plots('final_previuos/leopard/', longlat = FALSE, add_demes = T)
lion <- make_eems_plots('final_previuos/lion/', longlat = FALSE, add_demes = T)
elephant <- make_eems_plots('final_previuos/savannahElephant/', longlat = FALSE, add_demes = T)
warthog <- make_eems_plots('final_previuos/warthog/', longlat = FALSE, add_demes = T)
waterbuck <- make_eems_plots('final_previuos/waterbuck/', longlat = FALSE, add_demes = T)

# Retrieve relevant migration data and combine to matrix
pca_mat <- cbind(buffalo$mrates01$data$z,
                 leopard$mrates01$data$z,
                 lion$mrates01$data$z,
                 elephant$mrates01$data$z,
                 warthog$mrates01$data$z,
                 waterbuck$mrates01$data$z)

# Column names
colnames(pca_mat) <- c('buffalo','leopard','lion','elephant','warthog','waterbuck')

# PCA 
pca_data <- prcomp(pca_mat, scale. = T)
pca_data_t <- prcomp(t(pca_mat), scale. = F)

# PCA information mainly for plotting labs
summary_ <- summary(pca_data)
summary_t <- summary(pca_data_t)

# Get PCA loadings for plotting
pca_l <- data.frame(Species = rownames(pca_data$rotation), pca_data$rotation)


# PCA transposed
as_tibble(pca_data_t$x, rownames = 'Species') %>% ggplot(aes(x=PC1, y=PC2, col = Species))+
    geom_point() +
    my_colors +
    labs(x = paste('PC1', as.character(summary_t$importance[2,1]*100), '%'), 
         y = paste('PC2', as.character(summary_t$importance[2,2]*100), '%')) +
    theme_bw() +
    theme(legend.key = element_rect(fill = 'white'))


# PCA loadings plot
as_tibble(pca_data$x) %>% ggplot(aes(x=PC1, y=PC2))+
    geom_segment(data = pca_l,
                 aes(x = 0, y = 0, xend = PC1, yend = PC2, col=Species),
                 arrow = arrow(length = unit(1/2, "picas")), col = 'black') +
    geom_label_repel(data=pca_l, aes(x=PC1, y=PC2, label = Species))+
    labs(x = paste('PC1', as.character(summary_$importance[2,1]*100), '%'), 
         y = paste('PC2', as.character(summary_$importance[2,2]*100), '%')) +
    theme_bw()

# ggsave('project/plots/pca_scaled.png')

# Get PCA contribution for projecting onto map
PC1.Norm <- get_pca(pca_data_t)$contrib[,1]
PC2.Norm <- get_pca(pca_data_t)$contrib[,2]

PC1 <- get_pca(pca_data_t)$contrib[,1]
PC2 <- get_pca(pca_data_t)$contrib[,2]

# Scree plot
fviz_eig(pca_data, geom = 'line', main = '', xlab = 'Principal Components')
fviz_eig(pca_data_t, geom = 'line', main = '', xlab = 'Principal Components')

