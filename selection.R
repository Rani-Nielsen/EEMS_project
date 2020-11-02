# Dependencies
library("matrixStats")

# Data wrangling, can vary depending on objective.
# In this case only Leopard is excluded
cbind(pca_mat, xy_coords) %>% as_tibble() %>% 
    select(-leopard) %>%
    mutate(Median = rowMedians(as.matrix(across(buffalo:waterbuck))),
           Mean = rowMeans(across(buffalo:waterbuck)),
           Variance = rowVars(as.matrix(across(buffalo:waterbuck)))) -> Species

# Filter out common negative rows
Species %>%
    filter_at(vars(buffalo:waterbuck),
              all_vars(.<0)) -> neg

# Filter out common positive rows
Species %>% 
    filter_at(vars(buffalo:waterbuck),
              all_vars(.>0)) -> pos

# Combining negative and positive rows
combi <- rbind(neg, pos)


# Plotting, can also vary, again depending on what is needed
# In this case the combined negative and positive values are colored by mean
combi %>% 
    ggplot(aes(x=x, y=y)) + geom_tile(aes(fill=Mean)) + 
    scale_fill_gradient2(low = 'red', mid = 'yellow', high = 'black',
                         limits=c(-1.2, 1.2)) +
    my_theme +
    geom_path(data = map, aes(x = long, y = lat, group = group),
              color = "#888888", size = 0.5) +
    xlim(-12, 50) + ylim(-36, 17) +
    coord_quickmap()

# ggsave('project/plots/.png')

