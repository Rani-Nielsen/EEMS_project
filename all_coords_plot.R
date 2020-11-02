# Dependencies
library("reemsplots2")
library('tidyverse')
library('rworldmap')

# Coordinate paths
coord_files <- list.files("final_previuos/",
                    recursive=TRUE,
                    full.names=TRUE,
                    pattern="\\.coord$")

# Actual sampling coordinates of all six species.
# Could and should be turned into a actual function!
buffalo_coords <- as_tibble(read.table(coord_files[1], header = F, col.names = c('x','y')))
leopard_coords <- as_tibble(read.table(coord_files[2], header = F, col.names = c('x','y')))
lion_coords <- as_tibble(read.table(coord_files[3], header = F, col.names = c('x','y')))
elephant_coords <- as_tibble(read.table(coord_files[4], header = F, col.names = c('x','y')))
warthog_coords <- as_tibble(read.table(coord_files[5], header = F, col.names = c('x','y')))
waterbuck_coords <- as_tibble(read.table(coord_files[6], header = F, col.names = c('x','y')))

# Adding name to be able to color by species
buffalo_coords %>% add_column(Species = 'buffalo') -> buffalo_coords
leopard_coords %>% add_column(Species = 'leopard') -> leopard_coords
lion_coords %>% add_column(Species = 'lion') -> lion_coords
elephant_coords %>% add_column(Species = 'elephant') -> elephant_coords
warthog_coords %>% add_column(Species = 'warthog') -> warthog_coords
waterbuck_coords %>% add_column(Species = 'waterbuck') -> waterbuck_coords

# Combining the species
all_coords <- rbind(buffalo_coords,
                    leopard_coords,
                    lion_coords,
                    elephant_coords,
                    warthog_coords,
                    waterbuck_coords)

# Defining some plotting stuff to not clog the plotting code
my_colors <- scale_color_manual(values = c("#6e3b09", "#666361",'#000000', "#ff9900", '#fa3807', '#3b9404'))

my_theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_blank(),axis.title=element_blank(),
                  axis.text=element_blank(),
                  axis.ticks=element_blank(),
                  legend.key = element_rect(fill = 'white'))


# Plotting
all_coords %>% ggplot(aes(x=y, y=x, color = Species)) +
    geom_point() +
    my_colors + 
    my_theme +
    geom_path(data = map, aes(x = long, y = lat, group = group),
              color = "#888888", size = 0.5) + xlim(-12, 50) + ylim(-36, 17) +
    coord_quickmap()

# ggsave('project/plots/all_coords.png')







