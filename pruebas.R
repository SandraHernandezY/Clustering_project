library(rworldmap) 

world <- getMap(resolution = "low")

countries <- c("Ecuador","Colombia")
world_ec <- world[world@data$ADMIN %in% countries, ]

(with_world <- ggplot() +
    geom_polygon(data = world_ec, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, colour = "black") + 
    #geom_point(data = x_new,  # Add and plot species data
    #           aes(x = Long, y = Lat, 
    #               colour = Cluster)) +
    coord_quickmap() +  # Prevents stretching when resizing
    #theme_classic() +  # Remove ugly grey background
    #xlab("Longitude") +
    #ylab("Latitude") + 
    #uides(colour=guide_legend(title="Clusters")))
    #xlim(-2,2) +  # Set x axis limits, xlim(min, max)
    #ylim(0, 175) +  # Set y axis limits
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") + 
    guides(colour=guide_legend(title="Clusters")))