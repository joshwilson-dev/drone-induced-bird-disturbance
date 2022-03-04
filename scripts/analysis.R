#### Header ####
# Literature Review Analysis
# Josh Wilson
# 25-02-2022

#### Setup ####
# Install Packages
packages <- c("tidyverse")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if (length(new_packages)) {
    package_consent <- readline(
        prompt <- (paste("Install", new_packages, " y/n?\n")))
    if (tolower(package_consent) == "y") {
        install.packages(new_packages)
        }
    else print(paste("This code cannot be run without", new_packages))
}

# Import Packages
lapply(packages, require, character.only = TRUE)

# Clear Environment
rm(list = ls())

# Import Data
data <- read_csv(choose.files())

#### General Functions ####

histogram <- function(dataset, x_axis, y_axis) {
    x_axis <- enquo(x_axis)
    y_axis <- enquo(y_axis)

    # create a plot of the frequency of taxonomic rank
    ggplot(dataset, aes(x = reorder(!!x_axis, -!!y_axis), y = !!y_axis)) +
        theme(
            axis.text.x = element_text(
                angle = 90,
                vjust = 0.35,
                hjust = 0.95),
            text = element_text(size = 40),
            legend.position = "none") +
        geom_col(fill = "black", width = 0.5) +
        xlab(quo_name(x_axis)) +
        ylab("Number of Studies") +
        scale_y_continuous(expand = c(0, 2))

    # save the plot
    ggsave(
        file = paste0("plots/", quo_name(x_axis), ".png"),
        width = nrow(dataset) * 2,
        height = 10,
        limitsize = FALSE)
}

#### Target Analysis ####

# taxonomic rank frequency

# create a function to plot the frequency of different taxonomic ranks
species_analysis <- function(dataset, taxo_rank) {
    taxo_rank <- enquo(taxo_rank)

    # manipulate dataframe to get frequency of taxonomic rank
    taxo_freq <- dataset %>%
        # keep only unique species for each study
        group_by(reference, species) %>%
        slice(1) %>%
        # count occurences by taxonomic rank
        group_by(!!taxo_rank) %>%
        mutate(count = n()) %>%
        # keep only unique taxonomic ranks
        group_by(!!taxo_rank) %>%
        slice(1) %>%
        # filter out null values
        filter(!!taxo_rank != "-", !is.na(!!taxo_rank))
    
    # create histogram of results
    histogram(taxo_freq, !!taxo_rank, count)

    return(taxo_freq)
}

order_freq <- species_analysis(data, order)
family_freq <- species_analysis(data, family)
species_freq <- species_analysis(data, species)

# average abundance per approach per species
# note: not per drone or approachtype or lifestage or study

# manipulate dataframe to get abundance per approach per species
count_approach <- data %>%
    # keep only unique species for each study
    group_by(reference, species) %>%
    slice(1) %>%
    # get average count per species per approach over all studies
    group_by(species, common_name) %>%
    summarize(count_target_avg = mean(count_target_average, na.rm=TRUE)) %>%
    # drop bad values
    filter(species != "-", !is.na(species)) %>%
    drop_na()

# create a plot of the abundance per approach per species
histogram(count_approach, common_name, count_target_avg)

#### drone analysis ####

#### approach analysis ####

#### location analysis ####

# manipulate data to get unique gps locations
gps_data <- data %>%
    mutate(latitude_centre = (latitude_min + latitude_max) / 2, .after = longitude_max) %>%
    mutate(longitude_centre = (longitude_min + longitude_max) / 2, .after = longitude_max) %>%
    group_by(reference, location) %>%
    slice(1)

# get world map data
world <- map_data("world")

# plot world map and survey points
ggplot() +
    geom_map(
        data = world, map = world,
        aes(long, lat, map_id = region),
        color = "black", fill = "lightgray", size = 0.1) +
    geom_point(
        data = gps_data,
        aes(longitude_centre, latitude_centre),
        colour = "red",
        alpha = 0.7)

# save plot
ggsave(
    file = "plots/study-sites.png",
    width = 20,
    height = 20,
    limitsize = FALSE)

#### create guidelines ####