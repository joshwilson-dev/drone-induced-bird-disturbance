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
library(tidyverse)

# Clear Environment
rm(list = ls())

# Import Data
data <- read_csv(choose.files())

#### Species analysis ####

# order
order_freq <- data %>%
    # take out duplicates due to drone type
    group_by(reference, species) %>%
    slice(1) %>%
    # count occurences by order
    group_by(order) %>%
    mutate(count = n()) %>%
    # get unique order
    group_by(order) %>%
    slice(1) %>%
    # filter out null values
    filter(order != "-", !is.na(order))

ggplot(order_freq, aes(x = reorder(order, -count), y = count)) +
    theme(
        axis.text.x = element_text(
            angle = 90,
            vjust = 0.35,
            hjust = 0.95),
        text = element_text(size = 40),
        legend.position = "none") +
    geom_col(fill = "black", width = 0.5) +
    xlab("Order") +
    ylab("Number of Studies") +
    scale_y_continuous(expand = c(0, 2))
ggsave(
    file = "plots/studies-order-test.png",
    width = 10,
    height = 10,
    limitsize = FALSE)

# family
family_freq <- data %>%
    # take out duplicates due to drone type
    group_by(reference, species) %>%
    slice(1) %>%
    # count occurences by order
    group_by(family) %>%
    mutate(count = n()) %>%
    # get unique order
    group_by(family) %>%
    slice(1) %>%
    # filter out null values
    filter(family != "-", !is.na(family))

ggplot(family_freq, aes(x = reorder(family, -count), y = count)) +
    theme(
        axis.text.x = element_text(
            angle = 90,
            vjust = 0.35,
            hjust = 0.95),
        text = element_text(size = 40),
        legend.position = "none") +
    geom_col(fill = "black", width = 0.5) +
    xlab("Family") +
    ylab("Number of Studies") +
    scale_y_continuous(expand = c(0, 2))
ggsave(
    file = "plots/studies-family-test.png",
    width = 25,
    height = 10,
    limitsize = FALSE)

# genus
genus_freq <- data %>%
    # take out duplicates due to drone type
    group_by(reference, species) %>%
    slice(1) %>%
    # count occurences by order
    group_by(genus) %>%
    mutate(count = n()) %>%
    # get unique order
    group_by(genus) %>%
    slice(1) %>%
    # filter out null values
    filter(genus != "-", !is.na(genus))

ggplot(genus_freq, aes(x = reorder(genus, -count), y = count)) +
    theme(
        axis.text.x = element_text(
            angle = 90,
            vjust = 0.35,
            hjust = 0.95),
        text = element_text(size = 40),
        legend.position = "none") +
    geom_col(fill = "black", width = 0.5) +
    xlab("Genus") +
    ylab("Number of Studies") +
    scale_y_continuous(expand = c(0, 2))
ggsave(
    file = "plots/studies-genus-test.png",
    width = 50,
    height = 10,
    limitsize = FALSE)

# species
species_freq <- data %>%
    # take out duplicates due to drone type
    group_by(reference, species) %>%
    slice(1) %>%
    # count occurences by species
    group_by(species) %>%
    mutate(count = n()) %>%
    # get unique species
    group_by(species) %>%
    slice(1) %>%
    # filter out null values
    filter(species != "-", !is.na(species))

ggplot(species_freq, aes(x = reorder(common_name, -count), y = count)) +
    theme(
        axis.text.x = element_text(
            angle = 90,
            vjust = 0.35,
            hjust = 0.95),
        text = element_text(size = 40),
        legend.position = "none") +
    geom_col(fill = "black", width = 0.5) +
    xlab("Species") +
    ylab("Number of Studies") +
    scale_y_continuous(expand = c(0, 2))
ggsave(
    file = "plots/studies-species-test.png",
    width = 100,
    height = 10,
    limitsize = FALSE)

