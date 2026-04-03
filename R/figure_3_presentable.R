library(data.table)
library(tidyverse)
library(ggtext)

# load in data
get_fig_counts <- function() {
  ncbi_counts  <- read_csv(system.file("extdata", "ncbi_counts_with_names.csv",  package = "RADalign"))
  rad_counts   <- read_csv(system.file("extdata", "rad_counts_with_names.csv",   package = "RADalign"))
  silva_counts <- read_csv(system.file("extdata", "silva_counts_with_names.csv", package = "RADalign"))


  ncbi_species  <- ncbi_counts  %>% filter(Freq > 0) %>% pull(organism_short) %>% unique()
  rad_species   <- rad_counts   %>% filter(Freq > 0) %>% pull(organism_short) %>% unique()
  silva_species <- silva_counts %>% filter(Freq > 0) %>% pull(organism_short) %>% unique()

  common_species <- Reduce(intersect, list(ncbi_species, rad_species, silva_species))
  common_species <- common_species[common_species != "Bacteroides caecimuris"]

  combined <- bind_rows(
    ncbi_counts  %>% mutate(database = "NCBI - MetaScope"),
    rad_counts   %>% mutate(database = "RADlib - MetaScope"),
    silva_counts %>% mutate(database = "SILVA - MetaScope")
  ) %>%
    filter(organism_short %in% common_species, Freq > 0) %>%
    group_by(organism_short, database) %>%
    summarise(Freq = sum(Freq), .groups = "drop")

  return(combined)
}

# temporary fake blast data
add_blast_fake_data <- function(combined) {
  blast_fake_data <- combined %>%
    distinct(organism_short) %>%
    mutate(database = "NCBI - BLAST", Freq = 0.1)

  return(bind_rows(combined, blast_fake_data))
}

# italics
add_labels <- function(combined) {
  combined %>%
    mutate(label = paste0("<i>", word(organism_short, 1), " ", word(organism_short, 2), "</i>"))
}

generate_figure <- function() {
  combined <- get_fig_counts() %>%
    add_blast_fake_data() %>%
    add_labels()

  ggplot(combined, aes(x = Freq, y = label, fill = database)) +
    geom_col(position = position_dodge(0.9), alpha = 0.9,
             color = "black", linewidth = 0.3) +
    scale_fill_manual(
      values = c(
        "NCBI - BLAST"       = "#E8871A",
        "NCBI - MetaScope"   = "#888780",
        "SILVA - MetaScope"  = "#378ADD",
        "RADlib - MetaScope" = "#1D9E75"
      ),
      breaks = c(
        "NCBI - BLAST",
        "NCBI - MetaScope",
        "SILVA - MetaScope",
        "RADlib - MetaScope"
      )
    ) +
    labs(
      x    = "Read count",
      y    = NULL,
      fill = "Database - Algorithm"
    ) +
    theme_bw(base_size = 15) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(color = "grey90"),
      axis.text.y        = element_markdown(size = 15),
      legend.position    = "right",
      legend.key.size    = unit(0.8, "cm")
    )
}

generate_figure()
