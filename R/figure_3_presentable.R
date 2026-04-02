
library(data.table)

get_fig_3_counts <- function() {
  # needs to access actual files eventually
  # but for now...
  species_names <- c("sp_1", "sp_2", "sp_3")
  BLAST_counts <- c(10, 15, 20)
  RAD_counts <- c(11, 14, 21)
  SILVA_counts <- c(8, 12, 15)
  NCBI16s_counts <- c(9, 13, 12)

  counts <- data.table::data.table(
    species = species_names,
    BLAST = BLAST_counts,
    RAD = RAD_counts,
    SILVA = SILVA_counts,
    NCBI = NCBI16s_counts
  )

  return (counts)
}

calculate_proximity <- function(counts, column) {
  return (1 - abs(counts$BLAST - column)/counts$BLAST)
}

get_proximity_table <- function(counts) {

  RAD_proximities <- calculate_proximity(counts, counts$RAD)
  SILVA_proximities <- calculate_proximity(counts, counts$SILVA)
  NCBI_proximities <- calculate_proximity(counts, counts$NCBI)

  proximities <- data.table::data.table(
    species = counts$species,
    RAD = RAD_proximities,
    SILVA = SILVA_proximities,
    NCBI = NCBI_proximities
  )

  return(proximities)
}

generate_figure_3 <- function() {

  # get proximities data
  counts <- get_fig_3_counts()
  proximities <- get_proximity_table(counts)

  print(proximities)

  # draw figure

  return("Done")
}


