get_accession_ids <- function(species_list) {
  acc_df <- get_accessions_df()
  accession_ids <- c()

  for (species in species_list) {
    id <- acc_df$accession_id[acc_df$species_name == species]
    accession_ids <- c(accession_ids, id)
  }

  return(accession_ids)
}

print(get_accession_ids(c("Pseudomonas aeruginosa", "Brucella suis")))

