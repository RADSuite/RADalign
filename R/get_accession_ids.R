#' get_accession_ids
#'
#' Given a list of species, outputs accession ids
#'
#' @param species_list list of valid species names
#'
#' @return list of accession ids (with copy numbers)
#'
#' @export
#'
#' @examples
#' get_accession_ids(c("Pseudomonas aeruginosa", "Brucella suis"))
#' [1] "GCF_000006765.1.1" "GCF_000006765.1.2" "GCF_000006765.1.3" "GCF_000006765.1.4"
#' [5] "GCF_000007505.1.1" "GCF_000007505.1.2" "GCF_000007505.1.3"

get_accession_ids <- function(species_list) {
  #import accessions data frame, columns: species_name, accession_id
  acc_df <- get_accessions_df()
  accession_ids <- c()

  #for each species, find accessions id and add to accessions_ids
  for (species in species_list) {
    id <- acc_df$accession_id[acc_df$species_name == species]
    accession_ids <- c(accession_ids, id)
  }

  return(accession_ids)
}

#print(get_accession_ids(c("Pseudomonas aeruginosa", "Brucella suis")))

