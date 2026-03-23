
#'
#' get_accession_ids <- function(species_list) {
#'   #import accessions data frame, columns: species_name, accession_id
#'   acc_df <- get_accessions_df()
#'   accession_ids <- c()
#'
#'   #for each species, find accessions id and add to accessions_ids
#'   for (species in species_list) {
#'     id <- acc_df$accession_id[acc_df$species_name == species]
#'     accession_ids <- c(accession_ids, id)
#'   }
#'
#'   return(accession_ids)
#' }
#'

#'
#' get_species_list <- function(accession_ids) {
#'   #import accessions data frame, columns: species_name, accession_id
#'   acc_df <- get_accessions_df()
#'   species_list <- c()
#'
#'   #for each species, find accessions id and add to accessions_ids
#'   for (id in accession_ids) {
#'     species <- acc_df$species_name[acc_df$accession_id == id]
#'     species_list <- c(species_list, species)
#'   }
#'
#'   return(species_list)
#' }
#'
#' # print(get_accession_ids(c("Pseudomonas aeruginosa", "Brucella suis")))
#'
#' # print(get_species_list(c("GCF_000006765.1.1", "GCF_000006765.1.2", "GCF_000006765.1.3", "GCF_000006765.1.4", "GCF_000007505.1.1", "GCF_000007505.1.2", "GCF_000007505.1.3")))
#'
