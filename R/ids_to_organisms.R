library(stringi)
library(data.table)

# accessions_loaded <- FALSE

#' get_accessions_df
#'
#' This function generates a callable df with two columns: species_name and accession_id
#'
#' @return This function returns a callable df with two columns: species_name and accession_id
#'
#' @import data.table
#' @import stringi
#' @export
#'
#' @examples
#' get_accessions_df()
#' > species_name accession_id

get_accessions_df <- function() {
  #get path to RADlib
  file_path <- system.file("extdata", "RADlib16S.fa", package = "RADalign")
  lines <- readLines(file_path)

  #get
  index <- Biostrings::fasta.index(file_path, seqtype = "DNA")
  # print(index[desc])
  headers <- index[["desc"]] # remember to change this code for download functions now that header has changed

  # get data from headers
  labels <- stringi::stri_split_fixed(headers, "=", simplify = TRUE)
  gene_id <- stringi::stri_split_fixed(labels[,1], " ", simplify = TRUE)[,1]
  taxa_id <- stringi::stri_split_fixed(labels[,2], " ", simplify = TRUE)[,1]
  organism_name <- stringi::stri_split_fixed(labels[,3], "\"", simplify = TRUE)[,2]

  #create empty accessions
  n <- length(headers)
  accessions <- data.table(id = character(n), taxid = character(n), organism = character(n))

  #fill accessions with memory pointers
  accessions[, id := gene_id]
  accessions[, taxid := taxa_id]
  accessions[, organism := organism_name]

  return (accessions)

}

#' get_species_list
#'
#' Given a list of accession ids, outputs species list (with duplicates)
#'
#' @param accession_ids list of valid accession ids
#'
#' @return list of species (with duplicates)
#'
#' @export
#'
#' @examples
#' get_accession_ids(c("Pseudomonas aeruginosa", "Brucella suis"))
#' [1] "GCF_000006765.1.1" "GCF_000006765.1.2" "GCF_000006765.1.3" "GCF_000006765.1.4"
#' [5] "GCF_000007505.1.1" "GCF_000007505.1.2" "GCF_000007505.1.3"

get_species_list <- function(ids) {

  # if (accessions_loaded == FALSE) {
  #   accessions <- load_accessions()
  #   accessions_loaded <- TRUE
  # }

  accessions <- get_accessions_df()
  organisms <- accessions[id %in% ids, organism]

  return (organisms)
}

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
#' get_accession_ids(c("GCF_000006765.1.1", "GCF_000006765.1.2",
#' "GCF_000006765.1.3", "GCF_000006765.1.4", "GCF_000007505.1.1",
#' "GCF_000007505.1.2", "GCF_000007505.1.3"))
#' [1] "Pseudomonas aeruginosa" "Pseudomonas aeruginosa" "Pseudomonas aeruginosa" "Pseudomonas aeruginosa"
#' [5] "Brucella suis"          "Brucella suis"          "Brucella suis"

get_accession_ids <- function(organisms) {

  accessions <- get_accessions_df()
  organisms <- accessions[organism %in% organisms, id]

  return (organisms)
}

# id_list <- c("AB6B37_RS01935", "THECO_RS17145", "FW767_RS11870")
# org_list <- c("Fretibacter rubidus", "Thermobacillus composti KWC4", "Heminiphilus faecis")
#
# print(get_species_list(id_list))
# print(get_accession_ids(org_list))

# AB6B37_RS01935 taxid=570162 organism="Fretibacter rubidus"
# THECO_RS17145 taxid=717605 organism="Thermobacillus composti KWC4"
# FW767_RS11870 taxid=2601703 organism="Heminiphilus faecis"


#' get_all_organisms
#'
#' outputs all unique organisms in RADlib
#'
#' @return list of organism names
#'
#' @export
#'
#' @examples
#' > head(get_all_organisms())
#' [1] "Fretibacter rubidus"          "Enterobacter quasimori"       "Thermobacillus composti KWC4"
#' [4] "Allofustis seminis DSM 15817" "Heminiphilus faecis"          "Algirhabdus cladophorae"

get_all_organisms <- function() {
  accessions <- get_accessions_df()
  return(unique(accessions$organism))
}

# print(head(get_all_organisms()))

