#' get_accessions_df
#'
#' This function generates a callable df with two columns: species_name and accession_id
#'
#' @return This function returns a callable df with two columns: species_name and accession_id
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#' get_accessions_df()
#'                id   taxid               organism               genus
#'            <char>  <char>                 <char>              <char>
#' 1: AB6B37_RS01935  570162    Fretibacter rubidus         Fretibacter
#' 2:  KK102_RS15670 2838947 Enterobacter quasimori        Enterobacter
#' 3:  KK102_RS22065 2838947 Enterobacter quasimori        Enterobacter

get_accessions_df <- function() {
  #get path to RADlib
  file_path <- system.file("extdata", "RADlib16S.fa", package = "RADalign")
  lines <- readLines(file_path)

  #get all headers from RADlib
  index <- Biostrings::fasta.index(file_path, seqtype = "DNA")
  headers <- index[["desc"]] # remember to change this code for download functions now that header has changed

  # get data from headers
  labels <- stringi::stri_split_fixed(headers, "=", simplify = TRUE)
  gene_id <- stringi::stri_split_fixed(labels[,1], " ", simplify = TRUE)[,1]
  taxa_id <- stringi::stri_split_fixed(labels[,2], " ", simplify = TRUE)[,1]
  organism_name <- stringi::stri_split_fixed(labels[,3], "\"", simplify = TRUE)[,2]
  genus_name <- stringi::stri_split_fixed(organism_name, " ", simplify = TRUE)[,1]

  #create empty accessions
  n <- length(headers)
  accessions <- data.table::data.table(id = character(n), taxid = character(n), organism = character(n), header = character(n))

  #fill accessions with memory pointers
  accessions[, id := gene_id]
  accessions[, taxid := taxa_id]
  accessions[, organism := organism_name]
  accessions[, genus := genus_name]
  accessions[, header := headers]

  #filter out empty (unnamed / node not leaf sequences) and bracketed (under review) organism names
  # setorder(accessions, organism)
  accessions <- accessions[!is.na(organism) & organism != "" & stringi::stri_detect_regex(organism, "^[[:alnum:] ]+$")]

  return (accessions)

}

# get_accessions_df()
# head(get_all_organisms(), 50)
# tail(get_all_organisms(), 50)

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
  # organisms <- accessions[id %in% ids, organism]
  # organisms <- accessions[.(ids), on = .(id), organism]

  # return (organisms)

  organisms <- accessions[.(ids), on = .(id), .(organism, id)]
  # print(organisms[is.na(organism)]$id)

  return (organisms[!is.na(organism)]$organism)
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
  # ids <- accessions[organism %in% organisms, id]
  # print(ids)
  # ids <- accessions[.(organisms), on = .(organism), id]
  # return (ids)
  ids <- accessions[.(organisms), on = .(organism), .(organism, id)]
  print(ids[is.na(id)]$organism)

  return (ids[!is.na(id)]$id)
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
#' > get_all_organisms()
#' [1] "Abditibacterium - All Species"            "Abditibacterium utsteinense"
#' [3] "Abiotrophia - All Species"                "Abiotrophia defectiva"
#' [5] "Absicoccus - All Species"                 "Absicoccus intestinalis"

get_all_organisms <- function() {
  accessions <- get_accessions_df()
  organism_list <- unique(accessions$organism)
  genus_list <- unique(accessions$genus)
  genus_labels <- paste0(genus_list, " - All Species")

  full_list <- stringi::stri_sort(append(genus_labels, organism_list))

  return(full_list)
}

#' get_species_from_genus
#'
#' outputs all organisms in RADlib of a given genus
#'
#' @return list of organism names
#'
#' @export
#'
#' @examples
#' > get_species_from_genus("Segatella - All Species")
#' [1] "Segatella hominis"             "Segatella oris"                "Segatella copri DSM 18205"     "Segatella baroniae DSM 16972 "
#' [5] "Segatella cerevisiae"          "Segatella maculosa OT 289"     "Segatella bryantii"            "Segatella asaccharophila"
#' [9] "Segatella intestinalis"        "Segatella oulorum F0390"       "Segatella salivae F0493"       "Segatella sinensis"

get_species_from_genus <- function(genus_label) {
  accessions <- get_accessions_df()

  #get genus name from input string
  genus_name <- stringi::stri_split_fixed(genus_label, " - ", simplify = TRUE)[,1]

  #return all organisms with matching genus
  species <- unique(accessions[genus %in% genus_name]$organism)

  return (unique(species))
}

# get_species_from_genus("Segatella - All Species")

# get_genus_to_species <- function() {
#   accessions <- get_accessions_df()
#   genus_list <- unique(accessions$genus)
#
#   # accessions[genus_name %in% genus, organism]
#
#   genus_organism_df <- data.table(genus_name = genus_list)
#   genus_organism_df[, organism_names := lapply(genus_name, accessions[genus_name %in% genus, organism])]
#
#   return(genus_organism_df)
# }

# print(head(get_all_organisms()))

