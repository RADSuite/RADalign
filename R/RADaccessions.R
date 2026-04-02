#' get_accessions_df
#'
#' This function generates a metadata table from the header data of RADlib.
#'
#' @return A data.frame object, with <char> type columns id, taxid, organism, and genus.
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
  # labels <- stringi::stri_split_fixed(headers, "=", simplify = TRUE)
  # gene_id <- stringi::stri_split_fixed(labels[,1], " ", simplify = TRUE)[,1]
  gene_id <- stringi::stri_match_all_regex(headers, "^\\w+") |>
    unlist()
  # taxa_id <- stringi::stri_split_fixed(labels[,2], " ", simplify = TRUE)[,1]
  taxa_id <- stringi::stri_match_all_regex(headers, "taxid=(\\d+)") |>
    lapply(function(x) x[[2]]) |>
    unlist()
  # organism_name <- stringi::stri_split_fixed(labels[,3], "\"", simplify = TRUE)[,2]
  organism_name <- stringi::stri_match_all_regex(headers, "organism=\"(.*)\"") |>
    lapply(function(x) x[[2]]) |>
    unlist()
  # genus_name <- stringi::stri_split_fixed(organism_name, " ", simplify = TRUE)[,1]
  genus_name <- stringi::stri_match_first_regex(organism_name, "(?!\\W)(\\w+)") |>
    (function(x) x[,2])() # Selects the first word that's part of an organisms name ignoring leading non-word characters

  #create empty accessions
  n <- length(headers)
  accessions <- data.table::data.table(id = character(n),
                                       taxid = character(n),
                                       organism = character(n),
                                       header = character(n))

  #fill accessions with memory pointers
  accessions[, id := gene_id]
  accessions[, taxid := taxa_id]
  accessions[, organism := organism_name]
  accessions[, genus := genus_name]
  accessions[, header := headers]

  #filter out unnamed RADlib entries
  accessions <- accessions[!is.na(organism) & organism != ""]

  return (accessions)

}

#' get_species_list
#'
#' Given a list of accession ids, outputs organisms list (includes duplicates)
#'
#' @param accession_ids list of valid accession ids
#'
#' @return list of organisms (includes duplicates)
#'
#' @export
#'
#' @examples
#' get_species_list(c("EDX97_RS04345", "EDX97_RS05225", "EDX97_RS06840",
#' "EDX97_RS09045", "EDX97_RS10020", "EDX97_RS11935", "MOZ64_RS11590"))
#' [1] "Absicoccus porci"        "Absicoccus porci"        "Absicoccus porci"
#' [4] "Absicoccus porci"       "Absicoccus porci"        "Absicoccus porci"
#' [7] "Absicoccus intestinalis"

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
#' Given a list of organisms, outputs accession ids
#'
#' @param organisms list of valid organism names
#'
#' @return list of accession ids (with copy numbers)
#'
#' @export
#'
#' @examples
#' get_accession_ids(c("Absicoccus porci", "Absicoccus intestinalis"))
#' "GCF_000007505.1.2", "GCF_000007505.1.3"))
#' [1] "EDX97_RS04345" "EDX97_RS05225" "EDX97_RS06840" "EDX97_RS09045"
#' [5] "EDX97_RS10020" "EDX97_RS11935" "MOZ64_RS11590"

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
#' [1] "Abditibacterium - All Species (1)" "Abditibacterium utsteinense"       "Abiotrophia - All Species (1)"
#' [4] "Abiotrophia defectiva"             "Absicoccus - All Species (2)"      "Absicoccus intestinalis"

get_all_organisms <- function() {
  accessions <- get_accessions_df()
  organism_list <- unique(accessions$organism)
  genus_list <- unique(accessions$genus)

  genus_counts <- accessions |>
    dplyr::distinct(genus, organism) |>
    dplyr::count(genus, name = "n")

  # genus_labels <- paste0(genus_list, " - All Species")
  genus_labels <- paste0(genus_counts$genus, " - All Species (", genus_counts$n, ")")

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
#' > get_species_from_genus("Absicoccus - All Species (2)")
#' [1] "Absicoccus porci"        "Absicoccus intestinalis"

get_species_from_genus <- function(genus_label) {
  accessions <- get_accessions_df()

  #get genus name from input string
  genus_name <- stringi::stri_split_fixed(genus_label, " - ", simplify = TRUE)[,1]

  #return all organisms with matching genus
  species <- unique(accessions[genus %in% genus_name]$organism)

  return (unique(species))
}
