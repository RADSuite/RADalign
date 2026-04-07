#' get_accessions_df
#'
#' Generates a metadata table from the header data of RADlib.
#'
#' @return <data.frame> table with <char> columns: id, taxid, organism, and genus.
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#' > get_accessions_df()
#'                id   taxid               organism               genus
#'            <char>  <char>                 <char>              <char>
#' 1: AB6B37_RS01935  570162    Fretibacter rubidus         Fretibacter
#' 2:  KK102_RS15670 2838947 Enterobacter quasimori        Enterobacter
#' 3:  KK102_RS22065 2838947 Enterobacter quasimori        Enterobacter

get_accessions_df <- function() {

  # get path to RADlib
  file_path <- system.file("extdata", "RADlib16S.fa", package = "RADalign")
  lines <- readLines(file_path)

  # get all headers from RADlib
  index <- Biostrings::fasta.index(file_path, seqtype = "DNA")
  headers <- index[["desc"]]

  # get data from headers
  gene_id <- stringi::stri_match_all_regex(headers, "^\\w+") |>
    unlist()
  taxa_id <- stringi::stri_match_all_regex(headers, "taxid=(\\d+)") |>
    lapply(function(x) x[[2]]) |>
    unlist()
  organism_name <- stringi::stri_match_all_regex(headers, "organism=\"(.*)\"") |>
    lapply(function(x) x[[2]]) |>
    unlist()
  # selects the first word of an organism name, ignores leading non-word characters
  genus_name <- stringi::stri_match_first_regex(organism_name, "(?!\\W)(\\w+)") |>
    (function(x) x[,2])()

  # create empty accessions
  n <- length(headers)
  accessions <- data.table::data.table(id = character(n),
                                       taxid = character(n),
                                       organism = character(n),
                                       genus = character(n))

  # fill accessions with memory pointers
  accessions[, id := gene_id]
  accessions[, taxid := taxa_id]
  accessions[, organism := organism_name]
  accessions[, genus := genus_name]

  # filter out unnamed RADlib entries
  accessions <- accessions[!is.na(organism) & organism != ""]

  return (accessions)
}

#' get_organism_names
#'
#' Given a list of accession ids, outputs corresponding organism names (includes duplicates).
#'
#' @param acc_ids <char list> list of valid accession ids
#'
#' @return <char list> list of organism names (includes duplicates)
#'
#' @export
#'
#' @examples
#' > get_organism_names(c("EDX97_RS04345", "EDX97_RS05225", "EDX97_RS06840",
#' "EDX97_RS09045", "EDX97_RS10020", "EDX97_RS11935", "MOZ64_RS11590"))
#' [1] "Absicoccus porci"        "Absicoccus porci"        "Absicoccus porci"
#' [4] "Absicoccus porci"       "Absicoccus porci"        "Absicoccus porci"
#' [7] "Absicoccus intestinalis"

get_organism_names <- function(acc_ids) {

  accessions <- get_accessions_df()

  # filter accessions by ids
  organisms <- accessions[.(acc_ids), on = .(id), .(organism, id)]

  return (organisms[!is.na(organism)]$organism)
}

#' get_taxa_ids
#'
#' Given a list of accession ids, outputs corresponding taxa ids (includes duplicates).
#'
#' @param acc_ids <char list> list of valid accession ids
#'
#' @return <char list> list of taxa ids (includes duplicates)
#'
#' @export
#'
#' @examples
#' > get_taxa_ids(c("EDX97_RS04345", "EDX97_RS05225", "EDX97_RS06840",
#' "EDX97_RS09045", "EDX97_RS10020", "EDX97_RS11935", "MOZ64_RS11590"))
#' [1] "2486576" "2486576" "2486576" "2486576" "2486576" "2486576" "2926319"

get_taxa_ids <- function(acc_ids) {

  accessions <- get_accessions_df()

  # filter accessions by ids
  taxa_ids <- accessions[.(acc_ids), on = .(id), .(taxid, id)]

  return (taxa_ids[!is.na(taxid)]$taxid)
}

#' get_accession_ids
#'
#' Given a list of organism names, outputs all corresponding accession ids.
#'
#' @param organisms <char list> list of valid organism names
#'
#' @return <char list> list of accession ids
#'
#' @export
#'
#' @examples
#' > get_accession_ids(c("Absicoccus porci", "Absicoccus intestinalis"))
#' [1] "EDX97_RS04345" "EDX97_RS05225" "EDX97_RS06840" "EDX97_RS09045"
#' [5] "EDX97_RS10020" "EDX97_RS11935" "MOZ64_RS11590"

get_accession_ids <- function(organisms) {

  accessions <- get_accessions_df()

  # filter accessions by organisms
  ids <- accessions[.(organisms), on = .(organism), .(organism, id)]

  return (ids[!is.na(id)]$id)
}

#' get_all_organisms
#'
#' Generates a sorted list of all unique organisms in RADlib and adds genus labels ("Genus - All Species (#)").
#'
#' @return <char list> list of organism names (and genus labels)
#'
#' @export
#'
#' @examples
#' > get_all_organisms()
#' [1] "Abditibacterium - All Species (1)" "Abditibacterium utsteinense"       "Abiotrophia - All Species (1)"
#' [4] "Abiotrophia defectiva"             "Absicoccus - All Species (2)"      "Absicoccus intestinalis"

get_all_organisms <- function() {

  accessions <- get_accessions_df()

  # get unique organisms and genera in accessions
  organism_list <- unique(accessions$organism)
  genus_list <- unique(accessions$genus)

  # calculate number of species per genus
  genus_counts <- accessions |>
    dplyr::distinct(genus, organism) |>
    dplyr::count(genus, name = "n")

  # build genus label with format "Genus - All Species (#)"
  genus_labels <- paste0(genus_counts$genus, " - All Species (", genus_counts$n, ")")

  # combine organism and genus label lists and sort
  full_list <- stringi::stri_sort(append(genus_labels, organism_list))

  return(full_list)
}

#' get_species_from_genus
#'
#' Finds all organisms in RADlib of a given genus.
#'
#' @param genus_label <char> genus label with format "Genus - All Species (#)"
#'
#' @return <char list> list of organism names
#'
#' @export
#'
#' @examples
#' > get_species_from_genus("Absicoccus - All Species (2)")
#' [1] "Absicoccus porci"        "Absicoccus intestinalis"

get_species_from_genus <- function(genus_label) {

  accessions <- get_accessions_df()

  # get genus name from input string
  genus_name <- stringi::stri_split_fixed(genus_label, " - ", simplify = TRUE)[,1]

  # return all organisms with matching genus
  species <- unique(accessions[genus %in% genus_name]$organism)

  return (unique(species))
}
