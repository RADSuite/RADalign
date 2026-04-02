#' download_RAD_data
#'
#' This function downloads files for integration with analysis pipelines, currently supports MetaScope.
#'
#' @param pipeline <char> name of pipeline, valid inputs: "MetaScope"
#' @param organisms_list <char list> of organism names to download from RADlib
#' @param download_location <char> (optional) file path to desired download location, defaults to "Downloads" folder
#'
#' @return <char> path to downloaded folder of files
#'
#' @export
#'
#' @examples
#' > download_RAD_data("MetaScope", c("Pseudomonas aeruginosa", "Brucella suis"))
#' "Users/user/Downloads/RADdownloads_05032026_204428_QVrV4idv/MetaScope_reference_dir"

download_RAD_data <- function(pipeline, organisms_list, filter = FALSE, download_location = fs::path_home("Downloads")) {

  #generate unique folder name
  rand_string <- paste0(sample(c(letters, LETTERS, 0:9), 8, replace = TRUE), collapse = "")
  folder_name <- paste0("RADdownloads", format(Sys.time(), "_%d%m%Y_%H%M%S_"), rand_string)

  #create path for folder
  download_folder <- file.path(download_location, folder_name)
  if (!dir.exists(download_folder)) {
    dir.create(download_folder, recursive = TRUE)
  }

  #download necessary files for selected pipeline
  if (pipeline == "MetaScope") {
    if (filter == FALSE) {

      #get accession ids for all organisms in organisms_list
      accessions_list <- get_accession_ids(organisms_list)
      #generate reference sequence files & save folder name
      reference_folder <- download_MetaScope_reference(accessions_list, download_folder)

      return(file.path(download_folder, reference_folder$folder))

    } else if (filter == TRUE) {

      #get accession ids for all organisms in organisms_list
      accessions_list <- get_accession_ids(organisms_list)
      #generate filter sequence files & save folder name
      filter_folder <- download_MetaScope_reference(accessions_list, download_folder, TRUE)

      return(file.path(download_folder, filter_folder$folder))
    }
  } else {
    cat(paste0(pipeline, " integration currently unsupported"))
    return("")
  }
}

#' download_MetaScope_reference
#'
#' This function downloads selected sequences from RADlib to a desired folder as fasta files and places them inside a single sub folder.
#'
#' @param accessions_list <list> of string accession ids of sequences to download from RADlib
#' @param download_folder <char> path to desired destination folder (sub folder will be created)
#' @param filter <boolean> (optional) for filter/reference sequence download, defaults to FALSE
#'
#' @return <list> with <char> $folder containing the sub folder name and <char list> $files containing a list of downloaded fasta file names
#'
#' @export
#'
#' @examples
#' > download_MetaScope_reference(c("EDX97_RS10020", "EDX97_RS11935", "MOZ64_RS11590"), "/Users/user/Downloads")
#' $folder
#' [1] "MetaScope_reference_dir"
#' $files
#' $files[[1]]
#' [1] "Absicoccus_porci_EDX97_RS10020.fasta"
#' $files[[2]]
#' [1] "Absicoccus_porci_EDX97_RS11935.fasta"
#' $files[[3]]
#' [1] "Absicoccus_intestinalis_MOZ64_RS11590.fasta"

download_MetaScope_reference <- function(accessions_list, download_folder, filter = FALSE) {

  organisms_list <- get_organisms_list(accessions_list)

  #folder for sequence fastas
  if (!filter) {
    folder_name <- "MetaScope_reference_dir"
  } else {
    folder_name <- "MetaScope_filter_dir"
  }
  folder_path = file.path(download_folder, folder_name)
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }

  #get file path to RADlib
  RADlib_path <- system.file("extdata", "RADlib16S.fa", package = "RADalign")

  #return selected sequences from RADlib
  sequences <- readSequences(RADlib_path, accessions_list)

  #download one fasta file per sequence
  file_names <- vector("list", length(sequences))
  for (i in seq_along(sequences)) {
    id <- accessions_list[i]

    #generate file name
    organisms_name <- paste(unlist(strsplit(organisms_list[i], " ")), collapse = "_")
    seq_file_name <- paste0(organisms_name, "_", id, ".fasta")
    seq_file_path <- file.path(folder_path, seq_file_name)

    #write fasta file for sequence, and save name
    Biostrings::writeXStringSet(sequences[i], seq_file_path)
    file_names[[i]] <- seq_file_name
  }

  return (list(folder = folder_name, files = file_names))

}
