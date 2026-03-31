#' download_RAD_data
#'
#' This function allows users to download files for integration with analysis pipelines, currently supports MetaScope and Kraken.
#'
#' @param pipeline name of pipeline. Valid inputs: ("MetaScope")
#' @param organisms_list list of organism names to download from RADlib.
#' @param download_location optional parameter, file path for where downloaded files should go, defaults to working directory
#'
#' @return This function downloads a zipped folder to the user's working directory, and outputs the location of the downloaded folder.
#'
#' @export
#'
#' @examples
#' download_RAD_data("MetaScope", c("Pseudomonas aeruginosa", "Brucella suis"))
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

  # #store names of all created files for output
  # file_paths <- c()

  folder_path <- ""

  if (pipeline == "MetaScope") {
    if (filter == FALSE) {
      #get accession ids for all organisms in organisms_list
      accessions_list <- get_accession_ids(organisms_list)
      #generate reference sequence files & save folder name
      reference_folder <- download_MetaScope_reference(accessions_list, download_folder)
      folder_path <- file.path(download_folder, reference_folder$folder)
    } else if (filter == TRUE) {
      #get accession ids for all organisms in organisms_list
      accessions_list <- get_accession_ids(organisms_list)
      #generate reference sequence files & save folder name
      filter_folder <- download_MetaScope_reference(accessions_list, download_folder, TRUE)
      folder_path <- file.path(download_folder, filter_folder$folder)
    }
  } else {
    cat(paste0(pipeline, " integration currently unsupported"))
  }

  return (folder_path)
}

#' download_MetaScope_reference
#'
#' This function filters RADlib by accession ids and downloads a reference database to the provided folder
#'
#' @param accessions_list list of string accession ids of sequences to download from RADlib.
#' @param download_folder character path to desired destination folder
#'
#' @return This function downloads a filtered portion of RADlib (fasta) and outputs the file name.
#'
#' @export
#'
#' @examples
#' download_MetaScope_reference(c("NZ_CTYB01000002.1","NZ_CTYB01000003.1"), /Users/user/Downloads/RAD_downloads_folder)
#' > "Metascope_reference_db.fasta"

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

  #use RADlib readSequences function to return selected sequences from RADlib
  sequences <- readSequences(RADlib_path, accessions_list)

  file_names <- vector("list", length(sequences))
  for (i in seq_along(sequences)) {
    id <- accessions_list[i]
    organisms_name <- paste(unlist(strsplit(organisms_list[i], " ")), collapse = "_")
    seq_file_name <- paste0(organisms_name, "_", id, ".fasta")
    seq_file_path <- file.path(folder_path, seq_file_name)

    #use Biostrings to write fasta file for sequence, and save name
    Biostrings::writeXStringSet(sequences[i], seq_file_path)
    file_names[[i]] <- seq_file_name
  }

  return (list(folder = folder_name, files = file_names))

}
