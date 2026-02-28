#' download_RAD
#'
#' This function allows users to download files for integration with analysis pipelines, currently supports MetaScope and Kraken.
#'
#' @param pipeline name of pipeline. Valid inputs: ("MetaScope", "Kraken")
#' @param species_list list of species names to download from RADlib.
#' @param download_location optional parameter, file path for where downloaded files should go, defaults to working directory
#'
#' @return This function downloads a zipped folder to the user's working directory, and outputs the location of the downloaded folder.
#'
#' @export
#'
#' @examples
#' download_RADsynch("MetaScope", c("Pseudomonas aeruginosa", "Brucella suis"))
#' > "Downloaded Successfully to: ~/user/Downloads"

download_RAD_data <- function(pipeline, species_list, download_location = getwd()) {

  accessions_list <- get_accession_ids(species_list)

  download_folder <- file.path(download_location, "RADdownloads")
  if (!dir.exists(download_folder)) {
    dir.create(download_folder, recursive = TRUE)
  }

  file_paths <- c()

  if (pipeline == "MetaScope") {
    #accessions_SQLite_tmp_path <- get_MetaScope_accessions(accessions_list)
    # reference_fasta_tmp_path <- download_MetaScope_reference(accessions_list, download_location)

    reference_file_name <- download_MetaScope_reference(accessions_list, download_location)

    file_paths <- c(reference_file_name)

  } else if (pipeline == "Kraken") {
    # download_Kraken_files(accessions_list)
    print("You chose Kraken")
  }

  return (paste0("Files downloaded successfully to: ", download_folder))
}

download_MetaScope_reference <- function(accessions_list, download_folder) {

  #file details
  file_name = "Metascope_reference_db.fasta"
  file_path = file.path(download_folder, file_name)

  #get file path to RADlib
  RADlib_path <- system.file("extdata", "RADlib.fa", package = "RADalign")

  #use RADlib readSequences function to return selected sequences from RADlib
  sequences <- readSequences(RADlib_path, accessions_list)

  #use Biostrings to write fasta data for selected sequences to file
  writeXStringSet(sequences, file_path)

  # if(file.info(file_path)$size > 0) {
  #   return (paste0("Download successful: ", file_name))
  # } else {
  #   return ("Error writing file to ")
  # }

  return (paste0("Download successful: ", file_name))

}

# acc_list <- c("GCF_000006765.1.1", "GCF_000006765.1.2", "GCF_000006765.1.3", "GCF_000006765.1.4", "GCF_000007505.1.1", "GCF_000007505.1.2", "GCF_000007505.1.3")
#
# print(get_MetaScope_reference(acc_list, "/Users/myeshagilliland/BYU/BIO465/RADalign"))
#
# print(download_RAD_data("MetaScope", c("Pseudomonas aeruginosa", "Brucella suis")))

