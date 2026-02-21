#' download_RADsynch
#'
#' This function allows users to download files for integration with analysis pipelines, currently supports MetaScope and Kraken.
#'
#' @param pipeline name of pipeline. Valid inputs: ("MetaScope", "Kraken")
#' @param species_list list of species names to download from RADlib.
#'
#' @return This function downloads a zipped folder to the user's working directory, and outputs the location of the downloaded folder.
#'
#' @export
#'
#' @examples
#' download_RADsynch("MetaScope", c("Pseudomonas aeruginosa", "Brucella suis"))
#' > "Downloaded Successfully to: ~/user/Downloads"


download_RADsynch <- function(pipeline, species_list) {

  accessions_list <- get_accessions(species_list)
  file_paths <- c()

  if (pipeline == "MetaScope") {
    accessions_SQLite_tmp_path <- get_MetaScope_accessions(accessions_list)
    reference_fasta_tmp_path <- get_MetaScope_reference(accessions_list)
    file_paths <- c(accessions_SQLite, reference_fasta)
  } else if (pipeline == "Kraken") {
    file_paths <- get_Kraken_files(accessions_list)
  }

  tmp_folder = zip_files(file_paths)
  folder_location <- download_folder(tmp_folder)

  return ("Downloaded Successfully to: {folder_location}")
}


get_MetaScope_reference()
