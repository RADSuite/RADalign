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

  accessions_list <- get_accession_ids(species_list)
  file_paths <- c()

  if (pipeline == "MetaScope") {
    #accessions_SQLite_tmp_path <- get_MetaScope_accessions(accessions_list)
    reference_fasta_tmp_path <- get_MetaScope_reference(accessions_list)
    #file_paths <- c(accessions_SQLite, reference_fasta)
    file_paths <- c(reference_fasta_tmp_path)
  } else if (pipeline == "Kraken") {
    file_paths <- get_Kraken_files(accessions_list)
  }

  tmp_folder = zip_files(file_paths)
  folder_location <- download_folder(tmp_folder)

  return ("Downloaded Successfully to: {folder_location}")
}


get_MetaScope_reference <- function(accessions_list) {
  #get file path to RADlib
  RADlib_path <- system.file("extdata", "RADlib.fa", package = "RADalign")

  #use RADlib readSequences function to return select sequences from RADlib
  sequences <- readSequences(RADlib_path, accessions_list)

  #create temp fasta file for downloadable reference database
  temp_fasta <- tempfile(pattern = "reference_", fileext = ".fasta")
  #dir.create(temp_fasta)

  #write filtered sequence data to temp file
  writeXStringSet(sequences, temp_fasta)

  print(readLines("temp_fasta", n = 5))

  #output file path to temp file
  return (temp_fasta)
}

acc_list <- c("GCF_000006765.1.1", "GCF_000006765.1.2", "GCF_000006765.1.3", "GCF_000006765.1.4", "GCF_000007505.1.1", "GCF_000007505.1.2", "GCF_000007505.1.3")

print(get_MetaScope_reference(acc_list))

#print(download_RADsynch("MetaScope", c("Pseudomonas aeruginosa", "Brucella suis")))

