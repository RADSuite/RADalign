library(fs)
library(DBI)
library(RSQLite)
library(Biostrings)

#' download_RAD_data
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
#' download_RAD_data("MetaScope", c("Pseudomonas aeruginosa", "Brucella suis"))
#' > Files downloaded successfully to ~/Users/user/Downloads/RADdownloads_05032026_204428_QVrV4idv :
#' Metascope_reference_db.fasta
#' Metascope_accessions_db.sqlite

download_RAD_data <- function(pipeline, species_list, download_location = fs::path_home("Downloads")) {

  #get accession ids for all species in species_list
  accessions_list <- get_accession_ids(species_list)

  #just temporary while acc ids are different
  acc_list <- c("NZ_CTYB01000002.1",
               "NZ_CTYB01000003.1",
               "NZ_CTYB01000004.1",
               "NZ_LAWV01000006.1",
               "NZ_LAWV01000007.1",
               "NC_009641.1",
               "NZ_JBBIAE010000011.1",
               "NZ_JBBIAE010000012.1")

  #generate unique folder name
  rand_string <- paste0(sample(c(letters, LETTERS, 0:9), 8, replace = TRUE), collapse = "")
  folder_name <- paste0("RADdownloads", format(Sys.time(), "_%d%m%Y_%H%M%S_"), rand_string)

  #create path for folder
  download_folder <- file.path(download_location, folder_name)
  if (!dir.exists(download_folder)) {
    dir.create(download_folder, recursive = TRUE)
  }

  #store names of all created files for output
  file_paths <- c()

  #generate and download correct files based on pipeline
  if (pipeline == "MetaScope") {
    #generate MetaScope files & save names
    reference_file_name <- download_MetaScope_reference(accessions_list, download_folder)
    accession_file_name <- download_MetaScope_accessions(acc_list, download_folder)
    file_paths <- c(reference_file_name, accession_file_name)
  } else if (pipeline == "Kraken") {
    # download_Kraken_files(accessions_list)
    print("You chose Kraken")
  }

  cat("Files downloaded successfully to", download_folder, ":\n")
  cat(file_paths, sep = "\n")

  return ()
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
#' download_MetaScope_reference(c("NZ_CTYB01000002.1","NZ_CTYB01000003.1"), ~/Users/user/Downloads/RAD_downloads_folder)
#' > "Metascope_reference_db.fasta"

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

  return (file_name)

}

#' download_MetaScope_accessions
#'
#' This function filters RADaccessions by accession ids and downloads a reference database to the provided folder
#'
#' @param accessions_list list of string accession ids of sequences to download from RADlib.
#' @param download_folder character path to desired destination folder
#'
#' @return This function downloads a filtered portion of RADaccessions (sqlite) and outputs the file name.
#'
#' @export
#'
#' @examples
#' download_MetaScope_accessions(c("NZ_CTYB01000002.1","NZ_CTYB01000003.1"), ~/Users/user/Downloads/RAD_downloads_folder)
#' > "Metascope_accessions_db.sqlite"

download_MetaScope_accessions <- function(accessions_list, download_folder) {

  #location of accessions db
  db_path <- system.file("extdata", "example_accessions.sqlite", package = "RADalign")

  #open db connection
  con <- dbConnect(
    RSQLite::SQLite(),
    dbname = db_path
  )

  #prep file location
  file_name <- "Metascope_accessions_db.sqlite"
  file_path <- file.path(download_folder, file_name)

  #create temp database
  create_db_query <- paste0("ATTACH DATABASE '", file_path, "' AS dest_db;")
  dbExecute(con, create_db_query)

  #create accessionTaxa table filtered by accessions_list
  placeholders_acc <- paste(rep("?", length(accessions_list)), collapse = ",")
  create_table1_query <- paste0("CREATE TABLE dest_db.accessionTaxa AS SELECT *
                                FROM accessionTaxa
                                WHERE accession IN (", placeholders_acc, ");")
  dbExecute(con, create_table1_query, params = accessions_list)

  #get taxa_ids
  con2 <- dbConnect(
    RSQLite::SQLite(),
    dbname = file_path
  )
  taxa_ids <- dbGetQuery(con2, "SELECT taxa FROM accessionTaxa;")$taxa
  dbDisconnect(con2)

  #create names table filtered by taxa_ids
  placeholders_tax <- paste(rep("?", length(taxa_ids)), collapse = ",")
  create_table2_query <- paste0("CREATE TABLE dest_db.names AS SELECT *
                                FROM names
                                WHERE id IN (", placeholders_tax, ");")
  dbExecute(con, create_table2_query, params = taxa_ids)

  #create nodes table filtered by taxa_ids
  create_table3_query <- paste0("CREATE TABLE dest_db.nodes AS SELECT *
                                FROM nodes
                                WHERE id IN (", placeholders_tax, ");")
  dbExecute(con, create_table3_query, params = taxa_ids)

  #disconnect
  dbExecute(con, "DETACH DATABASE dest_db;")
  dbDisconnect(con)

  return (file_name)

}

# download_RAD_data("MetaScope", c("Pseudomonas aeruginosa", "Brucella suis"))

# acc_list <- c("GCF_000006765.1.1", "GCF_000006765.1.2", "GCF_000006765.1.3", "GCF_000006765.1.4", "GCF_000007505.1.1", "GCF_000007505.1.2", "GCF_000007505.1.3")
# print(download_MetaScope_reference(acc_list, "/Users/myeshagilliland/BYU/BIO465/RADalign"))
# accessions_list <- c("NZ_CTYB01000002.1",
#                      "NZ_CTYB01000003.1",
#                      "NZ_CTYB01000004.1",
#                      "NZ_LAWV01000006.1",
#                      "NZ_LAWV01000007.1",
#                      "NC_009641.1",
#                      "NZ_JBBIAE010000011.1",
#                      "NZ_JBBIAE010000012.1")
# accessions_list <- c("NZ_CTYB01000002.1")
# download_folder <- "/Users/myeshagilliland/BYU/BIO465/RADalign"
# download_MetaScope_accessions(accessions_list, "/Users/myeshagilliland/BYU/BIO465/RADalign")

