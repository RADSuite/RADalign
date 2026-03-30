download_data <- function(pipeline, species_list, filter = FALSE, download_location = fs::path_home("Downloads")) {

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
      #get accession ids for all species in species_list
      accessions_list <- get_accession_ids(species_list)
      #generate reference sequence files & save folder name
      reference_folder <- download_MetaScope_reference(accessions_list, download_folder)
      folder_path <- file.path(download_folder, reference_folder$folder)
    } else if (filter == TRUE) {
      #get accession ids for all species in species_list
      accessions_list <- get_accession_ids(species_list)
      #generate reference sequence files & save folder name
      filter_folder <- download_MetaScope_reference(accessions_list, download_folder, TRUE)
      folder_path <- file.path(download_folder, filter_folder$folder)
    }
  } else {
    cat(paste0(pipeline, " integration currently unsupported"))
  }

  return (folder_path)
}

# my_species <- c("Prevotella jejuni", "Faecalibacterium langellae","Prevotella intermedia ATCC 25611",
#                   "Porphyromonas gingivalis ATCC 33277", "Segatella oris", "Veillonella hominis",
#                   "Eggerthia catenaformis OT 569", "Capnocytophaga ochracea DSM 7271",
#                   "Corynebacterium matruchotii ATCC 14266", "Leptotrichia wadei")
#
#
# download_data("MetaScope", my_species)
# download_data("MetaScope", my_species, TRUE)
