# # library(stringi)
# # library(data.table)
#
# accessions_loaded <- FALSE
#
# load_accessions <- function() {
#   #get path to RADlib
#   file_path <- system.file("extdata", "RADlib16S.fa", package = "RADalign")
#   lines <- readLines(file_path)
#
#   #get
#   index <- Biostrings::fasta.index(file_path, seqtype = "DNA")
#   # print(index[desc])
#   headers <- index[["desc"]] # remember to change this code for download functions now that header has changed
#
#   # get data from headers
#   labels <- stri_split_fixed(headers, "=", simplify = TRUE)
#   gene_id <- stri_split_fixed(labels[,1], " ", simplify = TRUE)[,1]
#   taxa_id <- stri_split_fixed(labels[,2], " ", simplify = TRUE)[,1]
#   organism_name <- stri_split_fixed(labels[,3], "\"", simplify = TRUE)[,2]
#
#   #create empty accessions
#   n <- length(headers)
#   accessions <- data.table(id = character(n), taxid = character(n), organism = character(n))
#
#   #fill accessions with memory pointers
#   accessions[, id := gene_id]
#   accessions[, taxid := taxa_id]
#   accessions[, organism := organism_name]
#
#   return (accessions)
#
# }
#
# get_organisms <- function(ids) {
#
#   # if (accessions_loaded == FALSE) {
#   #   accessions <- load_accessions()
#   #   accessions_loaded <- TRUE
#   # }
#
#   accessions <- load_accessions()
#   organisms <- accessions[id %in% ids, organism]
#
#   return (organisms)
# }
#
# get_ids <- function(organisms) {
#
#   accessions <- load_accessions()
#   organisms <- accessions[organism %in% organisms, id]
#
#   return (organisms)
# }
#
# # get_ids <- function(organisms) {
# # }
#
# id_list <- c("AB6B37_RS01935", "THECO_RS17145", "FW767_RS11870")
# org_list <- c("Fretibacter rubidus", "Thermobacillus composti KWC4", "Heminiphilus faecis")
#
# print(get_organisms(id_list))
# print(get_ids(org_list))
#
# # AB6B37_RS01935 taxid=570162 organism="Fretibacter rubidus"
# # THECO_RS17145 taxid=717605 organism="Thermobacillus composti KWC4"
# # FW767_RS11870 taxid=2601703 organism="Heminiphilus faecis"
#
