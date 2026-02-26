#' library(Biostrings)
#'
#'
#'
#' #' readSequences
#' #'
#' #' Read in all sequences that include the given accession numbers in their
#' #' identification line.
#' #'
#' #' @param infile The file path to the desired library
#' #' @param accessions A list of accession numbers to read in
#' #'
#' #' @return A Biostrings DNAStringSet containing all sequences that had one
#' #' of the given accession numbers in their identification line
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' readSequences("/my/file/path", c("1234.1", "1234.2", "1234.3"))
#' readSequences <- function(infile, accessions) {
#'     # create summary dataframe for database file
#'     fasta_summary <- fasta.index(infile, seqtype = "DNA")
#'
#'     # filter the indexes of the sequences that contain the accession numbers
#'     # names are stored in the "desc" column of the dataframe
#'     matching_indexes <- grepl(paste(accessions, collapse = "|"), fasta_summary$desc)
#'
#'     # use matching indexes to read only desired sequences
#'     subset_index <- fasta_summary[matching_indexes, ]
#'     readDNAStringSet(subset_index)
#' }
#'
#' #' getVRegions
#' #'
#' #' Given a DNAStringSet of sequences with V-regions included in their names,
#' #' retrieve all instances of selected V-regions from the DNAStringSet.
#' #'
#' #' @param sequences a DNAStringSet of sequences to search
#' #' @param vregions a list of v-regions to search for in the DNAStringSet
#' #'
#' #' @return a subset of the input DNAStringSet containing all sequences from
#' #' specificed v-regions.
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' ## getVRegions(my_string_set, c("V1", "V2"))
#' getVRegions <- function(sequences, vregions) {
#'     sequences[grepl(paste(vregions, collapse = "|"), names(sequences))]
#' }
#'
#'
#' # testing scratch
#'
#' #radv_file <- "C:/Users/rache/OneDrive/Desktop/Capstone/RADalign/inst/extdata/RADlibV.fa"
#' #wanted <- c("GCF_000006765.1.1")
#' #filtered <- readSequences(radv_file, wanted)
#' #wanted <- c("V1", "V2")
#' #filtered_vregions <- getVRegions(filtered, wanted)
#' #print(filtered_vregions)
