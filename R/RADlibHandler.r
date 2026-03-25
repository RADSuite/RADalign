#' readSequences
#'
#' Read in all sequences that include the given accession numbers in their
#' identification line.
#'
#' @param infile The file path to the desired library
#' @param accessions A list of accession numbers to read in
#'
#' @return A Biostrings DNAStringSet containing all sequences that had one
#' of the given accession numbers in their identification line
#'
#' @export
#'
#' @examples
#' readSequences("/my/file/path", c("1234.1", "1234.2", "1234.3"))
readSequences <- function(infile, accessions) {
    # create summary dataframe for database file
    fasta_summary <- Biostrings::fasta.index(infile, seqtype = "DNA")

    # filter the indexes of the sequences that contain the accession numbers
    # names are stored in the "desc" column of the dataframe
    matching_indexes <- grepl(paste(accessions, collapse = "|"), fasta_summary$desc)

    # use matching indexes to read only desired sequences
    subset_index <- fasta_summary[matching_indexes, ]
    Biostrings::readDNAStringSet(subset_index)
}

#' getVRegions
#'
#' Given a DNAStringSet of sequences with V-regions included in their names,
#' retrieve all instances of selected V-regions from the DNAStringSet.
#'
#' @param sequences a DNAStringSet of sequences to search
#' @param vregions a list of v-regions to search for in the DNAStringSet
#'
#' @return a subset of the input DNAStringSet containing all sequences from
#' specificed v-regions.
#'
#' @export
#'
#' @examples
#' ## getVRegions(my_string_set, c("V1", "V2"))
getVRegions <- function(sequences, vregions) {
    # rename vregions so they match format of finalized RADlibVR
    renamed_regions <- character()
    tag <- "variable_region="
    for (region in vregions) {
        region <- substring(region, 2)
        region <- paste0(tag, region)
        renamed_regions <- c(renamed_regions, region)
    }
    renamed_regions <- sort(renamed_regions)

    sequences[grepl(paste(renamed_regions, collapse = "|"), names(sequences))]
}


# testing scratch

# radv_file <- "C:/Users/rache/OneDrive/Desktop/Capstone/RADalign/inst/extdata/RADlibVR.fa"
# wanted <- c("IW245_RS23890")
# filtered <- readSequences(radv_file, wanted)
# print(filtered)

# wanted <- c("V2", "V1")
# filtered_vregions <- getVRegions(filtered, wanted)
# print(filtered_vregions)
