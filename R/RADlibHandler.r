library(Biostrings)

radv_file <- "C:/Users/rache/OneDrive/Desktop/Capstone/RADalign/inst/extdata/RADlibV.fa"

# vector of desired accession numbers
wanted <- c("GCF_000006765.1.1")

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
    fastaindex <- fasta.index(infile, seqtype = "DNA")

    # filter the indexes of the sequences that contain the accession numbers
    # names are stored in the "desc" column of the dataframe
    wanted_index <- grepl(paste(accessions), fastaindex$desc)

    # use matching indexes to read only desired sequences
    subset_index <- fastaindex[wanted_index, ]
    filtered <- readDNAStringSet(subset_index)
}

print(readSequences(radv_file, wanted))
