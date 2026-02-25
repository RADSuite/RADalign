library(Biostrings)
library(msa)

RADlibV = "C:/Users/rache/OneDrive/Desktop/Capstone/RADalign/inst/extdata/RADlibV.fa"
all_v_regions <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9")

selectTaxa <- function(taxa) {
    print("selecting taxa")
}

#' alignVRegions
#'
#' For a group of sequences in RADlib, align the sequences in each V-region individually
#'
#' @param sequences a DNAStringSet of sequences to align
#'
#' @return a dataframe containing information about which sequences align exactly
#'
#' @export
#'
#' @examples
#' ## TODO::still in progress
#' alignVRegions(sequences)
alignVRegions <- function(sequences) {
    sequences <- readSequences(RADlibV, c("GCF_000006765.1"))
    for (region in all_v_regions) {
        region_sequences <- getVRegions(sequences, region)
        alignment <- msa(region_sequences, method = "ClustalOmega")
        ## TODO:: alignment is being successfully performed, but the output
        # seems to be just a matrix with the sequences aligned to each other.
        # Need to find a way to determine if distance between any two sequences
        # == 0 to create matrix of unique sequences. Sean used Phangorn in heat
        # map; should probably read through Phangorn in greater detail to
        # potentially use it here.

        # possible code for msa to phangorn:
        # phy <- phyDat(as.matrix(alignment), type = "DNA")
        # might need to convert alignment to DNAStringSet first
    }
}

selectVRegions <- function() {
    print("selecting v-regions")
}

# note: remember to always comment out scratch code you're using for tests
# so the package will load correctly!
# alignVRegions(sequences)
