library(Biostrings)
library(msa)
library(phangorn)

RADlibV <-
    "C:/Users/rache/OneDrive/Desktop/Capstone/RADalign/inst/extdata/RADlibV.fa"
all_v_regions <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9")

selectTaxa <- function(taxa) {
    print("selecting taxa")
}

#' alignVRegions
#'
#' For a group of sequences in RADlib, align the sequences in each
#'  V-region individually
#'
#' @param sequences a DNAStringSet of sequences to align
#'
#' @return a dataframe containing information about which sequences
#' align exactly
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

        # use phangorn's ML-based distance calculation to determine
        # which sequences are identical
        phy <- phyDat(as.matrix(alignment), type = "DNA")
        distance_matrix <- dist.ml(phy)

        # assign IDs to each unique group of sequences
        for (i in seq_along(region_sequences)) {
            for (j in seq_len(i)) {
                if (!is.na(distance_matrix[i][j]) && distance_matrix[i][j] == 0) {
                    # TODO:: use rownames(distance_matrix) to create table with
                    # names and IDs indicating which sequences match exactly
                    print("exact match")
                }
            }
        }
        # might need to convert alignment to DNAStringSet first
    }
}

selectVRegions <- function() {
    print("selecting v-regions")
}

# note: remember to always comment out scratch code you're using for tests
# so the package will load correctly!
alignVRegions(sequences)
