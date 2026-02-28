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
#' @return a list containing unique IDs for each group of exactly
#' aligned sequences in each v-region
#'
#' @export
#'
#' @examples
#' ## TODO::still in progress
#' alignVRegions(sequences)
alignVRegions <- function(sequences) {
    sequences <- readSequences(RADlibV, c("GCF_000006765.1"))

    IDs <- list()
    for (region in all_v_regions) {
        # get all sequences for region and perform msa
        region_sequences <- getVRegions(sequences, region)
        alignment <- msa(region_sequences, method = "ClustalOmega")

        # separate out groups of identical sequences
        alignment <- as(alignment, "DNAStringSet")
        groups <- split(seq_along(alignment), as.character(alignment))
        region_IDs <- list()
        for (i in seq_along(groups)) {
            ID <- paste0(region, i)
            region_IDs[ID] <- lapply(groups[i], function(i) names(alignment)[i])
        }
        IDs <- c(IDs, region_IDs)
    }
    return(IDs)
}

createCSV <- function(IDs) {
    species_vec <- character()
    region_vec <- character()
    copy_num_vec <- character()
    seq_id_vec <- character()
    for (i in seq_along(IDs)) {
        group <- IDs[i]
        id <- names(group)
        region <- substr(id, start = 1, stop = 2)

        seq_list <- IDs[[i]]
        accessions_df <- get_accessions_df()
        for (j in seq_along(seq_list)) {
            copy_n <- sub("^[^.]*\\.[^.]*\\.([^_]*).*", "\\1", seq_list[j])
            accession_num <- sub("^(([^_]*_){1}[^_]*)_.*", "\\1", seq_list[j])
            species <- accessions_df$species_name[accessions_df$accession_id == accession_num]
        }

        species_vec <- c(species_vec, species)
        region_vec <- c(region_vec, region)
        copy_num_vec <- c(copy_num_vec, copy_n)
        seq_id_vec <- c(seq_id_vec, id)
    }

    full_summary <- data.frame(
        species = species_vec, variable_region = region_vec,
        copy_num = copy_num_vec, seq_id = seq_id_vec
    )

    write.csv(full_summary, "C:/Users/rache/OneDrive/Desktop/Capstone/RADalign/inst/extdata/RADq.csv", )
}

getSpeciesName <- function(seq_id, accessions_df) {
    print("hurray!")
}

selectVRegions <- function() {
    print("selecting v-regions")
}

# note: remember to always comment out scratch code you're using for tests
# so the package will load correctly!
# IDs <- alignVRegions(sequences)
# createCSV(IDs)


# This is still useful code, but a full distance calculation is more than
# we need for now. I'm leaving this in here in case my implementation proves
# inadequate when tested on actual data.
# # use phangorn's ML-based distance calculation to determine
# # which sequences are identical
# phy <- phyDat(as.matrix(alignment), type = "DNA")
# distance_matrix <- dist.ml(phy)

# # assign IDs to each unique group of sequences
# for (i in seq_along(region_sequences)) {
#     for (j in seq_len(i)) {
#         if (!is.na(distance_matrix[i][j]) && distance_matrix[i][j] == 0) {
#             # TODO:: use rownames(distance_matrix) to create table with
#             # names and IDs indicating which sequences match exactly
#             print("exact match")
#         }
#     }
# }
