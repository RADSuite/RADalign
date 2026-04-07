# note: remember to always comment out scratch code you're using for tests
# so the package will load correctly!

# df <- createRADq(c("Pseudomonas aeruginosa", "Brucella suis"), TRUE)
# createSummarizedIDs(TRUE)
# createRADqGroups(c("V4","V5"), TRUE)

# df <- selectVRegions(c("V1","V5"), TRUE)
# print(df)


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


# testing scratch

# radv_file <- "C:/Users/rache/OneDrive/Desktop/Capstone/RADalign/inst/extdata/RADlibVR.fa"
# wanted <- c("IW245_RS23890")
# filtered <- readSequences(radv_file, wanted)
# print(filtered)

# wanted <- c("V2", "V1")
# filtered_vregions <- getVRegions(filtered, wanted)
# print(filtered_vregions)
