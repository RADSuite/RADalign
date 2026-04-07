# nolint start: line_length_linter
# create the user data directory if it doesn't already exist
data_dir <- tools::R_user_dir("RADalign", which = "data")
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

#' createRADq
#'
#' Given a list of species names, pull sequences for each species
#' from RADlibV, align them, and return either a csv or csv and
#' dataframe containing a summary of which variable regions aligned
#' exactly, designated by a unique ID for each group.
#'
#' @param sequences a vector containing species names
#' @param return_dataframe a boolean indicating whether a dataframe
#' containing the summary data should be returned in addition to the
#' csv created by default.
#'
#' @return a dataframe containing the summary data when return_dataframe = TRUE
#'
#' @export
#'
#' @examples
#' createRADq(c("Pseudomonas aeruginosa"), TRUE)
#'                        species variable_region    copy_id seq_id
#' 1  Pseudomonas aeruginosa PAO1              V1   PA0668.1    V11
#' 2  Pseudomonas aeruginosa PAO1              V1   PA4280.5    V11
#' ...
#' 62 Pseudomonas aeruginosa PAO1              V9   PA4690.5    V92
#' 63 Pseudomonas aeruginosa PAO1              V9   PA5369.5    V92
createRADq <- function(species_list, return_dataframe = FALSE) {
    sequences <- getSequences(species_list)
    IDs <- alignVRegions(sequences)
    createSummary(IDs, return_dataframe)
}

#' selectVRegions
#'
#' After createRADq has been run, filters the csv file from
#' createRADq to include only user-specificed variable regions.
#'
#' @param vregions a vector of variable regions to include in the
#' filtered file
#' @param return_df a boolean indicating whether a dataframe
#' containing the data should be returned in addition to the
#' csv created by default.
#'
#' @return a dataframe containing the summary data when return_dataframe = TRUE
#'
#' @export
#'
#' @examples
#' selectVRegions(c("V1","V5"))
#'     X                     species variable_region    copy_id seq_id
#' 1   1 Pseudomonas aeruginosa PAO1              V1   PA0668.1    V11
#' 2   2 Pseudomonas aeruginosa PAO1              V1   PA4280.5    V11
#' 3   3 Pseudomonas aeruginosa PAO1              V1   PA4690.5    V11
#' 4   4 Pseudomonas aeruginosa PAO1              V1   PA5369.5    V11
#' 5   5          Brucella suis 1330              V1 BR_RS07585    V12
#' 6   6          Brucella suis 1330              V1 BR_RS08575    V12
#' 7   7          Brucella suis 1330              V1 BR_RS15325    V12
#' 29 29          Brucella suis 1330              V5 BR_RS07585    V51
#' 30 30          Brucella suis 1330              V5 BR_RS08575    V51
#' 31 31          Brucella suis 1330              V5 BR_RS15325    V51
#' 32 32 Pseudomonas aeruginosa PAO1              V5   PA4690.5    V52
#' 33 33 Pseudomonas aeruginosa PAO1              V5   PA5369.5    V52
#' 34 34 Pseudomonas aeruginosa PAO1              V5   PA0668.1    V53
#' 35 35 Pseudomonas aeruginosa PAO1              V5   PA4280.5    V53
selectVRegions <- function(vregions, return_df = FALSE) {
    infile <- file.path(data_dir, "RADq.csv")
    if (!file.exists(infile)) {
        print("RADq.csv not yet created")
    }
    full_summary <- read.csv(infile)
    filtered <- full_summary[full_summary$variable_region %in% vregions, ]
    outfile <- file.path(data_dir, "RADq_filtered.csv")
    write.csv(filtered, outfile)
    if (return_df) {
        return(filtered)
    }
}

#' createSummarizedIDs
#'
#' After createRADq has been run, combines all unique IDs for each v-region in
#' each species into a single ID.
#'
#' @param return_df a boolean indicating whether a dataframe
#' containing the data should be returned in addition to the
#' csv created by default.
#'
#' @return a dataframe containing the summary data when return_dataframe = TRUE
#'
#' @export
#'
#' @examples
#' createSummarizedIDs(TRUE)
#'                  species  V1  V2  V3  V4     V5  V6  V7  V8  V9
#' 1 Pseudomonas aeruginosa V11 V21 V31 V41 V51V52 V61 V71 V81 V91
createSummarizedIDs <- function(return_df = FALSE) {
    infile <- file.path(data_dir, "RADq.csv")
    if (!file.exists(infile)) {
        print("RADq.csv not yet created")
    }
    data <- read.csv(infile)

    data <- tibble::as_tibble(data)
    vregion_data <- tidyr::pivot_wider(data, names_from = variable_region, values_from = seq_id) |>
    dplyr::group_by(species) |>
    dplyr::summarize(across(starts_with("V"), ~ {
        unique_ids <- unique(na.omit(.x))
        sorted <- sort(unique_ids)
        stringr::str_flatten(sorted, collapse = "")
    }))

    filepath <- file.path(data_dir, "RADq_summarized_IDs.csv")
    write.csv(vregion_data, filepath)


    if (return_df) return(as.data.frame(vregion_data))
}

#' createRADqGroups
#'
#' After createSummarizedIDs has been run, combines all summarized IDs
#' and sorts taxa into groups that share all the same IDs for the given
#' variable regions.
#'
#' @param vregions a vector containing all variable regions to be used
#' to split the groups
#' @param return_df a boolean indicating whether a dataframe
#' containing the data should be returned in addition to the
#' csv created by default.
#'
#' @return a dataframe containing the summary data when return_dataframe = TRUE
#'
#' @export
#'
#' @examples
#' createRADqGroups(c("V4","V5"), TRUE)
#'                     taxa    groups
#' 1          Brucella suis    V41V51
#' 2 Pseudomonas aeruginosa V42V52V53
createRADqGroups <- function(vregions, return_df = FALSE) {
    infile <- file.path(data_dir, "RADq_summarized_IDs.csv")
    if (!file.exists(infile)) {
        print("RADq_summarized_IDs.csv not yet created")
    }
    data <- read.csv(infile)

    ids <- tibble::as_tibble(data) |>
    dplyr::select(c(all_of(vregions))) |>
    tidyr::unite("final_id", everything(), sep = "") |>
    dplyr::pull(final_id)

    groups <- split(seq_along(ids), ids)
    taxa <- dplyr::pull(data, species)

    group_ids <- character()
    for (i in seq_along(taxa)) {
        for (group_id in names(groups)) {
            group <- groups[[group_id]]
            if (i %in% group) {
                group_ids[i] <- group_id
                break
            }
        }
    }

    taxa_groups <- data.frame(
        taxa = taxa,
        groups = group_ids
    )

    filepath <- file.path(data_dir, "RADq_groups.csv")
    write.csv(taxa_groups, filepath)

    if (return_df) return(taxa_groups)
}

#' getSequences
#'
#' Given a list of species, retrieves all sequences associated with
#' those taxa from RADlibV.
#'
#' @param taxa a vector containing species names
#'
#' @return a list of DNAStringSet objects containing the sequences for
#' each variable region for each species.
#'
#' @export
#'
#' @examples
#' getSequences(c("Longispora fulva"))
#' DNAStringSet object of length 36:
#'      width seq                                              names
#'  [1]    21 GAAAGGCCCTTCGGGGTACTC                            IW245_RS23890 tax...
#'  [2]   107 CTTGGCTTCGGGATAACCATCGG...GCCAGGGATGGGCTCGCGGCCT IW245_RS23890 tax...
#'  ...   ... ...
#' [36]    33 GCCGGTGGCCCAACCCGTAAGGGAGGGAGCCGT                IW245_RS40075 tax...
getSequences <- function(taxa) {
    RADlibV <- system.file("extdata", "RADlibVR.fa", package = "RADalign")
    if (RADlibV == "") {
        stop("Could not access RADlibV")
    }
    sequences <- readSequences(RADlibV, taxa)
}

#' alignVRegions
#'
#' For a group of sequences in RADlib, align the sequences in each
#' V-region individually.
#'
#' @param sequences a DNAStringSet of sequences to align
#'
#' @return a list containing unique IDs for each group of exactly
#' aligned sequences in each v-region
#'
#' @importFrom Biostrings DNAStringSet
#'
#' @export
#'
#' @examples
#' alignVRegions(sequences)
#' $V11
#' [1] "IW245_RS23890 taxid=619741 organism=\"Longispora fulva\" variable_region=1"
#' [2] "IW245_RS27830 taxid=619741 organism=\"Longispora fulva\" variable_region=1"
#' [3] "IW245_RS37080 taxid=619741 organism=\"Longispora fulva\" variable_region=1"
#' [4] "IW245_RS40075 taxid=619741 organism=\"Longispora fulva\" variable_region=1"
#' ...
#' $V91
#' [1] "IW245_RS23890 taxid=619741 organism=\"Longispora fulva\" variable_region=9"
#' [2] "IW245_RS27830 taxid=619741 organism=\"Longispora fulva\" variable_region=9"
#' [3] "IW245_RS37080 taxid=619741 organism=\"Longispora fulva\" variable_region=9"
#' [4] "IW245_RS40075 taxid=619741 organism=\"Longispora fulva\" variable_region=9"
alignVRegions <- function(sequences) {
    IDs <- list()
    all_v_regions <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9")

    for (region in all_v_regions) {
        # get all sequences for region and and delete any empty sequences
        region_sequences <- getVRegions(sequences, region)
        clean_dna <- region_sequences[Biostrings::width(region_sequences) > 0]
        if (length(clean_dna) != length(region_sequences)) {
            print(paste0("Warning: empty sequences deleted in region ", region))
        }

        # skip performing msa if only one sequence exists
        region_IDs <- list()
        if (length(clean_dna) < 2) {
            ID <- paste0(region, "1")
            region_IDs[ID] <- names(clean_dna)
            IDs <- c(IDs, region_IDs)
            next
        }

        # perform msa
        alignment <- msa::msa(clean_dna, method = "ClustalOmega")

        # separate out groups of identical sequences
        alignment <- as(alignment, "DNAStringSet")
        groups <- split(seq_along(alignment), as.character(alignment))
        for (i in seq_along(groups)) {
            ID <- paste0(region, i)
            region_IDs[ID] <- lapply(groups[i], function(i) names(alignment)[i])
        }
        IDs <- c(IDs, region_IDs)
    }
    return(IDs)
}

#' createSummary
#'
#' Takes the list of IDs created by alignVRegions and summarizes the
#' data in a csv. Can also return the summary as a dataframe, if
#' return_df is set to true.
#'
#' @param IDs a list containing unique IDs for each group of exactly
#' aligned sequences in each v-region
#' @param return_df a boolean indicating whether a dataframe
#' containing the summary data should be returned in addition to the
#' csv created by default.
#'
#' @return a dataframe containing the summary data when return_df = TRUE
#'
#' @export
#'
#' @examples
#' createSummary(IDs, TRUE)
#'                        species variable_region    copy_id seq_id
#' 1  Pseudomonas aeruginosa PAO1              V1   PA0668.1    V11
#' 2  Pseudomonas aeruginosa PAO1              V1   PA4280.5    V11
#' ...
#' 62 Pseudomonas aeruginosa PAO1              V9   PA4690.5    V92
#' 63 Pseudomonas aeruginosa PAO1              V9   PA5369.5    V92
createSummary <- function(IDs, return_df = FALSE) {
    # use vectors to retrieve and sort individual pieces of information from ID list
    species_vec <- character()
    region_vec <- character()
    copy_id_vec <- character()
    seq_id_vec <- character()

    for (i in seq_along(IDs)) {
        group <- IDs[i]
        id <- names(group)
        region <- substr(id, start = 1, stop = 2)

        seq_list <- IDs[[i]]
        for (j in seq_along(seq_list)) {
            capture_pattern = "^([^ ]+).*organism=\"([^\"]+)"
            matches = stringr::str_match(seq_list[j], capture_pattern)
            copy_id <- matches[2]
            species <- matches[3]

            species_vec <- c(species_vec, species)
            region_vec <- c(region_vec, region)
            copy_id_vec <- c(copy_id_vec, copy_id)
            seq_id_vec <- c(seq_id_vec, id)
        }
    }

    # create dataframe using sorted information
    full_summary <- data.frame(
        species = species_vec, variable_region = region_vec,
        copy_id = copy_id_vec, seq_id = seq_id_vec
    )

    # create csv from dataframe
    filepath <- file.path(data_dir, "RADq.csv")
    write.csv(full_summary, filepath)

    if (return_df) return(full_summary)
}

# nolint end
