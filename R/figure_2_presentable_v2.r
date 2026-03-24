# Outline
# download NCBI 16S database
# download GreenGene2 16S database
# download SILVA 16S database
# downlaod RADlib
#
# Process data from each database to format it the sampe
#
# Select * from Bacillus cereus group from *
# Count distinct reads
# 
# ggplot histogram
# fraction of each species colored? May be too busy
# 

DATADIR <- "..."
SILVA_URL <- "https://www.arb-silva.de/fileadmin/silva_databases/current/Exports/SILVA_138.2_SSURef_NR99_tax_silva.fasta.gz"
GREENGENE_URL <- "https://ftp.microbio.me/greengenes_release/current/2024.09.taxonomy.id.tsv.gz"
NCBI_URL <- "https://ftp.ncbi.nlm.nih.gov/refseq/TargetedLoci/Bacteria/bacteria.16SrRNA.fna.gz"
RADLIB_URL <- "https://zenodo.org/records/19102712/files/RADlib16S.fa?download=1"

# # ---- Download Data ----
# silva_conn <- RCurl::getURL(SILVA_URL)
# greengene_conn <- RCurl::getURL(GREENGENE_URL)
# ncbi_conn <- RCurl::getURL(NCBI_URL)
# radlib_conn <- RCurl::getURL(RADLIB_URL)

# TODO: replace read.data with function that actually reads the data (fasta etc.)
# TODO: All db are probably gzipped so need step to use gzfile() to unzip data
# silva_data <- read.data(testConnection(silva_conn)) # Read Fasta
# greengeen_data <- read.data(testConnection(greengene_conn), sep='\t') # This is like 2+ gigs so try to only download once
# ncbi_data <- read.data(testConnection(ncbi_conn))
# radlib_data <- read.data(testConnection(radlib_conn))

# ---- Create Dataframes ----

# ---- Combine Dataframes ----

# ---- GGPlot ----

# <- <- <- <- Old Stuff -> -> -> ->
# library(rentrez)
# library(glue)
#
# get_ncbi_file <- function() {
#   system.file("extdata", "bacteria.16SrRNA.fna", package = "RADalign")
# }
#
# create_figure_2 <- function() {
#   ncbi_file <- get_ncbi_file
#   target_taxa <- c(
#     "Bacillus cereus", "Bacteroides fragilis", "Escherichia coli",
#     "Enterococcus faecalis", "Staphylococcus aureus"
#   )
#   curated_vec <- numeric()
#   whole_genome_vec <- numeric()
#   for (taxon in target_taxa) {
#     fasta_summary <- Biostrings::fasta.index(ncbi_file, seqtype = "DNA")
#     matching_indexes <- grepl(paste(c(taxon), collapse = "|"), fasta_summary$desc)
#     subset_index <- fasta_summary[matching_indexes, ]
#     NCBI <- Biostrings::readDNAStringSet(subset_index)
#     NCBI <- NCBI[order(Biostrings::width(NCBI))]
#     curated_vec <- c(curated_vec, Biostrings::length(NCBI))
#
#     whole_genome_term <- glue('("{taxon}"[Organism]) AND "refseq"[filter] AND "complete genome"[Property]')
#     whole_genome_results <- entrez_search(
#       db = "nuccore",
#       term = whole_genome_term,
#       use_history = TRUE
#     )
#     whole_genome_vec <- c(whole_genome_vec, whole_genome_results$count)
#   }
#
#   data <- data.frame(
#     taxa = target_taxa,
#     curated = curated_vec,
#     whole_genome = whole_genome_vec
#   )
#   figure2 <- tibble::as_tibble(data) %>%
#     tidyr::pivot_longer(cols = -taxa, names_to = "category", values_to = "count") %>%
#     mutate(category = factor(category, levels = c("curated", "whole_genome"))) %>%
#     ggplot(aes(x = taxa, y = count, fill = category)) +
#     geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
#     scale_y_log10() +
#     scale_fill_manual(
#       values = c("whole_genome" = "#FFC107", "curated" = "#004D40"),
#       labels = c("whole_genome" = "Whole Genome References", "curated" = "Curated References")
#     ) +
#     theme_minimal() +
#     labs(
#       x = NULL,
#       y = "Number of References on NCBI (log scale)",
#       fill = "Reference Type"
#     ) +
#     theme(
#       legend.key.size = unit(.5, "cm"),
#       legend.text = element_text(size = 8),
#       legend.title = element_text(size = 11),
#       axis.text = element_text(size = 8)
#     )
#   return(figure2)
# }
