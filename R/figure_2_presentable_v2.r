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
