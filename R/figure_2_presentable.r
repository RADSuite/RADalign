library(Biostrings)
library(rentrez)
library(glue)
library(tidyverse)

ncbi_file <- RADlibV <- system.file("extdata", "bacteria.16SrRNA.fna", package = "RADalign")


target_taxa <- c(
  "Bacillus cereus", "Bacteroides fragilis", "Escherichia coli",
  "Enterococcus faecalis", "Staphylococcus aureus"
)
curated_vec <- numeric()
whole_genome_vec <- numeric()
for (taxon in target_taxa) {
  fasta_summary <- fasta.index(ncbi_file, seqtype = "DNA")
  matching_indexes <- grepl(paste(c(taxon), collapse = "|"), fasta_summary$desc)
  subset_index <- fasta_summary[matching_indexes, ]
  NCBI <- readDNAStringSet(subset_index)
  NCBI <- NCBI[order(width(NCBI))]
  curated_vec <- c(curated_vec, length(NCBI))

  whole_genome_term <- glue('("{taxon}"[Organism]) AND "refseq"[filter] AND "complete genome"[Property]')
  whole_genome_results <- entrez_search(
    db = "nuccore",
    term = whole_genome_term,
    use_history = TRUE
  )
  whole_genome_vec <- c(whole_genome_vec, whole_genome_results$count)
}

data <- data.frame(
  taxa = target_taxa,
  curated = curated_vec,
  whole_genome = whole_genome_vec
)
figure2 <- as_tibble(data) %>%
  pivot_longer(cols = -taxa, names_to = "category", values_to = "count") %>%
  mutate(category = factor(category, levels = c("curated", "whole_genome"))) %>%
  ggplot(aes(x = taxa, y = count, fill = category)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  scale_y_log10() +
  scale_fill_manual(
    values = c("whole_genome" = "#FFC107", "curated" = "#004D40"),
    labels = c("whole_genome" = "Whole Genome References", "curated" = "Curated References")
  ) +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Number of References on NCBI (log scale)",
    fill = "Reference Type"
  ) +
  theme(
    legend.key.size = unit(.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 11),
    axis.text = element_text(size = 8)
  )

print(figure2)
