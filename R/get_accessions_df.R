mock_accessions_df <- data.frame(
  accession = c(
    "GCF_000006765.1", "GCF_000007505.1", "GCF_000008365.1", "GCF_000006945.2",
    "GCF_000005845.2", "GCF_000007985.2", "GCF_000008185.1", "GCF_000008265.1",
    "GCF_000008325.1", "GCF_000007345.1", "GCF_000006685.1", "GCF_000007705.1",
    "GCF_000007205.1", "GCF_000007605.1", "GCF_000007745.1", "GCF_000007025.1",
    "GCF_000008205.1", "GCF_000007125.1", "GCF_000006925.2", "GCF_000008305.1",
    "GCF_000006985.1", "GCF_000007225.1", "GCF_000007805.1", "GCF_000007085.1",
    "GCF_000007365.1", "GCF_000007765.2", "GCF_000007905.1", "GCF_000008045.1",
    "GCF_000007625.1", "GCF_000007185.1"
  ),
  organism = c(
    "Pseudomonas aeruginosa PAO1", "Brucella suis 1330", "Mycoplasma mobile 163K",
    "Salmonella enterica subsp. enterica serovar Typhimurium LT2",
    "Escherichia coli K-12 substr. MG1655", "Geobacter sulfurreducens PCA",
    "Treponema denticola ATCC 35405", "Picrophilus oshimae DSM 9456",
    "Methylococcus capsulatus Bath", "Methanosarcina acetivorans C2A",
    "Shigella flexneri 2a str. 301", "Chromobacterium violaceum ATCC 12472",
    "Chlamydia pneumoniae CWL029", "Chlamydia caviae GPIC",
    "Mannheimia succiniciproducens MBEL55E", "Rickettsia conorii Malish 7",
    "Mesomycoplasma hyopneumoniae 232", "Brucella melitensis 16M",
    "Clostridium tepidum ATCC 43037", "Mesoplasma florum L1",
    "Chlorobaculum tepidum TLS", "Pyrobaculum aerophilum str. IM2",
    "Helicobacter hepaticus ATCC 51449", "Rickettsia typhi Wilmington",
    "Clostridium tetani E88", "Methanopyrus kandleri AV19", "Helicobacter pylori 26695",
    "Rickettsia typhi Wilmington", "Picrophilus oshimae DSM 9456",
    "Mesomycoplasma hyopneumoniae 232"
  ),
  species = c(
    "Pseudomonas aeruginosa", "Brucella suis", "Mycoplasma mobile",
    "Salmonella enterica", "Escherichia coli", "Geobacter sulfurreducens",
    "Treponema denticola", "Picrophilus oshimae", "Methylococcus capsulatus",
    "Methanosarcina acetivorans", "Shigella flexneri", "Chromobacterium violaceum",
    "Chlamydia pneumoniae", "Chlamydia caviae", "Mannheimia succiniciproducens",
    "Rickettsia conorii", "Mesomycoplasma hyopneumoniae", "Brucella melitensis",
    "Clostridium tepidum", "Mesoplasma florum", "Chlorobaculum tepidum",
    "Pyrobaculum aerophilum", "Helicobacter hepaticus", "Rickettsia typhi",
    "Clostridium tetani", "Methanopyrus kandleri", "Helicobacter pylori",
    "Rickettsia typhi", "Picrophilus oshimae", "Mesomycoplasma hyopneumoniae"
  )
)

get_accessions_df <- function() {
  file_path <- system.file("extdata", "RADlibV.fa", package = "RADalign")
  lines <- readLines(file_path)

  accession_id = c()
  species_name = c()

  for (line in lines) {

    copy_pattern = "[^>].*\\.[^_]*"
    copy_id = regmatches(line, regexpr(copy_pattern, line))

    if ((length(copy_id) > 0) && !(copy_id %in% accession_id)) {
      reg_pattern = "[^\\.]*\\.[^\\.]*"
      reg_id = regmatches(copy_id, regexpr(reg_pattern, copy_id))

      name = mock_accessions_df$species[mock_accessions_df$accession == reg_id]

      accession_id <- c(accession_id, copy_id)
      species_name <- c(species_name, name)
    }
  }

  accessions <- data.frame(species_name, accession_id)

  return(accessions)
}

#print(head(get_accessions_df()))
