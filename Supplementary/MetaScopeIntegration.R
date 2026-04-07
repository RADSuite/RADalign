# 1. LOAD METASCOPE
library("MetaScope")

# 2. DOWNLOAD REFERENCE DATABASE

# Option 1: full RADlib
rad_lib_file <- system.file("extdata", "RADlib16S.fa", package = "RADalign")
dir.create("refdata")
file.copy(rad_lib_file, "refdata")
ref <- "refdata"

# Option 2: partial RADlib for speed (only aligns against your selected species)
ref <- "your/RADdownload/file/path/here"

# 3. DOWNLOAD SAMPLE DATA
# To use example sample data, download “D1_16dnajoin.fastq” from https://doi.org/10.5061/dryad.d41v4, replace the file path in the line below and run
# data <- "path/to/your/download/of/D1_16dnajoin.fastq"
data <- "path/to/your/file/data_file.fastq"

# 4. PREP FOLDERS
indices <- tempfile()
dir.create(indices)
dir.create("out")

# 5. MAKE BOWTIE INDEX
mk_bowtie_index(
  ref_dir = ref,
  lib_dir = indices,
  lib_name = "target",
  overwrite = TRUE)

# 6. ALIGN SEQUENCES
target_map <- align_target_bowtie(
  data,
  lib_dir = indices,
  libs = "target",
  align_dir = "out",
  align_file = "bowtie_target",
  overwrite = TRUE)

# 7. GENERATE BAM FILE
bamFile <- Rsamtools::BamFile(target_map)

param <-
  Rsamtools::ScanBamParam(
    flag = Rsamtools::scanBamFlag(isSecondaryAlignment = FALSE),
    what = c("flag", "rname")
  )

aln <- Rsamtools::scanBam(bamFile, param = param)
accession_all <- aln[[1]]$rname

# 8. OUTPUT TABLE

# Option 1: species names and read counts
genome_name_all <- get_species_list(accession_all) #RADalign function
read_count_table <- sort(table(genome_name_all), decreasing = TRUE)

knitr::kable(
  read_count_table[1:10],
  col.names = c("Genome Assigned", "Read Count"))

# Option 2: taxa ids and read counts
taxa_id_all <- get_taxa_ids(accession_all) #RADalign function
read_count_table <- sort(table(taxa_id_all), decreasing = TRUE)

knitr::kable(
  read_count_table[1:10],
  col.names = c("Genome Assigned", "Read Count"))
