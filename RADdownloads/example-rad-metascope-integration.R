library("MetaScope")

target_ref_temp <- tempfile()
dir.create(target_ref_temp)
dir.create("tmp")
dir.create("filter_tmp")
dir.create("out")
dir.create("refdata")

# ---- This portion starts at 5 of the MetaScope tutorial ----
# Starting at part 5 of the "tutorial"Introduction to MetaScope" vignette
# Using download_accessions() isn't needed for us. I'm pretty sure we are replacing
# every taxonomizr portion with our stuff

# —————————————————————————————————————————————————————————————————————————————
# This creates a bowtie database (I think) using RADlib16S (could probably use
# RADlibVR as well?)
# ref_dir is where the RADlib fastas will live and lib_dir is the generated
# bowtie indecies

# ---- Target indecies (these are the taxa we want to identify) ----
# This should be the export of Jake's explorer? a filtered RADlib
mk_bowtie_index(
  ref_dir = "refdata",
  lib_dir = "tmp",
  lib_name = "target",
  overwrite = T)

# ---- Filter indecies (These are the taxa we don't want to id) ----
# Another filtered RADlib the researchers don't think they'll find?
# I'm guessing if taxa X doen't live in a microbiome having it the search pool my
# be added noise? This might be a talk to Dr. J moment/YouTube
# mk_bowtie_index(
#   ref_dir = "filterdata",
#   lib_dir = "filter_tmp",
#   lib_name = "filter",
#   overwrite = T)

fastq <- "D1_16dnajoin.fastq" # Data we analysing (sourced from https://doi.org/10.5061/dryad.d41v4)
target_map <- align_target_bowtie(
  fastq,
  lib_dir = "tmp",
  libs = "target",
  align_dir = "out",
  align_file = "bowtie_target",
  overwrite = T)

# ---- I have not tested this part ----
# final_map <-
#   filter_host_bowtie(
#     reads_bam = target_map,
#     lib_dir = index_temp,
#     libs = "filter",
#     make_bam = TRUE, # Set to true to create BAM output
#     # Default is to create simplified .csv.gz output
#     # The .csv.gz output is much quicker to create!
#     overwrite = TRUE,
#     threads = 1
#   )

bamFile <- Rsamtools::BamFile(target_map)

param <-
  Rsamtools::ScanBamParam(
    flag = Rsamtools::scanBamFlag(isSecondaryAlignment = FALSE),
    what = c("flag", "rname")
  ) #Gets info about primary alignments

aln <- Rsamtools::scanBam(bamFile, param = param)
accession_all <- aln[[1]]$rname

# TODO: Needs to do RAD stuff
# This maps the accessions used (for us it is the gene tag, not an accession)
# to a an organism/taxa
# genome_name_all <- accession_all |>
#   taxonomizr::accessionToTaxa(tmp_accession) |>
#   taxonomizr::getTaxonomy(sqlFile = tmp_accession, desiredTaxa = "strain")

# below change to genome_name_all, fix genome_name_all function to use our functions
genome_name_all <- get_species_list(accession_all)

# Create counts table and show the top hit taxa/gene tags
# read_count_table <- sort(table(accession_all), decreasing = TRUE)
# knitr::kable(
#   read_count_table[1:10],
#   col.names = c("Genome Assigned", "Read Count"))

# Create counts table and show the top hit taxa/gene tags
read_count_table <- sort(table(genome_name_all), decreasing = TRUE)
knitr::kable(
  read_count_table[1:10],
  col.names = c("Genome Assigned", "Read Count"))

# "/Users/myeshagilliland/BYU/BIO465/RADdownloads/metascope_rad_test"
