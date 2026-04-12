# MetaScope Integration

Follow the steps below to integrate with the MetaScope pipeline. Copy each code block and run it in your R console.

*For the full documentation of MetaScope, go to: https://wejlab.github.io/metascope-docs/index.html.*

## 1. Load MetaScope
Load the MetaScope package before running anything else.
```r
install.packages("BiocManager")
BiocManager::install("MetaScope")
library(MetaScope)
library(magrittr)
```

## 2. Set Up Reference Database
Choose one option below. You only need to run one.

### Option 1: Full RADlib
Downloads the full RADlib reference database bundled with RADalign.
```r
rad_lib_file <- system.file("extdata", "RADlib16S.fa", package = "RADalign")
dir.create("refdata")
file.copy(rad_lib_file, "refdata")
ref <- "refdata"
```

### Option 2: Partial RADlib
***It is HIGHLY recommended that you perform this step with RADx instead of running the code listed below.***

If using without RADx, replace the last two parameters of this function with your inputs. The last parameter defaults to your working directory, so you can leave that parameter blank if you want it downloaded there.

*Note: the names in your organisms list MUST match RADlib organism names EXACTLY. See the RADx main page for the list of all organisms in RADlib, or run RADalign::get_all_organisms().*
```r
ref <- RADalign::download_RAD_data("MetaScope", c("your", "organisms", "list"), ,"path/to/your/preferred/download/location")
```
For example, if you were using the example data (explained in Step 3), and wanted it downloaded to your working directory, for this step you would run:
```r
ref <- RADalign::download_RAD_data("MetaScope", c("Leptotrichia wadei", "Capnocytophaga ochracea DSM 7271"))
```

## 3. Load Sample Data
Point to your .fastq file. For example data, download D1_16dnajoin.fastq from https://doi.org/10.5061/dryad.d41v4.
```r
data <- "path/to/your/file/data_file.fastq"
```

## 4. Prepare Output Folders
Create folders for the Bowtie index and alignment output.
```r
indices <- tempfile()
dir.create(indices)
dir.create("out")
```

## 5. Build Bowtie Index
Index the reference database. This may take a while but only needs to be run once.
```r
mk_bowtie_index(
  ref_dir = ref,
  lib_dir = indices,
  lib_name = "target",
  overwrite = TRUE)
```

## 6. Align Sequences
Align your sample reads against the reference index.
```r
target_map <- align_target_bowtie(
  data,
  lib_dir = indices,
  libs = "target",
  align_dir = "out",
  align_file = "bowtie_target",
  overwrite = TRUE)
```

## 7. Generate Bam File
```r
bamFile <- Rsamtools::BamFile(target_map)

param <-
  Rsamtools::ScanBamParam(
    flag = Rsamtools::scanBamFlag(isSecondaryAlignment = FALSE),
    what = c("flag", "rname")
  )

aln <- Rsamtools::scanBam(bamFile, param = param)
accession_all <- aln[[1]]$rname
```

## 8. Output Table
Choose one option below. You only need to run one.
### Option 1: Species Names
Use species names and read counts.
```r
genome_name_all <- RADalign::get_organism_names(accession_all)
read_count_table <- sort(table(genome_name_all), decreasing = TRUE)
knitr::kable(
  read_count_table[1:10],
  col.names = c("Genome Assigned", "Read Count"))
```

### Option 2: TaxID
Use TaxID and read counts.
```r
taxa_id_all <- RADalign::get_taxa_ids(accession_all)
read_count_table <- sort(table(taxa_id_all), decreasing = TRUE)
knitr::kable(
  read_count_table[1:10],
  col.names = c("Genome Assigned", "Read Count"))
```

***All done! Your read count table is ready.***
