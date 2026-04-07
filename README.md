# RADalign

An R package designed to use the RAD library to explore the variation across 16S rRNA gene copies in microbial taxa by aligning 16S variable regions. Can be used as a backend for RADexplorer, an interactive RShiny app that explores RADalign outputs.

---

## What is RADalign?

Bacterial species carry multiple copies of the 16S rRNA gene (~5 on average, up to 15+), and those copies are often not identical. 
When planning an amplicon sequencing experiment, the variable region (v-region) you choose to sequence determines whether you can actually distinguish the species you care about from closely related taxa.

RADalign (**R**egional **A**lignment **D**atabase) utilizes the RADlib reference library to align the variable regions (v-regions) within the 16S rRNA gene. RADalign is designed to function with RADexplorer, an RShiny app that visualizes RADalign's output, but it also produces csv data files and optional dataframes that can be used to generate your own visualizations.

Aligning v-regions allows you to see which v-regions (V1-V9) produce unique sequences for a group of target taxa, which ones leave species indistinguishable from one another, and whether a single region is enough for your experimental needs.

This matters more than most researchers expect. The most commonly used v-regions in amplicon sequencing, V3 and V4, are frequently insufficient to differentiate between closely related organisms within the same genus or species group.

### How it works

RADalign is built on two reference libraries:

- **RADlib16S** : A bacteria and archaea 16S rRNA database containing all copies of the 16S rRNA gene in an organism
- **RADlibVR** : A database of all variable regions found in RADlib

**RADalign** runs multiple sequence alignment and grouping analyses on all variable regions of selected sequences and returns a csv and optional dataframe containing information about which sequences aligned exactly. Further functions facilitate filtering of variable regions and the creation of IDs used by RADexplorer to identify groups of aligned sequences and species.

---

## Installation

RADalign is installed directly from GitHub. All dependencies are handled automatically.  
Run the following commands in your R or RStudio console:

**Step 1: Install pak** (if not already installed)
```r
install.packages("pak")
```

**Step 2: Install Biostrings**
```r
install.packages("BiocManager")
BiocManager::install("msa")
```

**Step 3: Install RADalign**
```r
pak::pak("RADSuite/RADalign")
```

---

## Available Functions

For more details, view roxygen documentation using the command ?function or help(function).

### RADaccessions

Contains functions relating to RADlib header metadata (organism names, accession ids, taxa ids, genus).

- **get_accessions_df()** : Generates a metadata table from the header data of RADlib.

- **get_species_list()** : Given a list of accession ids, outputs corresponding organism names (includes duplicates).

- **get_taxa_ids()** : Given a list of accession ids, outputs corresponding taxa ids (includes duplicates).

- **get_accession_ids()** : Given a list of organism names, outputs all corresponding accession ids.

- **get_all_organisms()** : Generates a sorted list of all unique organisms in RADlib and adds genus labels ("Genus - All Species (#)").

- **get_species_from_genus()** : Finds all organisms in RADlib of a given genus.

### RADalign

!!! RACHEL

- **createRADq()** : Given a list of species names, pull sequences for each species from RADlibV, align them, and return either a csv or csv and dataframe containing a summary of which variable regions aligned exactly, designated by a unique ID for each group.

- **selectVRegions()** : After createRADq has been run, filters the csv file from createRADq to include only user-specificed variable regions.

- **createSummarizedIDs()** : After createRADq has been run, combines all unique IDs for each v-region in each species into a single ID.

- **createRADqGroups()** : After createSummarizedIDs has been run, combines all summarized IDs and sorts taxa into groups that share all the same IDs for the given variable regions.

- **getSequences()** : Given a list of species, retrieves all sequences associated with those taxa from RADlibV.

- **alignVRegions()** : For a group of sequences in RADlib, align the sequences in each V-region individually.

- **createSummary()** : Takes the list of IDs created by alignVRegions and summarizes the data in a csv. Can also return the summary as a dataframe, if return_df is set to true.

### RADdownload

Contains functions to download RADlib as a refrence database for pipeline integration.

- **download_RAD_data()** : Downloads files for integration with analysis pipelines, currently supports MetaScope.

- **download_MetaScope_reference()** : Downloads selected sequences from RADlib to a desired folder as fasta files and places them inside a single sub folder.

### RADlibHandler

!!! RACHEL

- **readSequences()** : Reads in all sequences that include the given accession ids in their header.

- **getVRegions()** : Given a DNAStringSet of sequences with V-regions included in their names, retrieves all instances of selected V-regions from the DNAStringSet.

---

## Dependencies

RADalign requires R >= 4.1. Core dependencies include:

- `Biostrings`, `dplyr`, `msa`, `tibble`, `tidyr`,
  `data.table`, `stringi`, `tidyverse`

All dependencies are installed automatically via `pak::pak("RADSuite/RADalign")`.

---

## Citation

If you use RADalign in your work, please cite the RADSuite package suite (citation tbd).

---

## License

MIT
