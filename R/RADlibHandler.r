library(Biostrings)

infile <- "C:/Users/rache/OneDrive/Desktop/Capstone/RADalign/data/RADlibV.fa"

# vector of desired accession numbers
wanted <- c("GCF_000006765.1.1")

# filter the indexes of the sequences that contain the accession numbers
# from the fasta.index dataframe that contains summary information about
# the database file. Names are stored in the "desc" column of the dataframe.
fastaindex <- fasta.index(infile, seqtype = "DNA")
wanted_index <- grepl(paste(wanted), fastaindex$desc)
subset_index <- fastaindex[wanted_index, ]
filtered <- readDNAStringSet(subset_index)
print(filtered)
