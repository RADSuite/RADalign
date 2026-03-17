library(Biostrings)
library(DBI)
library(RSQLite)

transform_accession_ids <- function(RADlib_path, RADacc_path) {

  #get list of accession ids from RADlib

  # lines <- readLines(file_path)
  index <- fasta.index(RADlib_path, seqtype = "DNA")
  ids <- index$desc
  # print(ids)

  con <- dbConnect(
    RSQLite::SQLite(),
    dbname = RADacc_path
  )

  dbExecute(con, "CREATE TEMP TABLE acc_keys(original TEXT, trimmed TEXT)")

  for (id in ids) {
    trimmed_id <- "test"
    dbExecute(con, "INSERT INTO acc_keys (original, trimmed) VALUES (?, ?)",
    params = list(id, trimmed_id))
  }

  # successfully creating temp table and can use this command to print it; just need to implement trimming the ids
  dbGetQuery(con, "SELECT * FROM acc_keys") %>% print()
  dbDisconnect(con)

  #connection to RADacc
  # con <- dbConnect(
  #   RSQLite::SQLite(),
  #   dbname = RADacc_path
  # )

  # dbWriteTable(con, "reads", data.frame(detailed_id = ids), temporary=TRUE)
  # dbExecute(con, "
  #   CREATE INDEX idx_reads_root_expr
  #   ON reads(substr(detailed_id,1,instr(detailed_id,'.')-1));
  #   ")
  # dbExecute(con, "CREATE INDEX idx_main_id ON main_table(id);")

  # dbListTables(con)
  #
  # dbExecute(con, "CREATE TEMP TABLE reads(id TEXT);")
  # dbExecute(con, "CREATE INDEX idx_reads_id ON reads(id);")
  #
  # dbExecute(con, "SELECT r.id, m.*
  #   FROM reads r
  #   JOIN accessionTaxa m
  #   ON r.id >= m.accession || '.'
  #   AND r.id <  m.accession || '/';")
  #
  # #
  #
  # dbDisconnect(con)



  return("Done")
}


RADlib_file_path <- system.file("extdata", "RADlib.fa", package = "RADalign")
RADacc_file_path <- system.file("extdata", "accessions.sqlite", package = "RADalign")
#
transform_accession_ids(RADlib_file_path, RADacc_file_path)
