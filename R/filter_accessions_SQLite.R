library(DBI)
library(RSQLite)

filter_accessions_SQLite <- function(accessions_list) {

  #location of accessions db
  file_path <- system.file("extdata", "example_accessions.sql", package = "RADalign")

  #open db connection
  con <- dbConnect(
    RSQLite::SQLite(),
    dbname = file_path
  )

  tables <- dbListTables(con)
  print(tables)

  columns1 <- dbListFields(con, "accessionTaxa")
  columns2 <- dbListFields(con, "names")
  columns3 <- dbListFields(con, "nodes")

  print(columns1)
  print(columns2)
  print(columns3)

  output <- dbGetQuery(
    con, "
    SELECT base, accession, taxa
    FROM accessionTaxa
    WHERE accession = ?;",
    params = accessions_list
  )

  print(output)

  #close db connection
  dbDisconnect(con)

  return(print("DONE"))

}

# accessions_list <- c("NZ_CTYB01000002.1",
#                      "NZ_CTYB01000003.1",
#                      "NZ_CTYB01000004.1",
#                      "NZ_LAWV01000006.1",
#                      "NZ_LAWV01000007.1",
#                      "NC_009641.1",
#                      "NZ_JBBIAE010000011.1",
#                      "NZ_JBBIAE010000012.1")
accessions_list <- c("NZ_CTYB01000002.1")

filter_accessions_SQLite(accessions_list)



