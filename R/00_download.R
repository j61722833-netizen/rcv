# 00_download.R
# Downloads the source datasets used to derive the Low Plurality Winner (LPW)
# table programmatically (see the recent_lpw section of 01_clean_and_merge.R).
#
# Each download is skipped if the destination file already exists, so the
# pipeline stays runnable offline once data/raw/ is populated (all files are
# committed to the repo).
#
# Sources:
#   1. MEDSL U.S. Senate 1976-2020 — statewide general election returns.
#      MIT Election Data and Science Lab, doi:10.7910/DVN/PEJ5QU
#   2. Amlani & Algara county-level gubernatorial returns 1865-2020.
#      Replication data for "Partisanship & Nationalization in American
#      Elections" (Electoral Studies), doi:10.7910/DVN/DGUMFI
#      Covers all states except Alaska (which has no counties).
#   3. Alaska Division of Elections official statewide results summaries for
#      the 2014 and 2018 general elections. The live site is behind a CAPTCHA,
#      so these come from the Internet Archive's captures of the official
#      results.txt files.
#
# Not downloadable anywhere (see data/raw/statewide_elections_manual.csv):
#   Alaska 2010 Governor — the official 2010 machine-readable results file
#   only ever contained the U.S. Senate race; OpenElections' Alaska coverage
#   starts in 2014. One hand-entered row, sourced in the manual CSV.

download_if_missing <- function(url, destfile) {
  if (file.exists(destfile)) {
    cat("exists, skipping:", destfile, "\n")
    return(invisible(FALSE))
  }
  cat("downloading:", destfile, "\n  from:", url, "\n")
  download.file(url, destfile, mode = "wb", quiet = TRUE)
  invisible(TRUE)
}

# 1. MEDSL U.S. Senate 1976-2020 (Harvard Dataverse, file id 7609736)
download_if_missing(
  "https://dataverse.harvard.edu/api/access/datafile/7609736?format=original",
  "data/raw/medsl_senate_1976_2020.csv"
)

# 2. Amlani & Algara gubernatorial county returns 1865-2020
#    (Harvard Dataverse, file id 5028535; loads as `gov_elections_release`)
download_if_missing(
  "https://dataverse.harvard.edu/api/access/datafile/5028535",
  "data/raw/gubernatorial_county_returns_1865_2020.RData"
)

# 3. Alaska statewide results summaries (Internet Archive captures of the
#    official elections.alaska.gov results.txt files; "id_" returns the raw
#    file without Wayback rewriting)
download_if_missing(
  "http://web.archive.org/web/2019id_/http://www.elections.alaska.gov/results/14GENR/data/results.txt",
  "data/raw/alaska_statewide_results_2014.txt"
)
download_if_missing(
  "http://web.archive.org/web/2019id_/http://www.elections.alaska.gov/results/18GENR/data/results.txt",
  "data/raw/alaska_statewide_results_2018.txt"
)

cat("All downloads present in data/raw/\n")
