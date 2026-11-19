# plasticdb

R client for the [PlasticDB](https://v2.plasticdb.org) API: microorganisms
and proteins linked to plastic biodegradation, and the evidence behind them.
It mirrors the Python client,
[plasticdb-py](https://github.com/VictorGambarini/plasticdb-py), and targets
[Bioconductor](https://bioconductor.org).

> **Pre-release (0.99.x).** Every public data endpoint, release pinning,
> provenance and citation, and analysis jobs (with an API key).

## Install

Documentation: <https://victorgambarini.github.io/plasticdb-r/> (the Python
client's is at <https://victorgambarini.github.io/plasticdb-py/>).

```r
# install.packages("pak")
pak::pak("VictorGambarini/plasticdb-r")
# or: remotes::install_github("VictorGambarini/plasticdb-r")
```

A Bioconductor release is planned; then
`BiocManager::install("plasticdb")` will do.

## Quick start

```r
library(plasticdb)

# Proteins with positive evidence on PET: a tibble, one flat row per protein
pet <- proteins_list(plastic = "PET", kind = "wild_type")
provenance(pet)$total       # the server's total
pet$source_microorganism.name

# ...and their sequences, as a Biostrings::AAStringSet named by accession
seqs <- proteins_sequences(plastic = "PET", kind = "wild_type")

# One record, with its evidence
petase <- proteins_get("PBFCTW")
petase$name

# The evidence spine: interaction -> experiment -> observation
obs <- evidence_interactions("PBFCTW", level = "observation")

# Engineered variants (with sequences), organisms, plastics, papers
as_aastringset(proteins_variants("PBFCTW"))
microorganisms_list(genus = "Pseudomonas", plastic = "PET")
plastics_get(16, protein_limit = 10)
papers_list(protein = "PBFCTW", min_year = 2020)

# Search, pathways, statistics, the change feed
search_hits("cutinase", limit = 20)
pathways_graph(16)$edges
stats_summary()
feed <- changes(since = Sys.Date() - 7)

# Files
downloads_proteins(tempdir(), format = "fasta", plastic = "PET")
downloads_dataset(tempdir(), max_wait = 300)
```

### Jobs

With an API key (`PLASTICDB_API_KEY`, created at
<https://v2.plasticdb.org/profile/api-keys>), run PlasticDB's analyses on the
server. Inputs are file paths (plain or gzip), FASTA text or `Biostrings`
sets:

```r
job <- annotate_genome("genome.faa.gz", release = "2026.50")
job <- wait_job(job, timeout = 3600)
hits <- job_results(job)          # every hit, paged: a tibble
job_provenance(job)$reference_sha256
job_download(job, "results/")     # the TSV with its provenance lines

compare_genomes(list(K12 = "k12.faa", O157 = "o157.faa"))
annotate_list(read.csv("taxa.csv"), genus_column = 1, species_column = 2)
pathway_analysis(Biostrings::readAAStringSet("prot.faa"), target_plastic = "PLA")
hmm_screen("prot.faa", target_polymers = c("PET", "PLA"))
tree <- job_results(wait_job(build_tree(plastic = "PET", genus = "Pseudomonas")))
tree$newick; tree$matrix          # tree$phylo with ape installed
search_sequence("MNFPRASRLMQAAVLGGLMAVSAAATAQTNPYARGPNPT")$hits

link <- job_share(job, expires_in_days = 30)
job_results(job_shared(link$url)) # anyone with the link; no key needed
```

Each submission carries an `Idempotency-Key` (pass `idempotency_key =` to
choose it), reused on every retry, so a retried or re-run submission returns
the first job (`job_replayed()`) instead of a duplicate.

Function names follow the Python client: `client.proteins.list()` is
`proteins_list()`, `client.evidence.interactions()` is
`evidence_interactions()`, and so on. The one exception is
`client.search()`, which is `search_hits()` here so as not to mask
`base::search()`.

### Lists, filters and tables

List functions fetch every page (500 records a request) unless `limit =`
caps them. Filters are the API's own query parameters, passed by name, with
the same meaning everywhere: `plastic`, `include_subtypes`, `microorganism`,
`polarity`, `include_negative`, `evidence_method`, `min_year`/`max_year`,
`updated_since`, `sort_by`/`sort_order`... An unknown name is a
`plasticdb_argument_error` that lists the valid ones; an unknown value is a
server 422 (`plasticdb_validation`). Entity lists default to records with a
**positive** result, as on the website.

Tables are tibbles: nested objects become `parent.child` columns, lists of
values become list columns, and per-plastic evidence becomes
`plastics_positive` / `plastics_negative` / `plastics_inconclusive`.
Single records (`*_get()`) are lists with the API's nesting.

### Configuration and reproducibility

Every function takes `client = plasticdb_client()`. Its settings come from
the argument, else an R option, else an environment variable:

| Argument | Option | Environment variable | Default |
|---|---|---|---|
| `api_key` | | `PLASTICDB_API_KEY` | none (reads are anonymous) |
| `release` | `plasticdb.release` | `PLASTICDB_RELEASE` | none (live database) |
| `base_url` | `plasticdb.base_url` | `PLASTICDB_BASE_URL` | `https://v2.plasticdb.org/api/v1` |
| `cache` | `plasticdb.cache` | `PLASTICDB_CACHE_DIR` | `TRUE` (`tools::R_user_dir("plasticdb", "cache")`) |
| `timeout`, `max_retries`, `max_retry_wait` | `plasticdb.timeout`, ... | | 30 s, 3, 60 s |

So `options(plasticdb.release = "2026.50")` pins a whole script, and
`plasticdb_client(release = "2026.30")` one call. A pinned read sends
`?release=` and is answered from that release's snapshot; `provenance(x)`
says what answered (release, source, URL, cache, total) and
`provenance()` adds the server and package versions. Answers from a past
release are cached in a BiocFileCache and reused; live answers are never
cached. Rate-limited requests (429) wait for `Retry-After`; 5xx answers and
connection errors are retried with backoff.

### Errors

Errors are classed conditions (`rlang::abort()`), mirroring the Python
exceptions: `plasticdb_not_found`, `plasticdb_gone`, `plasticdb_merged`
(field `merged_into`), `plasticdb_engineered_variant`,
`plasticdb_authentication`, `plasticdb_api_key_required`,
`plasticdb_job_failed`, `plasticdb_job_timeout`,
`plasticdb_results_expired`, `plasticdb_idempotency_key_reused`,
`plasticdb_payload_too_large`, `plasticdb_validation`, `plasticdb_rate_limit`
(`retry_after`), `plasticdb_server_error`, `plasticdb_checksum` and the
release errors (`plasticdb_release_unknown`, `plasticdb_release_live_only`,
`plasticdb_release_unsupported_filter`, ...). All are `plasticdb_error`. See
`?"plasticdb-conditions"`.

```r
tryCatch(proteins_get("PBGQPC"), plasticdb_merged = function(e) e$merged_into)
proteins_get("PBGQPC", follow_merges = TRUE)   # the survivor, with a warning
```

## Citation and licence

The code is MIT-licensed. **The PlasticDB data is licensed
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)**: cite the dataset
release you used, plus the PlasticDB paper. `cite()` returns both as a
`bibentry` (`toBibtex(cite())` for BibTeX):

> Gambarini V, Pantos O, Kingsbury JM, Weaver L, Handley KM, Lear G (2022).
> PlasticDB: a database of microorganisms and proteins linked to plastic
> biodegradation. *Database*, 2022:baac008. https://doi.org/10.1093/database/baac008

`citation("plasticdb")` cites this package.

## Provenance of the code

Assisted-by: Claude (Anthropic). Most of this package's code, tests and
documentation were written with the Claude Code assistant, directed and
reviewed by the maintainer, who is responsible for it. Commits it helped
write carry a `Co-Authored-By: Claude` trailer. No third-party code is copied
into the package; all of it is distributed under the package's MIT licence.

## Development

```r
roxygen2::roxygenise()
testthat::test_local()            # offline: httptest2 fixtures
rcmdcheck::rcmdcheck(args = "--as-cran")
BiocCheck::BiocCheck(`new-package` = TRUE)
```

`Rscript tools/capture-fixtures.R` re-captures the test fixtures from the
live anonymous API; `Rscript tools/make-job-fixtures.R` rewrites the
synthetic job fixtures (jobs need an API key) from the Python client's. `tests/testthat/contract/sdk-v1.json` is the server's SDK
contract (`/api/v1/openapi.sdk.json`); `test-contract.R` checks every call the
package makes against it.
