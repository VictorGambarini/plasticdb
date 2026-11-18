# plasticdb 0.99.0

* First version, for submission to Bioconductor. Mirrors the Python client
  (`plasticdb-py`) phases P1 and P2:
    * `plasticdb_client()` with the `plasticdb.*` options and the
      `PLASTICDB_API_KEY`, `PLASTICDB_RELEASE`, `PLASTICDB_BASE_URL` and
      `PLASTICDB_CACHE_DIR` environment variables; retries that honour
      `Retry-After`; classed conditions (`?"plasticdb-conditions"`).
    * Release pinning with `?release=`, provenance on every result
      (`provenance()`), `cite()` as a `bibentry`, and a BiocFileCache of
      answers from past releases (live answers are never cached).
    * Every public data endpoint: proteins, variants, microorganisms,
      plastics, papers, the evidence tables, search, pathways, statistics,
      the change feed, downloads and releases. Tables are tibbles; sequences
      are `Biostrings::AAStringSet`s.
* Jobs (annotation, genome comparison, pathway analysis, HMM screens,
  taxonomy trees) are not included yet.
