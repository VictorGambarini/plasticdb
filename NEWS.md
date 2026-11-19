# plasticdb (development)

# plasticdb 0.99.3

Adapted to server 2.90.0 (the contract is re-vendored).

* `job_results()` on an `hmm_screen` job gains the columns `profile_accession`
  (character), `domain_score`, `domain_c_evalue`, `domain_i_evalue` (double)
  and `hmm_from`, `hmm_to`, `env_from`, `env_to` (integer; 1-based, inclusive).
  They are `NA` on DIAMOND hits and on HMM jobs run before server 2.90.0.
* On HMM hits `pident`, `length`, `qcov` and `scov` are always `NA`, never 0,
  so identity and coverage filters do not apply to them. `evalue` and
  `bitscore` are the full-sequence values; `subject_seq_id` is the profile
  name.
* The HMM TSV download has 8 trailing columns after `calibration_status`; read
  it by header name.
* New HMM test fixtures: a hit with the new fields and an old-style row with
  all of them null.

* The README and package documentation state the code's provenance
  (AI-assisted), per Bioconductor's AI and third-party code policy.
* Security hardening: GitHub Actions are pinned to commit SHAs, Dependabot keeps
  them current, and `SECURITY.md` describes private vulnerability reporting.

# plasticdb 0.99.2

Adapted to server 2.87.1, 2.88.0 and 2.89.0 (the contract is re-vendored).

* **Behaviour change:** `proteins_get()` now asks for `view = "summary"` by
  default (the server's own default since 2.89.0, which the package used to
  override with `"full"`). A summary carries the record's fields and
  `counts` instead of the embedded `interactions`, `indirect_interactions`,
  `variants`, `kinetics` and `properties` lists; pass `view = "full"` for
  those, or read them as tibbles with `proteins_interactions()`,
  `proteins_variants()`, `proteins_kinetics()` and `proteins_properties()`.
  `as_aastringset()` works on either shape.
* `proteins_interactions()` and `evidence_interactions()` take
  `evidence_level` (`"all"`, `"direct"`, `"indirect"`) in place of `kind`,
  as the server and the new evidence lists name it. `kind` still works
  but is deprecated (a `plasticdb_deprecated` warning, once per session);
  only `evidence_level` is ever sent, and different values in both signal
  `plasticdb_argument_error`. Code that passed the level by position is
  unaffected.
* New top-level evidence lists, flat tibbles of the spine as the release
  tables have it: `interactions_list()`, `experiments_list()` and
  `observations_list()`, with `interactions_get()`, `experiments_get()` and
  `observations_get()` by id. Filters: `plastic`, `include_subtypes`,
  `plastic_id`, `microorganism`, `protein`, `evidence_level`, `paper_id`,
  `doi`, `polarity`, `evidence_method`, `substrate_form`,
  `measurement_type`, `has_quantitative`; experiments add `interaction_id`,
  observations `interaction_id` and `experiment_id`. All are pinnable.
* `proteins_list(include_sequence = TRUE)` (also `variants_list()`) adds a
  `sequence` column. `proteins_sequences()` and `variants_sequences()` use
  it, live and pinned, so they no longer download the FASTA file as well
  (they still fall back to it when the server does not return sequences).
  Each record's metadata no longer carries a `sequence` column in `mcols()`.
* `proteins_fasta()` answers a pinned release (server 2.87.1; it used to be
  live only).
* `build_tree()` needs an API key on the server since 2.88.0, as the
  package already required; the contract test now checks the contract has no
  anonymous option for it.

# plasticdb 0.99.1

* Jobs (R3), mirroring the Python client: `submit_job()` and the typed
  helpers `annotate_gene()`, `annotate_genome()`, `compare_genomes()`,
  `annotate_list()`, `pathway_analysis()` (`target_plastic`),
  `hmm_screen()` and `build_tree()`; the synchronous `search_sequence()`;
  `wait_job()`, `job_get()`, `job_status()`, `job_results()` (hits paged
  from `/jobs/{id}/results`, pathway steps, the annotated taxa table, or a
  tree as Newick text, an organism x plastic matrix and, with ape, a
  `phylo`), `job_comparison()`, `job_download()`, `job_share()`,
  `job_shares()`, `job_unshare()`, `job_shared()`, `jobs_list()`,
  `job_provenance()` and `job_replayed()`.
    * Inputs are file paths (plain or gzip), FASTA text, raw bytes,
      `Biostrings::AAStringSet`/`DNAStringSet`s or (taxa tables) data
      frames; each upload is named after its contents (`.gz` exactly when it
      is gzip).
    * Every submission carries an `Idempotency-Key` (`idempotency_key =`, or
      a new UUID), reused on each retry. With an API key, submissions are
      retried like reads (timeouts, connection errors, 5xx); without one a
      POST is resent only when it never reached the server, or on a 429.
      `Idempotent-Replayed` is reported by `job_replayed()`.
    * New conditions: `plasticdb_api_key_required` (before anything is
      sent), `plasticdb_job_error`, `plasticdb_job_failed`,
      `plasticdb_job_timeout`, `plasticdb_results_expired` (410 `expired`)
      and `plasticdb_idempotency_key_reused` (422).
* Adapted to server 2.86 and 2.87:
    * Record redirects are told apart by `reason`: `engineered_variant` or
      `merged` (with `merged_into`; `kind` may be `"microorganism"`, so
      `microorganisms_get(follow_merges = TRUE)` follows organism merges).
      A variant's accession on a per-protein table is a 404
      `engineered_variant`, now also `plasticdb_not_found`. Withdrawn records
      (and variants of a withdrawn protein) are `plasticdb_gone` with
      `reason` `"withdrawn"` and `accession`. Every condition has `reason`.
    * `proteins_fasta()` and `proteins_structure()` given a variant's
      accession return the variant's; `proteins_get()` takes a numeric id.
    * `variants_get()` and `variants_fasta()` answer a pinned release.
    * `evidence_scope = "enzyme_only"` on `microorganisms_get()` and
      `stats_summary()`.
    * Every search `route_key` is a string.
    * The dataset ZIP's 503 is JSON (`reason` `"archive_unavailable"`); a
      sequence search's 504 `timeout` is not retried.
    * A cached pinned download is revalidated with its `ETag`
      (`If-None-Match`; a 304, or an unreachable server, uses the cached
      copy). Release artefacts are also checked against
      `X-PlasticDB-SHA256`. `last_response()` reports `etag`, `sha256`,
      `source_sha256` and `replayed`.
    * The SDK contract is re-vendored and the fixtures re-captured from
      2.87.0.
* A second vignette, *Reproducible analyses with a pinned release*.

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
