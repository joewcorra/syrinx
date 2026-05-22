# syrinx

Data harmonization and cleaning package for greenhouse gas emissions inventories.

This internal R package provides tools for standardizing incoming data: dictionary-based
validation, LLM-assisted value correction, canonical unit conversion, and metadata
labeling.

## Key Functions

| Function | Purpose |
|---|---|
| `pre_clean()` | Standardize column names, remove empty rows/cols, squish whitespace |
| `use_dictionary()` | Validate column names and values against the data dictionary |
| `llm_dictionary()` | Use a Claude LLM to suggest corrections for dictionary discrepancies |
| `apply_labels()` | Apply human-readable variable labels from the data dictionary |
| `to_canonical()` | Convert a single column from a raw unit to its canonical unit |
| `to_canonical_all()` | Batch-convert multiple columns using a named unit map |

## Installation

```r
# From local development
devtools::load_all("path/to/syrinx")
```

## Repository Structure

```
syrinx/
├── R/
│   ├── pre_clean.R
│   ├── use_dictionary.R
│   ├── llm_dictionary.R
│   ├── apply_metadata_labels.R
│   ├── to_canonical.R
├── inst/extdata/
│   ├── data_dictionary_variables.csv
│   ├── data_dictionary_values.csv
│   └── canonical_units.csv
├── man/
├── DESCRIPTION
├── NAMESPACE
└── README.md
```

## Dependencies

`dplyr`, `ellmer`, `forcats`, `janitor`, `jsonlite`, `labelled`, `purrr`,
`readr`, `rlang`, `stringr`, `tibble`, `tidyr`

## Contribution

If you are proposing changes to harmonization logic, please include example
before/after values and any relevant test data or edge cases.
