# tanager-data-harmonize
Data harmonization and cleaning package for the Tanager data system. 

# Data Harmonize

This internal R package provides harmonization, cleaning, and QA/QC tools for standardizing incoming data. It includes metadata dictionaries and leverages large language models via the `ellmer` package to support variable alignment and validation.

## Key Features

- Variable name and value harmonization
- QA/QC checks (assertr, validate)
- Metadata management using CSV dictionaries
- Optional LLM support for ambiguous mappings

## Installation

```r
# From local development
devtools::load_all("path/to/data-harmonize")

## Repository Structure

data-harmonize/
├── R/
│   ├── harmonize.R
│   ├── metadata_helpers.R
│   └── qa_qc.R
├── inst/metadata/
│   ├── variables.csv
│   └── values.csv
├── DESCRIPTION
└── README.md

## Dependencies
`assertr`, `validate`, `tidyverse`, `ellmer`

## Contribution
If you’re proposing changes to harmonization logic, please include:

Example before/after harmonized values

Test data or edge cases
