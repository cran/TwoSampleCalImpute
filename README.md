# TwoSampleCalImpute

## Two-Sample Calibrated Imputation for Survey Data

TwoSampleCalImpute provides computational tools for preparing,
validating, cleaning, and exporting survey data when auxiliary
information is available from a separate reference sample.

The package currently provides functions for standardized data
import, basic data-integrity validation, controlled data cleaning,
and CSV export. These utilities support data preparation for
Two-Sample Calibrated Imputation workflows.

## Installation

You can install the development version from GitHub using the `remotes` package:

```r
install.packages("remotes")
remotes::install_github("Kamun2026/TwoSampleCalImpute")
```

After installation, load the package with:

```r
library(TwoSampleCalImpute)
```

## Basic Usage

### 1. Prepare and import survey data

Create a small example of primary-sample data and import it:

```r
primary_data <- data.frame(
  id = 1:5,
  age = c(25, 32, 41, 29, 36),
  outcome = c(1, 0, 1, 1, 0)
)

primary <- tsci_import_data(
  primary_data,
  sample_type = "primary",
  id = "id"
)
print(primary)
```

### 2. Validate the data

Check data integrity, missing values, and duplicate record identifiers:

```r
validation <- tsci_validate_data(
  primary_data,
  data_name = "primary sample",
  key = "id"
)
validation$valid
```

For the example above, validation returns `TRUE`.

### 3. Clean the data

Import the data and remove completely empty rows and columns when requested:

```r
cleaned <- tsci_clean_data(
  tsci_import_data(primary_data, sample_type = "primary", id = "id"),
  remove_empty_rows = TRUE,
  remove_empty_columns = TRUE
)
print(cleaned)
```

### 4. Export the data

Export the cleaned data to a CSV file:

```r
export_file <- file.path(tempdir(), "example_export.csv")
tsci_export_data(cleaned, file = export_file)
file.exists(export_file)
```

The export path can be changed to a suitable location when working with real survey data.
