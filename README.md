# BribeR

<!-- badges: start -->
<!-- badges: end -->

**BribeR** distributes curated and standardized transcript corpora with integrated metadata for political and institutional research. The datasets include structured transcript text, topic annotations, and speaker identifiers formatted to support reproducible qualitative, quantitative, and network-based analysis.

The package provides convenient access to transcript data such as the Vladivideos corpus — a collection of secretly recorded conversations involving Vladimiro Montesinos, Peru's former intelligence chief, that document corruption, bribery, and political manipulation during the Fujimori era.

## Installation

```r
# When available on CRAN
# install.packages("BribeR")

# For now, install the development version from GitHub
# install.packages("remotes")
remotes::install_github("agsotopl/BribeR_Test_Site")
```

## Overview

The package currently includes 3 main functions to read and explore transcript data:

| Function                         | Description                                    |
|:---------------------------------|:-----------------------------------------------|
| `read_transcripts()`             | Read standardized transcript datasets from `.rda` files |
| `read_transcript_meta_data()`    | Build a tidy metadata table (dates, speakers, topics, word counts) |
| `run_transcript_network_app()`   | Launch an interactive Shiny network visualizer  |

**{BribeR}** also includes 3 support functions that allow users to access the raw transcript data directly:

| Function                         | Description                                    |
|:---------------------------------|:-----------------------------------------------|
| `get_transcript_id()`            | List all available transcript IDs              |
| `get_transcript_speakers()`      | Retrieve the speaker roster for each transcript |
| `get_transcripts_raw()`          | Load individual raw transcript `.csv` files     |

The syntax of all **{BribeR}** functions follows a consistent logic, making it intuitive to load any dataset with a single line of code.

## Basic usage

First, load the library:

```r
library(BribeR)
```

### Reading transcripts

The `read_transcripts()` function loads pre-processed `.rda` files containing compiled transcript data. It supports loading from local file paths, remote URLs (e.g., GitHub raw links), or an installed package's `data/` directory.

```r
# Load the full Vladivideos transcript dataset
all_transcripts <- read_transcripts("vladivideos_transcripts", package = "BribeR")

head(all_transcripts)
#> # A tibble: 6 × 5
#>       n row_id speaker         speech                              speaker_std
#>   <dbl>  <dbl> <chr>           <chr>                               <chr>
#> 1     1      1 Montesinos      Buenos días, tome asiento...        montesinos
#> 2     1      2 Speaker 2       Gracias, doctor...                  speaker_2
#> ...
```

You can also filter to specific transcripts by ID:

```r
# Retrieve only transcript 1
t1 <- read_transcripts("vladivideos_transcripts", package = "BribeR", transcripts = 1)

# Retrieve transcripts 5, 7, and 13
subset_transcripts <- read_transcripts(
  "vladivideos_transcripts",
  package = "BribeR",
  transcripts = c(5, 7, 13)
)
```

### Exploring transcript metadata

The `read_transcript_meta_data()` function builds a tidy metadata table from multiple source files, combining transcript descriptions, speaker rosters, topic annotations, and word counts into a single tibble.

```r
meta <- read_transcript_meta_data(
  descriptions_path = "data-raw/Inventory & Descriptions/Descriptions.csv",
  speakers_per_transcript_path = "data-raw/Inventory & Descriptions/speakers per transcript.csv",
  transcripts_dir = "data-raw/transcripts",
  recursive = TRUE
)

head(meta)
#> # A tibble: 6 × 5
#>   n     date       speakers   n_words topics
#>   <chr> <chr>      <list>       <int> <list>
#> 1 1     2000-01-15 <chr [3]>     1245 <chr [2]>
#> 2 2     2000-01-20 <chr [2]>      893 <chr [1]>
#> ...
```

Each row represents one transcript, with:

- **`n`**: the transcript identifier
- **`date`**: date associated with the transcript
- **`speakers`**: a list-column of unique, sorted speaker names
- **`n_words`**: total word count across all speech turns
- **`topics`**: a list-column of topic labels derived from topic flags

### Accessing raw transcripts

The `get` family of functions provides direct access to the underlying raw data:

```r
# List all available transcript IDs
ids <- get_transcript_id()
ids
#> [1]   1   2   3   5   7  12  13  19  ...

# Load a specific raw transcript
t3 <- get_transcripts_raw(n = 3)

# Load multiple and combine into one tibble
combined <- get_transcripts_raw(n = c(3, 19, 104), combine = TRUE)
head(combined)

# Get the speaker roster per transcript
speakers <- get_transcript_speakers()
speakers
#> # A tibble: … × 2
#>   n     speakers
#>   <chr> <list>
#> 1 1     <chr [3]>
#> 2 2     <chr [2]>
#> ...
```

### Interactive network visualization

BribeR includes a Shiny application that visualizes the relationships between speakers and topics across the transcript corpus. The app provides two network views:

- **Speaker–Topic Network**: connects speakers to the topics discussed in their transcripts
- **Speaker Co-Appearance Network**: connects speakers who appear in the same transcript

```r
# Launch the interactive network app
run_transcript_network_app()
```

Speakers are color-coded by type (e.g., Congress, Security, Judiciary, Media, Illicit) and sized by the number of transcripts they appear in. Hovering over nodes reveals detailed metadata including speaker position, type, and transcript count.

## Data structure

Transcript data loaded via `read_transcripts()` follows a standardized structure:

| Column        | Description                                  |
|:------------- |:---------------------------------------------|
| `n`           | Transcript identifier (numeric)              |
| `row_id`      | Row number within each transcript            |
| `speaker`     | Original speaker name as transcribed         |
| `speech`      | The text content of the speech turn          |
| `speaker_std` | Standardized (lowercase) speaker identifier  |

This consistent format supports direct use with standard tidyverse workflows for text analysis, speaker comparison, and corpus-level aggregation.

## Citation

If you use **BribeR** in published research, please cite it:

```
Soto Plaza, Andres (2025). BribeR: Package for Understanding Corruption Transcripts.
R package version 0.0.0.9000. https://github.com/agsotopl/BribeR_Test_Site
```

## Acknowledgements

Original transcript data was compiled and standardized as part of research into political corruption and institutional accountability. The Vladivideos corpus documents secretly recorded conversations that were central to understanding corruption networks during Peru's Fujimori administration.



