# Introduction to BribeR

**{BribeR}** is an R package for working with structured transcript
corpora and associated metadata. It provides functions to read, explore,
and analyze transcripts from political and institutional datasets, with
consistent formatting and integrated metadata to support reproducible
workflows.

------------------------------------------------------------------------

## Installation

``` r
# Install the released version from CRAN (if available)
install.packages("BribeR")

# Or install the development version from GitHub
remotes::install_github("agsotopl/BribeR")
```

------------------------------------------------------------------------

## Basic Usage

The package currently includes 3 main functions:

| Function | Description |
|:---|:---|
| [`read_transcripts()`](https://agsotopl.github.io/BribeR_Test_Site/reference/read_transcripts.md) | Read standardized transcript datasets |
| [`read_transcript_meta_data()`](https://agsotopl.github.io/BribeR_Test_Site/reference/read_transcript_meta_data.md) | Extract metadata for transcripts |
| [`run_transcript_network_app()`](https://agsotopl.github.io/BribeR_Test_Site/reference/run_transcript_network_app.md) | Launch interactive network visualizer |

The package also contains 3 support functions that allow users to access
the raw transcript data directly:

| Function | Description |
|:---|:---|
| [`get_transcript_id()`](https://agsotopl.github.io/BribeR_Test_Site/reference/get_transcript_id.md) | Get unique transcript IDs |
| [`get_transcript_speakers()`](https://agsotopl.github.io/BribeR_Test_Site/reference/get_transcript_speakers.md) | List speakers for a transcript |
| [`get_transcripts_raw()`](https://agsotopl.github.io/BribeR_Test_Site/reference/get_transcripts_raw.md) | Load raw transcript text |

Example:

``` r
meta <- read_transcript_meta_data()
head(meta, 6)
```

***Note:*** The `get` functions operate separately from the main
functions, in that they access the raw data. The main functions are
meant to access compilation RDA’s.

Example:

``` r
transcript_5 <- get_transcripts_raw(n = 5, combine = FALSE, package = "bribeR")

head(transcript_1)
```
