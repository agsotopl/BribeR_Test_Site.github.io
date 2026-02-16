# Read Vladivideos Transcript Data

Loads the bundled `vladivideos_detailed` dataset and optionally filters
by transcript ID(s).

## Usage

``` r
read_transcripts(transcripts = NULL)
```

## Arguments

- transcripts:

  Optional numeric vector of transcript IDs (`n`) to keep. If `NULL`
  (the default), all transcripts are returned.

## Value

A data frame with columns `n`, `row_id`, `speaker`, `speech`, and
`speaker_std`.

## See also

[`get_transcripts_raw()`](https://agsotopl.github.io/BribeR_Test_Site/reference/get_transcripts_raw.md),
[`get_transcript_id()`](https://agsotopl.github.io/BribeR_Test_Site/reference/get_transcript_id.md),
[`get_transcript_speakers()`](https://agsotopl.github.io/BribeR_Test_Site/reference/get_transcript_speakers.md)

## Examples

``` r
# Load all transcripts
all <- read_transcripts()

# Load only transcript 1
t1 <- read_transcripts(transcripts = 1)

# Load transcripts 5, 7, and 13
subset <- read_transcripts(transcripts = c(5, 7, 13))
```
