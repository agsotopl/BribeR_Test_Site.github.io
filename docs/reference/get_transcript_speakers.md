# Get speakers present in each transcript

Loads the bundled `speakers_per_transcript` dataset and returns one row
per transcript `n` with a list-column of unique speakers present in that
transcript.

## Usage

``` r
get_transcript_speakers()
```

## Value

A tibble with columns:

- `n` (character): transcript id

- `speakers` (list): unique, sorted character vector of speakers for
  that transcript

## See also

[`read_transcripts()`](https://agsotopl.github.io/BribeR_Test_Site/reference/read_transcripts.md),
[`get_transcript_id()`](https://agsotopl.github.io/BribeR_Test_Site/reference/get_transcript_id.md),
[`get_transcripts_raw()`](https://agsotopl.github.io/BribeR_Test_Site/reference/get_transcripts_raw.md)

## Examples

``` r
# Load all unique speakers in each transcript
speakers <- get_transcript_speakers()
head(speakers)
#> # A tibble: 6 × 2
#>   n     speakers 
#>   <chr> <list>   
#> 1 1     <chr [4]>
#> 2 10    <chr [8]>
#> 3 100   <chr [2]>
#> 4 101   <chr [4]>
#> 5 102   <chr [2]>
#> 6 103   <chr [2]>
```
