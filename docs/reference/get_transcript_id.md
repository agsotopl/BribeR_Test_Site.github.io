# Retrieve Available Transcript IDs

Lists all available transcript IDs based on the `.csv` files stored in
the `data-raw/transcripts` folder of the **BribeR** package.

## Usage

``` r
get_transcript_id()
```

## Value

A sorted numeric vector of available transcript IDs.

## Details

Each transcript is named numerically (e.g., `1.csv`, `19.csv`,
`104.csv`), and its ID corresponds directly to that number.

## See also

[`read_transcripts()`](https://agsotopl.github.io/BribeR_Test_Site/reference/read_transcripts.md),
[`get_transcripts_raw()`](https://agsotopl.github.io/BribeR_Test_Site/reference/get_transcripts_raw.md),
[`get_transcript_speakers()`](https://agsotopl.github.io/BribeR_Test_Site/reference/get_transcript_speakers.md)

## Examples

``` r
# Retrieve all available transcript IDs
ids <- get_transcript_id()
#> Error: Transcript directory not found. Is the BribeR package installed correctly?
ids
#> Error: object 'ids' not found

# Use those IDs to load specific transcripts
subset <- get_transcripts_raw(n = ids[1:3])
#> Error: Transcript directory not found. Is the BribeR package installed correctly?
```
