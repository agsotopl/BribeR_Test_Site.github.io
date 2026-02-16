# Corpus Overview

## Historical Context

In September 2000, a series of secretly recorded videos surfaced in Peru
showing Vladimiro Montesinos — head of the National Intelligence Service
(SIN) under President Alberto Fujimori — bribing, coercing, and
negotiating with politicians, military officials, judges, media
executives, and businesspeople. These recordings, widely known as the
**Vladivideos**, exposed the systematic corruption at the heart of the
Fujimori regime and ultimately led to the collapse of his government.

The Vladivideos corpus represents one of the most extensive documented
records of institutional corruption in Latin American history. The
transcripts capture conversations spanning topics from election
manipulation and media control to military promotions and judicial
interference.

**{BribeR}** packages a curated and standardized version of this corpus,
making it accessible for reproducible political science, network
analysis, and text-based research.

------------------------------------------------------------------------

## Bundled Datasets

BribeR ships with seven `.rda` datasets in its `data/` directory. These
can be accessed directly through the package’s functions or loaded
manually.

### vladivideos_detailed

The core dataset: **47,375 rows** of transcribed speech across **104
transcripts**, loaded via
[`read_transcripts()`](https://agsotopl.github.io/BribeR_Test_Site/reference/read_transcripts.md).

``` r
df <- read_transcripts()
dim(df)
#> [1] 47375     7
head(df)
#> # A tibble: 6 × 7
#>   speaker              speech               speaker_std     n row_id date  topic
#>   <chr>                <chr>                <chr>       <dbl>  <int> <chr> <chr>
#> 1 BACKGROUND           ﻿Declaraciones de V…  BACKGROUND      1      1 3/25… topi…
#> 2 BACKGROUND           [La entrevista se r… BACKGROUND      1      2 3/25… topi…
#> 3 La señora            Levante su mano der… ALVA            1      3 3/25… topi…
#> 4 El señor Javier Alva Sí.                  ALVA            1      4 3/25… topi…
#> 5 El señor Neil Lewis  Señor Alva, mi nomb… LEWIS           1      5 3/25… topi…
#> 6 El señor Javier Alva Javier Alva Orlandi… ALVA            1      6 3/25… topi…
```

| Column | Type | Description |
|:---|:---|:---|
| `speaker` | character | Original speaker name as it appears in the transcript |
| `speech` | character | The text content of the speech turn |
| `speaker_std` | character | Standardized speaker identifier (uppercase) |
| `n` | numeric | Transcript identifier |
| `row_id` | integer | Row number within each transcript |
| `date` | character | Date of the recorded conversation |
| `topic` | character | Topic classification for the transcript |

------------------------------------------------------------------------

### descriptions

Transcript-level metadata for all **104 transcripts**, including dates,
topic flags, and summary information.

``` r
env <- new.env()
load(system.file("data", "descriptions.rda", package = "BribeR"), envir = env)
descriptions <- env$descriptions
dim(descriptions)
#> [1] 104  24
names(descriptions)
#>  [1] "n"                       "date"                   
#>  [3] "speakers"                "original_n"             
#>  [5] "Missing Topic"           "in_book"                
#>  [7] "in_online_archive"       "type"                   
#>  [9] "topic_referendum"        "topic_ecuador"          
#> [11] "topic_lucchetti_factory" "topic_municipal98"      
#> [13] "topic_reelection"        "topic_miraflores"       
#> [15] "topic_canal4"            "topic_media"            
#> [17] "topic_promotions"        "topic_ivcher"           
#> [19] "topic_foreign"           "topic_wiese"            
#> [21] "topic_public_officials"  "topic_safety"           
#> [23] "topic_state_capture"     "summary"
```

Key columns include:

- **`n`**: transcript identifier
- **`date`**: date of the conversation
- **`speakers`**: number of speakers present
- **`topic_*` columns**: binary flags (marked with `"x"`) indicating
  which topics are discussed in each transcript, covering areas such as
  `referendum`, `media`, `reelection`, `state_capture`, and others

------------------------------------------------------------------------

### speakers_per_transcript

A wide-format table (**101 rows × 20 columns**) mapping each transcript
to its speakers, accessed via
[`get_transcript_speakers()`](https://agsotopl.github.io/BribeR_Test_Site/reference/get_transcript_speakers.md).

``` r
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

Each transcript `n` is associated with up to 19 standardized speaker
identifiers stored in columns `speakrer_std_1` through
`speakrer_std_19`.

------------------------------------------------------------------------

### actors

Information on **125 individuals** who appear across the corpus,
including their political role, institutional type, and party
affiliation.

``` r
env <- new.env()
load(system.file("data", "actors.rda", package = "BribeR"), envir = env)
actors <- env$actors
dim(actors)
#> [1] 125   7
head(actors[, c("speaker", "speaker_std", "Type", "Position")])
#> # A tibble: 6 × 4
#>   speaker                         speaker_std Type             Position         
#>   <chr>                           <chr>       <chr>            <chr>            
#> 1 Vladimir Montesinos             MONTESINOS  MONTESINOS       Alberto Fujimori…
#> 2 Desconocido                     DESCONOCIDO NA               NA               
#> 3 Alexander Martin Kouri Bumachar KOURI       Congress         Elected Constitu…
#> 4 Lucchetti                       LUCCHETTI   Businessperson   Company speciali…
#> 5 Carlos Eduardo Ferrero Costa    FERRERO     Congress         Congressman (199…
#> 6 Alberto Fujimori                FUJIMORI    Elected Official President of Per…
```

| Column | Description |
|:---|:---|
| `speaker` | Full name as it appears in transcripts |
| `speaker_std` | Standardized identifier |
| `Type` | Institutional category (e.g., Congress, Security, Media, Judiciary, Illicit) |
| `Position` | Specific role or office held |
| `Party` | Political party affiliation (if applicable) |
| `notes` | Additional contextual notes |

------------------------------------------------------------------------

### actors_description

Short biographical descriptions for **79 actors**, keyed by
`speaker_std`.

``` r
env <- new.env()
load(system.file("data", "actors_description.rda", package = "BribeR"), envir = env)
actors_desc <- env$actors_description
dim(actors_desc)
#> [1] 79  2
head(actors_desc)
#> # A tibble: 6 × 2
#>   speaker_std description                                                       
#>   <chr>       <chr>                                                             
#> 1 MONTESINOS  Fujimori's Chief of Staff                                         
#> 2 DESCONOCIDO NA                                                                
#> 3 KOURI       Elected Constituent Congressman (1992 - 1995) for the Partido Pop…
#> 4 LUCCHETTI   Company specialized in pasta manufacturing                        
#> 5 FERRERO     Congressman (1995-2000)                                           
#> 6 FUJIMORI    President of Peru (1990-2000)
```

| Column        | Description                     |
|:--------------|:--------------------------------|
| `speaker_std` | Standardized speaker identifier |
| `description` | Brief biographical description  |

------------------------------------------------------------------------

### topic_descriptions

Descriptions for all **15 topics** used to classify transcripts in the
corpus.

``` r
env <- new.env()
load(system.file("data", "topic_descriptions.rda", package = "BribeR"), envir = env)
topic_desc <- env$topic_descriptions
topic_desc
#> # A tibble: 15 × 2
#>    topics                  descriptions                                         
#>    <chr>                   <chr>                                                
#>  1 topic_referendum        Referendum to Preuvian consitution that was supporte…
#>  2 topic_ecuador           Ensuring end to Peru-Ecuador war, while maintaining …
#>  3 topic_reelection        Extended discussions involving ensured reelection of…
#>  4 topic_media             Key transcripts highlighting control that Montesinos…
#>  5 topic_foreign           Conversations regarding Peru’s relations with foreig…
#>  6 topic_safety            Discussions on internal armed conflict, drug traffic…
#>  7 topic_lucchetti_factory Legal battle over whether Lucchetti Pasta could lega…
#>  8 topic_municipal98       Discussions involving Alex Kouri, Alberto Andrade an…
#>  9 topic_miraflores        Discussions about the elections for mayor in Lima an…
#> 10 topic_canal4            Key transcripts highlighting control that Montesinos…
#> 11 topic_promotions        Promotions being granted to different members of the…
#> 12 topic_ivcher            Conversations regarding Ivcher providing information…
#> 13 topic_wiese             Conversations Montesinos had with members of Wiese B…
#> 14 topic_public_officials  Conversations in which Montesinos reassigns, plans, …
#> 15 topic_state_capture     Discussions about capturing Congress, military bodie…
```

| Column         | Description                                                |
|:---------------|:-----------------------------------------------------------|
| `topics`       | Topic identifier (e.g., `topic_media`, `topic_reelection`) |
| `descriptions` | Human-readable description of the topic                    |

The 15 topics span a range of political and institutional themes,
including media control, election interference, foreign relations,
judicial manipulation, and state capture.

------------------------------------------------------------------------

### transcript_index

A comprehensive cross-reference matrix (**101 rows × 134 columns**)
linking each transcript to its topics and speakers in a wide binary
format.

``` r
env <- new.env()
load(system.file("data", "transcript_index.rda", package = "BribeR"), envir = env)
transcript_index <- env$transcript_index
dim(transcript_index)
#> [1] 101 134
names(transcript_index)[1:20]
#>  [1] "n"                       "file"                   
#>  [3] "format"                  "date"                   
#>  [5] "topic_referendum"        "topic_ecuador"          
#>  [7] "topic_lucchetti_factory" "topic_municipal98"      
#>  [9] "topic_reelection"        "topic_miraflores"       
#> [11] "topic_canal4"            "topic_media"            
#> [13] "topic_promotions"        "topic_ivcher"           
#> [15] "topic_foreign"           "topic_wiese"            
#> [17] "topic_public_officials"  "topic_safety"           
#> [19] "topic_state_capture"     "speaker_alva"
```

This dataset contains:

- **`n`**: transcript identifier
- **`file`**, **`format`**, **`date`**: file-level metadata
- **`topic_*` columns**: binary topic indicators
- **`speaker_*` columns**: binary speaker presence indicators
- **`topic_count`**, **`speaker_count`**: summary counts

This matrix is useful for quickly filtering or cross-tabulating
transcripts by any combination of topic and speaker.

------------------------------------------------------------------------

## Metadata via `read_transcript_meta_data()`

The
[`read_transcript_meta_data()`](https://agsotopl.github.io/BribeR_Test_Site/reference/read_transcript_meta_data.md)
function assembles a tidy summary from the bundled datasets:

``` r
meta <- read_transcript_meta_data()
head(meta)
#> # A tibble: 6 × 5
#>   n     date      speakers   n_words topics   
#>   <chr> <chr>     <list>       <int> <list>   
#> 1 104   7/1/2000  <chr [2]>     9289 <chr [2]>
#> 2 19    4/21/1998 <chr [11]>    5693 <chr [2]>
#> 3 11    2/10/1998 <chr [3]>     9420 <chr [1]>
#> 4 12    2/10/1998 <chr [2]>     1283 <chr [1]>
#> 5 5     1/8/1998  <chr [5]>     9384 <chr [2]>
#> 6 7     1/15/1998 <chr [6]>    11146 <chr [3]>
```

Each row represents one transcript with:

- **`n`**: transcript identifier
- **`date`**: date of the conversation
- **`speakers`**: list-column of unique speaker names
- **`n_words`**: total word count derived from the `speech` column
- **`topics`**: list-column of topic labels

------------------------------------------------------------------------

## Transcript Structure

All transcripts follow a turn-based structure where each row represents
a single speech turn. The standardized format supports direct use with
tidyverse workflows:

``` r
# Example: who speaks the most in transcript 6?
read_transcripts(transcripts = 6) |>
  filter(speaker_std != "BACKGROUND") |>
  count(speaker_std, sort = TRUE)
#> # A tibble: 2 × 2
#>   speaker_std     n
#>   <chr>       <int>
#> 1 MONTESINOS    442
#> 2 RICKETTS      428
```

``` r
# Example: how many words per speaker in transcript 6?
read_transcripts(transcripts = 6) |>
  filter(speaker_std != "BACKGROUND") |>
  mutate(word_count = stringr::str_count(speech, "\\S+")) |>
  group_by(speaker_std) |>
  summarise(total_words = sum(word_count, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(total_words))
#> # A tibble: 2 × 2
#>   speaker_std total_words
#>   <chr>             <int>
#> 1 MONTESINOS         9701
#> 2 RICKETTS           3105
```

------------------------------------------------------------------------

## Topic Coverage

The corpus covers **15 distinct topics**. You can quickly see how many
transcripts discuss each topic:

``` r
meta <- read_transcript_meta_data()

# Unnest topics and count
meta |>
  tidyr::unnest(topics) |>
  count(topics, sort = TRUE)
#> # A tibble: 15 × 2
#>    topics                n
#>    <chr>             <int>
#>  1 media                38
#>  2 reelection           26
#>  3 safety               20
#>  4 state capture        19
#>  5 foreign              12
#>  6 canal4               11
#>  7 public officials      9
#>  8 ivcher                8
#>  9 referendum            8
#> 10 ecuador               7
#> 11 promotions            7
#> 12 lucchetti factory     5
#> 13 wiese                 5
#> 14 municipal98           4
#> 15 miraflores            3
```
