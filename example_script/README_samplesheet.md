# RNA-seq Samplesheet Generator

## Overview

This script generates a samplesheet for the nf-core/rnaseq pipeline from a Synapse dataset.

**Related Issue:** #236 - Samplesheet convenience script

## Requirements

### Input Dataset Annotations

The input dataset must have the following annotations on its files:

**Required:**
- `specimenID`: Unique identifier for each specimen/sample
- `fileFormat`: File format (should be `fastq`, `fq`, `fastq.gz`, or `fq.gz`)
- `readPair`: Read pair indicator (`1` for R1, `2` for R2, or `NA` for single-end)

**Recommended:**
- `libraryStrand`: Strandedness of the library prep (`forward`, `reverse`, `unstranded`, or `auto`)
- `isMultiSpecimen`: Whether the file contains data from multiple specimens

### Samplesheet Format

The script generates a CSV file with the following columns conforming to the [nf-core/rnaseq input schema](https://raw.githubusercontent.com/nf-core/rnaseq/master/assets/schema_input.json):

| Column | Description | Required |
|--------|-------------|----------|
| `sample` | Sample identifier (maps to `specimenID`) - must not contain spaces | Yes |
| `fastq_1` | Synapse URI for Read 1 FASTQ file (`syn://synXXXXXXXX`) | Yes |
| `fastq_2` | Synapse URI for Read 2 FASTQ file (empty string for single-end) | No |
| `strandedness` | Library strandedness (`auto`, `forward`, `reverse`, or `unstranded`) | Yes |

**Note:** The official nf-core/rnaseq schema expects file paths with `.fq.gz` or `.fastq.gz` extensions. This script outputs Synapse URIs in the format `syn://synXXXXXXXX`.

### Example Output

```csv
sample,fastq_1,fastq_2,strandedness
NF0017-T2-organoids,syn://syn70079144,syn://syn70074497,auto
NF0017-T2,syn://syn70078584,syn://syn70074500,auto
NF0017-T5-organoids,syn://syn70074503,syn://syn70074506,auto
NF0018-T2,syn://syn70074511,,auto
```

## Usage

### Configuration

Edit the script to set your dataset and destination:

```r
# Input dataset with RNA-seq FASTQ files
INPUT_DATASET <- "syn70366294"

# Destination folder for samplesheet upload
OUTPUT_DESTINATION <- "syn70366350"

# Output filename
SAMPLESHEET_FILENAME <- "samplesheet.csv"
```

### Running the Script

```bash
# Make sure nfportalutils is installed and loaded
Rscript example_script/generate_rnaseq_samplesheet.R
```

Or in an R session:

```r
source("example_script/generate_rnaseq_samplesheet.R")
main()
```

## Handling Different Data Scenarios

### Paired-End Data

Files with `readPair` values of `1` and `2` will be matched by `specimenID`:

```r
# Input files:
# specimenID: sample1, readPair: 1, entityId: syn123
# specimenID: sample1, readPair: 2, entityId: syn456

# Output row:
sample1,syn://syn123,syn://syn456,auto
```

### Single-End Data

Files with only `readPair` value of `1` (or no R2 match):

```r
# Input file:
# specimenID: sample2, readPair: 1, entityId: syn789

# Output row:
sample2,syn://syn789,,auto
```

### Technical Replicates

Multiple files with the same `specimenID` will create multiple rows. The nf-core/rnaseq pipeline will automatically merge these technical replicates:

```r
# Input files:
# specimenID: sample3, readPair: 1, entityId: syn111
# specimenID: sample3, readPair: 2, entityId: syn222
# specimenID: sample3, readPair: 1, entityId: syn333
# specimenID: sample3, readPair: 2, entityId: syn444

# Output rows:
sample3,syn://syn111,syn://syn222,auto
sample3,syn://syn333,syn://syn444,auto
```

## Troubleshooting

### Missing Required Annotations

If required annotations are missing, you'll see an error:
```
Error: Missing required annotations: readPair
```

**Solution:** Add the missing annotations to your dataset files before running the script.

### Unpaired Files

If specimens have R2 but no R1, you'll see a warning:
```
Warning: Found specimens with only R2 reads: sample_xyz
```

**Solution:** Verify your data and ensure R1 files are present and annotated correctly.

### Invalid File Formats

If unexpected file formats are detected:
```
Warning: Unexpected file formats found: bam
```

**Solution:** Ensure only FASTQ files are included in the input dataset.

## Using the Samplesheet with nf-core/rnaseq

Once generated and uploaded, download the samplesheet and use it with the pipeline:

```bash
nextflow run nf-core/rnaseq \
  --input samplesheet.csv \
  --outdir <OUTDIR> \
  --genome GRCh38 \
  -profile <docker/singularity/.../institute>
```

See the [nf-core/rnaseq documentation](https://nf-co.re/rnaseq/3.11.2/usage) for more details.

## Schema Information

### Modified Sage-specific Schema

We use a known modification of the [official nf-core/rnaseq input schema](https://raw.githubusercontent.com/nf-core/rnaseq/master/assets/schema_input.json) with the following key differences:

**Original nf-core schema requirements:**
- `fastq_1` and `fastq_2` must be file paths with `.fq.gz` or `.fastq.gz` extensions
- Files must exist on the filesystem

**Modified Sage schema allowances:**
- Accepts Synapse URIs in the format `syn://synXXXXXXXX`
- Pattern for `fastq_1`: `^(syn://syn\\d+|\\S+\\.f(ast)?q\\.gz)$`
- Pattern for `fastq_2`: `^(syn://syn\\d+|\\S*\\.f(ast)?q\\.gz)?$`
- Enables direct integration with Synapse-stored data without downloading files first


## References

- [nf-core/rnaseq Official Input Schema](https://raw.githubusercontent.com/nf-core/rnaseq/master/assets/schema_input.json)
- [nf-core/rnaseq Usage Documentation](https://nf-co.re/rnaseq/latest/usage)
