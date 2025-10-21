# RNA-seq Samplesheet Generator

## Overview

This script generates a samplesheet for the nf-core/rnaseq pipeline from a Synapse dataset. It validates the input dataset annotations, transforms the metadata to the required format, and uploads the resulting CSV to Synapse.

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

The script generates a CSV file with the following columns per [nf-core/rnaseq v3.11.2 specification](https://nf-co.re/rnaseq/3.11.2/usage):

| Column | Description |
|--------|-------------|
| `sample` | Sample identifier (maps to `specimenID`) |
| `single_end` | Whether reads are single-end (1) or paired-end (0) |
| `fastq_1` | Synapse ID for Read 1 FASTQ file |
| `fastq_2` | Synapse ID for Read 2 FASTQ file (empty for single-end) |
| `strandedness` | Library strandedness (`auto`, `forward`, `reverse`, or `unstranded`) |

### Example Output

```csv
sample,single_end,fastq_1,fastq_2,strandedness
NF0003_specimen1,0,syn15261791,syn15261900,auto
NF0003_specimen2,0,syn15261974,syn15262033,auto
NF0003_specimen3,0,syn15262157,syn15262216,forward
NF0004_specimen1,1,syn15263000,,auto
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
SAMPLESHEET_FILENAME <- "rnaseq_samplesheet.csv"
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

## What the Script Does

1. **Validates the dataset** (`validate_dataset`)
   - Checks for required annotations
   - Verifies file formats are appropriate for RNA-seq
   - Validates read pairing (R1/R2 matching)
   - Provides summary statistics

2. **Generates the samplesheet** (`generate_samplesheet`)
   - Maps `specimenID` to `sample`
   - Converts Synapse entity IDs to URIs (`syn://synXXXXXXX`)
   - Pairs R1 and R2 files for paired-end data
   - Sets strandedness (from `libraryStrand` annotation or defaults to `auto`)
   - Handles both single-end and paired-end sequencing

3. **Uploads to Synapse** (`upload_samplesheet`)
   - Writes CSV file with proper formatting
   - Uploads to specified Synapse folder
   - Adds descriptive metadata

## Handling Different Data Scenarios

### Paired-End Data

Files with `readPair` values of `1` and `2` will be matched by `specimenID`:

```r
# Input files:
# specimenID: sample1, readPair: 1, entityId: syn123
# specimenID: sample1, readPair: 2, entityId: syn456

# Output row:
sample,fastq_1,fastq_2,strandedness
sample1,syn://syn123,syn://syn456,auto
```

### Single-End Data

Files with only `readPair` value of `1` (or no R2 match):

```r
# Input file:
# specimenID: sample2, readPair: 1, entityId: syn789

# Output row:
sample,fastq_1,fastq_2,strandedness
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
sample,fastq_1,fastq_2,strandedness
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

## References

- [nf-core/rnaseq v3.11.2 Usage](https://nf-co.re/rnaseq/3.11.2/usage)
- [NF-OSI Nextflow Documentation](https://help.nf.synapse.org/NFdocs/nextflow-data-processing-configuration)
- [Issue #236](https://github.com/nf-osi/nfportalutils/issues/236)
