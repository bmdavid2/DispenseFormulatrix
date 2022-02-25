# nsrgenomes
[![MIT license](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/bmdavid2/nsrgenomes/blob/main/LICENSE)
# Contents 
[Description](#description) \
[Installing nsrgenomes](#installing-nsrgenomes) \
[Using nsrgenomes](#using-nsrgenomes) 


# Description 
Generate formatted genome files for the [OligoRL](https://github.com/bmdavid2/OligoRL) tool, NSR-RL. 


# Installing nsrgenomes
`nsrgenomes` is an [R](https://www.rstudio.com/products/rstudio/download/) package. To install `nsrgenomes` run the following R code.   

```R
install.packages("BiocManager")
BiocManager::install("genbankr")
install.packages("devtools")
devtools::install_github("jensenlab/primer3")
devtools::install_github("https://github.com/bmdavid2/nsrgenomes")
```

# Using nsrgenomes 
Loading the `nsrgenomes` library will bring all necessary dependencies into the workspace. The function `nsrgenomes` downloads and formats genomes taken from the NCBI database. 

```R 
nsrgenomes(species_name,accession_number)
```
## Arguments 
- `species_name`: Name of the organsim as a string, ex. "E_coli"

- `accession_number`: Accession number for the organism as a string, ex "NC_000913.3". Accesion numbers can be found at the [NCBI website](https://www.ncbi.nlm.nih.gov/nuccore) by searching the organsim of interest.


## Output 
the nsrgenomes automatically saves two .csv files  in the working directory containing the formatted genomic information used by OligoRL's NSR-RL tool. 

- `species_name_genes.csv` contains the list of mRNA sequences and other supplementary information used by the NSR-RL tool

- `species_name_rRNA_tRNA.csv` contains the list of all rRNA and tRNA sequences. These sequences will be avoided by NSR-RL. 
 ## Example

The following code is an example showing how to download the *E coli* K-12  genome files. 


```R
library(nsrgenomes)
nsrgenomes("E_coli","NC_000913.3")
```
 