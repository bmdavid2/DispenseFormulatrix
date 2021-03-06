# DispenseFormulatrix
[![MIT license](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/bmdavid2/nsrgenomes/blob/main/LICENSE)
# Contents 
[Description](#description) \
[Installing DispenseFormulatrix](#installing-nsrgenomes) \
[Using DispenseFormulatrix](#using-nsrgenomes) 


# Description 
R package for creating custom reagent dispense lists for Formulatrix liquid handlers 


# Installing DispenseFormulatrix
To install `DispenseFormulatrix` run the following R code.   

```R

install.packages("devtools")
devtools::install_github("https://github.com/bmdavid2/DispenseFormulatrix")
```

# Using the `make_dispense_list` function
`make_dispense_list` takes a volumetric experimental design where each column is a reagent and each row is a run. The final two columns must be called "Row" and "Col", where Row contains the row name (ex. A,B,C), and Col contains the column number (ex. 1,2,3) for each run.  
```R 
make_dispense_list(design,name,...))
```
## Arguments 
* `design`: A dataframe where each column is a reagent and each row is a run. Must include run layout information. See function description. 

* `name`: name of the experiment for the output file.

## Optional Arguments
 * `platetype`="96-well": Specifies the type of plate for the experiment.
 allowed plates are: 
    - "96-well": A standard 96 well plate 
    - "384-well": A standard 394 well plate
    - "breakaway_pcr_96": A breakaway 96 well PCR plate (Secured to Mantis by placing it in a standard 96 well plate)



 
## Output 


*-* `name.dl.txt` dispense list file 

 