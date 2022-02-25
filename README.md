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
`make_dispense_list` takes a volumetric experimental design where each column is a reagent and each row is a run. The final column, called "Well", must be included to specify the well position of each run (ex. "A1"). Users can also instead input the well information as two columns "Row" and "Col", were row is a letter and col is a number. `make_dispense_list` can accept either of these configurations by changing the `Well` parameter. `Well=TRUE` if the last column is "Well", and `Well=FALSE` if the "Row" and "Col" configuration is used. `make_dispense_list` cannot accept both configurations at once, and will treat "Row" and "Col" as reagents in that case. 
```R 
make_dispense_list(design,name,...))
```
## Arguments 
- `design`: A dataframe where each column is a reagent and each row is a run. Must include run layout information. See function description. 

- `name`: name of the experiment for the output file.

## Optional Arguments
 - `platetype`="96-well": Specifies the type of plate for the experiment. Currently only allows a 96- well plate

 - `Well`=True: Function looks for a final column in the design calle "Well" that has the well information. change to `FALSE` if using "Row"/"Col" configuration. 
 
## Output 


- `name.dl.txt` dispense list file 

 