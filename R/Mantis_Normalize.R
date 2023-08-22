#' Normalize DNA concentrations with water using the Mantis 
#' 
#' @param input_path path to the normalization input template. Template can be found at https://github.com/bmdavid2/DispenseFormulatrix/blob/main/mantis_normalize_template.csv 
#' @param platetype="breakaway_pcr_96" specifies the plate type. Must be one of: "96-well", "breakaway_pcr_96" , 384-well".
#' @param maxvol=200 the maximum volume allowed for the final concentration
#' @param usenanomolar=FALSE standard final concentrations are ng/ul
#' @export
#' 
mantis_normalize <- function(input_path,platetype="breakaway_pcr_96",maxvol=200, usenanomolar=FALSE){
  # read data from template 
  data=read.csv(input_path)
  init_conc=data$concentration # ng/uL
  init_volume <- data$volume # uL
  dna_length <- data$amplicon_length # bp dsDNA
  final_conc <- data$final_concentration # either ng/uL or uM 
  Row <- data$Row
  Col <- data$Col
  
  #calculate mass of amplicons 
  
  mass <- init_conc * init_volume  # ng dsDNA
  final_volume <- mass / final_conc # uL 
  
  if (usenanomolar){
    MW <- dna_length * 617.96 + 36.04 # g/mol
    moles <- mass * 10^-9 /MW * 10^9   # ng * g/ng * mol/g * nmol/mol -> nmoles
    final_volume <- moles / final_conc * 10^6  # nmoles * L/nmol * uL/L -> uL 
  }
  
  overshot <- final_volume > maxvol
  toolow <- final_volume < init_volume 
  can_normalize <- rep(TRUE,nrow(data))
  can_normalize[overshot]=FALSE
  can_normalize[toolow]=FALSE
  comment <- rep("" ,nrow(data))
  comment[overshot]="too concentrated"
  comment[toolow]="too dilute"
  data$can_normalize=can_normalize
  data$comment=comment
  data$final_volume=final_volume
  dispense_volume <- final_volume - init_volume
  dispense_volume[!can_normalize]=NaN
  data$dispense_volume=dispense_volume
  
  if (FALSE %in% can_normalize){
    warning("some samples cannot be normalized because they are either too concentrated or too dilute.")
  }
  
  dispense <- data.frame(dispense_volume,Row,Col)
  dispense <- dispense[can_normalize,]
  names(dispense)=c("Water","Row","Col")
  outfile=tools::file_path_sans_ext(input_path)
  mantis_dispense(dispense,outfile,platetype=platetype)
  
  
  return(data)
  
}




