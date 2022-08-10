#' Convert a coded experimental design into a volume-based design for automated dispensing 
#' 
#' @param coded_design
#' @param reagent_names
#' @param volume_levels
#' @param separate_stocks
#' @param dispense_group
#'
#' @export
process_design <- function(coded_design,reagent_names,volume_levels,separate_stocks,dispense_group){
  nfactors <- length(reagent_names)
  if(nfactors != length(volume_levels) || nfactors != length(separate_stocks) || nfactors != length(dispense_group)){
    stop("Number of reagents does not agree with the number of volume levels, separate stocks, or dipsense group")
  }
  n <- nrow(coded_design)
  colnames=names(coded_design)
  if(!is.element("Row",colnames) || !is.element("Col",colnames)){
    stop("Must provide `Row` and `Col` columns for well positions")
  }
  names(separate_stocks) <- reagent_names
  factor_levels <- lapply(reagent_names,get_factor_levels,design=coded_design)
  names(factor_levels) <- reagent_names
  volume_mapping <- list()
  for (i in 1:length(reagent_names)){
    volume_mapping[[i]] <- volume_levels[[i]]
    names(volume_mapping[[i]]) <- factor_levels[[i]]
  }
  names(volume_mapping) <- reagent_names
  
  if(length(unique(dispense_group)) != max(dispense_group)){
    stop("Dispense groups must be ordered cronologically")
  }
  designs <- list()
  for (i in 1:max(dispense_group)){
    designs[[i]] <- data.frame(coded_design$Row,coded_design$Col)
    names(designs[[i]]) <- c("Row","Col")
    for (reg in reagent_names[dispense_group==i]){
      if (separate_stocks[[reg]]){
        suffixes <- LETTERS[1:length(volume_mapping[[reg]])]
        names <- paste(reg,suffixes,sep="_")
        for (j in 1:length(names)){
          designs[[i]][[names[j]]] <- rep(0,n)
          expts <- as.character(coded_design[[reg]])==factor_levels[[reg]][j]
          designs[[i]][[names[j]]][expts] <- volume_mapping[[reg]][[j]]
        }
      } else{
       designs[[i]][[reg]] <-  volume_mapping[[reg]][as.character(coded_design[[reg]])]
      }
    }
  }
  return(designs)
}




get_factor_levels <- function(reagent,design){
  levels <- as.character(sort(unique(design[[reagent]])))
  return(levels)
  }


#' Get the stock volume required for each reagent 
#' 
#' @param processed_design
#'
#' @export
stock_volumes <- function(processed_design,priming_buffer=10,uncertainty=0.1){
  reagent_names=setdiff(names(processed_design),c("Row","Col"))
  vols=rep(0,length(reagent_names))
  for (i in 1:length(reagent_names)){
    vols[i]=ceiling((1+uncertainty)*sum(processed_design[[reagent_names[i]]])+priming_buffer)
  }
  df=data.frame(reagent_names,vols)
  names(df)=c("name","volume_required")
  print(df)
  return(df)
}


