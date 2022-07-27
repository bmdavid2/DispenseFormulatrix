#' Convert a reagent plate layout to a mantis dispense list 
#' 
#' @param filename a filename for the dispense list. must end in ".dl.txt"
#' @param reagent_layout a list of reagent volume matrices. see 'reagents_to_layout'
#' @export
export_mantis_worklist <- function(filename,reagent_layout,plate){
  num_reagents <- length(reagent_layout)
  delay_header <- c(num_reagents)
  for (i in c(1:num_reagents)){
    delay_header <- c(delay_header,c(0,""))
  }
  outfile=file(filename,open="w",encoding = "UTF-8")
  write.table(c("[ Version: 5 ]"),file=outfile,append=FALSE,row.names=FALSE,col.names=FALSE,na="",quote=FALSE,fileEncoding = "UTF-8",sep="\t",eol="\r\n")
  write.table(c(plate$filename),file=outfile,append=TRUE,row.names=FALSE,col.names=FALSE,na="",quote=FALSE,fileEncoding = "UTF-8",sep="\t",eol="\r\n")
  write.table(t(delay_header),file=outfile,append=TRUE,row.names=FALSE,col.names=FALSE,na="",quote=FALSE,fileEncoding = "UTF-8",sep="\t",eol="\r\n")
  write.table(t(c(1)),file=outfile,append=TRUE,row.names=FALSE,col.names=FALSE,na="",quote=FALSE,fileEncoding = "UTF-8",sep="\t",eol="\r\n")
  write.table(t(delay_header),file=outfile,append=TRUE,row.names=FALSE,col.names=FALSE,na="",quote=FALSE,fileEncoding = "UTF-8",sep="\t",eol="\r\n")
  
  for (name in names(reagent_layout)){
    write.table(t(c(name,"","Normal")),file=outfile,append=TRUE,row.names=FALSE,col.names=FALSE,na="",quote=FALSE,fileEncoding = "UTF-8",sep="\t",eol="\r\n")
    write.table(t(c("Well",1)),file=outfile,append=TRUE,row.names=FALSE,col.names=FALSE,na="",quote=FALSE,fileEncoding = "UTF-8",sep="\t",eol="\r\n")
    write.table(reagent_layout[[name]],file=outfile,append=TRUE,row.names=FALSE,col.names=FALSE,na="",quote=FALSE,fileEncoding = "UTF-8",sep="\t",eol="\r\n")
  }
  close(outfile)
}


#' Convert an ordered reagent volume dataframe to a plate layout 
#' 
#' @param reagent_df reagent volumes in the order they should appear on the plate. Must only be reagent volumes.
#' @param plate plate information
#' @export
reagents_to_layout<- function(reagent_df,plate){
  n_reagents <- length(names(reagent_df))
  reagentnames <- names(reagent_df)
  reagent_layout <- list()
  for (i in 1:n_reagents){
      vols <- matrix(0,plate$nrow,plate$ncol)
      vols[1:nrow(reagent_df)] <- reagent_df[,i]
      vols <- format(vols,nsmall=1,trim=TRUE)
      reagent_layout <- append(reagent_layout,I(list(vols)))
  }
  names(reagent_layout) <- reagentnames
  return(reagent_layout)
}
#' Convert a volume based experimental design to an ordered reagent dataframe
#' 
#' @param design_df Formatted dataframe with columns for each reagent, followed by either well location or row col 
#' @param plate plate information 
#' @export
design_to_reagents <- function(design_df,plate){
  design_df$Well <- paste(as.character(design_df$Row),as.character(design_df$Col),sep="")
  if (nrow(design_df) != length(unique(design_df$Well))){
    stop("Cannot have duplicate well locations")
  }
  RCW <- c("Row","Col","Well")
  reagentnames <- setdiff(names(design_df),RCW)
  unused_wells <- setdiff(plate$wellnames,design_df$Well)
  n_unused_wells <- length(unused_wells)
  expts_to_fill_plate <- data.frame(matrix(0,n_unused_wells,ncol(design_df)))
  names(expts_to_fill_plate) <- names(design_df)
  expts_to_fill_plate$Well <- unused_wells
  design_df_filled <- rbind(design_df,expts_to_fill_plate)
  ordered_design_df <- design_df_filled[order(factor(design_df_filled[,ncol(design_df_filled)],levels=plate$wellnames)),] 
  print(ordered_design_df)
  reagent_df <- ordered_design_df[,reagentnames,drop=FALSE]
  names(reagent_df)=reagentnames
  print(reagent_df)
  return(reagent_df)
}

#' Add well locations to a volumetric experimental design 
#' 
#' @param design Volumetric experimental design 
#' @param filename="" Optional output file. Default outputs to the console
#' @param platetype="breakaway_pcr_96" Must be either "96-well" or "384-well"
#' @param randomize=FALSE If true, randomize the assigned wells. Else, put them in order.
#' @export
assign_wells <- function(design,filename="",platetype="breakaway_pcr_96",randomize=FALSE){
  plate <- plateinfo(platetype)
  wellnames <- plate$wellnames
  if (randomize){
    wellnames <- sample(wellnames)
  }
  wellnames <- wellnames[1:nrow(design)]
  welldf <- data.frame(strsplit(wellnames, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE))
  rows <- unlist(welldf[1,])
  cols <- unlist(welldf[2,])
  design$Row <- rows
  design$Col <- cols
  write.csv(design,file=filename)
  return(design)
}

plateinfo <- function(platetype="breakaway_pcr_96"){
  plate=list()
  ### Supported Plate Types
  # breakaway_pcr_96
  # "96-well"
  # "384-well"
  ###
  if(platetype=="breakaway_pcr_96"){
    plate$name <- platetype
    plate$nrow <- 8
    plate$ncol <- 12
    rownames <- rep(c("A","B","C","D","E","F","G","H"),ncol)
    colnames <- rep(1:ncol,each=nrow)
    plate$wellnames <- paste0(rownames,colnames)
    plate$wellnums <- c(1:(plate$nrow*plate$ncol))
    plate$filename <- "breakaway_pcr_96.pd.txt"
  } else if(platetype=="96-well"){
    plate$name <- platetype
    plate$nrow <- 8
    plate$ncol <- 12
    rownames <- rep(c("A","B","C","D","E","F","G","H"),ncol)
    colnames <- rep(1:ncol,each=nrow)
    plate$wellnames <- paste0(rownames,colnames)
    plate$wellnums <- c(1:(plate$nrow*plate$ncol))
    plate$filename <- "PT3-96-Assay.pd.txt"
  } else if(platetype=="384-well"){
    plate$name <- platetype
    plate$nrow <- 16
    plate$ncol <- 24
    rownames <- rep(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P"),ncol)
    colnames <- rep(1:ncol,each=nrow)
    plate$wellnames <- paste0(rownames,colnames)
    plate$wellnums <- c(1:(plate$nrow*plate$ncol))
    plate$filename <- "PT9-384-Assay.pd.txt"
  } else {
    stop("enter a supported plate type")
  }
  return(plate)
}
#' Main running funciton. Turn an experimental design into a dispense list
#' 
#' @param design an Nx(M+2) dataframe containing N experiment rows and M reagent columns. Each entry is a volume in µL. Two extra columns called "Row" (ex. "A") and "Col" (ex. "1") must be included to denote well positions. 
#' @param name name of the experiment. Appropriate extension is added automatically. 
#' @param platetype="breakaway_pcr_96" specifies the plate type. Must be one of: "96-well", "breakaway_pcr_96" , 384-well".
#' @export
#' 
dispense_list <- function(design,name,platetype="breakaway_pcr_96"){

  plate <- plateinfo(platetype)
  colnames=names(design)
  if(!is.element(colnames,"Row") && !is.element(colnames,"Col")){
    stop("Must provide `Row` and `Col` columns for well positions")
  }
  filename <- paste(name,".dl.txt")
  reagents <- design_to_reagents(design,plate,...)
  layout <- reagents_to_layout(reagents,plate,...)
  export_mantis_worklist(filename,layout,plate,...)
}


