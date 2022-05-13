#' Convert a reagent plate layout to a mantis dispense list 
#' 
#' @param filename a filename for the dispense list. must end in ".dl.txt"
#' @param reagent_layout a list of reagent volume matricies. see 'reagents_to_layout'
#' @export
export_mantis_worklist <- function(filename,reagent_layout,platetype="breakaway_pcr_96"){
  num_reagents <- length(reagent_layout)
  delay_header <- c(num_reagents)
  platefile <- c("breakaway_pcr_96.pd.txt","PT3-96-Assay.pd.txt","PT9-384-Assay.pd.txt")
  names(platefile) <- c("breakaway_pcr_96","96-well","384-well")

  for (i in c(1:num_reagents)){
    delay_header <- c(delay_header,c(0,""))
  }
  outfile=file(filename,open="w",encoding = "UTF-8")
  write.table(c("[ Version: 5 ]"),file=outfile,append=FALSE,row.names=FALSE,col.names=FALSE,na="",quote=FALSE,fileEncoding = "UTF-8",sep="\t",eol="\r\n")
  write.table(c(platefile[platetype]),file=outfile,append=TRUE,row.names=FALSE,col.names=FALSE,na="",quote=FALSE,fileEncoding = "UTF-8",sep="\t",eol="\r\n")
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
#' @param platetype="96-well" specifies the plate type for the layout. Must be either "96-well" or "384-well".
#' @export
reagents_to_layout<- function(reagent_df,platetype="breakaway_pcr_96"){
  if (platetype =="96-well" || platetype== "breakaway_pcr_96"){
    nrows=8
    ncols=12
     
  } else if (platetype=="384-well"){
    nrows=16
    ncols=24
  } else {
    stop("Function requires `96-well` or `384-well`")
  }
  n_reagents <- length(names(reagent_df))
  reagentnames <- names(reagent_df)
  reagent_layout <- list()
  for (i in 1:n_reagents){
      plate <- matrix(0,nrows,ncols)
      plate[1:nrow(reagent_df)] <- reagent_df[,i]
      plate <- format(plate,nsmall=1,trim=TRUE)
      reagent_layout <- append(reagent_layout,I(list(plate)))
  }
  names(reagent_layout) <- reagentnames
  return(reagent_layout)
}
#' Convert a volume based experimental design to an ordered reagent dataframe
#' 
#' @param design_df Formatted dataframe with columns for each reagent, followed by either well location or row col 
#' @param platetype="96-well" specifies the plate type for the design
#' @export
design_to_reagents <- function(design_df,platetype="breakaway_pcr_96"){
  welldict <- create_welldict(platetype = platetype)
  design_df$Well <- paste(as.character(design_df$Row),as.character(design_df$Col),sep="")
  reagentnames <- names(design_df)[1:(ncol(design_df)-3)]
  unused_wells <- setdiff(names(welldict),design_df$Well)
  n_unused_wells <- length(unused_wells)
  expts_to_fill_plate <- data.frame(matrix(0,n_unused_wells,ncol(design_df)))
  names(expts_to_fill_plate) <- names(design_df)
  expts_to_fill_plate$Well <- unused_wells
  design_df_filled <- rbind(design_df,expts_to_fill_plate)
  ordered_design_df <- design_df_filled[order(factor(design_df_filled[,ncol(design_df_filled)],levels=wellnames)),] 
  print(ordered_design_df)
  reagent_df <- ordered_design_df[,c(1:(ncol(ordered_design_df)-3)),drop=FALSE]
  names(reagent_df)=reagentnames
  #print(reagent_df)
  return(reagent_df)
}

create_welldict <- function(platetype="breakaway_pcr_96"){
  if (platetype =="96-well" || platetype== "breakaway_pcr_96"){
    nrows=8
    ncols=12
    welldict <- c(1:96)
    wellnames <- c()
    rownames <- c("A","B","C","D","E","F","G","H")
    colnames <- as.character(c(1:12))
    for (i in colnames){
      for (j in rownames){
        wellnames <- c(wellnames,paste(j,i,sep=""))
      }
    }
    names(welldict) <- wellnames
  } else if (platetype=="384-well") {
    nrows=16
    ncol=24
    welldict <- c(1:384)
    wellnames <- c()
    rownames <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")
    colnames <- as.character(c(1:24))
    for (i in colnames){
      for (j in rownames){
        wellnames <- c(wellnames,paste(j,i,sep=""))
      }
    }
    names(welldict) <- wellnames
  } else {
    stop("Enter a valid plate type")
  }
  return(welldict)
}

#' Add well locations to a volumetric experimental design 
#' 
#' @param design Volumetric experimental design 
#' @param filename="" Optional output file. Default outputs to the console
#' @param platetype="96-well" Must be either "96-well" or "384-well"
#' @param randomize=FALSE If true, randomize the assigned wells. Else, put them in order.
#' @export
assign_wells <- function(design,filename="",platetype="breakaway_pcr_96",randomize=FALSE){
  welldict <- create_welldict(platetype=platetype)
  if (randomize){
    welldict <- sample(welldict)
  }
  design$Row <- names(welldict)[1:nrow(design)]
  design$Col <- names(welldict)[1:nrow(design)]
  design$Well <- names(welldict)[1:nrow(design)]
  write.csv(design,file=filename)
  return(design)
}
#' Main running funciton. Turn an experimental design into a dispense list
#' 
#' @param design Volumetric experimental design. Must include either Well or Row & Col columns
#' @param name Design name. Funciton adds appropriate suffix automatically
#' @param platetype="96-well" specifies the plate type. Must be one of: "96-well", "breakaway_pcr_96" , 384-well".
#' @export
#' 
make_dispense_list <- function(design,name,platetype="breakaway_pcr_96",...){
  kwargs=list(...)
  filename <- paste(name,".dl.txt")
  reagents <- design_to_reagents(design,platetype=platetype,...)
  layout <- reagents_to_layout(reagents,platetype=platetype,...)
  export_mantis_worklist(filename,layout,platetype = platetype,...)
}

#test_reagents <- data.frame(A=matrix(0,96,1))



#test_reagents$A <- rep(1,96)
#test_reagents$B <- rep(1,96)
#test_reagents$C <- rep(1,96)
#test_reagents$A[1:9] <- c(1:9)
#test_reagents$B[25:33] <- c(1:9)

#design <- assign_wells(test_reagents)

#make_dispense_list(design,"testing",Well=T)

