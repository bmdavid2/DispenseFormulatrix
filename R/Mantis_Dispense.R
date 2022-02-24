#' Convert a reagent plate layout to a mantis dispense list 
#' 
#' @param filename a filename for the dispense list. must end in ".dl.txt"
#' @param reagent_layout a list of reagent volume matricies. see 'reagents_to_layout'
#' @export
export_mantis_worklist <- function(filename,reagent_layout){
  num_reagents <- length(reagent_layout)
  delay_header <- c(num_reagents)

  for (i in c(1:num_reagents)){
    delay_header <- c(delay_header,c(0,""))
  }
  outfile=file(filename,open="w",encoding = "UTF-8")
  write.table(c("[ Version: 5 ]"),file=outfile,append=FALSE,row.names=FALSE,col.names=FALSE,na="",quote=FALSE,fileEncoding = "UTF-8",sep="\t",eol="\r\n")
  write.table(c("breakaway_pcr_96.pd.txt"),file=outfile,append=TRUE,row.names=FALSE,col.names=FALSE,na="",quote=FALSE,fileEncoding = "UTF-8",sep="\t",eol="\r\n")
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
#' @param reagent_df reagent volumes in the order they should appear on the plate. Must only be reagent volumes
#' @param platetype="96-well" specifies the plate type for the layout
#' @export
reagents_to_layout<- function(reagent_df,platetype="96-well"){
  if (platetype =="96-well"){
    nrows=8
    ncols=12
     
  } else {
    stop("Function can only handle 96 well plates right now")
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
#' @param Well=TRUE if Well=TRUE, then use well designation, else combine row and col into well info
#' @export
design_to_reagents <- function(design_df,platetype="96-well",Well=TRUE){
  if (platetype =="96-well"){
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
  } else {
    stop("Function can only handle 96 well plates right now")
  }
  omit <- 0
  if (!Well){
    design_df$Well <- paste(as.character(design_df[,(ncol(design_df)-1)]),as.character(design_df[,ncol(design_df)]),sep="")
    omit <- 2
  }
  unused_wells <- setdiff(wellnames,design_df$Well)
  n_unused_wells <- length(unused_wells)
  expts_to_fill_plate <- data.frame(matrix(0,n_unused_wells,ncol(design_df)))
  names(expts_to_fill_plate) <- names(design_df)
  expts_to_fill_plate$Well <- unused_wells
  design_df_filled <- rbind(design_df,expts_to_fill_plate)
  ordered_design_df <- design_df_filled[order(factor(design_df_filled[,ncol(design_df_filled)],levels=wellnames)),] 
  print(ordered_design_df)
  reagent_df <- ordered_design_df[,c(1:(ncol(ordered_design_df)-omit-1))]
  print(reagent_df)
  return(reagent_df)
}

#' Add well locations to a volumetric experimental design 
#' 
#' @param design Volumetric experimental design 
#' @param filename="" Optional output file. Default outputs to the console
#' @param platetype="96-well" Specifies the well naming scheme
#' @param randomize=FALSE If true, randomize the assigned wells. Else, put them in order.
#' @export
assign_wells <- function(design,filename="",platetype="96-well",randomize=FALSE){
  wellnames <- c()
  if (platetype =="96-well"){
    rownames <- c("A","B","C","D","E","F","G","H")
    colnames <- as.character(c(1:12))
    for (i in colnames){
      for (j in rownames){
        wellnames <- c(wellnames,paste(j,i,sep=""))
      }
    }
  } else {
    stop("Function can only handle 96 well plates right now")
  }
  if (randomize){
    wellnames <- sample(wellnames)
  }
  design$Well <- wellnames[1:nrow(design)]
  write.csv(design,file=filename)
  return(design)
}
#' Main running funciton. Turn an experimental design into a dispense list
#' 
#' @param design Volumetric experimental design. Must include either Well or Row & Col columns
#' @param name Design name. Funciton adds appropriate suffix automatically
#' @param platetype="96-well" specifies the plate type 
#' @param Well=TRUE must be adjusted based on the design's well information 
#' @export
#' 
make_dispense_list <- function(design,name,platetype="96-well",Well=TRUE,...){
  kwargs=list(...)
  filename <- paste(name,".dl.txt")
  reagents <- design_to_reagents(design,platetype=platetype,Well=Well,...)
  layout <- reagents_to_layout(reagents,platetype=platetype,...)
  export_mantis_worklist(filename,layout)
}

#test_reagents <- data.frame(A=matrix(0,96,1))



#test_reagents$A <- rep(1,96)
#test_reagents$B <- rep(1,96)
#test_reagents$C <- rep(1,96)
#test_reagents$A[1:9] <- c(1:9)
#test_reagents$B[25:33] <- c(1:9)

#design <- assign_wells(test_reagents)

#make_dispense_list(design,"testing",Well=T)

