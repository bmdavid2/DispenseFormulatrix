#' Automatically create an experimental plate layout that minimizes pipetting operations 
#' 
#' @param design an experimental design dataframe containing the coded level of each factor
#' @param factors an array of the factor names 
#' @param types an array of the factor types: must be "auto" ,"col" "wafer","plate
#' @param num_generations the number of GA generations
#' @param popsize the GA population size
#' @export
plate_layout <- function(design,factors,types,num_generations=50,popsize=30){
  julia <- JuliaCall::julia_setup()
  JuliaCall::julia_library("PlateLayout")
  JuliaCall::julia_library("DataFrames")
  
  
  JuliaCall::julia_assign("design",design)
  JuliaCall::julia_assign("factors",factors)
  JuliaCall::julia_assign("types",types)
  JuliaCall::julia_assign("num_generations",num_generations)
  JuliaCall::julia_command("num_generations=Int(num_generations)")
  JuliaCall::julia_assign("popsize",popsize)
  JuliaCall::julia_command("popsize=Int(popsize);")
  JuliaCall::julia_eval("print(design);")
  juliaCall::julia_command("plate=FactorAssignGA(design,factors,types;num_generations=num_generations,popsize=popsize);")
  JuliaCall::julia_command("out_designs=UpdateDesign(design,plate);")
  JuliaCall::out_designs=julia_eval("out_designs")
  
  return(out_designs)
}

#' Install the julia pacakges for plate layout if needed 
#' 
#' @export
install_plate_layout <- function(){
  JuliaCall::julia_install_package_if_needed("https://github.com/jensenlab/PlateLayout")
  JuliaCall::julia_install_package_if_needed("DataFrames")
}
