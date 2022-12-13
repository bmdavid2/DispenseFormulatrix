#' Automatically create an experimental plate layout that minimizes pipetting operations 
#' 
#' @param filename a filename for the dispense list. must end in ".dl.txt"
#' @param reagent_layout a list of reagent volume matrices. see 'reagents_to_layout'
#' @export
plate_layout <- function(design,factors,types,num_generations=50,popsize=30){
  julia <- julia_setup()
  julia_library("PlateLayout")
  julia_library("DataFrames")
  
  
  julia_assign("design",design)
  julia_assign("factors",factors)
  julia_assign("types",types)
  julia_assign("num_generations",num_generations)
  julia_command("num_generations=Int(num_generations)")
  julia_assign("popsize",popsize)
  julia_command("popsize=Int(popsize);")
  julia_eval("print(design);")
  julia_command("plate=FactorAssignGA(design,factors,types;num_generations=num_generations,popsize=popsize);")
  julia_command("out_designs=UpdateDesign(design,plate);")
  out_designs=julia_eval("out_designs")
  
  return(out_designs)
}


install_plate_layout <- function(){
  julia_install_package_if_needed("https://github.com/jensenlab/PlateLayout")
  install_julia_package_if_needed("DataFrames")
}
