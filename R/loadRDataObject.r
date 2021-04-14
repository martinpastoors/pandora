# Read specific object from RData file

loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

loadRDataObject <- function(fileName, object){
  
  try(missing(fileName), "Stop: no fileName supplied")
  try(missing(object), "Stop: no object supplied")
  try(length(object)>1, "Stop: only one object can be supplied")
  load(fileName)
  get(ls()[ls() %in% object])
}


listDataObjects <- function(fileName){
  
  try(missing(fileName), "Stop: no fileName supplied")
  load(fileName, ex <- new.env())
  print(ls(ex)) 
  remove(ex)
}

