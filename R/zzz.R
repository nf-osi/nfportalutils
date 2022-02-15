synapseclient <- NULL

.onLoad <- function(libname, pkgname) {

  syn_inst <- reticulate::py_module_available("synapseclient")

  if(syn_inst) {
    synapseclient <<- reticulate::import('synapseclient', delay_load = FALSE)
  } else {
    warning("Python module `synapseclient` is not available")
  }
  
  pandas_inst <- reticulate::py_module_available("pandas")
  if(!pandas_inst) warning("Python module `pandas` is not available and some package functions may not work")
  
}
