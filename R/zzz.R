synapseclient <- NULL

.onLoad <- function(libname, pkgname) {

  syn_inst <- reticulate::py_module_available("synapseclient")

  if(syn_inst) {
    synapseclient <<- reticulate::import("synapseclient", delay_load = FALSE)
    synapseutils <<- reticulate::import("synapseutils")
  } else {
    warning("Python modules `synapseclient` & `synapseutils` not available. Main package functions will not work.")
  }
  
  pandas_inst <- reticulate::py_module_available("pandas")
  if(!pandas_inst) warning("Python module `pandas` is not available and some package functions may not work.")
  
}
