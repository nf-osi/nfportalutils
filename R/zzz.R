synapseclient <- NULL

# Default NA recoding values for cBioPortal export
.default_na_recode <- c("NA", "NaN", "unknown", "Unknown", "UNKNOWN", "NULL", "null", "Null", 
                        "", " ", ".", "-", "N/A", "n/a", "Not Available", "not available",
                        "Not Reported", "not reported", "Missing", "missing", "Not Collected", 
                        "not collected", "Not Applicable", "not applicable", "Pending", "pending", 
                        "TBD")

.onLoad <- function(libname, pkgname) {

  syn_inst <- reticulate::py_module_available("synapseclient")

  if(syn_inst) {
    synapseclient <<- reticulate::import("synapseclient", delay_load = FALSE)
    synapseutils <<- reticulate::import("synapseutils", delay_load = FALSE)
  } else {
    warning("Python modules `synapseclient` & `synapseutils` not available. Main package functions will not work.")
  }
  
  pandas_inst <- reticulate::py_module_available("pandas")
  if(!pandas_inst) warning("Python module `pandas` is not available and some package functions may not work.")
  
  # Set default package options
  options(nfportalutils.na_recode = .default_na_recode)
}
