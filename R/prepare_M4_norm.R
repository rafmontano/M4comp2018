#' Load or Create the Normalized M4 Dataset
#'
#' Ensures that `M4_norm` is available. If not, normalizes `M4`, saves it, and loads it.
#'
#' @return The `M4_norm` object, a normalized version of the `M4` dataset.
#' @export
prepare_M4_norm <- function() {
  data_path <- system.file("data", "M4_norm.rda", package = "M4comp2018")
  
  # If M4_norm exists, load it
  if (file.exists(data_path)) {
    load(data_path, envir = .GlobalEnv)
    message("M4_norm loaded successfully.")
  } else {
    message("M4_norm not found. Creating and saving it...")
    
    # Load M4 data
    data("M4", package = "M4comp2018", envir = environment())
    
    # Normalize M4 dataset
    M4_norm <- normalize_database(M4)
    
    # Save M4_norm as an R dataset
    save(M4_norm, file = file.path(system.file("data", package = "M4comp2018"), "M4_norm.rda"))
    
    # Load into environment
    load(file.path(system.file("data", package = "M4comp2018"), "M4_norm.rda"), envir = .GlobalEnv)
    
    message("M4_norm created and saved successfully.")
  }
  
  return(M4_norm)
}