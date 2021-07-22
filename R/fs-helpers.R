get_files <- function(path, ext) {
  require(fs)
  
  if (!require(fs)) {
    stop("Package 'fs' is required. Use `install.packages('fs') to install it.")
  }
  
  files <- dir_ls(path, recurse = TRUE, glob = paste0("*.", ext))
  file_names <- path_ext_remove(path_file(files))
  files <- as.list(files)
  names(files) <- file_names
  files
}

