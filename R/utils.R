


# --- function to copy all files with a specific pattern from one directory to another
copy_files = function(source, dest, pattern){
  # INPUT:
    # source: directory where the files are
    # dest: directory where we want to copy the files
    # pattern: the extension of the files (e.g. ".zip", ".tif")
  # -----------
  # get name of files with specific pattern in a directory
  files = list.files(source, pattern = pattern)
  # check that we have at least one file with the specified pattern (extension)
  if(length(files) > 0){
    # get file path for each of the files
    file_source = paste0(source, files)
    # define path where we want to move the file
    file_dest = paste0(dest, files)
    # copy the file(s)
    file.copy(file_source, file_dest)

  } else{
    paste0("there are no files with the pattern '", pattern, "' in ", source)
  }
}


# Github releases

# - upload files
# https://cli.github.com/manual/gh_release_upload
gh_release_upload = function(file, tag) {
  msg = glue::glue("gh release upload {tag} {file} --clobber")
  message("Running this command:\n", msg)
  system(msg)
}


# - download files
# https://cli.github.com/manual/gh_release_download
gh_release_download = function(tag, pattern, dest) {
  msg = glue::glue("gh release download {tag} --pattern {pattern} -D {dest} --skip-existing")
  message("Running this command:\n", msg)
  # system(msg)
  system(msg)
}


