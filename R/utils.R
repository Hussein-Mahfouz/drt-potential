


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

gh_release_download(tag = "gtfs-feeds",
                    pattern = "gtfs_england_06_23.zip",
                    dest = "data_raw")
