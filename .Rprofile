try({
  src <- file.path(getwd(), "rstudio-prefs.json")
  dst <- path.expand("~/.config/rstudio/rstudio-prefs.json")
  if (file.exists(src)) {
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    file.copy(src, dst, overwrite = TRUE)
  }
}, silent = TRUE)
