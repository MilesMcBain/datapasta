read_clip_or_editor <- function() {
  clipr_readable <- clipr::clipr_available()
  if (clipr_readable) {
    x <- tryCatch({
      clipr::read_clip()
    }, error = function(e) {
      clipr_readable <<- FALSE
      NULL
    })
  }
  if (!clipr_readable || is.null(x) || identical(x, "")) {
    x <- read_rstudio_editor()
    if (identical(x, "")) {
      # clipboard failed but nothing selected
      stop("The clipboard is not accessible, but you can paste your text into ",
           "an RStudio editor, select it, and run the datapasta addin again.")
    }
  }
  x
}

read_rstudio_editor <- function() {
  if (!rstudioapi::hasFun("getSourceEditorContext")) {
    # just throwing the no clipboard message because we're not in RStudio
    stop(.global_datapasta_env$no_clip_msg)
  }
  ctx <- rstudioapi::getSourceEditorContext()
  if (length(ctx$selection) > 1) {
    warning("Multiple sections are selected in the editor, ",
            "using the first selection only.")
  }
  # returns a length-one string of the text
  # currently selected in the active editor
  text <- ctx$selection[[1]]$text

  if (identical(text, "")) {
    return("")
  }

  # clipr::read_clip() uses scan() internally --
  # (I guess utils::readClipboard does this on Windows?) --
  # so we have to jump through some hoops to introduce the editor text to scan()
  tmpfile <- tempfile()
  on.exit(unlink(tmpfile))
  cat(text, file = tmpfile, sep = "\n")
  scan(tmpfile, what = character(), sep = "\n", blank.lines.skip = FALSE, quiet = TRUE)
}
