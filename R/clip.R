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
      # not in RStudio or no text selected
      x <- ask_user_for_paste()
    }
    if (identical(x, "")) {
      # at this point: clipboard not available, not in RStudio or no text is
      # selected in the editor, and user declined to paste text into the input
      stop(.global_datapasta_env$no_clip_msg)
    }
  }
  x
}

read_rstudio_editor <- function() {
  if (!rstudioapi::hasFun("getSourceEditorContext")) {
    return("")
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
    # okay, for real there's nothing selected and nothing we can do about it
    return("")
  }

  # clipr::read_clip() uses scan() internally --
  # (I guess utils::readClipboard does this on Windows?) --
  # so we have to jump through some hoops to introduce the editor text to scan()
  text <- paste(text, collapse = "\n")
  txtcon <- textConnection(text)
  read_file_to_clipr_vec(txtcon)
}

read_file_to_clipr_vec <- function(file) {
  scan(file, what = character(), sep = "\n", blank.lines.skip = FALSE, quiet = TRUE)
}

ask_user_for_paste <- function() {
  if (!requireNamespace("utils", quietly = TRUE)) {
    warning("The `utils` package is not available, but could be used to help ",
            "you manually enter your clipboard text.")
    return("")
  }
  quiet <- getOption("datapasta.quiet_manual_paste", FALSE)
  if (!quiet) {
    msg <- paste(
      "The clipboard isn't accessible. Paste your copied text into the %s",
      "to import your data."
    )
    if (rstudioapi::hasFun("showDialog") && !quiet) {
      rstudioapi::showDialog(
        "datapasta",
        sprintf(msg, "text box in the next screen and then press Save"))
    } else {
      message(sprintf(msg, "editor that will open and then save and exit"))
      readline("Press enter to continue...")
    }
    options(datapasta.quiet_manual_paste = TRUE)
  }
  tmpfile <- tempfile()
  cat("", file = tmpfile)
  on.exit(unlink(tmpfile))
  utils::file.edit(tmpfile)
  read_file_to_clipr_vec(tmpfile)
}
