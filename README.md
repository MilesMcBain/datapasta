# datapasta

datapasta is about reducing resistance associated with copying and pasting data into R Studio. I found myself using Sublime as an intermediate text munging step when looking to copy data to R Studio. Hopefully addins in this package will remove such intermediate steps from our copy-pasta workflows.  

At the moment I have plans for these R Studio addins:
* `tribble_paste()` **(complete)** which pastes a table on the clipboard as a nicely formatted call to `tribble()`
* `vector_paste()` **(in development)** which will paste delimited data as a character vector definition, e.g. `c("a", "1")` etc.

