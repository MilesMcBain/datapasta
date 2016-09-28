# data pasta 0.2
* Added graceful error handling on failed parse of text on clipboard to table.
* `tribble_paste()` and `vector_paste()` now pastse NA's as unquoted, so R will parse as propper NA.
* `tribble_paste()` can parse an paste table text copied from raw delimited file e.g. csv, tsv, pipe delimited, seimi-colon delimited.
* `vector_paste()` uses a space between elements.

# datapasta 0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Fixed the handling of NAs in tab delimited files which resulted in phantom NA columns sometimes appearing with `tribble_paste()`



