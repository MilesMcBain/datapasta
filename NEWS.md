# datapasta 2.0.0 'Fusilli Jerry'

* Added the ability to parse objects from R and output as neatly formatted tibbles, dataframes and vectors with `dpasta`. The clipboard is not involved.
* Added the ability to send these same types of objects to the clipboard formatted for markdown output with `dmdclip`.
* Package can now operate in a close to fully featured way in editors other than RStudio. Output goes to console rather than cursor.
* Added hooks for output customisation with `_construct()` functions that return the formatted output as an R character vector.
* The decimal mark can be set for numeric data with `dp_set_decimal_mark`.
* User can now paste natural looking comma separated lists as vectors, with automatic comma-splitting and whitespace trimming.  

# datapasta 1.1.0 'CopyPesto'

* Added `df_paste()` which pastes a table from the clipboard using a nicely formatted call to `data.frame()` rather than `tribble()`
* Better handling for empty lines that get accidently copied onto clipboard with table. Gracefully ignored.

# datapasta 1.0.0

* Added new addin 'Paste as vector (vertical)' to provide nicer formatting for long lists.
* All addins now guess data types and format correctly in the source editor.
* Empty rows in tables and empty cells in lists are formatted as NA's when pasting instead of being ignored.
* Added vignette, automated tests etc in prep for CRAN submission.

# datapasta 0.2

* Added graceful error handling on failed parse of text on clipboard to table.
* `tribble_paste()` and `vector_paste()` now pastse NA's as unquoted, so R will parse as propper NA.
* `tribble_paste()` can parse an paste table text copied from raw delimited file e.g. csv, tsv, pipe delimited, seimi-colon delimited.
* `vector_paste()` uses a space between elements.

# datapasta 0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Fixed the handling of NAs in tab delimited files which resulted in phantom NA columns sometimes appearing with `tribble_paste()`



