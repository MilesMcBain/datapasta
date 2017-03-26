# datapasta 2.0.0 'Fusilli Jerry'
[![CRAN status.](http://www.r-pkg.org/badges/version/datapasta)](http://www.r-pkg.org/pkg/datapasta)
[![Downloads](http://cranlogs.r-pkg.org/badges/datapasta)](https://CRAN.R-project.org/package=datapasta)

# The Goods
![pow!](https://raw.githubusercontent.com/milesmcbain/datapasta/master/inst/media/tribble_paste.gif)

0 to `datapasta` in 64 seconds via a video vignette:

[![Datapasta in 64 seconds](http://img.youtube.com/vi/Sz-tEVqZh5s/0.jpg)](https://youtu.be/Sz-tEVqZh5s)

# Introducing datapasta

`datapasta` is about reducing resistance associated with copying and pasting data to and from R. It is a response to the realisation that I often found myself using intermediate programs like Sublime to munge text into suitable formats. A lot of work has gone into making `datapasta` support a wide variety of input and output situations, so it (probably) "just works". Hopefully tools in this package will remove such intermediate steps and associated frustrations from our data slinging workflows.  

## Use with RStudio

### Getting data into source

At the moment this package contains these RStudio addins that paste data to the cursor:

* `tribble_paste` which pastes a table as a nicely formatted call to `tibble::tribble()`
    - Recommend <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>t</kbd> as shortcut.
    - Table can be delimited with tab, comma, pipe or semicolon. 
* `vector_paste` which will paste delimited data as a vector definition, e.g. `c("a", "b")` etc.
    - Recommend <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>Shift</kbd> + <kbd>v</kbd> as shortcut.
* `vector_paste_vertical` which will paste delimited data as a vertically formatted vector definition.
    - Recommend <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>v</kbd> as shortcut 
    - example output:
```
c("Mint",
  "Fedora",
  "Debian",
  "Ubuntu",
  "OpenSUSE")
```
* `df_paste` which pastes a table on the clipboard as a standard `data.frame` definition rather than a `tribble` call. This has certain advantages in the context of reproducible examples and educational posts. Many thanks to [Jonathan Carroll](https://github.com/jonocarroll) for getting this rolling and coding the bulk of the feature.
    - Recommend <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>Shift</kbd> + <kbd>d</kbd> as shortcut.
    
### Getting Data out of an R session
There are two R functions available that accept R objects and output formatted text for pasting to other applications:

* `dpaste` accepts tibbles, data.frames, and vectors. Data is output in a format that matches in input class. Formatted text is pasted at the cursor.

* `dmdclip` accepts the same inputs as `dpaste` but inserts the formatted text onto the clipboard, preceded by 4 spaces so that is can be as pasted as a preformatted block to Github, Stackoverflow etc.
    
## Use with other editors

The only hard dependency of `datapasta` is `readr` for type guessing. All the above `*paste` functions can be called directly instead of as an addin, and will fall back to console output if the `rsudioapi` is not available.

On system without access to the clipboard (or without `clipr` installed) `datapasta` can still be used to output R objects from an R session. `dpaste` is probably the only function you care about in this scenario.

### Custom Installation

`datapasta` imports `clipr` and `rstudioapi` so as to make installation smooth and easy for most users. If you wish to avoid installing an `rstudioapi` you will never use you can use: 

* `install.packages("datapasta", dependencies = "Depends")`.
* Followed by `install.packages("clipr")` to enable clipboard features. 


# Pitfalls
* `tribble_paste` works well with CSVs, excel files, and html tables, but is currently brittle with respect to irregular table structures like merged cells or multi-line column headings. For some reason Wikipedia seems chock full of these. :(
* Quoted csv data, where the quotes contain commas will not be parsed correctly.
* Nested list columns have limited support with `tribble_paste()` nested lists will work but nested `tibbles` will be converted to list calls. 

# Prerequisites
* Linux users will need to install either `xsel` or `xclip`. These applications provide an interface to X selections (clipboard-like).
    - For example: `sudo apt-get install xsel` - it's 72kb...
* Windows and MacOS have nothing extra to do.


# Installation

1. Get the package: `install.packages("datapasta")`
2. Set the keyboard shortcuts using **Tools** -> **Addins** -> **Browse Addins**, then click **Keyboard Shortcuts...**

# Prior art

This package is made possible by [mdlincon's clipr](https://github.com/mdlincoln/clipr), and Hadley's packages [tibble](https://github.com/hadley/tibble) and [readr](https://github.com/hadley/tibble) (for data-type guessing). I especially appreciate `clipr's` thoughtful approach to the clipboard on Linux, which pretty much every other R clipboard package just nope'd out on.

# Future developments
I am interested in expanding the types of objects supported by the output functions `dpaste` and `dmdclip`. Feel free to contribute your ideas to the open issues.


