# datapasta 1.1.0 'CopyPesto'
[![CRAN status.](http://www.r-pkg.org/badges/version/datapasta)](http://www.r-pkg.org/pkg/datapasta)
[![Downloads](http://cranlogs.r-pkg.org/badges/datapasta)](https://CRAN.R-project.org/package=datapasta)

#The Goods
## Paste as tribble
![pow!](https://raw.githubusercontent.com/milesmcbain/datapasta/master/inst/media/tribble_paste.gif)

## Paste as vector 
![blam!](https://raw.githubusercontent.com/milesmcbain/datapasta/master/inst/media/vector_paste.gif)

## Paste as data.frame
![sock!](https://raw.githubusercontent.com/milesmcbain/datapasta/master/inst/media/df_paste.gif)

# Introducing datapasta

datapasta is about reducing resistance associated with copying and pasting data into R Studio. It is a response to the realisation that I often found myself using Sublime as an intermediate text munging step when copying data. Hopefully addins in this package will remove such intermediate steps from our copy-pasta workflows.  

At the moment this package contains these R Studio addins:
* `tribble_paste()` which pastes a table on the clipboard as a nicely formatted call to `tibble::tribble()`
    - Recommend <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>t</kbd> as shortcut.
    - Table can be delimited with tab, comma, pipe or semicolon. 
* `vector_paste()` which will paste delimited data as a vector definition, e.g. `c("a", "b")` etc.
    - Recommend <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>Shift</kbd> + <kbd>v</kbd> as shortcut.
* `vector_paste_vertical()` which will paste delimited data as a vertically formatted vector definition.
    - Recommend <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>v</kbd> as shortcut 
    - example output:
```
c("Mint",
  "Fedora",
  "Debian",
  "Ubuntu",
  "OpenSUSE")
```
* `df_paste()` which pastes a table on the clipboard as a standard `data.frame` definition rather than a `tribble` call. This has certain advantages in the context of reproducible examples and educational posts. Many thanks to [Jonathan Carroll](https://github.com/jonocarroll) for getting this rolling and coding the bulk of the feature.
    - Recommend <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>Shift</kbd> + <kbd>d</kbd> as shortcut.
    
# Pitfalls
* `tribble_paste` works well with CSVs, excel files, and html tables, but is currently brittle with respect to irregular table structures like merged cells or multi-line column headings. For some reason Wikipedia seems chock full of these. :(
* Quoted csv data, where the quotes contain commas will not be parsed correctly.
* The addin does not currently work in RStudio server. The instance not have access to the local clipboard.

# Prerequisites
* Linux users will need to install either `xsel` or `xclip`. These applications provide an interface to X selections (clipboard-like).
    - For example: `sudo apt-get install xsel` - it's 72kb...
* Windows and MacOS have nothing extra to do.


# Installation

1. Get the package: `install.packages("datapasta")`
2. Set the keyboard shortcuts using **Tools** -> **Addins** -> **Browse Addins**, then click **Keyboard Shortcuts...**

# Prior art

This package is made possible by [mdlincon's clipr](https://github.com/mdlincoln/clipr), and Hadley's packages [tibble](https://github.com/hadley/tibble) and [readr](https://github.com/hadley/tibble) (for data-type guessing). I especially appreciate `clipr's` thoughtful approach to the clipboard on Linux, which pretty much every other R clipboard package just nope'd out on.

#Future developments
I'm looking at ways to address the pitfalls. The next thing I will look at is some kind of support on RStudio server, since I use this often myself. Feel free to contribute your ideas for solving these problems to the open issues.


