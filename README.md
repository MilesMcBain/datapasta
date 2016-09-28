# The Goods 
## tribble_paste
![pow!](https://raw.githubusercontent.com/milesmcbain/datapasta/master/inst/media/tribble_paste.gif)

## vector_paste 
![blam!](https://raw.githubusercontent.com/milesmcbain/datapasta/master/inst/media/vector_paste.gif)


# Introducing datapasta

datapasta is about reducing resistance associated with copying and pasting data into R Studio. It is a response to the realisation that I often found myself using Sublime as an intermediate text munging step when copying data. Hopefully addins in this package will remove such intermediate steps from our copy-pasta workflows.  

At the moment this package contains functional versions of these R Studio addins:
* `tribble_paste()` which pastes a table on the clipboard as a nicely formatted call to `tibble::tribble()`
    - Recomend `ctrl + shift + t` as shortcut.
    - Table can be delimited with tab, comma, pipe or semicolon. 
* `vector_paste()` which will paste delimited data as a character vector definition, e.g. `c("a", "1")` etc.
    - Recommend `ctrl + shift + v` as shortcut.

# Pitfalls

* Data is not parsed from string to any other format. This is challenging to do robustly in an efficient manner. Ideas on this are welcome!
* `tribble_paste` works well with csv's, excel files, and html tables, but is currently brittle with respect to irregular table structures like merged cells.
* Quoted csv data, where the quotes contain commas will not be parsed correctly.

# Prerequisites
* Linux users will need to install either `xsel` or `xclip`. These applications provide an interface to X selections (clipboard-like).
    - For example: `sudo apt-get install xsel` - it's 72kb...
* Windows and MacOS have nothing extra to do.


# Installation

1. Get the package: `devtools::install_github("milesmcbain/datapasta")`
2. Set the keyboard shortcuts using **Tools** -> **Addins** -> **Browse Addins**, then click **Keyboard Shortcuts...**

# Prior art

This package is made possible by [mdlincon's clipr](https://github.com/mdlincoln/clipr) and [hadley's tibble](https://github.com/hadley/tibble). I especially appreciate `clipr's` thoughtful approach to the clipboard on Linux, which pretty much every other R clipboard package just nope'd out on.

#Future developments
I'm looking at ways to address the pitfalls. The current priority is handling data types as I think that has the most utility. Feel free to contribute your ideas for solving these problems to the open issues.


