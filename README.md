# pythonR
`pythonR` includes helper functions for using Python in R. It is essentially a wrapper of the `reticulate` R package, and can helpful in an R package development or project environment where you want more readable code for setting up a python environment and installing any necessary python dependencies.

It may decrease some of the flexibility offered via `reticulate` for setting up virtual or conda environments (mostly in ignoring the `RETICULATE_PYTHON` python path), but it allows you to reproducibly set up the same environment each session, and install all python packages in a single call.


Additional helper functions are provided for ensuring required python packages are installed or importing them at various stages within an R function/script.
