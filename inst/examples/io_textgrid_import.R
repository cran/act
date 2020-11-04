library(act)

# Path to the TextGrid that you want to read
path <- system.file("extdata", "examplecorpus", "GAT", "ARG_I_PAR_Beto.TextGrid", package="act")

# To import a TextGrid file of your choice:
\dontrun{
path <- "PATH_TO_AN_EXISTING_TEXTGRID_ON_YOUR_COMPUTER"
}

# Read
t <- act::textgrid_import(input_path=path)

t

