library(act)

# Get a transcript
t <- examplecorpus@transcripts[[1]]

# Create print transcript
printtranscript <- act::snippets_printtranscript (t=t)

# Display on screen
cat(stringr::str_c(printtranscript, sep="\n", collapse = "\n"))

