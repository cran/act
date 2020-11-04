library(act)

# Search
mysearch <- act::search_new(examplecorpus, pattern="yo")

# Create print transcript snippets for all search results
test <- act::snippets_printtranscripts_search (x=examplecorpus,
                                                   s=mysearch)

# Display all transcript snippets on screen
cat(stringr::str_c(test$transcript, sep="\n", collapse = "\n"))

# Create print transcript snippets including 1 sec before and 5 sec after
test <- act::snippets_printtranscripts_search (x=examplecorpus,
                                                   s=mysearch,
                                                   contextBeforeSec=1,
                                                   contextAfterSec=5)

# Display all transcript snippets on screen
cat(stringr::str_c(test$transcript, sep="\n", collapse = "\n"))

