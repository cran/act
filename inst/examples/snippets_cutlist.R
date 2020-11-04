library(act)

# Please note: in the example corpus the transcripts are assigned media links.
# The actual media files are however not included in when installing the package.
# Please see the section 'examplecorpus' for isntruction how to download the
# media files.

# Search
mysearch <- act::search_new(examplecorpus, pattern="yo")

# Create cut list for Mac
cutlist <- act::snippets_cutlist (x=examplecorpus, s=mysearch)

# Create cut list for Windows
cutlist <- act::snippets_cutlist (x=examplecorpus,
                                         s=mysearch,
                                         returnCutlistForMacInsteadOfWindows=TRUE)

# Display cut list on screen, so you can copy&paste this into the Terminal or the CLI
cat(stringr::str_c(cutlist, sep="\n", collapse = "\n"))

# It is, however, more convenient to specify the argument 'outputFolder' in order to get
# the cut list as a (executable) file/batch list.
