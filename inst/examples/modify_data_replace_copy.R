library(act)

# Have a look at the first transcript in the examplecorpus:
printtranscript <- act::snippets_printtranscript(examplecorpus@transcripts[[1]])
cat(stringr::str_c(printtranscript, sep="\n", collapse = "\n"))
# In line 01 there is the word "UN".

# Replace this word by "XXX" in the entire corpus
test <- act::modify_data_replace_copy(x=examplecorpus,
									  pattern="\\bUN\\b",
									  replacement="XXX")

# Have a look at the first transcript in the corprus object test:
printtranscript <- act::snippets_printtranscript(test@transcripts[[1]])
cat(stringr::str_c(printtranscript, sep="\n", collapse = "\n"))
# In line 01 there is now "XXX" instead of "UN"


# Insert a tier called "newTier" into all transcripts in the corpus:
for (t in examplecorpus@transcripts) {
	sortVector <- c(names(t@tiers), "newTier")
	examplecorpus <- act::modify_tiers_reorder (x=examplecorpus,
	sortVector=sortVector,
	filterTranscriptsInclude=t@name,
	addMissingTiers=TRUE)
}
# Check that the first transcript now contains the newTier
examplecorpus@transcripts[[1]]@tiers

# Now replace "UN" by "YYY" in the entire corpus and
# copy the search hit to "newTier".
test <- act::modify_data_replace_copy(x=examplecorpus,
									  pattern="\\bUN\\b",
									  replacement="YYY",
									  destTier = "newTier")

# Have a look again at the first transcript in the corpus object test.
printtranscript <- act::snippets_printtranscript(test@transcripts[[1]])
cat(stringr::str_c(printtranscript, sep="\n", collapse = "\n"))
# In line 01 you see that "UN" has been replaced by "YYY.
# In line 02 you see that it has been copied to the tier "newTier".


# If you only want to copy a search hit but not replace it in the original
# leave replacement="", which is the default
test <- act::modify_data_replace_copy(x=examplecorpus,
									  pattern="\\bUN\\b",
									  destTier = "newTier")
printtranscript <- act::snippets_printtranscript(test@transcripts[[1]])
cat(stringr::str_c(printtranscript, sep="\n", collapse = "\n"))
# In line 01 you see that "UN" has been maintained.
# In line 02 you see that "UN" it has been copied to the tier "newTier".

 