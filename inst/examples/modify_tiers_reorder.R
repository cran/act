library(act)

# Check the order of the existing tiers in the first two transcripts
names(examplecorpus@transcripts[[1]]@tiers)
names(examplecorpus@transcripts[[2]]@tiers)

# Get tiernames to create the sort vector
sortVector <- c(names(examplecorpus@transcripts[[1]]@tiers),
                names(examplecorpus@transcripts[[2]]@tiers))

# Revert the vector for demonstration
sortVector <- sortVector[length(sortVector):1]

# This will only reorder the tiers
examplecorpus <- act::modify_tiers_reorder (x=examplecorpus, 
sortVector=sortVector)

# Check again the order of the tiers
names(examplecorpus@transcripts[[1]]@tiers)
names(examplecorpus@transcripts[[2]]@tiers)

# This will  reorder the tiers and additionally add tiers that are given  
# in the sort vector but not present in the transcript
examplecorpus <- act::modify_tiers_reorder (x=examplecorpus,
sortVector=sortVector,
addMissingTiers=TRUE)
# Check again the tiers
names(examplecorpus@transcripts[[1]]@tiers)
names(examplecorpus@transcripts[[2]]@tiers)

# Insert a tier called "newTier" into all transcripts in the corpus:
for (t in examplecorpus@transcripts) {
 sortVector <- c(names(t@tiers), "newTier")
 examplecorpus <- act::modify_tiers_reorder (x=examplecorpus,
sortVector=sortVector,
filterTranscriptsInclude=t@name,
addMissingTiers=TRUE)
}
# Check for example the first transcript: it now contains a tier called "newTier"
examplecorpus@transcripts[[1]]@tiers
