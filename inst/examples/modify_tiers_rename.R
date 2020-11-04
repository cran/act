library(act)

# Check the names of the existing tiers in the first two transcripts
names(examplecorpus@transcripts[[1]]@tiers)
names(examplecorpus@transcripts[[2]]@tiers)

examplecorpus <- act::modify_tiers_rename(examplecorpus, "Entrevistador", "E")

names(examplecorpus@transcripts[[1]]@tiers)
names(examplecorpus@transcripts[[2]]@tiers)

