library(act)

# The example files that come with the act library are located here:
path <- system.file("extdata", "examplecorpus", package="act")

# This is the examplecorpus object that comes with the library
examplecorpus

# Make sure that the input folder of the example corpus object is set correctly
examplecorpus@folders.annotationfiles <- path
examplecorpus@folders.media <- path

# Load annotation files into the corpus object (again)
examplecorpus <- act::corpus_load(x=examplecorpus)

# Creating the full texts may take a long time.
# If you do not want to create the full texts immediately use the following command:
examplecorpus <- act::corpus_load(x=examplecorpus, createFullText=FALSE )
