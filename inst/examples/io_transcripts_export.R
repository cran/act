library(act)

# Set destination folder
outputFolder <- tempdir()

# It makes more sense, however, to you define a folder
# that is easier to access on your computer
\dontrun{
outputFolder <- "PATH_TO_AN_EMPTY_FOLDER_ON_YOUR_COMPUTER"
}

# Exports all transcript objects in Praat TextGrid format
act::transcripts_export(x=examplecorpus,
                              outputFolder=outputFolder,
                              formats="textgrid")

# Exports all transcript objects in ELAN eaf format.
# By default WITHOUT creating media links
act::transcripts_export(x=examplecorpus,
                                 outputFolder=outputFolder,
                                 formats="eaf")

# Same same, but now WITH media links.
# Only Media links are only exported that are in
# the '@path.media' attribute in the transcript object(s))
act::transcripts_export(x=examplecorpus,
                                 outputFolder=outputFolder,
                                 formats="eaf",
                                 createMediaLinks=TRUE)

# Exports in 'eaf' and 'textgrid' format
act::transcripts_export(x=examplecorpus,
                                 outputFolder=outputFolder,
                                 formats=c("eaf", "textgrid"),
                                 createMediaLinks=TRUE)

# In case you do not want to export all tiers of the transcripts
# you can use the following options:
# * \code{act.export.filter.tiers.exclude}
# * \code{act.export.filter.tiers.include}
# Please see the section about setting options for act.
