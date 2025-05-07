library(remotes)
#install_github("brry/rdwd", build_vignettes=TRUE)
library(rdwd)

?selectDWD
# select a dataset (e.g. last year's daily climate data from Potsdam city):
link <- selectDWD(id = 2997, res="daily", var=NA, per="recent")



link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")

# Actually download that dataset, returning the local storage file name:
file <- dataDWD(link, read=FALSE)

# Read the file from the zip folder:
clim <- readDWD(file, varnames=TRUE)

# Inspect the data.frame:
str(clim)