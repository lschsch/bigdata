#
#
# 
#
#
#
#
#
#
# 
work.dir <- getwd()
# 
#
#
make.folder.names <- c("data", "figures", "data.output")


for(i in 1:length(make.folder.names))
  if(file.exists(make.folder.names[i]) == FALSE)
    dir.create(make.folder.names[i])

path.data <- paste(work.dir, "/", make.folder.names[1], "/", sep = "")
path.figures <- paste(work.dir, "/", make.folder.names[2], "/", sep = "")
path.data.output <- paste(work.dir, "/", make.folder.names[3], "/", sep = "")
#
#
#
#