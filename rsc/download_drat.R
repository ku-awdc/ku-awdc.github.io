## Script to download the zipped drat repo on the SFTP server, unpack it and add the tempfile to the repos

if(length(find('sftp'))==0) stop("The variable 'sftp' must be set")
if(length(find('userpwd'))==0) stop("The variable 'userpwd' must be set")

cat("Checking for (and if necessary installing) required packages...\n")

if(!suppressPackageStartupMessages(require('curl', quietly=TRUE))) install.packages('curl')
if(!suppressPackageStartupMessages(require('zip', quietly=TRUE))) install.packages('zip')

cat("Downloading and unpacking drat repo to temporary folder...\n")

tmp <- tempfile('folder')
dir.create(tmp)
curl::curl_download(url = file.path(sftp, "Rpackages/drat.zip"), destfile = file.path(tmp,'drat.zip'), handle = curl::new_handle(userpwd = userpwd))

zip::unzip(file.path(tmp, 'drat.zip'), exdir = tmp)
r <- getOption("repos")
r <- c(r, temporary = paste0('file:', file.path(tmp, tempfile='drat')))
options(repos = r)

cat("Done - you should now be able to use install.packages()\n")


