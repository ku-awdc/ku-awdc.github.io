## PUBLIC script to download a zipped drat repo on an SFTP server, unpack it and add the tempfile to the repos ##

# Functions taken from the R pacakge:

.wrapfun <- function(){

check_curl <- 
function (feedback) 
{
    if ("sftp" %in% curl::curl_version()$protocols) {
        return(TRUE)
    }
    else {
        if (Sys.info()["sysname"] == "Darwin") {
            cat("The standard version of the curl package for macOS does not have support for sftp. Please do the following:\n1) Open a terminal window (Applications/Utilities/Terminal)\n2) Install the macOS compilers via the terminal window using `xcode-select --install`\n3) Install homebrew by following the instructions at https://brew.sh\n4) Type `brew install curl-openssl` in the terminal window and wait for the command to finish\n5) Type `export PKG_CONFIG_PATH=\"/usr/local/opt/curl-openssl/lib/pkgconfig\"; export LDFLAGS=\"-L/usr/local/opt/curl-openssl/lib\"; export CPPFLAGS=\"-I/usr/local/opt/curl-openssl/include\"`\n6) Open R and re-install curl using `install.packages(\"curl\", type=\"source\")`\n7)", 
                feedback, "to check it works\nIf after doing this you still have problems, then please contact Matt for assistance.\n")
        }
        else {
            cat("The version of curl installed on your system does not have support for curl. Please contact Matt for assistance.\n")
        }
        return(FALSE)
    }
}

safe_unzip <- 
function (zipfile, exdir = gsub("\\.zip$", "", zipfile), files = NULL, 
    silent = TRUE) 
{
    if (!silent) 
        cat("Unzipping the file/folder...\n")
    prompted <- FALSE
    ss <- try({
        zip::unzip(zipfile, files = files, exdir = exdir)
    })
    if (inherits(ss, "try-error")) {
        unlink(exdir, recursive = TRUE)
        if (Sys.info()["sysname"] == "Darwin") {
            cat("Unzipping the file failed with zip::unzip - trying again with system utilities...\n")
            Sys.sleep(0.5)
            ss <- try({
                system(str_c("open \"", zipfile, "\""))
            })
            readline(prompt = "Hit any key to continue")
            prompted <- TRUE
        }
    }
    if (inherits(ss, "try-error")) {
        unlink(exdir, recursive = TRUE)
        if (!interactive()) 
            stop("File could not be unzipped automatically")
        cat("An error occured while using zip::unzip from within R - please uncompress the folder manually\nOpening folder...")
        Sys.sleep(1)
        system(str_c("open \"", gsub("/[^/]*$", "", zipfile), 
            "\""))
        system(str_c("open \"", zipfile, "\""))
        readline(prompt = "Hit any key to continue")
        prompted <- TRUE
    }
    if (!file.exists(exdir)) {
        file.copy(zipfile, getwd())
        stop("Attempt to unzip failed - the zip file has been copied to your working directory", 
            call. = FALSE)
    }
    fld <- strsplit(zipfile, "/", fixed = TRUE)[[1]]
    fld <- gsub(".zip", "", fld[length(fld)], fixed = TRUE)
    if (file.exists(file.path(exdir, fld)) && file.info(file.path(exdir, 
        fld))[1, "isdir"]) {
        exdir <- file.path(exdir, fld)
    }
    if (!prompted) {
        dly <- suppressWarnings(as.numeric(Sys.getenv("SAFE_ZIP_DELAY")))
        if (Sys.getenv("SAFE_ZIP_DELAY") != "" && is.na(dly)) {
            readline(prompt = "Hit any key to continue")
        }
        else if (!is.na(dly)) {
            Sys.sleep(dly)
        }
        else {
            Sys.sleep(0.5)
        }
    }
    if (!silent) 
        cat("Done\n")
    return(exdir)
}

fetch_drat_ll <- 
function (server = "", userpwd = "", feedback, silent = FALSE) 
{
    curlok <- check_curl(feedback)
    if (!curlok) 
        stop("Unable to fetch the drat repository: curl does not have sftp support", 
            call. = FALSE)
    if (identical(server, "")) {
        cat("Enter the SFTP server address (this should start with 'sftp://') \n")
        server <- readline(prompt = "\tServer address: ")
        if (!grepl("^sftp://", server)) {
            stop("The specified SFTP server address '", server, 
                "' should start with 'sftp://'", call. = FALSE)
        }
    }
    server <- gsub("COVID19_Model_share/", "", server)
    server <- gsub("COVID19_Model_share", "", server)
    server <- gsub("COVID19_Model/", "", server)
    server <- gsub("COVID19_Model", "", server)
    if (grepl("/$", server)) 
        server <- gsub("/$", "", server)
    if (identical(userpwd, "")) {
        userpwd <- readline(prompt = "\tUsername: ")
        if (!grepl(":", userpwd)) {
            pass <- readline(prompt = "\tPassword: ")
            userpwd <- str_c(userpwd, ":", pass)
        }
    }
    if (!silent) 
        cat("Downloading and unpacking the drat repository to a temporary folder ... ")
    td <- c(Sys.getenv("TMPDIR"), Sys.getenv("TMP"), Sys.getenv("TEMP"), 
        "/tmp")
    td <- td[which(td != "")[1]]
    tmp <- tempfile("folder", td)
    dir.create(tmp)
    ss <- try({
        curl::curl_download(url = file.path(server, "COVID19_Model_share/Rpackages/drat.zip"), 
            destfile = file.path(tmp, "drat.zip"), handle = curl::new_handle(userpwd = userpwd))
    })
    if (inherits(ss, "try-error")) {
        if (!silent) 
            cat("\n")
        unlink(tmp, recursive = TRUE)
        stop("An error occured while fetching the drat repository - check the message above for clues", 
            call. = FALSE)
    }
    Sys.setenv(TEMP_SFTP_SERVER = server)
    Sys.setenv(TEMP_SFTP_USERPWD = userpwd)
    safe_unzip(file.path(tmp, "drat.zip"), exdir = tmp)
    tr <- paste0("file:", file.path(tmp, tempfile = "drat"))
    r <- getOption("repos")
    if ("tmpdrat" %in% names(r)) {
        unlink(r["tmpdrat"], recursive = TRUE)
        r["tmpdrat"] <- tr
    }
    else {
        r <- c(r, tmpdrat = tr)
    }
    options(repos = r)
    if (!silent) 
        cat("done\n")
    if (!silent) 
        cat("\nThe drat repository has been downloaded from the SFTP server and added to your available repositories\nThe private R packages are now available via install.packages() and/or update.packages()\n")
    if (getRversion() < "3.6") {
        warning("Your version of R (", as.character(getRversion()), 
            ") is not up to date, so pre-built binares are not available\nYou will have to install packages from source using install.packages(..., type='source')", 
            call. = FALSE)
    }
}


# Auto-run to setup:

if(!suppressPackageStartupMessages(require('curl', quietly=TRUE))) stop('The curl package could not be loaded - please make sure that both the curl and zip packages are installed and try again')
if(!suppressPackageStartupMessages(require('zip', quietly=TRUE))) stop('The zip package could not be loaded - please make sure that both the curl and zip packages are installed and try again')

fetch_drat_ll(server='', userpwd='', feedback='Re-source this online script', silent=FALSE)
}

.wrapfun()
