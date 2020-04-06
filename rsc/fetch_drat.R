## PUBLIC script to download a zipped drat repo on an SFTP server, unpack it and add the tempfile to the repos ##

# Functions taken from the R pacakge:

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
        stop("An error occured while fetching the drat repository - check the message above for clues", 
            call. = FALSE)
    }
    zip::unzip(file.path(tmp, "drat.zip"), exdir = tmp)
    r <- getOption("repos")
    if ("covid19drat" %in% names(r)) {
        r["covid19drat"] <- paste0("file:", file.path(tmp, tempfile = "drat"))
    }
    else {
        r <- c(r, covid19drat = paste0("file:", file.path(tmp, 
            tempfile = "drat")))
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
