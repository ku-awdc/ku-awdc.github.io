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
    unzip_path, silent = TRUE) 
{
    if (!silent) 
        cat("Unzipping the file/folder...\n")
    dly <- suppressWarnings(as.numeric(Sys.getenv("SAFE_ZIP_DELAY")))
    if (is.na(dly)) 
        dly <- 0.5
    tdir <- strsplit(zipfile, split = "/", fixed = TRUE)[[1]]
    tdir <- paste(tdir[-length(tdir)], collapse = "/")
    oldfiles <- list.files(tdir)
    ss <- !inherits(try({
        zip::unzip(zipfile, files = files, exdir = exdir)
    }), "try-error")
    if (!ss) {
        unlink(exdir, recursive = TRUE)
        cat("Unzipping the file failed with zip::unzip - trying again with system unzip...\n")
        if (missing(unzip_path)) {
            unzip_path <- system("which unzip", intern = TRUE)
        }
        ss <- system(paste0(unzip_path, " -o -d \"", exdir, "\" \"", 
            zipfile, "\"")) == 0
        Sys.sleep(dly)
    }
    prompted <- FALSE
    if (!ss) {
        unlink(exdir, recursive = TRUE)
        cat("Unzipping the file failed with system unzip - trying to open the file directly...\n")
        Sys.sleep(0.5)
        ss <- system(paste0("open \"", zipfile, "\"")) == 0
        if (interactive()) {
            readline(prompt = "Hit any key to continue")
        }
        else {
            Sys.sleep(dly)
        }
        prompted <- TRUE
    }
    if (!ss) {
        unlink(exdir, recursive = TRUE)
        if (!interactive()) 
            stop("File could not be unzipped automatically")
        cat("An error occured while using zip::unzip from within R - please uncompress the folder manually\nOpening folder...")
        Sys.sleep(1)
        system(paste0("open \"", gsub("/[^/]*$", "", zipfile), 
            "\""))
        system(paste0("open \"", zipfile, "\""))
        readline(prompt = "Hit any key to continue")
        prompted <- TRUE
    }
    newfiles <- list.files(tdir)
    newfiles <- newfiles[!newfiles %in% oldfiles]
    if (!file.exists(exdir) && length(newfiles) == 1) {
        cat("NOTE: renaming \"", newfiles, "\" to correspond to the ZIP file name...\n", 
            sep = "")
        file.rename(file.path(tdir, newfiles), exdir)
    }
    if (!file.exists(exdir)) {
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
        if (Sys.getenv("SAFE_ZIP_DELAY") != "" && is.na(dly) && 
            interactive()) {
            readline(prompt = "Hit any key to continue")
        }
        else {
            Sys.sleep(dly)
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
    serverset <- FALSE
    if (identical(server, "")) {
        cat("Enter the SFTP server address (this should start with 'sftp://') \n")
        server <- readline(prompt = "\tServer address: ")
        if (!grepl("^sftp://", server)) {
            stop("The specified SFTP server address '", server, 
                "' should start with 'sftp://'", call. = FALSE)
        }
        serverset <- TRUE
    }
    server <- gsub("COVID19_Model_share/", "", server)
    server <- gsub("COVID19_Model_share", "", server)
    server <- gsub("COVID19_Model/", "", server)
    server <- gsub("COVID19_Model", "", server)
    if (grepl("/$", server)) 
        server <- gsub("/$", "", server)
    if (userpwd == "") {
        userpwd <- getPass::getPass(msg = "\tEnter username: ")
        if (!grepl(":", userpwd)) {
            pass <- getPass::getPass(msg = "\tEnter password: ")
            userpwd <- paste0(userpwd, ":", pass)
        }
    }
    if (!silent) 
        cat("Downloading and unpacking the drat repository to a temporary folder ... \n")
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
        unlink(tmp, recursive = TRUE)
        stop("An error occured while fetching the drat repository - check the message above for clues", 
            call. = FALSE)
    }
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
    if (!silent) {
        ap <- available.packages(repos = tr)[, c("Package", "Version"), 
            drop = FALSE]
        ap <- data.frame(Package = ap[, 1], Available = as.character(ap[, 
            2]), stringsAsFactors = FALSE)
        ip <- installed.packages()[, c("Package", "Version"), 
            drop = FALSE]
        ip <- data.frame(Package = ip[, 1], Installed = as.character(ip[, 
            2]), stringsAsFactors = FALSE)
        pp <- merge(ip, ap, by = "Package", all.x = FALSE, all.y = TRUE)
        pp$Installed[is.na(pp$Installed)] <- "<none>"
        cat("The drat repository has been downloaded from the SFTP server and added to your available repositories\nThe following R packages are now available via install.packages():\n\n")
        print(pp, row.names = FALSE)
        srctp <- FALSE
        if (getRversion() < "3.6") {
            srctp <- TRUE
            warning("Your version of R (", as.character(getRversion()), 
                ") is not up to date, so pre-built binares are not available", 
                call. = FALSE)
        }
        pp$Current <- sapply(seq_len(nrow(pp)), function(i) pp$Installed[i] != 
            "<none>" && package_version(pp$Installed[i]) >= package_version(pp$Available[i]))
        if (all(pp$Current) && !"<none>" %in% pp$Installed) {
            cat("\nYour private repo packages are up to date!\n")
        }
        else {
            if (interactive()) {
                optcmd <- character(4)
                tpp <- pp$Package[!pp$Current && pp$Installed != 
                  "<none>"]
                if (length(tpp) > 0) {
                  optcmd[1] <- str_c("install.packages(\"", paste(tpp, 
                    collapse = "\", \""), "\"", if (srctp) 
                    ", type=\"source\"", ")")
                }
                tools <- which(grepl("tools$", pp$Package))
                if (length(tools) == 1 && !pp$Current[tools]) {
                  optcmd[2] <- str_c("install.packages(\"", pp$Package[tools], 
                    "\"", if (srctp) 
                      ", type=\"source\"", ")")
                }
                tpp <- pp$Package[pp$Installed == "<none>"]
                if (length(tpp) > 0) {
                  optcmd[3] <- str_c("install.packages(\"", paste(tpp, 
                    collapse = "\", \""), "\"", if (srctp) 
                    ", type=\"source\"", ")")
                }
                optcmd[4] <- str_c("install.packages(\"", paste(pp$Package, 
                  collapse = "\", \""), "\"", if (srctp) 
                  ", type=\"source\"", ")")
                optcmd <- unique(optcmd[optcmd != ""])
                cat("\nSuggested actions:\n", sep = "")
                for (a in seq_along(optcmd)) {
                  cat(a, ": ", optcmd[a], "\n", sep = "")
                }
                cat("[Or any other key to do nothing]\n", sep = "")
                action <- readline("Enter selection: ")
                if (action %in% as.character(seq_along(optcmd))) {
                  eval(parse(text = optcmd[as.numeric(action)]))
                }
            }
        }
    }
}


# Auto-run to setup:

if(!suppressPackageStartupMessages(require('curl', quietly=TRUE))) stop('The curl package could not be loaded - please make sure that the curl, zip and getPass packages are installed and try again')
if(!suppressPackageStartupMessages(require('zip', quietly=TRUE))) stop('The zip package could not be loaded - please make sure that the curl, zip and getPass packages are installed and try again')
if(!suppressPackageStartupMessages(require('getPass', quietly=TRUE))) stop('The getPass package could not be loaded - please make sure that the curl, zip and getPass packages are installed and try again')

if(length(find('server')) == 0) server <- ''
fetch_drat_ll(server=server, userpwd='', feedback='Re-source this online script', silent=FALSE)
}

.wrapfun()
