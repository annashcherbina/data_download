### PREAMBLE ######################################################################################

source("http://depot.sagebase.org/CRAN.R")
pkgInstall("synapseClient")

require(RSQLite)
require(parallel)
require(synapseClient)

data.path <- "/scratch/PI/euan/projects/mhc/data"
cache.path <- "/scratch/PI/euan/projects/mhc/data/synapseCache"
synapseCacheDir(cache.path)
synapseLogin()

### DATA ##########################################################################################

### survey data

sync.survey <- function() {
    # query your project for all of the tables
    projectId <- "syn3270436"
    pq <- synQuery(paste("SELECT id, name FROM table WHERE parentId=='", projectId, "'", sep=""))

    # download survey function
    download.survey <- function(i,pq) {
        cat(paste0("* DOWNLOAD TABLE: ", pq[i,"table.name"],"\n"))
        sq <- synTableQuery(paste('SELECT * FROM ', pq[i,"table.id"]), filePath = file.path(paste0(data.path, "tables"),paste0(pq[i,'table.name'],".csv")))
	Sys.sleep(0.5)
        #sq <- synTableQuery(paste('SELECT * FROM ', pq[i,"table.id"]), filePath = file.path(tempdir(), paste0(i,".csv")))
        write.table(sq@values, file.path(paste0(data.path, "tables"),paste0(pq[i,'table.name'],".tsv")), quote=F,sep="\t",row.names=T)
	Sys.sleep(0.5)
        }

    # download all surveys
    sq.all <- mclapply(as.list(1:nrow(pq)), download.survey, pq, mc.cores=5)
    #sq.all <- lapply(as.list(1:nrow(pq)), download.survey)
    }

sync.survey()


### blob / accelerometer data

sync.blob <- function(x) {
    # check which columns are filehandle function
    is.filehandle.col <- function(k,sc) {
        if(sc[[k]]@columnType=="FILEHANDLEID"){
            return(sc[[k]]@name)
            } 
        else{
            return(NULL)
            }
        }


#ORIGINAL SET OF TABLES:
#for (j in c('syn3458480','syn3420486','syn4214144','syn4214144','syn4095792','syn3560085')) {
    #NEW TABLES FOR HealthKitWorkoutCollector, HealthKitSleepCollector, cardiovascular-motionActivityTracker
    for (j in c('syn4536838','syn3560095','syn3560086','syn3458480','syn3420486','syn4214144','syn4214144','syn4095792','syn3560085','syn4857044')) {
        cat(paste0("* DOWNLOAD TABLE: ", j,"\n"))

        # query table for column annotation
        tq <- synTableQuery(paste('SELECT * FROM ', j), filePath = file.path(tempdir(), paste0(j,".csv")))
	Sys.sleep(0.5)		  
        # get column data from a particular survey
        sc <- synapseClient:::synGetColumns(tq@schema)

        # check which columns are filehandles
        theseCols <- sapply(as.list(1:length(sc)), is.filehandle.col,sc)
        theseCols <- unlist(theseCols)

        # download
        theseFiles <- list()
        for (col in theseCols) {
            cat(paste0("* DOWNLOAD BLOB: ", col,"\n"))
            get_blob_filename <- function(file.code) {
                ped.cache.dir <-  paste0(cache.path, "/", as.numeric(substr(file.code, 5,8)),"/", as.numeric(file.code))
                return(ped.cache.dir)
                }
            theseFiles[[col]] <-  mclapply(as.list(rownames(tq@values)), function(rn,col,tq) {file.code <- tq@values[rn,col];if(file.exists(get_blob_filename(file.code))){return(NA)}else{cat(paste0("* DOWNLOADING BLOB ROW: ", rn,"\n"));tryCatch(synDownloadTableFile(tq, rn, col), error=function(e) NA)}}, col,tq, mc.cores=20)
            }

        }
    }

sync.blob()

