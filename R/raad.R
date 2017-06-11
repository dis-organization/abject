
#' @name raad
#' @export
#' @importFrom R6 R6Class
raad <- R6::R6Class("raad",
                public = list(
                  name = NULL,
                  files = NULL,
                  initialize = function(name = NA) {
                    self$name <- name
                    self$files<- dplyr::mutate(self$refresh_files(), fullname = file.path(getOption('default.datadir'), fullname))
                    self$greet()
                  },
                  refresh_files = function() {
                    self$files <- ifiles()
                  },
                  summary_files = function() {
                    self$files %>% summarize(nfiles = n(), start_date = min(date), end_date = max(date), days_between = diff(range(unclass(as.Date(date)))))
                  },
                  get_date = function(date) {
                    if (length(date) > 1) stop("get_date is for a single slice, use get_date_range instead")
                    date <- as.POSIXct(date, tz = "GMT")
                    files<- self$files
                    raster::setZ(.readNSIDC(files$fullname[findInterval(date, files$date)]), date)
                  },
                  get_date_range = function(date) {
                    if (!length(date) == 2) stop("get_date_range is for a series within a range, use get_date for a time step and get_date_any for an arbitrary set")
                    date_range <- as.POSIXct(date, tz = "GMT")
                    files<- self$files %>% filter(date >= date_range[1L], date <= date_range[2L])
                    raster::setZ(raster::brick(stack(lapply(files$fullname, function(x) .readNSIDC(x)))), files$date)
                  },
                  get_date_any = function(date) {
                    #if (length(date) < 2) stop("get_date_any is for arbitrary sets of dates, use get_date for a time step and get_date_range for a sequence")
                    date <- as.POSIXct(date, tz = "GMT")
                    files<- self$files[findInterval(date, self$files$date), ]

                    raster::setZ(raster::brick(raster::stack(lapply(files$fullname, function(x) .readNSIDC(x)))), date)
                  },
                  calculate_climatology = function(date) {
                    raster::calc(self$get_date_any(date), fun = mean, na.rm = TRUE)
                  },
                  extract_xyt = function(xyt) {
                    extract(raadtools::readice, xyt)
                  },
                  greet = function() {
                    cat(paste0("Hello, my name is ", self$name, ".\n"))
                  }
                )
)


ifiles <- function() {
  file_src_path <- "allfiles.sqlite"
  library(dplyr)
  flist <- dplyr::tbl(dplyr::src_sqlite(file.path(getOption('default.datadir'), 'admin', 'filelist', file_src_path)), "file_list")
  flist <- flist %>% filter(fullname %like% "%sidads.colorado.edu%",
                            #  fullname %like% "%bin",
                            fullname %like% "%v1.1%",
                            fullname %like% "%south%",
                            #  fullname %like% "%nt_%",
                            fullname %like% "%daily%") %>%
    collect()

  datepart <- sapply(strsplit(basename(flist$fullname), "_"), function(x) x[length(x) - 3])

  datepat <-  "%Y%m%d"
  flist$date <- raadtools::timedateFrom(as.POSIXct(strptime(datepart, datepat, tz = "GMT")))
  flist <- filter(flist, !is.na(date))
  arrange(flist, date)
}



.readNSIDC <- function(fname) {
  con <- file(fname, open = "rb")
  trash <- readBin(con, "integer", size = 1, n = 300)
  dat <- readBin(con, "integer", size = 1, n = prod(dims), endian = "little", signed = FALSE)
  close(con)
  r100 <- dat > 250
  r0 <- dat < 1
  if (rescale) {
    dat <- dat/2.5  ## rescale back to 100
  }
  if (setNA) {
    dat[r100] <- NA
    ##dat[r0] <- NA
  }

  # 251  Circular mask used in the Arctic to cover the irregularly-shaped data gap around the pole (caused by the orbit inclination and instrument swath)
  # 252	Unused
  # 253	Coastlines
  # 254	Superimposed land mask
  # 255	Missing data
  #
  ## ratify if neither rescale nor setNA set
  r <- raster(t(matrix(dat, dims[1])), template = rtemplate)
  if (!setNA && !rescale) {
    ##r <- ratify(r)
    rat <- data.frame(ID = 0:255, icecover = c(0:250, "ArcticMask", "Unused", "Coastlines", "LandMask", "Missing"),
                      code = 0:255, stringsAsFactors = FALSE)
    levels(r) <- rat
    r
  } else {
    r
  }
}
