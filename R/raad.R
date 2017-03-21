
#' @name raad
#' @export
#' @importFrom R6 R6Class
raad <- R6::R6Class("raad",
                public = list(
                  name = NULL,
                  files = NULL,
                  initialize = function(name = NA) {
                    self$name <- name
                    self$files<- self$refresh_files()
                    self$greet()
                  },
                  refresh_files = function() {
                    self$files <- raadtools::icefiles()
                  },
                  summary_files = function() {
                    self$files %>% summarize(nfiles = n(), start_date = min(date), end_date = max(date), days_between = diff(range(unclass(as.Date(date)))))
                  },
                  get_date = function(date) {
                    if (length(date) > 1) stop("get_date is for a single slice, use get_date_range instead")
                    date <- as.POSIXct(date, tz = "GMT")
                    files<- self$files
                    raster::raster(files$fullname[findInterval(date, files$date)])
                  },
                  get_date_range = function(date) {
                    if (!length(date) == 2) stop("get_date_range is for a series within a range, use get_date for a time step and get_date_any for an arbitrary set")
                    date_range <- as.POSIXct(date, tz = "GMT")
                    files<- self$files %>% filter(date >= date_range[1L], date <= date_range[2L])
                    raster::brick(stack(lapply(files$fullname, raster::raster)))
                  },
                  get_date_any = function(date) {
                    #if (length(date) < 2) stop("get_date_any is for arbitrary sets of dates, use get_date for a time step and get_date_range for a sequence")
                    date <- as.POSIXct(date, tz = "GMT")
                    files<- self$files[findInterval(date, self$files$date), ]

                    raster::brick(raster::stack(lapply(files$fullname, raster::raster)))
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
