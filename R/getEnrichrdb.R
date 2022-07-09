#' Get EnrichrDB geneset and geneset_name
#' Geneset is a data.frame of 2 columns with term id and gene id
#'
#' @param org Organism from 'human','fly','yeast','worm','zebrafish'.
#' @param library Choose one library name from `enrichr_metadata`.
#' @param download.method "auto" (as default if NULL), "wininet" (for windows)
#' @importFrom dplyr %>% filter pull
#'
#' @return A list including geneset and geneset name.
#' @export
#' @examples
#' \dontrun{
#' x = getEnrichrdb(org = "human", library = "COVID-19_Related_Gene_Sets")
#' }
getEnrichrdb <- function(org = c('human','fly','yeast','worm','zebrafish'),
                         library = NULL,
                         download.method = NULL) {

  #--- args ---#
  org <- match.arg(org)
  if(is.null(library)) stop('Please choose gene set library from: `enrichr_data()`')

  data_dir <- tools::R_user_dir("geneset", which = "data")
  data_dir <- paste0(data_dir, "/anno/enrichrdb/")
  make_dir(data_dir)

  #--- download ---#
  res <- list()
  for(i in c("geneset")){
    # i = 'geneset'
    url <- paste0('https://genekitr-china.oss-accelerate.aliyuncs.com/anno/enrichrdb/',
                  org,"/",library,"_",i,".fst")
    destfile <- paste0(data_dir, "/", org, "_",library,"_",i,".fst")
    web_f_size <- check_web_size(url)
    local_f_size <- file.size(destfile)
    if(is.na(local_f_size)) local_f_size = 0

    geneset_download(url, destfile, method = download.method,
                     data_dir, web_f_size, local_f_size)
    res[[i]] = suppressMessages(fst::read.fst(destfile))
  }

  invisible(res)
}

