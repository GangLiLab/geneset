#' Get EnrichrDB geneset and geneset_name
#' Geneset is a data.frame of 2 columns with term id and gene id
#'
#' @param org Organism from 'human','fly','yeast','worm','zebrafish'.
#' @param library Choose one library name from `enrichr_metadata`.
#' @param download.method "auto" (as default if NULL), "wininet" (for windows)
#' @param data_dir data saving location, default is the package data directory
#' @importFrom dplyr %>% filter pull
#'
#' @return A list including geneset and geneset name.
#' @export
#' @examples
#' x = getEnrichrdb(org = "human", library = "COVID-19_Related_Gene_Sets",
#' data_dir = tempdir())

getEnrichrdb <- function(org = c('human','fly','yeast','worm','zebrafish'),
                         library = NULL,
                         download.method = NULL,
                         data_dir = NULL) {

  #--- args ---#
  # org <- match.arg(org)
  if(is.null(library)) stop('Please choose gene set library from: `enrichr_metadata`')

  org <- map_enrichrdb_org(org)

  libs <- geneset::enrichr_metadata %>% dplyr::filter(organism %in% org) %>%
    dplyr::pull(library)
  if(! library %in% libs) stop('Please choose gene set library from: `enrichr_metadata`')

  if(is.null(data_dir)){
    data_dir <- tools::R_user_dir("geneset", which = "data")
  }
  sub_dir <- "/anno/enrichrdb/"
  data_dir <- paste0(data_dir, sub_dir)
  make_dir(data_dir)

  #--- download ---#
  res <- list()
  for(i in c("geneset")){
    # i = 'geneset'
    url <- paste0(web.url(),sub_dir, org,"/",library,"_",i,".fst")
    destfile <- paste0(data_dir, "/", org, "_",library,"_",i,".fst")
    web_f_size <- check_web_size(url)
    local_f_size <- file.size(destfile)
    if(is.na(local_f_size)) local_f_size = 0

    geneset_download(url, destfile, method = download.method,
                     data_dir, web_f_size, local_f_size)
    res[[i]] = suppressWarnings(fst::read.fst(destfile))
  }

  res[['geneset_name']] <- NA

  #--- add org for other use ---#
  ensOrg_name <- ensOrg_name_data()
  rm(ensOrg_name, envir = .genesetEnv)
  add_org <- ensOrg_name %>%
    dplyr::filter(tolower(common_name) %in% org) %>%
    dplyr::pull(latin_short_name)
  if(length(add_org)==0) add_org = NA

  res$organism <- add_org

  res$type <- 'enrichrdb'

  invisible(res)
}

utils::globalVariables(c("enrichr_metadata","organism","latin_short_name","ensOrg_name"))
