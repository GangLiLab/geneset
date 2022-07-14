#' Get WikiPathway geneset and geneset_name
#' Geneset is a data.frame of 2 columns with term id and gene id;
#' Geneset_name is a data.frame of 2 columns with term id and term description
#'
#' @param org Organism name from `wiki_org`.
#' @param download.method "auto" (as default if NULL), "wininet" (for windows)
#' @param data_dir data saving location, default is the package data directory
#' @importFrom dplyr %>% filter pull
#'
#' @return A list including geneset and geneset name.
#' @export
#' @examples
#' \donttest{
#' x = getWiki(org = "human", data_dir = tempdir())
#' }
getWiki <- function(org = 'human',
                    download.method = NULL,
                    data_dir = NULL) {

  #--- args ---#
  org <- map_wiki_org(org)

  if(is.null(data_dir)){
    data_dir <- tools::R_user_dir("geneset", which = "data")
  }
  sub_dir <- "/anno/wikipathway/"
  data_dir <- paste0(data_dir, sub_dir)
  make_dir(data_dir)

  #--- download ---#
  res <- list()
  for(i in c("geneset","geneset_name")){
    # i = 'geneset'
    url <- paste0(web.url(),sub_dir,org,"_",i,".fst")
    destfile <- paste0(data_dir, "/", org, "_",i,".fst")
    web_f_size <- check_web_size(url)
    local_f_size <- file.size(destfile)
    if(is.na(local_f_size)) local_f_size = 0

    geneset_download(url, destfile, method = download.method,
                     data_dir, web_f_size, local_f_size)
    res[[i]] = suppressWarnings(fst::read.fst(destfile))
  }

  #--- add org for other use ---#
  ensOrg_name <- ensOrg_name_data()
  rm(ensOrg_name, envir = .genesetEnv)
  org2 <- gsub('_',' ',org)
  add_org <- ensOrg_name %>%
    dplyr::filter(latin_full_name %in% org2) %>%
    dplyr::pull(latin_short_name)
  if(length(add_org)==0) add_org = NA

  res$organism <- add_org

  res$type <- 'wikipathway'

  invisible(res)
}

utils::globalVariables(c("wiki_org", ".","latin_short_name"))
