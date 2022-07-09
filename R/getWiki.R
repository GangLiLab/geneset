#' Get WikiPathway geneset and geneset_name
#' Geneset is a data.frame of 2 columns with term id and gene id;
#' Geneset_name is a data.frame of 2 columns with term id and term description
#'
#' @param org Organism name from `wiki_org`.
#' @param download.method "auto" (as default if NULL), "wininet" (for windows)
#' @importFrom dplyr %>% filter pull
#'
#' @return A list including geneset and geneset name.
#' @export
#' @examples
#' \dontrun{
#' x = getWiki(org = "human")
#' }
getWiki <- function(org = 'human',
                    download.method = NULL) {

  #--- args ---#
  org <- map_wiki_org(org)

  data_dir <- tools::R_user_dir("geneset", which = "data")
  data_dir <- paste0(data_dir, "/anno/wikipathway/")
  make_dir(data_dir)

  #--- download ---#
  res <- list()
  for(i in c("geneset","geneset_name")){
    # i = 'geneset'
    url <- paste0('https://genekitr-china.oss-accelerate.aliyuncs.com/anno/wikipathway/',
                  org,"_",i,".fst")
    destfile <- paste0(data_dir, "/", org, "_",i,".fst")
    web_f_size <- check_web_size(url)
    local_f_size <- file.size(destfile)
    if(is.na(local_f_size)) local_f_size = 0

    geneset_download(url, destfile, method = download.method,
                     data_dir, web_f_size, local_f_size)
    res[[i]] = suppressMessages(fst::read.fst(destfile))
  }

  invisible(res)
}

utils::globalVariables(c("wiki_org", "."))
