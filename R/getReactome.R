#' Get Reactome geneset and geneset_name
#' Geneset is a data.frame of 2 columns with term id and gene id;
#' Geneset_name is a data.frame of 2 columns with term id and term description
#'
#' @param org Organism name from `reactome_org`.
#' @param download.method "auto" (as default if NULL), "wininet" (for windows)
#' @importFrom dplyr %>% filter pull
#'
#' @return A list including geneset and geneset name.
#' @export
#' @examples
#' \dontrun{
#' x = getReactome(org = "human")
#' }
getReactome <- function(org = 'human',
                        download.method = NULL) {

  #--- args ---#
  org <- map_reactome_org(org)

  data_dir <- tools::R_user_dir("geneset", which = "data")
  data_dir <- paste0(data_dir, "/anno/reactome/")
  make_dir(data_dir)

  #--- download ---#
  res <- list()
  for(i in c("geneset","geneset_name")){
    # i = 'geneset'
    url <- paste0('https://genekitr-china.oss-accelerate.aliyuncs.com/anno/reactome/',
                  org,"_",i,".fst")
    destfile <- paste0(data_dir, "/", org, "_",i,".fst")
    web_f_size <- check_web_size(url)
    local_f_size <- file.size(destfile)
    if(is.na(local_f_size)) local_f_size = 0

    geneset_download(url, destfile, method = download.method,
                     data_dir, web_f_size, local_f_size)
    res[[i]] = suppressMessages(fst::read.fst(destfile))
  }

  #--- add org for other use ---#
  org2 <- gsub('_',' ',org)
  add_org <- genekitr::ensOrg_name %>%
    dplyr::filter(latin_full_name %in% org2) %>%
    dplyr::pull(latin_short_name)
  if(length(add_org)==0) add_org = NA

  res$organism <- add_org

  res$type <- 'reactome'

  invisible(res)
}

utils::globalVariables(c("reactome_org","latin_short_name"))
