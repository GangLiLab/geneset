#' Get KEGG geneset and geneset_name
#' Geneset is a data.frame of 2 columns with term id and gene id;
#' Geneset_name is a data.frame of 2 columns with term id and term description
#'
#' @param org Organism name from `kegg_org`.
#' @param category Choose one category from "pathway","module", "enzyme",
#' 'disease' (human only),'drug' (human only) or 'network' (human only)
#' @param download.method "auto" (as default if NULL), "wininet" (for windows)
#' @importFrom dplyr %>% filter pull
#'
#' @return A list including geneset and geneset name.
#' @export
#' @examples
#' \dontrun{
#' x = getKEGG(org = "hsa",category = "pathway")
#' }
getKEGG <- function(org = 'hsa',
                    category = c('pathway','module','enzyme','disease','drug','network'),
                    download.method = NULL) {

  #--- args ---#
  category <- match.arg(category)
  # category <- tolower(category)
  if(category %in% c('disease','drug','network')){
    message(paste0("The categoty ",category, ' only support human...'))
    org <- 'human'
  }

  org <- map_kegg_org(org)

  data_dir <- tools::R_user_dir("geneset", which = "data")
  data_dir <- paste0(data_dir, "/anno/kegg/", category)
  make_dir(data_dir)

  #--- download ---#
  res <- list()
  for(i in c("geneset","geneset_name")){
    # i = 'geneset'
    url <- paste0('https://genekitr-china.oss-accelerate.aliyuncs.com/anno/kegg/',
                  category,'/',org,"_",i,".fst")
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

utils::globalVariables(c("kegg_name", "kegg_org"))

