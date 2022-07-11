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
  org <- map_kegg_org(org)

  # category <- tolower(category)
  if(category %in% c('disease','drug','network') & org != 'hsa'){
    stop(paste0('The categoty "',category, '" only support human...'))
  }

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

  #--- add org for other use ---#
  tryCatch(utils::data(list="ensOrg_name", package="genekitr"))
  org2 <- geneset::kegg_org %>%
    dplyr::filter(kegg_name %in% org) %>%
    dplyr::pull(latin_full_name)

  add_org <- ensOrg_name %>%
    dplyr::filter(latin_full_name %in% org2) %>%
    dplyr::pull(latin_short_name)
  if(length(add_org)==0) add_org = NA

  res$organism <- add_org

  res$type <- 'kegg'

  invisible(res)
}

utils::globalVariables(c("kegg_name", "kegg_org","latin_short_name"))

