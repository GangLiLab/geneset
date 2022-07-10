#' Get MsigDb geneset and geneset_name
#' Geneset is a data.frame of 2 columns with term id and gene id
#'
#' @param org Organism name from `msigdb_org`.
#' @param category Choose one of "H", "C1", "C2-CGP", "C2-CP-BIOCARTA", "C2-CP-KEGG", "C2-CP-PID",
#' "C2-CP-REACTOME", "C2-CP-WIKIPATHWAYS", "C3-MIR-MIRDB","C3-MIR-MIR_Legacy", "C3-TFT-GTRD",
#' "C3-TFT-TFT_Legacy","C4-CGN", "C4-CM", "C5-GO-BP", "C5-GO-CC", "C5-GO-MF","C5-HPO", "C6",
#' "C7-IMMUNESIGDB", "C7-VAX", "C8" (refer to: http://www.gsea-msigdb.org/gsea/msigdb/collections.jsp)
#' @param download.method "auto" (as default if NULL), "wininet" (for windows)
#' @importFrom dplyr %>% filter pull
#'
#' @return A list including geneset.
#' @export
#' @examples
#' \dontrun{
#' x = getMsigdb(org = "human", category = "H")
#' }
getMsigdb <- function(org = 'human',
                      category = c("H", "C1", "C2-CGP", "C2-CP-BIOCARTA", "C2-CP-KEGG", "C2-CP-PID",
                                   "C2-CP-REACTOME", "C2-CP-WIKIPATHWAYS", "C3-MIR-MIRDB",
                                   "C3-MIR-MIR_Legacy", "C3-TFT-GTRD", "C3-TFT-TFT_Legacy",
                                   "C4-CGN", "C4-CM", "C5-GO-BP", "C5-GO-CC", "C5-GO-MF",
                                   "C5-HPO", "C6", "C7-IMMUNESIGDB", "C7-VAX", "C8"),
                      download.method = NULL) {

  #--- args ---#
  category <- match.arg(category)
  org <- map_msigdb_org(org)

  data_dir <- tools::R_user_dir("geneset", which = "data")
  data_dir <- paste0(data_dir, "/anno/msigdb/")
  make_dir(data_dir)

  #--- download ---#
  res <- list()
  for(i in c("geneset")){
    # i = 'geneset'
    url <- paste0('https://genekitr-china.oss-accelerate.aliyuncs.com/anno/msigdb/',category,'/',
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

utils::globalVariables(c("msigdb_org"))