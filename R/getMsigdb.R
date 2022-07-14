#' Get MsigDb geneset and geneset_name
#' Geneset is a data.frame of 2 columns with term id and gene id
#'
#' @param org Organism name from `msigdb_org`.
#' @param category Choose one of "H", "C1", "C2-CGP", "C2-CP-BIOCARTA", "C2-CP-KEGG", "C2-CP-PID",
#' "C2-CP-REACTOME", "C2-CP-WIKIPATHWAYS", "C3-MIR-MIRDB","C3-MIR-MIR_Legacy", "C3-TFT-GTRD",
#' "C3-TFT-TFT_Legacy","C4-CGN", "C4-CM", "C5-GO-BP", "C5-GO-CC", "C5-GO-MF","C5-HPO", "C6",
#' "C7-IMMUNESIGDB", "C7-VAX", "C8" (refer to: http://www.gsea-msigdb.org/gsea/msigdb/collections.jsp)
#' @param download.method "auto" (as default if NULL), "wininet" (for windows)
#' @param data_dir data saving location, default is the package data directory
#' @importFrom dplyr %>% filter pull
#'
#' @return A list including geneset.
#' @export
#' @examples
#' \donttest{
#' x = getMsigdb(org = "human", category = "H", data_dir = tempdir())
#' }
getMsigdb <- function(org = 'human',
                      category = c("H", "C1", "C2-CGP", "C2-CP-BIOCARTA", "C2-CP-KEGG", "C2-CP-PID",
                                   "C2-CP-REACTOME", "C2-CP-WIKIPATHWAYS", "C3-MIR-MIRDB",
                                   "C3-MIR-MIR_Legacy", "C3-TFT-GTRD", "C3-TFT-TFT_Legacy",
                                   "C4-CGN", "C4-CM", "C5-GO-BP", "C5-GO-CC", "C5-GO-MF",
                                   "C5-HPO", "C6", "C7-IMMUNESIGDB", "C7-VAX", "C8"),
                      download.method = NULL,
                      data_dir = NULL) {

  #--- args ---#
  category <- match.arg(category)
  org <- map_msigdb_org(org)

  if(is.null(data_dir)){
    data_dir <- tools::R_user_dir("geneset", which = "data")
  }
  sub_dir <- "/anno/msigdb/"
  data_dir <- paste0(data_dir, sub_dir)
  make_dir(data_dir)

  #--- download ---#
  res <- list()
  for(i in c("geneset")){
    # i = 'geneset'
    url <- paste0(web.url(),sub_dir,category,'/',org,"_",i,".fst")
    destfile <- paste0(data_dir, "/", org, "_",i,".fst")
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
  org2 <- gsub('_',' ',org)
  add_org <- ensOrg_name %>%
    dplyr::filter(latin_full_name %in% org2) %>%
    dplyr::pull(latin_short_name)
  if(length(add_org)==0) add_org = NA

  res$organism <- add_org

  res$type <- 'msigdb'

  invisible(res)
}

utils::globalVariables(c("msigdb_org","latin_short_name"))
