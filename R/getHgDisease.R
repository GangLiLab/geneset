#' Get HgDisease geneset and geneset_name
#' Human disease gene sets from Disease Ontology (DO),DisGeNET,
#' Network of Cancer Gene (NCG) version 6 and v7, and covid19-specific.
#' Geneset is a data.frame of 2 columns with term id and gene id;
#' Geneset_name is a data.frame of 2 columns with term id and term description
#'
#' @param source Choose from 'do','ncg_v7','ncg_v6','disgenet','covid19'.
#' @param download.method "auto" (as default if NULL), "wininet" (for windows)
#' @importFrom dplyr %>% filter pull
#'
#' @return A list.
#' @export
#' @examples
#' \dontrun{
#' x = getHgDisease(source = "do")
#' }
getHgDisease <- function(source = c('do','disgenet','ncg_v7','ncg_v6','covid19'),
                         download.method = NULL) {

  #--- args ---#
  source <- match.arg(source)

  data_dir <- tools::R_user_dir("geneset", which = "data")
  sub_dir <- "/anno/hgdisease/"
  data_dir <- paste0(data_dir, sub_dir)
  make_dir(data_dir)

  #--- download ---#
  res <- list()
  save_dir <- paste0(data_dir,'/',source)
  make_dir(save_dir)

  for(i in c("geneset")){
    # i = 'geneset'
    url <- paste0(web.url(),sub_dir,source,'/human_',i,".fst")
    destfile <- paste0(save_dir, "/human_",i,".fst")
    web_f_size <- check_web_size(url)
    local_f_size <- file.size(destfile)
    if(is.na(local_f_size)) local_f_size = 0

    geneset_download(url, destfile, method = download.method,
                     data_dir, web_f_size, local_f_size)
    res[[i]] = suppressWarnings(fst::read.fst(destfile))
  }

  if(source %in% c('do','disgenet')){
    i = "geneset_name"
    url <- paste0(web.url(),sub_dir,source,'/human_',i,".fst")
    destfile <- paste0(save_dir, "/human_",i,".fst")
    web_f_size <- check_web_size(url)
    local_f_size <- file.size(destfile)
    if(is.na(local_f_size)) local_f_size = 0

    geneset_download(url, destfile, method = download.method,
                     data_dir, web_f_size, local_f_size)
    res[[i]] = suppressMessages(fst::read.fst(destfile))
  }else{
    res[['geneset_name']] <- NA
  }

  #--- add org for other use ---#
  res$organism <- 'hsapiens'

  res$type <- source

  invisible(res)
}

