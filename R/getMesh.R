#' Get MeSH geneset and geneset_name
#' MeSH is the annotation used for MEDLINE/PubMed articles and is manually curated by NLM (U.S. National Library of Medicine).
#' Geneset is a data.frame of 2 columns with term id and gene id;
#' Geneset_name is a data.frame of 2 columns with term id and term description
#'
#' @param org Organism mesh_name from `mesh_org`.
#' @param download.method "auto" (as default if NULL), "wininet" (for windows)
#' @param method Method of mapping MeSH ID to gene ID. Choose one from "gendoo", "gene2pubmed" or "RBBH" (mainly for some minor species).
#' See also `mesh_metadata`.
#' @param category MeSH descriptor categories from `mesh_metadata` (refer to: https://wikipedia.org/wiki/List_of_MeSH_codes).
#' @param data_dir data saving location, default is the package data directory
#' @importFrom dplyr %>% filter pull
#'
#' @return A list including geneset and geneset name.
#' @export
#' @examples
#' \donttest{
#' x = getMesh(org = "human", method = "gendoo", category = "A", data_dir = tempdir())
#' }
getMesh <- function(org = 'human',
                    method = c('gendoo', 'gene2pubmed', 'RBBH'),
                    category = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L","M", "N", "Z"),
                    download.method = NULL,
                    data_dir = NULL) {

  #--- args ---#
  method <-  match.arg(method); met <- method
  category <-  match.arg(category)

  org <- map_mesh_org(org)

  org_methods <- mesh_metadata %>% dplyr::filter(mesh_org %in% org) %>% dplyr::pull(method)
  if(!method %in% org_methods) stop(paste0('The organism "', org, '" only supports methods: ', paste(org_methods,collapse = ' | ')))

  org_cates <- mesh_metadata %>% dplyr::filter(mesh_org %in% org) %>%
    dplyr::filter(method %in% met) %>%
    dplyr::pull(category) %>%
    strsplit('\\|') %>% unlist()
  if(!category %in% org_cates) stop(paste0('You have chosen organism "',org,'" and method "',met,
                                           '"\nNow please choose category from:\n', paste(org_cates,collapse = ' | ')))

  if(is.null(data_dir)){
    data_dir <- tools::R_user_dir("geneset", which = "data")
  }
  sub_dir <- "/anno/mesh/"
  data_dir <- paste0(data_dir, sub_dir)
  make_dir(data_dir)

  #--- download ---#
  res <- list()
  # geneset
  i = 'geneset'
  url <- paste0(web.url(),sub_dir,org,"/",org,'_',method,'_',category,'_',i,".fst")
  destfile <- paste0(data_dir, "/", org,'_',method,'_',category, "_",i,".fst")
  web_f_size <- check_web_size(url)
  local_f_size <- file.size(destfile)
  if(is.na(local_f_size)) local_f_size = 0

  geneset_download(url, destfile, method = download.method,
                   data_dir, web_f_size, local_f_size)
  res[[i]] = suppressWarnings(fst::read.fst(destfile)) %>%
    dplyr::select(1,2) %>%
    dplyr::arrange(1)

  # geneset_name
  n = 'geneset_name'
  url <- paste0(web.url(),sub_dir,org,"/",org,"_",n,".fst")
  destfile <- paste0(data_dir, "/", org,'_',n,".fst")
  web_f_size <- check_web_size(url)
  local_f_size <- file.size(destfile)
  if(is.na(local_f_size)) local_f_size = 0

  geneset_download(url, destfile, method = download.method,
                   data_dir, web_f_size, local_f_size)
  res[[n]] = suppressWarnings(fst::read.fst(destfile))

  #--- add org for other use ---#
  ensOrg_name <- ensOrg_name_data()
  rm(ensOrg_name, envir = .genesetEnv)
  org2 <- geneset::mesh_org %>%
    dplyr::filter(mesh_org %in% org) %>%
    dplyr::pull(latin_full_name)
  add_org <- ensOrg_name %>%
    dplyr::filter(latin_full_name %in% org2) %>%
    dplyr::pull(latin_short_name)
  if(length(add_org)==0) add_org = NA

  res$organism <- add_org

  res$type <- 'mesh'

  invisible(res)
}

utils::globalVariables(c("mesh_org",'mesh_metadata',"latin_short_name"))
