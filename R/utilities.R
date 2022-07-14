#############################
### Part I: organism name
#############################
.initial <- function() {
  pos <- 1
  envir <- as.environment(pos)
  assign(".genesetEnv", new.env(), envir = envir)
}

go_org_data <- function() {
  .initial()
  utils::data(list = "go_org", package = "geneset",envir = .genesetEnv)
  get("go_org", envir = .genesetEnv)
}

kegg_org_data <- function() {
  .initial()
  utils::data(list = "kegg_org", package = "geneset",envir = .genesetEnv)
  get("kegg_org", envir = .genesetEnv)
}

ensOrg_name_data <- function() {
  .initial()
  utils::data(list = "ensOrg_name", package = "geneset",envir = .genesetEnv)
  get("ensOrg_name", envir = .genesetEnv)
}

wiki_org_data <- function() {
  .initial()
  utils::data(list = "wiki_org", package = "geneset",envir = .genesetEnv)
  get("wiki_org", envir = .genesetEnv)
}

msigdb_org_data <- function() {
  .initial()
  utils::data(list = "msigdb_org", package = "geneset",envir = .genesetEnv)
  get("msigdb_org", envir = .genesetEnv)
}

reactome_org_data <- function() {
  .initial()
  utils::data(list = "reactome_org", package = "geneset",envir = .genesetEnv)
  get("reactome_org", envir = .genesetEnv)
}

mesh_org_data <- function() {
  .initial()
  utils::data(list = "mesh_org", package = "geneset",envir = .genesetEnv)
  get("mesh_org", envir = .genesetEnv)
}

org2cate_data <- function() {
  .initial()
  utils::data(list = "org2cate", package = "geneset",envir = .genesetEnv)
  get("org2cate", envir = .genesetEnv)
}

#############################
### Part II: match organism
#############################
#---  get go org ---#
map_go_org <- function(org) {
  org <- tolower(org)
  if (org == "hg" | org == "human" | org == "hsa" | org == "hs" | org == "homo sapiens") org <- "human"
  if (org == "mm" | org == "mouse" | org == "mus musculus") org <- "mouse"
  if (org == "rat" | org == "rn" | org == 'rattus norvegicus') org <- "rat"

  orgs <- go_org_data()
  rm(go_org, envir = .genesetEnv)
  orgs <- apply(orgs, 2, tolower) %>% as.data.frame()

  if (org %in% orgs$common_name) {
    org <- org
  } else if (org %in% orgs$latin_full_name) {
    org <- orgs %>%
      dplyr::filter(latin_full_name == org) %>%
      dplyr::pull(common_name)
  } else {
    stop(
      "Check organism name in `go_org`! \n USE latin_full_name: e.g. ",
      paste0(go_org_data() %>% dplyr::slice_head(n=5)  %>% dplyr::pull(latin_full_name), collapse = " | "),
      "\n OR USE common_name: e.g. ",
      paste0(go_org_data() %>% dplyr::slice_head(n=5)  %>% dplyr::pull(common_name), collapse = " | ")
    )
  }

  return(org)
}

#---  get kegg org ---#
map_kegg_org <- function(org) {
  org <- tolower(org)
  if (org == "hg" | org == "human" | org == "hsa" | org == "hs"| org == "homo sapiens") org <- "hsa"
  if (org == "mm" | org == "mouse" | org == "house mouse" | org == "mus musculus") org <- "mmu"
  if (org == "rat" | org == "rn"| org == 'rattus norvegicus') org <- "rno"
  if (org == "fruit fly" | org == "dm" | org == 'drosophila melanogaster') org <- "dme"

  orgs <- kegg_org_data()
  rm(kegg_org, envir = .genesetEnv)
  orgs <- apply(orgs, 2, tolower) %>% as.data.frame()

  if (org %in% orgs$kegg_name) {
    org <- org
  } else if (org %in% orgs$latin_full_name) {
    org <- orgs %>%
      dplyr::filter(latin_full_name %in% org) %>%
      dplyr::pull(kegg_name)
  } else if (org %in% orgs$common_name) {
    org <- orgs %>%
      dplyr::filter(common_name %in% org) %>%
      dplyr::pull(kegg_name)
  } else {
    stop(
      "Check organism name in `kegg_org`! \n USE kegg_name: e.g. ",
      paste0(kegg_org_data() %>% dplyr::slice_head(n=5) %>% dplyr::pull(kegg_name), collapse = " | "),
      "\n OR USE latin_full_name: e.g. ",
      paste0(kegg_org_data() %>% dplyr::slice_head(n=5) %>% dplyr::pull(latin_full_name), collapse = " | "),
      "\n OR USE common_name: e.g. ",
      paste0(kegg_org_data() %>% dplyr::slice_head(n=5) %>% dplyr::pull(common_name), collapse = " | ")
    )
  }

  return(org)
}

#---  get wiki org ---#
map_wiki_org <- function(org) {
  org <- tolower(org)
  if (org == "hg" | org == "human" | org == "hsa" | org == "hs") org <- "homo_sapiens"
  if (org == "mm" | org == "mouse" | org == "house mouse") org <- "mus_musculus"
  if (org == "rat" | org == "rn") org <- "rattus_norvegicus"
  if (org == "fruit fly" | org == "dm") org <- "drosophila_melanogaster"

  orgs <- wiki_org_data()
  rm(wiki_org, envir = .genesetEnv)

  if (org %in% tolower(orgs$latin_full_name)) {
    org <- gsub(' ','_',org)
  } else if (org %in% gsub(' ','_',tolower(orgs$latin_full_name))) {
    org <- org
  } else if (org %in% tolower(orgs$common_name)) {
    org <- orgs %>%
      dplyr::filter(tolower(common_name) %in% org) %>%
      dplyr::pull(latin_full_name) %>%
      gsub(' ','_',.)
  } else {
    stop(
      "Check organism name in `wiki_org`! \n USE latin_full_name: e.g. ",
      paste0(wiki_org_data() %>% dplyr::slice_head(n=5) %>% dplyr::pull(latin_full_name), collapse = " | "),
      "\n OR USE common_name: e.g. ",
      paste0(wiki_org_data() %>% dplyr::slice_head(n=5) %>% dplyr::pull(common_name), collapse = " | ")
    )
  }

  org <- stringr::str_to_title(org)

  return(org)
}

#---  get msigdb org ---#
map_msigdb_org <- function(org) {
  org <- tolower(org)
  if (org == "hg" | org == "human" | org == "hsa" | org == "hs") org <- "homo_sapiens"
  if (org == "mm" | org == "mouse" | org == "house mouse") org <- "mus_musculus"
  if (org == "rat" | org == "rn") org <- "rattus_norvegicus"
  if (org == "fruit fly" | org == "dm") org <- "drosophila_melanogaster"

  orgs <- msigdb_org_data()
  rm(msigdb_org, envir = .genesetEnv)

  if (org %in% tolower(orgs$latin_full_name)) {
    org <- gsub(' ','_',org)
  } else if (org %in% gsub(' ','_',tolower(orgs$latin_full_name))) {
    org <- org
  } else if (org %in% tolower(orgs$common_name)) {
    org <- orgs %>%
      dplyr::filter(tolower(common_name) %in% org) %>%
      dplyr::pull(latin_full_name) %>%
      gsub(' ','_',.)
  } else {
    stop(
      "Check organism name in `msigdb_org`! \n USE latin_full_name: e.g. ",
      paste0(msigdb_org_data() %>% dplyr::slice_head(n=5) %>% dplyr::pull(latin_full_name), collapse = " | "),
      "\n OR USE common_name: e.g. ",
      paste0(msigdb_org_data() %>% dplyr::slice_head(n=5) %>% dplyr::pull(common_name), collapse = " | ")
    )
  }

  org <- stringr::str_to_title(org)

  return(org)
}

#---  get reactome org ---#
map_reactome_org <- function(org) {
  org <- tolower(org)
  if (org == "hg" | org == "human" | org == "hsa" | org == "hs") org <- "homo_sapiens"
  if (org == "mm" | org == "mouse" | org == "house mouse") org <- "mus_musculus"
  if (org == "rat" | org == "rn") org <- "rattus_norvegicus"
  if (org == "fruit fly" | org == "dm") org <- "drosophila_melanogaster"

  orgs <- reactome_org_data()
  rm(reactome_org, envir = .genesetEnv)

  if (org %in% tolower(orgs$latin_full_name)) {
    org <- gsub(' ','_',org)
  } else if (org %in% gsub(' ','_',tolower(orgs$latin_full_name))) {
    org <- org
  } else if (org %in% tolower(orgs$common_name)) {
    org <- orgs %>%
      dplyr::filter(tolower(common_name) %in% org) %>%
      dplyr::pull(latin_full_name) %>%
      gsub(' ','_',.)
  } else {
    stop(
      "Check organism name in `reactome_org`! \n USE latin_full_name: e.g. ",
      paste0(reactome_org_data() %>% dplyr::slice_head(n=5) %>% dplyr::pull(latin_full_name), collapse = " | "),
      "\n OR USE common_name: e.g. ",
      paste0(reactome_org_data() %>% dplyr::slice_head(n=5) %>% dplyr::pull(common_name), collapse = " | ")
    )
  }

  org <- stringr::str_to_title(org)

  return(org)
}

#---  get mesh org ---#
map_mesh_org <- function(org) {
  org <- tolower(org)
  if (org == "hg" | org == "human" | org == "hsa" | org == "hs" | org == "homo sapiens") org <- "hsa"
  if (org == "mm" | org == "mouse" | org == "house mouse"| org == "mus musculus") org <- "mmu"
  if (org == "rat" | org == "rn" | org == 'rattus norvegicus') org <- "rno"
  if (org == "fruit fly" | org == "dm" | org == 'drosophila melanogaster') org <- "dme"

  orgs <- mesh_org_data()
  rm(mesh_org, envir = .genesetEnv)
  # orgs$latin_full_name <- tolower(orgs$latin_full_name)

  if (org %in% orgs$mesh_org) {
    org <- org
  } else if (org %in% gsub(' ','_',tolower(orgs$latin_full_name))) {
    org <- orgs %>%
      dplyr::filter(gsub(' ','_',tolower(orgs$latin_full_name))%in% org) %>%
      dplyr::pull(mesh_org)
  } else if (org %in% tolower(orgs$latin_full_name)) {
    org <- orgs %>%
      dplyr::filter(tolower(latin_full_name) %in% org) %>%
      dplyr::pull(mesh_org)
  } else {
    stop(
      "Check organism name in `mesh_org`! \n USE mesh_org: e.g. ",
      paste0(mesh_org_data() %>% dplyr::slice_head(n=5) %>% dplyr::pull(mesh_org), collapse = " | "),
      "\n OR USE latin_full_name: e.g. ",
      paste0(mesh_org_data() %>% dplyr::slice_head(n=5) %>% dplyr::pull(latin_full_name), collapse = " | ")
    )
  }

  return(org)
}

#---  get enrichrdb org ---#
map_enrichrdb_org <- function(org) {
  org <- tolower(org)
  if (org == "hg" | org == "human" | org == "hsa" | org == "hs" | org == "homo sapiens") org <- "human"
  if (org == "fly" | org == "fruit fly" | org == "dm" | org == 'drosophila melanogaster') org <- "fly"
  if (org == "worm" | org == "ce" | org == 'caenorhabditis elegans') org <- "worm"
  if (org == "yeast" | org == "sc" | org == 'saccharomyces cerevisiae') org <- "yeast"
  if (org == "zebrafish" | org == "dr" | org == 'danio rerio') org <- "zebrafish"

  return(org)
}

#############################
### Part III: data query
#############################
#--- get Enrichrdb metadata ---#
enrichr_data <- function() {
  .initial()
  utils::data(list = "enrichr_metadata", package = "geneset", envir = .genesetEnv)
  get("enrichr_metadata", envir = .genesetEnv)
}

#--- get MeSH metadata ---#
mesh_data <- function() {
  .initial()
  utils::data(list = "mesh_metadata", package = "geneset", envir = .genesetEnv)
  get("mesh_metadata", envir = .genesetEnv)
}

#--- get web server file size ---#
check_web_size <- function(url){
  web_f_size <- RCurl::getURL(url, nobody = 1L, header = 1L) %>%
    strsplit("\r\n") %>%
    unlist() %>%
    stringr::str_extract("Content-Length.*[0-9]") %>%
    stringr::str_remove_all("Content-Length: ") %>%
    stringi::stri_remove_empty_na() %>%
    as.numeric()
  return(web_f_size)
}

#--- create data dir ---#
make_dir <- function(data_dir){
  if (!dir.exists(data_dir)) {
    tryCatch(
      {
        dir.create(data_dir, recursive = TRUE)
      },
      error = function(e) {
        message(paste0(
          "Seems like you cannot access dir: ", data_dir,
          "\nPlease spefify a valid dir to save data..."
        ))
        data_dir <- readline(prompt = "Enter directory: ")
        dir.create(data_dir, recursive = TRUE)
      }
    )
  }
}


#--- download function ---#
# url: web url
# destfile: local file name
# data_dir: destfile directory
# method: "auto" (default), "wininet" (for windows)
# web_f_size: file size on webserver
# local_f_size: file size in local
geneset_download <- function(url, destfile,data_dir,
                             method, web_f_size, local_f_size){
  if (!file.exists(destfile)) {
    # message("Initializing, please wait (just once)...")
    if (is.null(method)) method <- getOption("geneset.download.method")

    if (!is.null(method) && method != "auto") {
      tryCatch(utils::download.file(url, destfile, quiet = TRUE, method = method, mode = "wb"),
               error = function(e) {
                 message(paste0(
                   "Auto download failed...\nPlease download via: ", url,
                   "\nThen save to: ", data_dir, "\n"
                 ))
               })
    }else{
      tryCatch(utils::download.file(url, destfile, quiet = TRUE,method = 'auto', mode = "wb"),
        error = function(e) {
          message(paste0(
            "Auto download failed...\nPlease download manually via: ", url,
            "\nThen save to: ", data_dir, "\n"
          ))
        }
      )
    }
  } else if (web_f_size != local_f_size) {
    message("Detected new version data, updating...")
    if (!is.null(method) && method != "auto") {
      tryCatch(utils::download.file(url, destfile, quiet = TRUE, method = method, mode = "wb"),
               error = function(e) {
                 message(paste0(
                   "Auto download failed...\nPlease download manually via: ", url,
                   "\nThen save to: ", data_dir, "\n"
                 ))
               })
    }else{
      tryCatch(utils::download.file(url, destfile, quiet = TRUE,method = 'auto', mode = "wb"),
               error = function(e) {
                 message(paste0(
                   "Auto download failed...\nPlease download manually via: ", url,
                   "\nThen save to: ", data_dir, "\n"
                 ))
               }
      )
    }
  }
}

#--- web url ---#
web.url <- function(){
  x = "https://genekitr-china.oss-accelerate.aliyuncs.com"
  return(x)
}


utils::globalVariables(c(".genesetEnv"))









