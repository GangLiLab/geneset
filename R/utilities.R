#############################
### Part I: organism name
#############################
go_org_data <- function() {
  utils::data(list = "go_org", package = "geneset")
  get("go_org", envir = .GlobalEnv)
}

kegg_org_data <- function() {
  utils::data(list = "kegg_org", package = "geneset")
  get("kegg_org", envir = .GlobalEnv)
}

wiki_org_data <- function() {
  utils::data(list = "wiki_org", package = "geneset")
  get("wiki_org", envir = .GlobalEnv)
}

msigdb_org_data <- function() {
  utils::data(list = "msigdb_org", package = "geneset")
  get("msigdb_org", envir = .GlobalEnv)
}

reactome_org_data <- function() {
  utils::data(list = "reactome_org", package = "geneset")
  get("reactome_org", envir = .GlobalEnv)
}

mesh_org_data <- function() {
  utils::data(list = "mesh_org", package = "geneset")
  get("mesh_org", envir = .GlobalEnv)
}

org2cate_data <- function() {
  utils::data(list = "org2cate", package = "geneset")
  get("org2cate", envir = .GlobalEnv)
}

#############################
### Part II: match organism
#############################
#---  get go org ---#
map_go_org <- function(org) {
  org <- tolower(org)
  if (org == "hg" | org == "human" | org == "hsa" | org == "hs") org <- "human"
  if (org == "mm" | org == "mouse") org <- "mouse"
  if (org == "rat" | org == "rn") org <- "rat"

  orgs <- go_org_data()
  rm(go_org, envir = .GlobalEnv)

  if (org %in% orgs$common_name) {
    org <- org
  } else if (org %in% orgs$latin_full_name) {
    org <- orgs %>%
      dplyr::filter(latin_full_name == org) %>%
      dplyr::pull(common_name)
  } else {
    stop(
      "Check organism name in `go_org()`! \n USE latin_full_name: e.g. ",
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
  if (org == "hg" | org == "human" | org == "hsa" | org == "hs") org <- "hsa"
  if (org == "mm" | org == "mouse" | org == "house mouse") org <- "mmu"
  if (org == "rat" | org == "rn") org <- "rno"
  if (org == "fruit fly" | org == "dm") org <- "dme"

  orgs <- kegg_org_data()
  rm(kegg_org, envir = .GlobalEnv)

  if (org %in% orgs$kegg_name) {
    org <- org
  } else if (org %in% tolower(orgs$latin_full_name)) {
    org <- orgs %>%
      dplyr::filter(latin_full_name %in% org) %>%
      dplyr::pull(kegg_name)
  } else if (org %in% tolower(orgs$common_name)) {
    org <- orgs %>%
      dplyr::filter(common_name %in% org) %>%
      dplyr::pull(kegg_name)
  } else {
    stop(
      "Check organism name in `kegg_org()`! \n USE kegg_name: e.g. ",
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
  rm(wiki_org, envir = .GlobalEnv)

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
      "Check organism name in `wiki_org()`! \n USE latin_full_name: e.g. ",
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
  rm(msigdb_org, envir = .GlobalEnv)

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
      "Check organism name in `msigdb_org()`! \n USE latin_full_name: e.g. ",
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
  rm(reactome_org, envir = .GlobalEnv)

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
      "Check organism name in `reactome_org()`! \n USE latin_full_name: e.g. ",
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
  if (org == "hg" | org == "human" | org == "hsa" | org == "hs") org <- "hsa"
  if (org == "mm" | org == "mouse" | org == "house mouse") org <- "mmu"
  if (org == "rat" | org == "rn") org <- "rno"
  if (org == "fruit fly" | org == "dm") org <- "dme"

  orgs <- mesh_org_data()
  rm(mesh_org, envir = .GlobalEnv)

  if (org %in% orgs$mesh_org) {
    org <- org
  } else if (org %in% gsub(' ','_',tolower(orgs$latin_full_name))) {
    org <- orgs %>%
      dplyr::mutate(latin_full_name = gsub(' ','_',tolower(latin_full_name))) %>%
      dplyr::filter(latin_full_name %in% org) %>%
      dplyr::pull(mesh_org)
  } else if (org %in% tolower(orgs$latin_full_name)) {
    org <- orgs %>%
      dplyr::filter(tolower(latin_full_name) %in% org) %>%
      dplyr::pull(mesh_org)
  } else {
    stop(
      "Check organism name in `mesh_org()`! \n USE mesh_org: e.g. ",
      paste0(mesh_org_data() %>% dplyr::slice_head(n=5) %>% dplyr::pull(mesh_org), collapse = " | "),
      "\n OR USE latin_full_name: e.g. ",
      paste0(mesh_org_data() %>% dplyr::slice_head(n=5) %>% dplyr::pull(latin_full_name), collapse = " | ")
    )
  }

  return(org)
}

#############################
### Part III: data query
#############################
#--- get Enrichrdb metadata ---#
enrichr_data <- function() {
  utils::data(list = "enrichr_metadata", package = "geneset")
  get("enrichr_metadata", envir = .GlobalEnv)
}

#--- get MeSH metadata ---#
mesh_data <- function() {
  utils::data(list = "mesh_metadata", package = "geneset")
  get("mesh_metadata", envir = .GlobalEnv)
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















