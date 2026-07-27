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
  if (length(org) != 1L || is.na(org) || !nzchar(trimws(org))) {
    stop("`org` must be one non-empty organism name.", call. = FALSE)
  }

  # Normalize user input only for matching. The returned value is generated
  # from the exact latin_full_name stored in package metadata so that it is
  # guaranteed to match filenames produced by 04-msigdb_data.R, e.g.
  # Homo_sapiens and Mus_musculus.
  org_key <- tolower(trimws(as.character(org)))
  org_key <- gsub("_", " ", org_key, fixed = TRUE)
  org_key <- gsub("\\s+", " ", org_key)

  aliases <- c(
    "hg" = "homo sapiens",
    "human" = "homo sapiens",
    "hsa" = "homo sapiens",
    "hs" = "homo sapiens",
    "mm" = "mus musculus",
    "mouse" = "mus musculus",
    "house mouse" = "mus musculus",
    "rn" = "rattus norvegicus",
    "rat" = "rattus norvegicus",
    "dm" = "drosophila melanogaster",
    "fly" = "drosophila melanogaster",
    "fruit fly" = "drosophila melanogaster"
  )

  if (org_key %in% names(aliases)) {
    org_key <- unname(aliases[[org_key]])
  }

  orgs <- msigdb_org_data()
  rm(msigdb_org, envir = .genesetEnv)

  latin <- trimws(as.character(orgs$latin_full_name))
  common <- trimws(as.character(orgs$common_name))

  latin_key <- tolower(gsub("_", " ", latin, fixed = TRUE))
  common_key <- tolower(gsub("_", " ", common, fixed = TRUE))

  idx <- match(org_key, latin_key)
  if (is.na(idx)) {
    idx <- match(org_key, common_key)
  }

  if (is.na(idx)) {
    stop(
      "Check organism name in `msigdb_org`!\n",
      "USE latin_full_name, e.g. ",
      paste(utils::head(latin, 5L), collapse = " | "),
      "\nOR USE common_name, e.g. ",
      paste(utils::head(common, 5L), collapse = " | "),
      call. = FALSE
    )
  }

  gsub(" ", "_", latin[idx], fixed = TRUE)
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
check_web_size <- function(url) {
  if (length(url) != 1L || is.na(url) || !nzchar(url)) {
    return(NA_real_)
  }

  header <- tryCatch(
    RCurl::getURL(
      url,
      nobody = TRUE,
      header = TRUE,
      followlocation = TRUE,
      timeout = 20L
    ),
    error = function(e) NA_character_
  )

  if (length(header) != 1L || is.na(header) || !nzchar(header)) {
    return(NA_real_)
  }

  lines <- unlist(strsplit(header, "\\r?\\n"))
  hits <- grep("^Content-Length\\s*:", lines, ignore.case = TRUE, value = TRUE)

  if (length(hits) == 0L) {
    return(NA_real_)
  }

  value <- sub("^[^:]+:\\s*", "", utils::tail(hits, 1L))
  size <- suppressWarnings(as.numeric(trimws(value)))

  if (length(size) != 1L || is.na(size) || !is.finite(size) || size < 0) {
    return(NA_real_)
  }

  size
}

#--- create data dir ---#
make_dir <- function(data_dir) {
  if (length(data_dir) != 1L || is.na(data_dir) || !nzchar(data_dir)) {
    stop("`data_dir` must be one non-empty directory path.", call. = FALSE)
  }

  if (!dir.exists(data_dir)) {
    ok <- dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(ok) && !dir.exists(data_dir)) {
      stop(
        "Cannot create/access directory: ", data_dir,
        "\nPlease specify a writable `data_dir`.",
        call. = FALSE
      )
    }
  }

  invisible(data_dir)
}


#--- download function ---#
# url: web url
# destfile: local file name
# data_dir: destfile directory
# method: "auto" (default), "wininet" (for windows)
# web_f_size: file size on webserver; NA means unavailable
# local_f_size: local file size
#
# Download is first written to a temporary file and only replaces the cache
# after a successful, non-empty download. Thus a failed update does not destroy
# a previously valid local file.
geneset_download <- function(url, destfile, data_dir,
                             method = NULL,
                             web_f_size = NA_real_,
                             local_f_size = 0) {

  if (is.null(method)) {
    method <- getOption("geneset.download.method")
  }
  if (is.null(method) || !nzchar(method)) {
    method <- "auto"
  }

  local_exists <- file.exists(destfile)
  local_f_size <- suppressWarnings(as.numeric(local_f_size)[1L])
  web_f_size <- suppressWarnings(as.numeric(web_f_size)[1L])

  if (is.na(local_f_size) || !is.finite(local_f_size)) {
    local_f_size <- 0
  }
  if (is.na(web_f_size) || !is.finite(web_f_size)) {
    web_f_size <- NA_real_
  }

  need_download <- !local_exists ||
    (!is.na(web_f_size) && !identical(as.numeric(local_f_size), as.numeric(web_f_size)))

  if (!need_download) {
    return(invisible(TRUE))
  }

  if (local_exists) {
    message("Detected new version data, updating...")
  }

  make_dir(dirname(destfile))
  tmpfile <- tempfile(
    pattern = paste0(basename(destfile), "."),
    tmpdir = dirname(destfile),
    fileext = ".part"
  )
  on.exit(unlink(tmpfile), add = TRUE)

  status <- tryCatch(
    utils::download.file(
      url = url,
      destfile = tmpfile,
      quiet = TRUE,
      method = method,
      mode = "wb"
    ),
    error = function(e) {
      message(
        "Auto download failed: ", conditionMessage(e), "\n",
        "Please download manually via: ", url, "\n",
        "Then save to: ", data_dir
      )
      return(1L)
    },
    warning = function(w) {
      # Convert download warnings into a clean failure so that an existing
      # cache is never overwritten by a partial/HTML error response.
      message(
        "Auto download warning: ", conditionMessage(w), "\n",
        "URL: ", url
      )
      return(1L)
    }
  )

  tmp_size <- if (file.exists(tmpfile)) as.numeric(file.info(tmpfile)$size) else 0
  ok <- identical(as.integer(status), 0L) && is.finite(tmp_size) && tmp_size > 0

  if (ok && !is.na(web_f_size) && tmp_size != web_f_size) {
    message(
      "Downloaded file size does not match server Content-Length; ",
      "keeping the existing cache if available."
    )
    ok <- FALSE
  }

  if (!ok) {
    if (!local_exists) {
      message(
        "No valid local cache is available.\n",
        "Please download manually via: ", url, "\n",
        "Then save to: ", destfile
      )
    }
    return(invisible(FALSE))
  }

  copied <- file.copy(tmpfile, destfile, overwrite = TRUE)
  if (!isTRUE(copied)) {
    message("Downloaded successfully but failed to update local cache: ", destfile)
    return(invisible(FALSE))
  }

  invisible(TRUE)
}

#--- web url ---#
web.url <- function(){
  x = "https://genekitr-china.oss-accelerate.aliyuncs.com"
  return(x)
}


utils::globalVariables(c(".genesetEnv"))









