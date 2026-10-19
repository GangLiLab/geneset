# geneset

[![CRAN version](https://www.r-pkg.org/badges/version/geneset)](https://cran.r-project.org/package=geneset) [![Development version](https://img.shields.io/badge/devel%20version-0.2.8-green.svg)](https://github.com/GangLiLab/geneset) [![Total downloads](https://cranlogs.r-pkg.org/badges/grand-total/geneset?color=orange)](https://cran.r-project.org/package=geneset) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)

**geneset** is an R package for retrieving gene sets from public databases for enrichment analysis. It provides a consistent interface across multiple databases and supports both model and non-model organisms.

Use the retrieved gene sets for over-representation analysis (ORA), gene set enrichment analysis (GSEA), or gene set scoring with tools such as GSVA and ssGSEA. Data are downloaded on demand and cached locally.

## Installation

Install the released version from CRAN:

```r
install.packages("geneset")
```

Install the development version from GitHub:

```r
install.packages("remotes") # Run once if remotes is not installed
remotes::install_github("GangLiLab/geneset")
```

A Gitee mirror is also available for users in mainland China:

```r
remotes::install_git("https://gitee.com/genekitr/pacakge_geneset")
```

## Quick start

Retrieve human Gene Ontology molecular function gene sets:

```r
library(geneset)

x <- getGO(org = "human", ont = "mf")

head(x$geneset)      # Term-to-gene mappings
head(x$geneset_name) # Term IDs and descriptions
x$organism
x$type
```

Retrieval requires an internet connection, including when checking cached files. The number of genes and terms may change as the backend data are updated.

### Returned object

Retrieval functions return a list with four components:

| Component | Contents |
| --- | --- |
| `geneset` | A two-column data frame: term/pathway ID in the first column and gene ID in the second. |
| `geneset_name` | A two-column data frame of term IDs and descriptions, when available; otherwise `NA`. |
| `organism` | An organism identifier, such as `"hsapiens"`; may be `NA` if no corresponding identifier is found. |
| `type` | The ontology, database, or disease source associated with the result. |

Column names vary between databases. For example, the GO molecular function example uses `mf` and `gene` in `x$geneset`. Use column positions when writing code that should work across databases.

`geneset_name` is `NA` for MSigDB, Enrichr, and the NCG and COVID-19 sources of `getHgDisease()`.

## Databases and organisms

The package provides eight retrieval functions:

| Database | Function | Main selection arguments | Organisms listed in version 0.2.8 |
| --- | --- | --- | --- |
| [Gene Ontology](https://geneontology.org/) | `getGO()` | `org`; `ont`: `"bp"`, `"cc"`, `"mf"` | 143 |
| [KEGG](https://www.kegg.jp/) | `getKEGG()` | `org`; `category`: `"pathway"`, `"module"`, `"enzyme"`, `"disease"`, `"drug"`, `"network"` | 8,213 |
| [MeSH](https://www.ncbi.nlm.nih.gov/mesh/) | `getMesh()` | `org`, `method`, `category`; see `mesh_metadata` | 71 |
| [MSigDB](https://www.gsea-msigdb.org/gsea/msigdb/) | `getMsigdb()` | `org`; `category`, e.g. `"H"` or `"C5-GO-BP"` | 20 |
| [WikiPathways](https://www.wikipathways.org/) | `getWiki()` | `org` | 16 |
| [Reactome](https://reactome.org/) | `getReactome()` | `org` | 11 |
| [Enrichr](https://maayanlab.cloud/Enrichr/) | `getEnrichrdb()` | `org`, `library`; see `enrichr_metadata` | 5 |
| Human disease gene sets | `getHgDisease()` | `source`: `"do"`, `"disgenet"`, `"ncg_v6"`, `"ncg_v7"`, `"covid19"` | Human only |

These counts come from the organism tables bundled with package version 0.2.8, rather than a live inventory of the server. Coverage varies by database and collection. KEGG disease, drug, and network gene sets are available for human only.

Human disease sources include [Disease Ontology](https://disease-ontology.org/), [DisGeNET](https://www.disgenet.org/), [Network of Cancer Genes](https://ncg.kcl.ac.uk/) versions 6 and 7, and [COVID-19 gene sets](https://maayanlab.cloud/covid19/). `getHgDisease()` uses `source` and does not take an `org` argument.

Inspect the bundled tables to choose an organism or collection:

```r
head(geneset::go_org)       # GO common names and scientific names
head(geneset::kegg_org)     # KEGG codes, scientific names, and common names
head(geneset::mesh_metadata)

human_libraries <- subset(geneset::enrichr_metadata, organism == "human")
head(human_libraries$library)
```

Other organism tables are `wiki_org`, `msigdb_org`, `reactome_org`, and `mesh_org`. Accepted organism names depend on the function; for example, use `org = "human"` for GO and `org = "hsa"` for KEGG. See individual help pages, such as `?getMsigdb`, for the full argument choices.

## Analysis examples

### Count terms or pathways

Retrieve human KEGG pathways and count the distinct pathway IDs:

```r
gs <- geneset::getKEGG(org = "hsa", category = "pathway")
gs_df <- gs$geneset

length(unique(gs_df[[1]]))
```

### GSVA and ssGSEA

Install GSVA from Bioconductor if needed:

```r
install.packages("BiocManager")
BiocManager::install("GSVA")
```

The following example continues from the KEGG example above and uses the parameter-object interface available in GSVA 1.50.0 and later:

```r
# Convert the term-to-gene table to a named list of gene vectors
gs_list <- lapply(split(gs_df[[2]], gs_df[[1]]), unique)

# Synthetic expression data for demonstrating the interface only
set.seed(42)
gene_ids <- unique(as.character(gs_df[[2]]))
express_data <- matrix(
  rnorm(length(gene_ids) * 6),
  nrow = length(gene_ids),
  dimnames = list(gene_ids, paste0("sample", seq_len(6)))
)

ssgsea_param <- GSVA::ssgseaParam(exprData = express_data, geneSets = gs_list)
ssgsea_mat <- GSVA::gsva(ssgsea_param, verbose = FALSE)

# For GSVA scoring, use GSVA::gsvaParam() instead of GSVA::ssgseaParam()
```

For your analysis, replace `express_data` with a numeric matrix of appropriately normalized expression values, with genes in rows and samples in columns. Row names must be unique gene IDs matching the identifiers in the gene sets, such as gene symbols or Entrez IDs. Consult the [GSVA documentation](https://bioconductor.org/packages/GSVA/) for preprocessing requirements and method choices.

### ORA and GSEA with genekitr

Install the optional analysis package if needed:

```r
install.packages("genekitr")
```

This example uses the ranked human gene list bundled with genekitr:

```r
data("geneList", package = "genekitr")
geneList <- sort(geneList, decreasing = TRUE)
hg_gs <- geneset::getGO(org = "human", ont = "mf")

# ORA: use the top 100 ranked genes as an illustrative selection
input_id <- head(names(geneList), 100)
go_ent <- genekitr::genORA(
  id = input_id,
  geneset = hg_gs,
  universe = names(geneList)
)

# GSEA: use the full ranked gene list
gse <- genekitr::genGSEA(genelist = geneList, geneset = hg_gs)
```

For ORA, replace `input_id` with your selected genes and `universe` with all genes eligible for selection in your experiment. The top 100 genes above are for demonstration only. For GSEA, supply a named numeric vector of gene-level statistics, sorted in decreasing order; names identify genes and values can be signed test statistics or log fold changes. Use compatible gene identifiers and the same organism throughout the analysis.

## Data storage and reproducibility

By default, downloaded files are stored under `tools::R_user_dir("geneset", which = "data")`, with subdirectories for each database. All retrieval functions accept `data_dir` to choose a different cache location:

```r
x <- geneset::getGO(
  org = "human",
  ont = "mf",
  data_dir = file.path("data", "geneset_cache")
)
```

Calls check the server and may refresh cached files when the remote file size changes. A cache directory therefore does not freeze a database version. To reuse the exact gene sets from an analysis, save the returned object separately and record the retrieval date and package version:

```r
saveRDS(
  list(
    geneset = x,
    retrieved_at = Sys.Date(),
    package_version = as.character(utils::packageVersion("geneset"))
  ),
  file = "human_go_mf_snapshot.rds"
)

# Reuse the saved object without contacting the data server
x <- readRDS("human_go_mf_snapshot.rds")$geneset
```

For reports and publications, also record the database, organism, and selection arguments, and cite the original data sources. The retrieval date is not an upstream database release identifier.

## Documentation and support

- [genekitr book: gene sets](https://www.genekitr.fun/get-gene-sets-1.html)
- [Organism documentation](https://genekitr.online/docs/species.html)
- [Report an issue](https://github.com/GangLiLab/geneset/issues)

## Author and license

[Yunze Liu](https://www.jieandze1314.com/)

geneset is licensed under [GPL-3](LICENSE.md).

[![WeChat](https://img.shields.io/badge/follow%20me%20on-WeChat-orange.svg)](https://genekitr.online/img/bioinfoplanet.png)

![Repository activity](https://repobeats.axiom.co/api/embed/1398fe8b05f49210229b9c8bca9b50a59337a7f7.svg "Repobeats analytics image")
