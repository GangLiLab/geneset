# Genset

[![](https://img.shields.io/badge/devel%20version-0.2.4-green.svg)](https://github.com/GangLiLab/genekitr)  [![lifecycle](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html) 

![Alt](https://repobeats.axiom.co/api/embed/1398fe8b05f49210229b9c8bca9b50a59337a7f7.svg "Repobeats analytics image")



### Overview

Omic-age brings huge amoung of gene data, which bring a problem of how to uncover their potential biological effects. One effective way is gene enrichment analysis.

Inside gene enrichment analysis, the central and fundamental part is the access of gene sets, no matter of traditional Over-representation analysis (ORA) method or advanced Functional class scoring (FCS) method (e.g. Gene Set Enrichment Analysis (GSEA) ).

Currently, many available enrichment analysis tools provide built-in data sets for few model species or ask users to download online. This causes a problem that user needs to download different gene sets from various public database for non-model species. For example, `enrichGO() and gseGO()` of `clusterProfiler` utilized organism-level annotation package for about 20 species. If research target is not listed in these organisms, user needs to build one via [AnnotationHub](http://bioconductor.org/packages/AnnotationHub) or download from [biomaRt](http://bioconductor.org/packages/biomaRt) or [Blast2GO](https://www.blast2go.com/), which is time-comsuming and hard task for biologists without programming skills.

Here, we develop an R package name "geneset", aimming at accessing for updated gene sets with less time. 

It includes [GO](http://geneontology.org/) (BP, CC and MF), [KEGG](https://www.kegg.jp/kegg/) (pathway, module, enzyme, network, drug and disease), [WikiPathway](https://wikipathways.org/), [MsigDb](https://www.gsea-msigdb.org/gsea/msigdb/), [EnrichrDb](https://maayanlab.cloud/Enrichr/), [Reactome](https://reactome.org/), [MeSH](https://www.ncbi.nlm.nih.gov/mesh/), [DisGeNET](https://www.disgenet.org/), [Disease Ontology](https://disease-ontology.org/) (DO), [Network of Cancer Gene](http://ncg.kcl.ac.uk/) (NCG) (version 6 and v7) and [COVID-19](https://maayanlab.cloud/covid19/). Besides, it supports both model and non-model species.

#### Supported organisms

> For more details, please refer to [this site](https://genekitr.online/docs/species.html).

- GO supports **143** species
- KEGG supports **8213** species
- MeSH supports **71** species
- MsigDb supports **20** species
- WikiPahtwaysupports **16** species
- Reactome supports **11** species
- EnrichrDB supports **5** species 
- Disease-related only support **human** (DO, NCG, DisGeNET and COVID-19)

#### About the data

All gene sets are stored on our website and could be easily accessed with simple functions. 

We will follow a monthly-update frequency to make better user experience.

## 🛠 Installation

#### (submitted to CRAN ...) Install stable version from CRAN:

```
install.packages("geneset")
```

#### Install development version from GitHub:

```
remotes::install_github("GangLiLab/geneset")
```

#### Install development version from Gitee (for CHN mainland users):

```
remotes::install_git("https://gitee.com/genekitr/pacakge_geneset")
```



















