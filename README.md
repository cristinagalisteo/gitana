# gitana.R: phyloGenetic Imaging Tool for Adjusting Nodes and other Arrangements

Phylogenetic trees are essential diagrams used in different sciences, such as evolutionary biology or taxonomy, and they depict the relationships between a given set of taxa from a common ancestor. So far, multitude of tools have already been developed to infer phylogenies, and even more to visualize the resulting trees. However, edition of graphical plots to obtain ready-to-publish figures is still a major issue. Most available tools do not take into consideration important aspects in prokaryotic nomenclature, as the use of italic face for taxon names or the <sup>T</sup> superscript that must be displayed after type strain designation, at least not automatically. The lack of available tools to achieve these tasks is challenging for scientists, since manual formatting of taxon information is very time-consuming.

Here we present a tool known as **'gitana.R' (phyloGenetic Imaging Tool for Adjusting Nodes and other Arrangements)**, designed to fill in the gap left by other programs and to automate the creation of trees with standard in nomenclature. Moreover, 'gitana.R' allows node comparisons among phylogenies constructed using different treeing algorithms to denote conserved branches. Furthermore, additional useful options were included, as setting a new root or unrooting the tree, rotating nodes, highlighting selected clades or taxa, choosing of output format for figures, among others.

'gitana.R' is based on the most popular R libraries ('ape', 'ggtree', 'phytools') for phylogeny and mixes them to create an interactive R script with multitude of optional arguments that allows the imaging of very simple but good looking trees to others more sophisticated.

## Installation and R packages

'gitana.R' runs in **Linux/Windows/Mac** systems where R software (https://cran.r-project.org) and the following R packages had been previously installed.

It was built on **R version 4.4.0** (2024-04-24).

| Library  | Recommended version (\*) | Link                                                |
|:--------:|:------------------------:|:----------------------------------------------------|
|   ape    |          \>=5.8          | <https://cran.r-project.org/web/packages/ape/>      |
| ggplot2  |         \>=3.5.1         | <https://cran.r-project.org/web/packages/ggplot2/>  |
|  ggtext  |         \>=0.1.2         | <https://cran.r-project.org/package=ggtext>         |
|  ggtree  |        \>=3.12.0         | <https://guangchuangyu.github.io/software/ggtree/>  |
| optparse |         \>=1.7.5         | <https://cran.r-project.org/web/packages/optparse/> |
| phytools |         \>=2.1-1         | <https://cran.r-project.org/web/packages/phytools/> |

(\*) Different versions has been tested with success, but some of the optional graphic modifications may behave funny.

## Usage

`gitana.R -t maximum_likelihood.tree -f infoStrains.tsv`

## Input files

-   The main input is a **phylogenetic tree** previously calculated. Besides, two more trees for the same taxa, based on different algorithms, can be provided for topology comparison.
-   **Tab separated file (no headers)** with OTU information. Optional, but recommended for plotting trees ready for publication.\n
    -   Column 1: Tip labels from raw tree.
    -   Column 2: Accession number of the sequences.
    -   Column 3: Scientific name. Blank species are allowed. Everything in this column will be plotted in italics.
    -   Column 4: Strain name. Do not add <sup>T</sup> for type strains.
    -   Column 5: "type" for type strain. Anything else or blank space for nontype strains.

Example:

| Column1  | Column2  |           Column3            | Column4 | Column5 |
|:--------:|:--------:|:----------------------------:|:-------:|:-------:|
| ZZZ16450 | MF618255 | Rhodohalobacter barkolensis  |  15182  |  type   |
| H6XSpe11 | MF782427 | "Aliifodinibius salipaludis" |  WN023  |  type   |
| H6XSedim | JQ923476 |     Fodinibius sediminis     | YIM J21 |         |
| H6XRoseu | JQ923475 |      Fodinibius roseus       | YIM D15 |  type   |
|   ...    |   ...    |             ...              |   ...   |   ...   |

## Output files

-   Plotted tree. By default, pdf format, maximum quality (dpi 300) and A4 size (or larger for \>= 50 OTUs).
-   Rerooted tree (`--root`) plus unedited plotted tree.
-   Unedited tree (`--noedit`).
-   R objects (`--save_object`). To combine the plotted tree with other figures.
-   Log file with used arguments.

## Arguments

-   **`-t/--treeOne`**: Phylogenetic tree previously calculated with tools such as [ARB](http://www.arb-home.de), [FastTree](http://www.microbesonline.org/fasttree/), [MEGA](https://www.megasoftware.net), etc.
-   **`-e/--treeTwo`**: Second tree to compare topology with treeOne.
-   **`-n/--treeThree`**: Third tree to compare topology with treeOne and treeTwo.
-   **`-f/--file`**: Data file with information about the tree taxa (species and strain names, accession numbers).
-   **`-o/--output`**: Name and extension (pdf \| jpg \| tiff \| ...) of the plotted tree. Default: treeOne.pdf.
-   **`--save_object`**: Save plotted tree as R object (.rds). Default: `FALSE`.
-   **`--noedit`**: Plot a basic version of the tree with raw names, nodes and tips numeration. Very useful to know node number for edit options.
-   **`-i/--superindex`**: <sup>T</sup> appears behind the strain name (eg.: CECT 7585<sup>T</sup>) for OTUs with "type" in the fifth column of the data file. Default: `FALSE`.
-   **`--layout`**: Plot design. Options: rectangular (*default*) \| slanted \| circular \| radial \| unrooted \| equal_angle \| daylight.
-   **`-l/--ladderize`**: Get ladderize effect for the tree. Default: `FALSE`.
-   **`--bootstrap_percentage`**: Change bootstrap values to percentage. The original scale must be indicated: 1 \| 1000.
-   **`-b/--bootstrap_threshold`**: Bootstrap under the selected values are not represented over the node. Default: `70`.
-   **`--root`**: Select a node/OTU as new root. Numeration can be checked with `--noedit`.
-   **`--position`**: Position along the target edge at which to re-root the tree. Default: middle branch.
-   **`--unroot`**: Unroot a rooted tree.
-   **`-r/--rotate`**: Rotate selected node. Insert multiple positions separated by ','. Rotate all positions with `all`.
-   **`-c/--collapse`**: Collapse nodes. Node position and mode (max \| min \| mixed) separated by ','. `--collapse 35,min`
-   **`--Cotu`**: Color OTU and its branch. Select color with `--color`.
-   **`--Cclades`**: Color clade and its branch. Select color with `--color`.
-   **`--color`**: Color clade/OTU selected with `--Cotu/--Cclades`.
-   **`--Hclades`**: Highlight clade. Select node, color and extension separated by ','. `--Hclades 35,pink,0.01`
-   **`--Bclades`**: Add labels. Select node and add label, separated by ','. `--Bclades 35,Group1`
-   **`--offset`**: Set position of `--Bclades` label.
-   **`--fontsize`**: Tip labels font size. Default: `6`.
-   **`--nodesize`**: Node labels (bootstrap) and bar font size. Default: `5`.
-   **`--bar_width`**: Bar size. Default: automatically set.
-   **`-W/--output_width`**: Plot width (cm). Default: `29.7` (A4).
-   **`-H/--output_height`**: Plot height (cm). Default: `21` (A4).
-   **`-s/--tree_size`**: Plot is adapted to the sheet size without considering the tip labels. Usually, it is necessary to set it. Lower values expand the tree along the X axis.It is recommended to use bigger sizes for circular layouts
-   **`--scale`**: Scale tree (y-axis). Individual nodes or complete tree. Smaller \<1; bigger \>1. `--scale 35,0.5`
-   **`--scalexy`**: Scale tree (x and y-axis). Individual nodes or complete tree. Smaller \<1; bigger \>1. `--scale 35,0.5`
-   **`-x/--bootstrap_X`**: Position bootstrap values on x-axis. Default: `0`.
-   **`-y/--bootstrap_Y`**: Position bootstrap values on y-axis. Default: `0`.

## Examples

#### Basic plot (not ready for publishing)

```         
gitana.R --treeOne maximum_likelihood.tree
# By default, only bootstrap values above 70% are displayed. 
```

<img src="https://github.com/user-attachments/assets/9e4d140c-92e2-4356-bc78-584217598a4b" width="75%" />

We can easily plot our tree file using a single argument, which is quite useful for quick peek of the result. This is similar to the visualization with other popular tools. Clearly, it is not suitable for publication.

#### Basic plot ready for publication

```         
gitana.R --treeOne maximum_likelihood.tree --treeTwo maximum_parsimony.tree --treeThree neighbor_joining.tree --file infoStrains.tsv -l -i -s 0.5 --bar 0.05 -y 0.3 -x -0.01 --output basic.png
# --treeOne/--treeTwo/--treeThree: Phylogenetic tree based on three different algorithms.
# --file: information about the species
# -l: beautify the topology
# -i: add superindex T to selected OTUs in --file
# -s: adapt tree size to fit the OTU labels in the sheet
# --bar: select bar width
# -y: select bootstrap position in the y-axis 
# -x: select bootstrap position in the x-axis 
# --output: name output file and select png as format
```

<img src="https://github.com/user-attachments/assets/60af5892-742c-4246-8ab3-f36d24480714" width="75%" />

We have a really nice basic plot ready for publish! OTU labels were perfectly edited and <sup>T</sup> added to the strain names. Bootstrap values under 70% were filtered out and the percentage was positioned next to the nodes. Black dots indicate common nodes for the three topologies based on maximum-likelihood, maximum-parsimony and neighbor-joining, respectively.

However, we may want to personalize it a little bit more. For example, name clusters or highlight one or more OTUs. For that, we will need to check the node numeration of the tree with `--noedit`.

#### Get node/tip position

```         
gitana.R --treeOne maximum_likelihood.tree --noedit
# --treeOne: our tree for plotting. Even if we will be using two or more trees for comparison, node/tip position will be based on --treeOne, which topology is plotted.
# --noedit
```

<img src="https://github.com/user-attachments/assets/11932333-85b8-43e2-aa2b-8adf9cc1c150" width="75%" />

OTU numeration is represented in yellow and node numeration in blue.

#### Pretty plot ready for publication

```         
gitana.R --treeOne maximum_likelihood.tree --treeTwo maximum_parsimony.tree --treeThree neighbor_joining.tree --file infoStrains.tsv -l -i -s 0.5 --bar 0.05 -y 0.3 -x -0.01 --output pretty.png --collapse 23,mixed --Bclade 23,Gracilimonas,#FF6347,bold.italic --offset 0.04 --scale 23,0.5 --Hclade 29,#FFA54F,0.25 --Cotu 14 --color seagreen
# --collapse: collapse the node 23 with mode "mixed"
# --Bclade: add a label to the collapse node 23 with the name of the genus (Gracilimonas). Select the color and the fontface for the label
# --offset: set label placement
# --scale: reduce the size (0.5) of the collapse node 23 to save space
# --Hclade: highlight a genus of interest (node 29), color it and set the size of the rectangle
# --Cotu: color a select OTU (14)
# --color: select the color for the OTU 14.
```

<img src="https://github.com/user-attachments/assets/259ff16a-e7b5-4663-b0e2-e0221fc32246" width="75%" />

OTU numeration is represented in yellow and node numeration in blue.

#### Pretty plot (v2) ready for publication

```         
gitana.R --treeOne maximum_likelihood.tree --treeTwo maximum_parsimony.tree --treeThree neighbor_joining.tree --file infoStrains.tsv -l -i -s 0.6 --bar 0.05 -y 0.3 -x -0.012 --output pretty2.png --Cclade 29 --color seagreen --Bclade 29,Fodinibius,seagreen,bold.italic --offset 0.29 --Hclade 14,darkseagreen,0.3:35,steelblue,0.27 --scalexy 21,0.75
# --Cclade: color the genus under study
# --color: select the color for our genus
# --Bclade: name the clade (genus)
# --offset: set label placement
# --Hclade: highlight two groups of OTU within the genus under study, separated by ":". 
# --scalexy: scale the other OTUs in the y and x axis 
```

<img src="https://github.com/user-attachments/assets/8855a950-f6a1-4405-a2a3-f416c256a316" width="75%" />

In this second version, we are focusing on the genus *Fodinibius* exclusively. We color all the members and name the cluster including all the species. Besides, we highlight the main species under study and the group of two species we are comparing it with. Last, we shrink (without collapsing) the rest of the OTU to remove attention from them.

#### Advance figure from our plot ready for publication

gitana.R generates high quality figures without any coding In R. However, it is more limited than working directly with the libraries in R. The argument `--save_object` export the result of gitana.R. That way, it is possible to explore are plot options for phylogenetic trees, or combine it with other plots.

```         
gitana.R --treeOne maximum_likelihood.tree --treeTwo maximum_parsimony.tree --treeThree neighbor_joining.tree --file infoStrains.tsv -l -i -s 0.5 --bar 0.05 -y 0.3 -x -0.01 --output basic.png --save_object
# --save_object: save as R object as 'maximum_likelihood.rds'
```

The RDS object can be load in a new R script:

```         
# Load 'gitana.R' results: 
load("maximum_likelihood.rds") # Plotted tree is saved in 'tr'

# Read  table with extra data
df <- read.table("gene_presence.tsv", sep="\t", header =T)
head(df)
#        otu genA genB genC genD genE genF
# 1 I37Mengy    1    1    1    0    1    0
# 2 I37Rosea    0    0    0    0    0    1
# 3 I37Spec4    0    0    1    0    0    1
# 4   G_trop    1    1    1    0    0    1
# 5 KU987442    1    0    1    1    0    0
# 6   B_vulg    0    0    0    0    1    0

# Prepare data                 
mdf <- reshape::melt(df)
mdf$value <- as.character(mdf$value)

# Plot heatmap
library(ggplot2)
hmap <- ggplot(mdf, aes(x=variable, y =otu, fill=value)) + 
  geom_tile(color="black") + coord_fixed() + 
  theme(
    legend.position="none", 
    axis.text.y = element_blank(), 
    axis.title = element_blank(), 
    axis.text.x = element_text(face = "italic", size = 10), 
    legend.title = element_blank()
  ) + 
    scale_fill_manual(values = c("white", "orange"))
    
# Tree scale add extra blank space at the bottom of the tree. 
# So, let's remove it the three layers creating the tree scale.
tr$layers[[7]] <- NULL
tr$layers[[6]] <- NULL
tr$layers[[5]] <- NULL
# And plot is somewhere else
tr + geom_treescale(x = 0, y =15, width = 0.05) + 
     xlim(0,1.2) # Reajust tree size to fit better in the new plot

# Plot our new heatmap with the tree in the left side.
# The library 'aplot' sort heatmap to match the OTU order of the tree.
hmap %>% aplot::insert_left(tr, width = 2) 

ggsave("aplot.png", width = 29.7, height = 21, units = "cm", dpi= 300)  
```

<img src="https://github.com/user-attachments/assets/339733f3-ac19-40d9-a880-5f4511cdeee9" width="75%" />

gitana.R tree is plotted at the left. The presence (yellow) and absence (white) of genes is at the right, sorted by tree topology.

#### Rerooting

Phylogeny inferring tools does not always root trees.

'gitana.R' can root or reroot trees.

First, we check the node numeration with `--noedit`.

```         
gitana.R --treeOne maximum_likelihood.tree --root 29
# A new tree is created: rerooted_maximum_likelihood_node29_0.021415.tree
# As well a pdf with the new tree plotted with --noedit: rerooted_maximum_likelihood_node29_0.021415.pdf
# The new tree is named after the original tree, plus the node and the position of the branch
# Then, gitana.R is performed as usual to beautify the tree
gitana.R --treeOne rerooted_maximum_likelihood_node29_0.021415.tree --treeTwo maximum_parsimony.tree --treeThree neighbor_joining.tree --file infoStrains.tsv -l -i -s 0.5 --bar 0.05 -y 0.3 -x -0.01 --output rerooted_maximum_likelihood_node29_0.021415.png
```

<img src="https://github.com/user-attachments/assets/372d0b83-929f-4dc8-a56a-f8ca6fc44eb4" width="75%" />

```         
# The position can be manually select with --position
gitana.R --treeOne maximum_likelihood.tree --root 29 --position 0.01
# Plot it
gitana.R --treeOne rerooted_maximum_likelihood_node29_0.01.tree --treeTwo maximum_parsimony.tree --treeThree neighbor_joining.tree --file infoStrains.tsv -l -i -s 0.55 --bar 0.05 -y 0.3 -x -0.01 --output rerooted_maximum_likelihood_node29_0.01.png
```

<img src="https://github.com/user-attachments/assets/091d8ccf-139c-48f9-8a05-5d71c08d7f61" width="75%" />
