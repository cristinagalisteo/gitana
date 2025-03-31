# gitana: phyloGenetic Imaging Tool for Adjusting Nodes and other Arrangements

Phylogenetic trees are essential diagrams used in various scientific disciplines, such as evolutionary biology or taxonomy. 
They depict the relationships between a specified set of taxa from a common ancestor. 
Numerous tools have already been developed to infer phylogenies, and even more to visualize the resulting trees. 
Nevertheless, a persistent challenge persists in the edition of graphical plots to obtain figures suitable for publication. 
The existing tools often neglect crucial aspects of nomenclature, as the use of italicization taxon names or the <sup>T</sup> superscript following type strain/specimen designations. 
The lack of available tools to achieve these issues presents a significant challange for scientists, as manual formatting of taxa information is a laborious and time-consuming task.

Here we present a tool known as **'gitana' (phyloGenetic Imaging Tool for Adjusting Nodes and other Arrangements)**, designed to fill in the gap left by other programs to automate the creation of trees with standard in nomenclature. 
Furthermore, 'gitana' is able to perform node comparisons among phylogenies (e.g., constructed using different treeing algorithms) to denote conserved branches. 
Multitude of useful functionalities are offered, such as rerooting or unrooting a tree, rotating nodes, highlighting selected clades or taxa, or choosing the output format for figures, among others.

'gitana' is based on the most popular R libraries for phylogeny ('ape', 'ggtree', 'phytools') and combines them to create an interactive R script ('gitana.R') with large selection of optional arguments that allows the imaging of very simple but good looking trees to others more sophisticated.

## Installation and R packages

'gitana.R' runs in **Linux/Windows/Mac** systems where R software (https://cran.r-project.org) and the following R packages must be previously installed.

It was built on **R version 4.4.2**.

| Library  | Recommended version (\*) | Link                                                |
|:--------:|:------------------------:|:----------------------------------------------------|
|   ape    |          \>=5.8.1        | <https://cran.r-project.org/web/packages/ape/>      |
| ggplot2  |         \>=3.5.1         | <https://cran.r-project.org/web/packages/ggplot2/>  |
|  ggtext  |         \>=0.1.2         | <https://cran.r-project.org/web/packages/ggtext/>   |
|  ggtree  |        \>=3.14.0         | <https://guangchuangyu.github.io/software/ggtree/>  |
| optparse |         \>=1.7.5         | <https://cran.r-project.org/web/packages/optparse/> |
| phytools |         \>=2.4.4         | <https://cran.r-project.org/web/packages/phytools/> |

(\*) Different versions has been tested with success, but some of the optional graphic modifications might produce unusual results.

Authors recommend installation with conda: 
```
conda install -c conda-forge -c bioconda r-base=4.2.2 r-ape r-ggplot2 r-ggtext r-optparse r-phytools bioconductor-ggtree
# Optional: you may want to create a new environment just in case ;)
```

## Usage

`gitana.R --tree trees.file --file infoStrains.tsv`

## Input files

-   The main input is a list of one or more **phylogenetic tree(s)** previously calculated. 
The trees must be in Newick or NEXUS format. 
Nevertheless, NEXUS trees can give an error when they are read by "ape" library (`ape::read.nexus()`), thus, if using NEXUS trees as input, we strongly recommend to use the standard NEXUS format (see NEXUS template file in the [examplefiles](https://github.com/cristinagalisteo/gitana/tree/main/examplefiles) folder). \
With the aim of topology comparison, more than one tree with the exact same species can be provided. \
The input file will list the single or multiple trees.

Example:
```  
cat trees.file
./maximum_likelihood.nwk
./maximum_parsimony.nwk
./neighbor_joining.nwk
```  

-   A **tab separated file (no headers)** that includes species information. Optional, but recommended for plotting ready-to-publish trees.
    -   Column 1: Tip labels from raw tree.
    -   Column 2: Accession number of the sequences.
    -   Column 3: Scientific name. Space bars are allowed. Everything in this column will be plotted in italics.
    -   Column 4: Strain/specimen name. Do not add <sup>T</sup> for type strains/specimens.
    -   Column 5: "type" for type strain/specimen. For nontype strains/specimens, leave the field empty or enter any other text.

Example:

| Column1  | Column2  |           Column3            | Column4 | Column5 |
|:--------:|:--------:|:----------------------------:|:-------:|:-------:|
| ZZZ16450 | MF618255 | Rhodohalobacter barkolensis  |  15182  |  type   |
| H6XSpe11 | MF782427 | "Aliifodinibius salipaludis" |  WN023  |  type   |
| H6XSedim | JQ923476 |     Fodinibius sediminis     | YIM J21 |         |
| H6XRoseu | JQ923475 |      Fodinibius roseus       | YIM D15 |  type   |
|   ...    |   ...    |             ...              |   ...   |   ...   |


## Output files

-   Plotted tree. By default, PDF format, high quality (400 dpi) and A4 size (29.7 x 21 cm) or larger for \>= 50 species.
-   Log file with used arguments (gitana.log).
-   Unedited tree (`--noedit`).
-   Rerooted tree (`--root`) plus unedited plotted tree.
-   R objects (`--Robject`, gitana.Rds). Plotted tree is saved as an R object and can be combined with other figures.

## Arguments

-   **`-t/--tree`**: File listing the location of the input phylogenetic tree file. Trees should have been previously inferred with tools such as 
[PHYLIP](https://phylipweb.github.io/phylip/), [FastTree](http://www.microbesonline.org/fasttree/),
[BEAST](https://beast.community), 
[MrBayes](https://nbisweden.github.io/MrBayes/),
[MEGA](https://www.megasoftware.net), 
[RAxML](https://github.com/amkozlov/raxml-ng), 
[PAUP*](https://paup.phylosolutions.com) or 
[IQ-TREE](http://www.iqtree.org), among others.
-   **`-f/--file`**: Data file with information about the tree taxa (species, strain/specimen names and accession numbers).
-   **`--nexus`**: If `TRUE`, the phylogenetic trees format is standard NEXUS. Default: `FALSE` (Newick format).
-   **`-o/--output`**: Name and extension (`pdf | jpg | tiff | ...`) of the plotted tree. Default: `gitanaPlot.pdf`.
-   **`--Robject`**: If `TRUE`, plotted tree is saved as R object (`gitana.rds`). Default: `FALSE`.
-   **`--noedit`**: Plot a basic version of the tree with raw names, nodes and tips numeration (`noedit.pdf`). Very useful for knowing the node and tip number for editing options.
-   **`--isna`**: If `TRUE`, it will plot missing information from `--file` as NA. Default: `FALSE`.
-   **`-i/--superindex`**: <sup>T</sup> is displayed behind the strain/specimen name (e.g., CECT 7585<sup>T</sup>) for species marked as "type" in the fifth column of the data file. Default: `FALSE`.
-   **`--layout`**: Plot design. Options: `rectangular` (*default*) \| `slanted` \| `circular` \| `radial`.
-   **`-l/--ladderize`**: Get ladderize effect for the tree. Note that this option is not compatible with `--rotate`. Default: `FALSE`.
-   **`--bootstrap_percentage`**: Change bootstrap values to percentage. The original scale must be indicated: `1` \| `1000`.
-   **`-b/--bootstrap_threshold`**: Bootstrap below the selected threshold will not be displayed. Default: `70`.
-   **`--root`**: Select a node/species as new root. Numeration can be checked with `--noedit`.
-   **`--position`**: Numeric value for position along the target edge at which to reroot the tree. By default, it will be set at middle branch. E.g., `--position 0.02`.
-   **`--unroot`**: Unroot a rooted tree.
-   **`-r/--rotate`**: Rotate the selected node. Insert multiple positions separated by ','. To rotate all positions, use `--rotate all`. E.g., `--rotate 20,26` or `--rotate all,24`.
-   **`-c/--collapse`**: Collapse nodes. Node position and mode (`max | min | mixed`) separated by ','. To collapse multiple nodes use ":" as separator. E.g., `--collapse 35,min` or `--collapse 35,min:24,max` for multiple nodes.
-   **`--hide_taxa_number`**: If `TRUE`, hide the number of taxa within the collapsed node after using `--collapse` function.  Default: `FALSE`.
-   **`--Ctip`**: Color single species (tip) and its branch. To color multiple tips and branches use "," as separator. Select color with `--color`. E.g., `--Ctip 4` or `--Ctip 4,10`
-   **`--Cclades`**: Color clade and its branch. To color multiple clades and branches use ',' as separator. Choose color with `--color`. E.g., `--Cclades 35` or `--Cclades 35,40` 
-   **`--color`**: Color clade/species selected with `--Ctip/--Cclades`. E.g., `--color blue` or `--color green,pink`
-   **`--Hclades`**: Highlight clade. Select node, color and extension separated by ','. To highlight multiple clades use ':' as separator. E.g., `--Hclades 35,pink,0.01` or `--Hclades 35,pink,0.01:40,green,0.015` 
-   **`--Bclades`**: Add labels to clade. Select node and add label, separated by ','. To add multiple labels use ':' as separator. E.g., `--Bclades 35,Group1` or `--Bclades 35,Group1:40,'Group of interest'`
-   **`--offset`**: Set position of `--Bclades` label. E.g., `--offset 0.012`
-   **`--fontsize`**: Font size for tip labels. Default: `6`.
-   **`--nodesize`**: Font size for node (bootstrap) and bar labels. Default: `5`.
-   **`--bar`**: Bar size. Default: automatic. E.g., `--bar 0.01`.
-   **`-W/--output_width`**: Output width (cm). Default: `29.7` (A4).
-   **`-H/--output_height`**: Output height (cm). Default: `21` (A4).
-   **`-s/--size`**: Adapt tree horizontal expansion. By default, `1`. 
-   **`--xlim`**: Circular/radial plots requiere an adjustment of the tree size to fit the species names correctly in the sheet. Lower values expand the tree along the X axis. By default, `NULL`. 
-   **`--scale`**: Scale tree (y-axis). Individual nodes or complete tree. Smaller \<1; bigger \>1. Use 'all' to scale the complete tree.  To scale multiple nodes use ":" as separator. E.g.: single node: `--scale 35,0.5`; multiple nodes: `--scale 35,0.5;27,1.2`; full tree: `--scale all,0.5`
-   **`--scalexy`**: Scale tree (x and y-axis). Individual nodes or full tree. Smaller \<1; bigger \>1.  To scale multiple nodes use ":" as separator. E.g.: single node: `--scalexy 35,0.5`; multiple nodes: `--scalexy 35,0.5;27,1.2`; full tree: `--scalexy all,0.5`
-   **`-x/--bootstrap_X`**: Bootstrap values position on the x-axis. Default: `0`.
-   **`-y/--bootstrap_Y`**: Bootstrap values position on the y-axis. Default: `0`.

## Examples
Files used in these examples can be found in the 'examplefiles' folder.

#### **Basic plot (not ready for publishing)**

We only need a single file (tree1.file) pointing to the tree file:  

``` 
cat tree1.file
./maximum_likelihood.nwk
# The file will contain a single item. 
```

```        
gitana.R --tree tree1.file
# 'tree1.file' indicates (path and) name of the tree file
# By default, only bootstrap values above 70% are displayed. 
```

<img src="https://github.com/user-attachments/assets/fedc5dd8-1af9-4e6d-a8e2-78fb7216c71f" alt="oneTree" align="middle" width="75%" />

***Fig 1.** Basic tree without any kind of editing. Tip labels (species names) are printed as shown in the phylogenetic tree file. The plot is similar to the visualization with other popular tools.*

\
We can easily plot our tree file using a single argument, which is quite useful for a first peek of the result. This is similar to the visualization with other popular tools. Clearly, it is not suitable for publication.


#### **Basic plot ready for publication**

To transform the previous tree (**Fig 1**) into a ready-to-publish image (**Fig 2**), the first thing is to have ready a tab-separated file as explained in the **Input file** section. 
Instead of using a single tree, we want to compare the phylogenies generated for the same dataset. They will all be listed in one file:

``` 
cat trees.file
./maximum_likelihood.nwk
./maximum_parsimony.nwk
./neighbor_joining.nwk
```

Since there are type strains/specimens among the species, we will select the `--superindex/-i` option.
Also, we want a nicer topology, so we will use the `--ladderize/-l` option to give the tree a ladder-like effect.
The `--bar` argument selects the size of the bar width.
The `-y` and `-x` arguments will adjust the position of the bootstrap values above the nodes. As we are not adjusting the `--bootstrap_threshold/-b` argument, bootstrap values under 70% will be filtered out.
Finally, we will specify the name of the output file as well as the file format (PNG instead of the default PDF) with the `--output/-o` argument.

```         
gitana.R --tree trees.file --file infoStrains.tsv -l -i --bar 0.05 -y 0.3 -x -0.01 --output basic_ready.png
# --tree: file indicanting path and name of the tree or trees to produce the final plot. All trees should harbour the same taxa dataset. 
# --file: file with information about the species. Nomenclature, strain and accession number will be taken from this file.
# -l: beautify the topology
# -i: add superindex T to species in which "type" was specified in the column 5 of --file 
# --bar: select bar width instead of the automatic bar width selection
# -y: select bootstrap position on the y-axis 
# -x: select bootstrap position on the x-axis 
# --output: name of the output file and selection of PNG format
```

<img src="https://github.com/user-attachments/assets/ef742382-3ea0-4ff8-8acc-43e24f083ae1" alt = "basic" align="middle" width="75%" />

***Fig 2.** Ready-to-publish tree.*

We have a really nice basic plot ready to publish! Species names were perfectly edited and <sup>T</sup> added to the strain names. 
Bootstrap values below 70% have been filtered out and the percentages have been placed next to the nodes. 
Filled circles indicate common nodes for the input trees.

However, we may want to keep the original topology. In that case, we remove the `--ladderize/-l` option:
```           
gitana.R --tree trees.file --file infoStrains.tsv -i --bar 0.05 -y 0.3 -x -0.01 --output basic_noladderize.png
```

<img src="https://github.com/user-attachments/assets/25d789e2-7f09-4325-a71e-ffa98ad469de" alt="basic_noladderize" align="middle" width="75%" />

***Fig 3.** Phylogeneetic tree with original topology.*

\
Or, we may want to plot all the bootstrap values instead of those above a certain threshold: 
```           
gitana.R --tree trees.file --file infoStrains.tsv -l -i --bar 0.05 -y 0.3 -x -0.01 -b 0 --output basic_b0.png
# -b 0: all bootstrap values will be displayed.
```
<img src="https://github.com/user-attachments/assets/4c53cc12-9dbe-49cb-8ffc-705c02be43cb" alt="basic_b0.png" align="middle" width="75%" />

***Fig 4.** Phylogenetic tree with all bootstrap values above the nodes.*

\
We can exclude some of the taxa information such as some accession numbers or even taxa names. By default, the tree will be plotted without this missing information:
```           
gitana.R --tree trees.file --file infoStrainsNA.tsv -l -i --bar 0.05 -y 0.3 -x -0.01 --output basic_NA.png
```

<img src="https://github.com/user-attachments/assets/15ee6b15-2f52-459c-b31a-525c75be5ed7" alt="basic_NA" align="middle" width="75%" />

***Fig 5.** Phylogenetic tree without missing information.*

\
The previous tree lacks several data due to the missing information in `--file`. If we want to highlight the possible missing information in the generated tree we may use `--isna`.

```           
gitana.R --tree trees.file --file infoStrainsNA.tsv -l -i --bar 0.05 -y 0.3 -x -0.01 --isna --output basic_isNA.png
```

<img src="https://github.com/user-attachments/assets/574582f7-da02-497e-81fc-82cf4e5e4ee0" alt ="basic_isNA" align="middle" width="75%" />

***Fig 6.** Phylogenetic tree showing **NA** at missing information.*


#### **Get node/tip position**

```           
gitana.R --tree trees.file --noedit
# --tree: our tree for plotting. Even if we provide two or more input trees for comparison, node/tip numbering will be based on first listed tree, whose topology will be plotted.
# --noedit: print tree with node/tip numbering.
#  No more arguments needed. Output image will be save as a PDF file.
```
<img src="https://github.com/user-attachments/assets/65e5baf9-52e4-4ec2-8adc-631799f8a5cf" alt="maximum_likelihood_noedit" align="middle" width="75%" />

***Fig 7.** Tree plotted with node (blue) and tip (yellow) numbering based on the 'ape' R package.*

\
Node/tip identification is required for some of the arguments, such as `--rotate`: 

```  
gitana.R --tree trees.file --file infoStrains.tsv -i --bar 0.05 -y 0.3 -x -0.01 --rotate 29 -o basic_noladderize_rotate.png
# --rotate: indicates node that we wish to rotate from the original topology. Remember: --rotate and --ladderize are mutually exclusive.
```

<img src="https://github.com/user-attachments/assets/d668aaf9-8cd9-4bc5-851f-95b6903fe699" alt = "basic_noladderize_rotate" align="middle" width="75%" />

***Fig 8.** Node 29 has been rotated from the original topology.*


#### **Collapse**
`--collapse` will allow the collapse of taxa within the selected node: 
```  
gitana.R --tree trees.file --file infoStrains.tsv -l -i --bar 0.05 -y 0.3 -x -0.01 --collapse 23,mixed --output collapseBasic.png
# --collapse: select the node to collapse and the type of collapsing (max | min | mixed)
```

<img src="https://github.com/user-attachments/assets/45b1a17d-8eef-47b1-ab84-b368094b9a46" alt="collapseBasic" align="middle" width="75%" />

***Fig 9.** Node 23 has been collapsed with 'mixed' layout. The number of taxa within the collapsed node is indicated next to the node.*

\
The collapsed node can be annotated using `--Bclade` to add the label and select the color and font face, and `--offset` to set its position.
```  
gitana.R --tree trees.file --file infoStrains.tsv -l -i --bar 0.05 -y 0.3 -x -0.01 --collapse 23,mixed --Bclade 23,Gracilimonas,#FF6347,bold.italic --offset 0.04 --output collapsedAnotated.png 
# --collapse: select the node to collapse and the type of collapsing (max | min | mixed)
# --Bclade: select the node to annotate (23), the annotation text (Gracilimonas), the color of the text (#FF6347) and font face (bold.italic). Font face can be: plain | bold | italic | bold.italic
# --offset: position of the added label
```

<img src="https://github.com/user-attachments/assets/9107b51b-7715-49d0-8c25-6507f316ab39" alt="collapsedAnotated.png" align="middle" width="75%" />

***Fig 10.** Node 23 has been collapsed with 'mixed' layout. The number of taxa within the collapsed node is indicated next to the node. All the taxa within the node 23 belong to the genus* Gracilimonas *thus, it is labeled with the name of that genus.*

\
Depending on the number of taxa constituting the collapsed node, it could still take up a lot of space in the tree plot. The `--scale` argument edits the height of the selected node. 
```  
gitana.R --tree trees.file --file infoStrains.tsv -l -i --bar 0.05 -y 0.3 -x -0.01 --collapse 23,mixed --Bclade 23,Gracilimonas,#FF6347,bold.italic --offset 0.04 --scale 23,0.5 --output collapsedAnotatedScaled.png
# --scale: select node (23) and set it to smaller size (x0.5).
```

<img src="https://github.com/user-attachments/assets/8a5f5315-565a-4c3e-9cb2-cdb2808f9deb" alt="collapsedAnotatedScaled" align="middle" width="75%" />

***Fig 11.** Node 23 has been collapsed with 'mixed' layout. The number of taxa within the collapsed node is indicated next to the node. All the taxa within the node 23 belong to genus* Gracilimonas *, thus it is labeled with the name of the genus.*

\
To remove the label with number of taxa within the collapsed node, use `--hide_taxa_number`.
``` 
gitana.R --tree trees.file --file infoStrains.tsv -l -i --bar 0.05 -y 0.3 -x -0.01 --collapse 23,mixed --scale 23,0.5 --Bclade 23,Gracilimonas,#FF6347,bold.italic --offset 0.04 --hide_taxa_number --output collapsedAnotatedScaledNtaxa.png
# --hide_taxa_number: remove the text label indicating the number of taxa within the node 23.
```

<img src="https://github.com/user-attachments/assets/ebb9849d-76fc-4465-89a5-e3b9db9c6bef" alt="collapsedAnotatedScaledNTaxa" align="middle" width="75%" />

***Fig 12.** Node 23 has been collapsed with 'mixed' layout. All the species within the node 23 belong to genus* Gracilimonas *thus, it is labeled with the name of the genus.*


#### **Color**
Selected taxa or nodes can be colored using `--Ctip` or `--Cclade` combined with `--Hclade`. Beware, `--Ctip` and `--Cclade` are mutually exclusive.

```  
gitana.R --tree trees.file --file infoStrains.tsv -l -i --bar 0.05 -y 0.3 -x -0.01 --Ctip 14 --color seagreen --output Cotu.png
# --Ctip: select species to color.
# --color: select the color by name or HEX code.
```

<img src="https://github.com/user-attachments/assets/5c877bbb-55bf-406c-bbb8-92a4ee1d128e" alt="Cotu.png" align="middle" width="75%" />

***Fig 13.** Taxon of interes located at tip 14 has been colored.*

\
Furthermore, we can combine `--Ctip` and `--Hclade`:
```  
gitana.R --tree trees.file --file infoStrains.tsv -l -i --bar 0.05 -y 0.3 -x -0.01 --Ctip 14 --color seagreen --Hclade 29,#FFA54F,0.25 --output CtipHclade.png
# --Hclade: select node, color and the width of the rectangle.
```

<img src="https://github.com/user-attachments/assets/d40241ea-90c0-4f2e-b476-6626f26d9519" alt="CtipHclade" align="middle" width="75%" />

***Fig 14.** The taxon of interest located at tip 14 has been colored and the cluster it belong to (node 29) has been highlighted.*

\
Similarly, we can combine the `--Cclade`, `--Hclade` and `--Bclade`:
```  
gitana.R --tree trees.file --file infoStrains.tsv -l -i --bar 0.05 -y 0.3 -x -0.012 -Cclade 29 --color seagreen --Bclade 29,Fodinibius,seagreen,bold.italic --offset 0.29 --Hclade 14,#EEAEEE,0.3:35,#EEAEEE,0.27 --size 1.2 --output Cclade.png -
# --Cclade: select clade to color
# --color: select color for Cclade
# --Bclade: create text annotation for the colored clade
# --offset: set position of the text annotation (--Bclade)
# --Hclade: select one tip and one node and their respective colors and width of the rectangles, separated by ":"
# --size: squeeze image to the left for better accomodation of text annotation
```

<img src="https://github.com/user-attachments/assets/e6a88175-76d0-48cd-9748-4f9de803e6a2" alt="Cclade.png" align="middle" width="75%" />

***Fig 15.** Color clade related to the genus *Fodinibius*, label this node and highlight species and clade of species of interest.*

\
Of course, we can combine it with other functions such as `--collapse`: 
```  
gitana.R --tree trees.file --file infoStrains.tsv -l -i --bar 0.05 -y 0.3 -x -0.01 --collapse 23,mixed --Bclade 23,Gracilimonas,#FF6347,bold.italic --offset 0.04 --scale 23,0.5 --Ctip 14 --color seagreen --Hclade 29,#FFA54F,0.25 --output collapsedCtipHclade.png 
```

<img src="https://github.com/user-attachments/assets/ae83f3fe-9118-49f7-8123-d90a9a2716cc" alt="collapsedCtipHclade" align="middle" width="75%" />

***Fig 16.** Collapse the node 23, annotate it, and scale it. Species located at tip 14 is colored and node 29 is highlighted.*


#### **Rerooting**
Rerooting is as simple as using the `--root` option. It creates a new tree that is ready to edit. By default, it will set equivalent branch length but it can be edited by `--position`.
```  
## Reroot first:
gitana.R --tree trees.file --root 29
## It generates the following files:
# rerooted__node29_0.021415.pdf     # Plotted tree as '--noedit'.
# rerooted__node29_0.021415.tree    # Rooted tree.

## Rerooted tree name should be save in a new file:
ls rerooted__node29_0.021415.tree > treesReroot.file

## Plot it as a regular tree:
gitana.R --tree treesReroot.file --file infoStrains.tsv -l -i --bar 0.05 -y 0.3 -x -0.01 --output rerooted_maximum_likelihood_node29_0.021415.png
```

<img src="https://github.com/user-attachments/assets/7a512999-c504-46db-bf4b-69c0d464103c" alt="rerooted_maximum_likelihood_node29_0.021415" align="middle" width="75%" />

***Fig 17.** Phylogenetic tree previously rerooted at node 29.*

\
To manually select the position along the target edge at which to reroot the tree, use `--position` option:
```  
## Reroot first:
gitana.R --tree trees.file --root 29 --position 0.01
# --position: indicates the position of the root along the target root.

## It generates the following files:
# rerooted__node29_0.01.pdf     # Plotted tree as '--noedit'.
# rerooted__node29_0.01.tree    # Rooted tree.

## Name of the new tree should be saved in 'treesRerootPosition.file': 
ls rerooted__node29_0.01.tree > treesRerootPosition.file    

## Plot it as a regular tree:
gitana.R --tree treesRerootPosition.file --file infoStrains.tsv -l -i --bar 0.05 -y 0.3 -x -0.01 --output rerooted_maximum_likelihood_node29_0.01.png
```

<img src="https://github.com/user-attachments/assets/e30b6771-410d-48de-abc8-a26429785cd0" alt="rerooted_maximum_likelihood_node29_0.01" align="middle" width="75%" />

***Fig 18.** Phylogenetic tree previously rerooted at node 29 with customized position of the root brach length.*


#### **Layout**
Until now, we have only plotted trees using the `rectangular` layout (default). 'gitana.R' also allows `slanted`, `circular` and `radial` layouts:

a) Layout `slanted`:
```  
gitana.R --tree trees.file --file infoStrains.tsv -l -i --bar 0.05 --layout slanted -x -0.013 --output basic_slanted.png
# --layout: select 'slanted'
# -x: select position of bootstrap values in the x-axis
## In this case, we don't ajust the bootstrap position in the y-axis
```

<img src="https://github.com/user-attachments/assets/22a30741-79dd-4e19-8091-1d2bb196959e" alt="basic_slanted" align="middle" width="75%" />

***Fig 19.** Phylogenetic tree with `slanted` layout.*

\
b) Layout `circular`:
\
Circular trees are more difficult to customize because the tree takes up more space than the `rectangular`/`slanted` layout. \
The sheet size will be set to square dimensions by default. Taxa names may not be fully displayed. `--xlim` will adjust the tree along the x-axis and `--fontsize` and `--nodesize` will set smaller font size for better visualization.
```  
gitana.R --tree trees.file --file infoStrains.tsv -l -i --bar 0.05 --layout circular -x -0.015 -y 0.15 --xlim 0.6 --fontsize 4 --nodesize 3 --output basic_circular.png
# --layout: select 'circular'
# --xlim: lower values expand the tree along the x-axis. It is recommended to use bigger sizes for circular layouts.
# --fontsize: set a smaller font size for taxa names (by default, 6).
# --nodesize: set a smaller font size for bootstrap (by default, 4).
```

<img src="https://github.com/user-attachments/assets/b4ed828d-56ac-4d97-970d-1063fcdd8df7" alt="basic_circular" align="middle" width="75%" />

***Fig 20.** Phylogenetic tree with `circular` layout.*

\
Circular layouts can be edited in a similar way than the rectangular layouts:
```  
gitana.R --tree trees.file --file infoStrains.tsv -l -i --bar 0.05 --layout circular -x -0.015 -y 0.15 --xlim 0.6 --fontsize 4 --nodesize 3 --Cclade 29 --color seagreen --output circular_Cclade.png
```

<img src="https://github.com/user-attachments/assets/4ddb62bd-73c3-4eae-90a6-f80a4d1b6258" alt="circular_Cclade.png" align="middle" width="75%" />

***Fig 21.** Phylogenetic tree with `circular` layout with node 29 colored.*

\
c) Layout `radial`:
\
Radial layout is similar to `circular` layout, which means that `--xlim`, `--fontsize` and `--nodesize` might need to be modified.

```  
gitana.R --tree trees.file --file infoStrains.tsv -l -i --bar 0.05 --layout radial -x -0.02 --xlim 0.6 --fontsize 4 --nodesize 3 --output basic_radial.png 
```

<img src="https://github.com/user-attachments/assets/1c018abc-900a-4a1d-9182-3121665f53ed" alt="basic_radial" align="middle" width="75%" />

***Fig 22.** Phylogenetic tree with "radial" layout.*


#### **Working with big trees**
In general, the larger the tree, the harder it is to visualize. 'gitana.R' automatically adjusts the height of sheet size depending on the number of taxa. 
However, in some cases it may be necessary to adjust it manually. It is also recommended to set the font size smaller than the default to avoid overlapping.
The authors recommend to fit the trees and then change position options like `--bootstrap_X`, `--bootstrap_Y`, `--offset` or `--Hclades`. 

```  
gitana.R --tree big_tree_MP.file --file infoStrains_bigTree.tsv -l --bar 0.01 -H 85 --fontsize 3 --nodesize 2 -x -0.006 -y 0.4 --output big_tree.png 
# -H: gitana.R selects 80 cm. However, we decided to add 5 cm for a better fit.
# --fontsize: the font size of the species names has been reduced from 5 to 3 to avoid overlapping.
# --nodesize: similarly, the font size of the nodes and bar text has been reduced from 4 to 2.
```

<img src="https://github.com/user-attachments/assets/dd040382-f681-419d-be2d-08e6802098e1" alt="big_tree.png" align="middle" width="75%" />

***Fig 23.** Phylogenetic tree with 245 species.*

\
To plot them with circular/radial layout, 'gitana.R' adjusts a 1:1 sheet size depending on the number of taxa: 

```  
gitana.R --tree big_tree_MP.file --file infoStrains_bigTree.tsv -l --bar 0.01 -H 60 -W 60 --fontsize 3 --nodesize 2 -x -0.004 -y 0.4 --xlim 0.3 --output big_tree_circular.png 
# gitana.R selected a size of 80x80 cm for the > 200 species of this tree. We selected bigger sheet size. Furthermore, we squeezed the tree to avoid white spaces in the middle using "--xlim".
# -H: gitana.R selects 80 cm. However, we decided for a smaller sheet: 60 cm
# -W: gitana.R selects 80 cm. However, we decided for a smaller sheet: 60 cm
# --fontsize: the fontsize of the species names has been reduced from 5 to 3 to avoid overlapping 
# --nodesize: similarly, the font size of the nodes and bar text has been reduced from 4 to 2
# --xlim: squeeze the tree closer to the middle as we are using values above 0
```

<img src="https://github.com/user-attachments/assets/9b4edd3e-bfcc-48f3-8887-7002eb5f487a" alt="big_tree_circular" align="middle" width="75%" />

***Fig 24.** Phylogenetic tree with 245 species with "circular" layout.*


#### **Advance modifications to our ready-to-publish plot**

'gitana.R' produces high quality figures without any R coding skills. 
However, it is more limited than working directly with the libraries in R. 
The argument `--Robject` exports the result of 'gitana.R' into a `RDS` file. 
This makes it possible to explore other plotting options for phylogenetic trees, or combine it with other plots.

```           
gitana.R --tree trees.file --file infoStrains.tsv -l -i --bar 0.05 -y 0.3 -x -0.01 --output basic.png --Robject
# --Robject: saves R object as 'gitana.rds'
```

The `RDS` object can be loaded in a new R script:

``` R         
# Load 'gitana.R' results: 
load("gitana.rds") # Plotted tree is saved in a variable called 'tr'.

# Read table with extra data:
df <- read.table("gene_presence.tsv", sep="\t", header =T)
head(df)
#        taxa genA genB genC genD genE genF
# 1 I37Mengy    1    1    1    0    1    0
# 2 I37Rosea    0    0    0    0    0    1
# 3 I37Spec4    0    0    1    0    0    1
# 4   G_trop    1    1    1    0    0    1
# 5 KU987442    1    0    1    1    0    0
# 6   B_vulg    0    0    0    0    1    0

# Prepare data:               
mdf <- reshape::melt(df)
mdf$value <- as.character(mdf$value)

# Plot heatmap:
library(ggplot2)
hmap <- ggplot(mdf, aes(x=variable, y=taxa, fill=value)) + 
  geom_tile(color="black") + coord_fixed() + 
  theme(
    legend.position="none", 
    axis.text.y = element_blank(), 
    axis.title = element_blank(), 
    axis.text.x = element_text(face = "italic", size = 10), 
    legend.title = element_blank()
  ) + 
  scale_fill_manual(values = c("white", "orange"))
    
# Tree bar scale add extra blank space at the bottom of the tree. 
# So, let's remove the three layers creating the tree bar scale:
tr$layers[[7]] <- NULL
tr$layers[[6]] <- NULL
tr$layers[[5]] <- NULL
# We still want to display the bar scale, so we manually plot it indicating the position with `x` and `y` arguments: 
tr <- tr + geom_treescale(x = 0, y = 15, width = 0.05)

# Besides, we reajust tree size to fit better in the new plot:
tr <- tr + xlim(0,1.2)

# Plot our new heatmap with the tree in the left side.
# The library 'aplot' sort heatmap to match the taxa order of the tree.
hmap %>% aplot::insert_left(tr, width = 2) 

ggsave("aplot.png", width = 29.7, height = 21, units = "cm", dpi= 300)  
```

<img src="https://github.com/user-attachments/assets/c3889389-cbaf-4997-8f1b-d81d4df51325" alt="aplot" align="middle" width="75%" />

***Fig 25.** Phylogenetic tree plotted with `gitana.R`, exported as a `Robject` and further combined with other R plot. The presence (orange) and absence (white) of genes is shown on the right side, sorted by tree topology.*
\
