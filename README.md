# gitana
#### phyloGenetic Imaging Tool for Adjusting Nodes and other Arrangements

Phylogenetic trees are essential diagrams used in different sciences, such as evolutionary biology or taxonomy, and they depict the relationships between a given set of taxa from a common ancestor. So far, multitude of tools have already been developed to infer phylogenies, and even more to visualize the resulting trees. However, edition of graphical plots to obtain ready-to-publish figures is still a major issue. Most available tools do not take into consideration important aspects in prokaryotic nomenclature, as the use of italic face for taxon names or the <sup>T</sup> superscript that must be displayed after type strain designation, at least not automatically. The lack of available tools to achieve these tasks is challenging for scientists, since manual formatting of taxon information is very time-consuming. 

Here we present a tool known as **‘gitana’ (phyloGenetic Imaging Tool for Adjusting Nodes and other Arrangements)**, designed to fill in the gap left by other programs and to automate the creation of trees with standard in nomenclature. Moreover, 'gitana' allows node comparisons among phylogenies constructed using different treeing algorithms to denote conserved branches. Furthermore, additional useful options were included, as setting a new root or unrooting the tree, rotating nodes, highlighting selected clades or taxa, choosing of output format for figures, among others.

'gitana' is based on the most popular R libraries ('ape', 'ggtree', 'phytools') and mixes them to create an interactive R script with multitude of optional arguments that allows the imaging of very simple but good looking trees to others more sofisticated.


## R Libraries and version 
'gitana' was created on R version 4.1.0. 

The fellowing libraries are needed for the code. 

- ape # version 5.5 
behave 
- ggplot2  # version 3.3.5

- ggtree  # version 3.0.2

- optparse # version 1.6.6

- phytools # version 0.7-80

It has been tested on different versions and it still works, but some of the optional graphic modifications may behave funny. 

# Usage

The simplest use is to included your trees (or only one tree), the organiza data. 

`gitana. R--treeOne tree1.tree --treeTwo tree2.tree --treeThree tree3.tree -f data.txt --superindex --align --ladderize -o basico.jpg`

The `-f` file should follow this structure: 

- Column 1: Exact name of the OTUs as they appear on the .tree file

- Column 2: Accession number of the sequences. No need for parentesis. They will be added automatically. 

- Column 3: Scientific name. Blank spaces are allowed. 

- Column 4: Strain name. For superindex <sup>T</sup>, use "--superindex" argument.

# Workflow
![gitana_workflow](https://user-images.githubusercontent.com/94676441/171913474-31212b42-5cdc-4d25-95f1-589d11f79429.png)

