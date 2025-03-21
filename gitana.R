#!/usr/bin/env -S Rscript --vanilla
# -*-coding: utf-8 -*-

# @autor: Cristina Galisteo
# @Date: 2024
# @Version: p0.1

############### LIBRARIES ############### 
requiredPackages <- c("ggtree","phytools", "ape", "optparse", "ggtext", "ggplot2")
for (package in requiredPackages) { # Install packages if not yet installed
  if (!requireNamespace(package, quietly = TRUE)) {
    if (package == "ggtree"){
      cat("R library '", package, "' is not installed. Please, try 'BiocManager::install('ggtree')' from your R session\n", sep = "")
    } else {
      cat("R library '", package, "' is not installed. Please, try 'install.package('", package, "')' from your R session\n", sep = "")
    }
  stop("\033[0;31mMake sure R libraries are install in your R environment before continuing.\033[0m
     If you are having issues, we recommend you to create a new conda environment and install as follow:\
     'conda install -c conda-forge -c bioconda r-base=4.4.2 r-ape r-ggplot2 r-ggtext r-optparse r-phytools bioconductor-ggtree'", call.=FALSE)
  }
}
suppressMessages({
  library(ggtree)   # Visualization and annotation of phylogenetic trees, based on 'ggplot2' library.
  library(phytools) # Node comparison on phylogenetic trees.
  library(ape)      # Plot node and OTU numeration.
  library(optparse) # Add arguments.
})


############### ARGUMENTS ############### 
# Arguments list: 
option_list = list(
  ### Input files
  make_option(c("-t", "--tree"),   # Required
              action="store",
              type="character",
              default = NULL,
              help="List of phylogenetic tree(s).\n\t\tFinal tree plot present the topology of the first one listed when multiple tree are compared."
  ),
  
  make_option(c("--nexus"),   # Required
              action="store_true",
              default = FALSE,
              help="NEXUS format."
  ),
  make_option(c("-f", "--file"),      # Info species. Required to correct edit of the OTU labels
              action="store",
              type="character",
              default = NULL,
              help="Data file with species information.\n\t\tIt must present 4 columns, separated by TABULATION, as specified below:\n\t\t\t- Column 1: Tip labels from raw tree.\n\t\t\t- Column 2: Accession number of the sequences.\n\t\t\t- Column 3: Scientific name. Blank species are allowed. Everything in this column will be plotted in italics.\n\t\t\t- Column 4: Strain name. Do not add T for type strains.\n\t\t\t- Column 5: “type” for type strain. Anything else or blank space for nontype strains.\n\t\t"
  ),

  ### Save plotted tree as R object
  make_option(c("--Robject"),
              action="store_true",
              default = FALSE,
              help="Save plotted tree as R object (gitana.rds)."
  ),
   ### Plot tree one with node and tip numeration
  make_option(c("--noedit"),
              action="store_true",
              default=FALSE,
              help="Plot a basic version of the tree with raw names, nodes and tips numeration."
  ),
   ### Show information missing in '--file' as NA:
  make_option(c("--isna"),
              action="store_true",
              default=FALSE,
              help="Plot missing information as 'NA'."
  ),
   ## Root changes
   ### New root
  make_option(c("--root"),
              action="store",
              type="character",
              default = NULL,
              help="Select a node/OTU as new root.\n\t\t\tNumber of the new node/OTU has to be indicated. It can be checked with '--noedit'.\n\t\t\tE.g.: --root 9"
  ),
   ### Root position
  make_option(c("--position"),
              action="store",
              type="character",
              default = NULL,
              help="Select position for new root.\n\t\t\tPosition along the target edge at which to re-root the tree.\n\t\t\tE.g.: --position 0.01"
  ),
   ### Unroot tree
  make_option(c("--unroot"),
              action="store_true",
              default=FALSE,
              help="Unroot a rooted tree."
  ),

   ## Other structural modifications
   ### Rotate nodes ## WARNING! Incompatible with --ladderize option
  make_option(c("-r", "--rotate"),
              action="store",
              type="character",
              default = NULL,
              help="Rotate selected node. For more than one rotation, insert positions separated by ','. \n\t\t\tWARNING! Incompatible with '--ladderize'.\n\t\t\tE.g.: --rotate 17,42,3 or -r 17,42,3\n\t\tFor rotating all nodes, insert: --rotate all or -r all."
  ),

   ### Add superindexed 'T' after strain names:
  make_option(c("-i", "--superindex"),
              action="store_true",
              default=FALSE,
              help="A superindexed 'T' appears behind the strain name.\n\t\tThis 'T' must NOT be added as part of the 'strain' column in the data file.\n\t\tBy default, FALSE."
  ),

#   ### Change layout.
  make_option(c("--layout"),
              action="store",
              type="character",
              default = "rectangular",
              help="Layout. \n\t\tOptions: rectangular (by default) | slanted | fan | circular | radial | unrooted | equal_angle | daylight. "
  ),

   ### Ladderizing. Reorganize structure with a ladderized effect.
  make_option(c("-l", "--ladderize"),
              action="store_true",
              default = FALSE,
              help="Tree plot presents an ladderized effect.\n\t\tBy default, FALSE."
  ),

   ## Bootstrap values:
   ### Change scale
  make_option(c("--bootstrap_percentage"),
              action="store",
              default=NULL,
              help="Replace the original scale with %.\n\t\tThe original percentage must be indicated: 1 | 1000."
  ),
   ### Bootstrap threshold: If 'bootstrap_percentage' used, this value has to be in a 0-100 range.
  make_option(c("-b", "--bootstrap_threshold"),
              action="store",
              type="double",
              default = 70,
              help="Threshold for bootstrap values. By default, 70 (%).\n\t\tWarning! Be aware of the scale of the tree file."
  ),
   ## Bar width:
  make_option(c("--bar"),
              action="store",
              type="double",
              default = NULL,
              help="Bar width. By default, automatic."
  ),
   ## Collapse:
  make_option(c("-c", "--collapse"),
              action="store",
              type="character",
              default = NULL,
              help="Collapse selected nodes.\n\t\t'node' and 'mode' must be inserted, separated by ','. \n\t\t\t'mode' options: 'none', 'max', 'min', 'mixed'.\n\t\t\tE.g.: --collapse 7,min or -c 7,min\n\t\tTo collapse more than one node, separated by ':'.\n\t\t\tE.g.: --collapse 7,min:5,max\n\t\tThis option could be used along 'scale' to modify the size.\n\t\t\tE.g.: --collapse 7,min --scale 0.5,7"
  ),
  make_option(c("--hide_taxa_number"),
              action="store_false",
              default = FALSE,
              help="Hide number of leaves within collapsed node.\n\t\tBy default, FALSE"
  ),
   ## Scale:
  make_option(c("--scale"),
              action="store",
              type="character",
              default = NULL,
              help="Y axis only. Could be applied to individual nodes or the whole tree. \n\t\tSelect size (being 1 original size), and node, separated by ','. For the whole tree, select node 1. Multiples nodes separated by '_'. \n\t\t\tE.g.: --scale 1.5,1_0.4,5, which means: bigger size (x1.5) for the whole tree (node 1 selected) and smaller size (x0.4) for node 5."
  ),
  make_option(c("--scalexy"),
              action="store",
              type="character",
              default = NULL,
              help="X and Y axes. Same uses as '--scale'."
  ),
   ### Highlight:
   ## Color OTUs and clades:
  make_option(c("--Ctip"),
              action="store",
              type="character",
              default = NULL,
              help="Color branch and name of the selected OTU(s).(It could be checked with a previous '--noedit').\n\t\t\tE.g.: --Ctip G_trop\n\t\tMultiple values separated by ','.\n\t\tCannot be used at the same time than '--Cclades'."
  ),
  make_option(c("--Cclades"),
              action="store",
              type="character",
              default = NULL,
              help="Color branch and name of the selected clade(s).\n\t\t\tE.g.: --Cclade 8\n\t\tMultiple values separated by ','.\n\t\t\tE.g.: --Cclade 8,16\n\t\tCannot be used at the same time than '--Ctip'."
  ),
  make_option(c("--color"),
              action="store",
              type="character",
              default = NULL,
              help="Choose color for '--Ctip' and '--Cclades'."
  ),
 ## Frame:
  make_option(c("--Hclades"),
              action="store",
              type="character",
              default = NULL,
              help="Highlight selected clade(s). Add color separated by ','. Add extension separated by ','.\n\t\tIf the extension is too high, nothing appears.\n\t\t\tE.g.: --Hclades 9,red,0.0015\n\t\tMultiples values separated by ':'. \n\t\t\tE.g.: --Hclades 9,red,0.0015:20,green,0.0020"
  ),
  ## Labels
  # Label selected clade:
  make_option(c("--Bclades"),
              action="store",
              type="character",
              default = NULL,
              help="Add labels. Select node and add label, separated by ','. \n\t\t\tE.g.: --Bclades 9,Group1\n\t\tMultiples values separated by ':'. \n\t\t\tE.g.: --Bclades 9,Group1:20,Group2\n\t\tMultiples labels are aligned.\n\t\tOption '--offset' positions it/them.\n\t\tCould be used to label collapsed nodes by '--collapse'.\n\t\t\tE.g.: --collapse 7,min --Bclades 7,Group1"
  ),
  make_option(c("--offset"),
              action="store",
              type="double",
              default = 0,
              help="Position --Bclades. By default, '0.1'. \n\t\tThe higher the value, the further away from the tree."
  ),
  ### Sizes and scales
#   ## Fontsize:
  make_option(c("--fontsize"),
              action="store",
              type="double",
              default = 6,
              help="Font size. By default, 6.\n\t\tFor big trees, it is recommended to ajust it to lower values. E.g.: --fontsize 4"
  ),
  make_option(c("--nodesize"),
              action="store",
              type="double",
              default = 5,
              help="Bootstrap values font size. By default, 5."
  ),
   ## Tree size:
  make_option(c("-s", "--size"),
              action="store",
              type="double",
              default = 1,
              help="Adapt tree size. Higher values squeeze the tree (X-axis). By default, 1. \n\t\tFor short text label, '-s 0.5' is also recommended. \n\t\tTo remove this option, set it to NA\n\t\tIt's recommended to use bigger sizes for circular layouts."
  ),
  make_option(c("--xlim"),
            action="store",
            type="double",
            default = NULL,
            help="Recommended in circular and radial layout.\n\t\tAdapt tree size. Higher values squeeze the tree (X-axis). By default, 1. \n\t\tFor short text label, '-s 0.5' is also recommended. \n\t\tTo remove this option, set it to NA\n\t\tIt's recommended to use bigger sizes for circular layouts."
),
#
#   ## Bootstrap position on X and Y axes:
  make_option(c("-x", "--bootstrap_X"),
              action="store",
              type="double",
              default = 0,
              help="Position on X axis for bootstrap values."
  ),
  make_option(c("-y", "--bootstrap_Y"),
              action="store",
              type="double",
              default = 0,
              help="Position on Y axis for bootstrap values."
  ),
#   ## Output size:
  make_option(c("-W", "--width"),
              action="store",
              type="double",
              default = 29.7,
              help="Output file width (cm). By default, A4 width (29.7 cm).\n\t\tBigger trees need bigger sheet size."
  ),
  make_option(c("-H", "--height"),
              action="store",
              type="double",
              default = 21,
              help="Output file height (cm). By default, A4 height (21 cm).\n\t\tBigger trees need bigger sheet size."
  ),
# ### Output format.
#   ## Output format selection:
#     # If not specified, generate a file by default.
  make_option(c("-o", "--output"),
              action="store",
              type="character",
              default = NULL,
              help="Output name and format (pdf | jpg | png). \n\t\tBy default, gitanaPlot.pdf."
              )
)

opt_parser = OptionParser(option_list=option_list, add_help_option=TRUE,
                          usage = "%prog [--help] --tree TREE
                          [--file FILE] [--nexus] [--Robject] [--noedit] [--isna]
                          [--root ROOT] [--position POSITION] [--unroot] [--rotate ROTATE]
                          [--superindex] [--layout LAYOUT] [--ladderize]
                          [--bootstrap_percentage BOOTSTRAP_PERCENTAGE] [--bootstrap_threshold BOOTSTRAP_THRESHOLD] [--bar BAR]
                          [--collapse COLLAPSE] [--hide_taxa_number] [--scale SCALE] [--scalexy SCALEXY]
                          [--Ctip CTIP] [--Cclades CCLADES] [--color COLOR] [--Hclades HCLADES] [--Bclades BCLADES] [--offset OFFSET]
                          [--fontsize FONTSIZE] [--nodesize NODESIZE] [--size SIZE] [--xlim XLIM] [--bootstrap_X BOOTSTRAP_X] [--bootstrap_Y BOOTSTRAP_Y]
                          [--width WIDTH] [--height HEIGHT] [--output OUTPUT]",
                          prog = "gitana.R",
                          description = "\nCreates ready-to-publish figures from raw phylogenetic trees.", 
                          epilogue = "Thank you for using 'gitana'!\nCheck our manual for examples: github.com/cristinagalisteo/gitana")
opt = parse_args(opt_parser)

### ERRORS
## This tool needs, at least, one tree to work. Therefore, the following error calls out if none inserted. 
## The other options are optional, so any errors are generated. 
if (is.null(opt$tree)){
  stop("\033[0;31m At least one tree file must be selected ('--tree' or '-t').\033[0m
       gitana.R --help", call.=FALSE)
}
if (!is.null(opt$Cclades) && !is.null(opt$Ctip)){
  stop("\033[0;31m '--Cclades' and '--Ctip' cannot be used at the same time.\033[0m", 
       call.=FALSE)
}
if (!is.null(opt$rotate) && (opt$ladderize == T)){
  stop("\033[0;31m '--rotate' and '--ladderize' are incompatible.\033[0m", 
       call.=FALSE)
}

############### FUNCTIONS ###############
###### FILE READ  
get_df <- function(tree, data_file) {
  ### Return a dataframe with taxa data with format ready for plotting.
  ## It needs a base tree and a taxa data file. 
  ## 'data_file' input must follow an specific format: 
  ## It must have 4 columns, separated by TABULATOR, and WITHOUT header:
  ##	First column. Later renamed as 'ilab'. (This tool was thought for working with results from ARB software). 
  ##			OTUs as named on tree file. It's the name that would appear in any other tree viewers. 
  ##	Second column. Later renamed as 'acc'.
  ##			Accession numbers of sequences 
  ##			This tool places them in brackets. 
  ##	Third column. Later renamed as 'gen_sp'. 
  ##			Scientist name. Blank spaces can be added. 
  ##			Will be shown in ITALICS.
  ##	Fourth column. Later renamed as 'strain'. 
  ##			Strain name. Blank spaces can be added. If working with type strain, DO NOT add superindexed T. It will be added automatically when opt$superindex option = TRUE. 
  ## From this data, essential for publishing figures, a new column will be added to the dataframe. This new column will be read by 'ggtree' plot functions. 
  ##	Fifth column. Later renamed as 'type'.
  ##      "type" is the strain is a type strain.
  
  ### File read:
  # No header. Columns separated by tabulator. 
  csv <- read.table(file = data_file, header = F, sep = "\t")
  colnames(csv) <- c("ilab", "acc","gen_sp", "strain") # Creates a new header for the dataframe. 
  if (ncol(csv) >= 5){colnames(csv)[5] <- "type"}
  ### Clean strains names:
  # This tool was thought for working with ARB software output. Generally, more characters than necessary are present. Therefore, characters that don't belong to the strain name must be erased. 
  csv$strain[grepl("^type strain: ", csv$strain)] <- 
    gsub("^type strain: ", "", csv$strain[grepl("^type strain: ", csv$strain)])
  csv$strain[grepl("\\[T]", csv$strain)] <- 
    substr(csv$strain[grepl("\\[T]", csv$strain)], start = 1, stop = (regexpr("\\[T]", csv$strain[grepl("\\[T]", csv$strain)])-3))
  csv$strain[grepl("\\, |; |/ ", csv$strain)] <- 
    substr(csv$strain[grepl("\\, |; |/ ", csv$strain)],  start = 1 , stop = (regexpr("\\, |; |/ ", csv$strain[grepl("\\, |; |/ ", csv$strain)])-1))

  rownames(csv) <- csv$ilab
  # Order csv with the same order than tip.label 
  csv <- csv[tree$tip.label, ]
  csv$flab <- rep(NA, nrow(csv))
  
  # Add '()' to accession numbers:
  csv$acc[csv$acc != ""] <- sprintf("(%s)",
                                    csv$acc[csv$acc != ""])
  
  # Add italics to 'gen_sp' column:
  csv$gen_sp[csv$gen_sp != ""] <- sprintf("*%s*",
                                          csv$gen_sp[csv$gen_sp != ""])
  
  # Add "NA" to missing information:
  if(opt$isna){csv[(csv == "")] <- "**NA**"}
  
  # Add 'T' for type strains:
  if (opt$superindex){ 
    csv$strain[csv$type == "type"] <- sprintf("%s<sup>T</sup>",
                                              csv$strain[csv$type == "type"])
  }
  
  # Combine info in one sentence:
  csv$flab <- sprintf("%s %s %s", 
                      csv$gen_sp, 
                      csv$strain, 
                      csv$acc) 
  
  # Remove extra blank spaces:
  csv$flab <- gsub("  ", " ", csv$flab)
  
  ### Return
  return(csv)
}


get_df_circular <- function(tree, data_file) {
  ### File read:
  # No header. Columns separated by tabulator. 
  csv <- read.table(file = data_file, header = F, sep = "\t")
  colnames(csv) <- c("ilab", "acc","gen_sp", "strain") # Creates a new header for the dataframe. 
  if (ncol(csv) >= 5){colnames(csv)[5] <- "type"}
  ### Clean strains names:
  # This tool was thought for working with ARB software output. Generally, more characters than necessary are present. Therefore, characters that don't belong to the strain name must be erased. 
  csv$strain[grepl("^type strain: ", csv$strain)] <- 
    gsub("^type strain: ", "", csv$strain[grepl("^type strain: ", csv$strain)])
  csv$strain[grepl("\\[T]", csv$strain)] <- 
    substr(csv$strain[grepl("\\[T]", csv$strain)], start = 1, stop = (regexpr("\\[T]", csv$strain[grepl("\\[T]", csv$strain)])-3))
  csv$strain[grepl("\\, |; |/ ", csv$strain)] <- 
    substr(csv$strain[grepl("\\, |; |/ ", csv$strain)],  start = 1 , stop = (regexpr("\\, |; |/ ", csv$strain[grepl("\\, |; |/ ", csv$strain)])-1))
  
  rownames(csv) <- csv$ilab
  # Order csv with the same order than tip.label 
  csv <- csv[tree$tip.label, ]
  csv$flab <- rep(NA, nrow(csv))
  
  # Add '()' to accession numbers:
  csv$acc[csv$acc != ""] <- sprintf("(%s)",
                                    csv$acc[csv$acc != ""])
  
  # Add italics to 'gen_sp' column:
  csv$gen_sp[csv$gen_sp != ""] <- sprintf("italic('%s')~",
                                          csv$gen_sp[csv$gen_sp != ""])
  
  # Add "NA" to missing information:
  if(opt$isna){csv[(csv == "")] <- "bold('NA')~"}
  
  # Add 'T' for type strains:
  if (opt$superindex){ 
    csv$strain[csv$type == "type"] <- sprintf("%s'^'T'*'",
                                              csv$strain[csv$type == "type"], 
                                              csv$acc[csv$type == "type"])
  }
  
  # Combine info in one sentence:
  # csv$flab <- sprintf("%s %s%s", 
  #                     csv$gen_sp, 
  #                     csv$strain, 
  #                     csv$acc) 
  # 
  csv$flab <- sprintf("%s '%s ' %s",
                      csv$gen_sp, 
                      csv$strain,
                      csv$acc)
  
  # Remove extra blank spaces:
  csv$flab <- gsub("  ", " ", csv$flab)
  
  ### Return
  return(csv)
}



###### SHARED NODES
shared_nodes <- function(mylistoftrees) {
  tree <- mylistoftrees[[1]]
  nodos_todos <- tree$node.label 
  
  # Compare base tree (tree) with the other two, separately, using 'matchNodes' function from 'phytools' library. 
  mtt <- list()
  mtt[[1]] <- nodos_todos
  for (n in 2:length(mylistoftrees)) {
    mtt[[n]] <- phytools::matchNodes(tree, mylistoftrees[[n]], method = "descendants")
  }
  
  # Save matching positions: 
  posiciones <- list()
  for (n in 2:length(mylistoftrees)) {
    mtt2 <- mtt[[n]]
    posiciones_t2 <- c()
    for (i in 1:length(mtt2[,2])){		# Parse it. Column two is the one with tree 2 data. 
      if (!is.na(mtt2[i,2])) {			  # If the value is not NA, it will be saved on a new vector ('posiciones_t2').
        posiciones_t2 <- c(posiciones_t2, i)
      }
    }
    posiciones[[n-1]] <- posiciones_t2
  }
  
  posicion_nodos <- posiciones[[1]] # Tree2
  if (length(mylistoftrees) >= 2){ # If there are 2 or more trees:
    for (n in 2:length(posiciones)){
      posicion_nodos <- intersect(posicion_nodos, posiciones[[n]])
    }
  }
  
  posicion_nodos <- posicion_nodos + 1 # The right position is the following one. That is why plus 1 is added. 

  # To avoid non-existant position in 'posicion_nodos' (which happens when the last node is present on it, because of the plus 1): 
  if (posicion_nodos[length(posicion_nodos)] == length(tree$node.label)+1) {
    posicion_nodos[length(posicion_nodos)] <- 1 # Last node is actually the first one. 
  }
  posicion_nodos <- sort(posicion_nodos)
  
  # A new vector is created to stock all the shared position values that must be marked on the plot. It has the same length than the original one. 
  # Non shared positions have 'NA' value. 
  # Shared positions are replaced with the value from 'nodos_nodos'. 
  nodos_punto <- rep(NA,length(tree$node.label))
  for (r in posicion_nodos) { 	
    nodos_punto[r] <- nodos_todos[r]
  }
  nodos_punto[2] <- NA		# Avoid a black point in the very first node (LUCA). 
  
  ### Return
  return(nodos_punto)
}

###### BOOTSTRAP
bootstrap <- function(tree, value) {
### Return the base tree with 'node.label' values equal or over the selected threshold ('--bootstrap_threshold'). 
## Most phylogenetic trees only represent bootstrap values over a chosen percentage. 
## Therefore, it is interesting to have a filter function.  
## This function needs a tree and a threshold. 
  # Transforms "tree$node.label" into a numeric vector. 
  tree$node.label <- as.numeric(tree$node.label)
  # Values above the threshold are reclave with NA.
  tree$node.label[tree$node.label < value] <- NA
  # Add extra blank space to values with two digits. They will be easier to place over the node.
  tree$node.label <- as.character(tree$node.label)
  tree$node.label[which(tree$node.label != 100)] <- sprintf(" %s", tree$node.label[which(tree$node.label != 100)])
### Return 
  return(tree)
}

###### SAVING 
saving <- function(saved_file, 
                   tr, width, height) {
### Save the plot.  
## By default, it will be saved as 'yourTree.pdf'
## This function needs a plot, an output (with format), and an output size (width, height, cm). 
  ggsave(saved_file, tr, width = width, height = height, units = "cm", limitsize = FALSE, dpi = 400)
  
  # Print info #
  cat("...Output file: ", saved_file, "\n", sep = "")
  cat("..Your ", saved_file, " is ready! Are your species labels within the sheet limits? Try setting the --size/-s (for 'rectangular' and 'slanted' layouts) or --xlim (for 'circular' and 'radial' layouts) ;)\n", sep = "")
  quit()
}

###### PRINT NODE POSITION
# Check tree and its node positions:
check_node_position <- function(mytree, output){
  ### Save a plot based only on '--treeOne' in which bootstrap values are replaced by node position. 
  ## The function needs just a tree to work.  
  ## Later tree modifications may need a node position to work into. This option offers the user that information. 
  
  ## Plot with "ape" library:
  pdf(output,
      width = opt$width, height = opt$height)
  plot(mytree)
  nodelabels()
  tiplabels()
  dev.off()
}

###### SPLIT ARGUMENTS
divide <- function(chain, sep){
  ### Convert a character chain into different vectors. 
  ## Separator must be specified.
  values <- unlist(strsplit(as.character(chain), sep))
  
### Return:
  return(values)
}

###### REROOT
myreroot <- function(t, mynode, myposition){
  # Reroot tree based on phytools library. 
  # By default, the position of the branch is the original_length/10. 
  t <- phytools::reroot(tree=t, node.number = mynode, position = myposition)
  # Save as new tree:
  write.tree(t, 
             file = paste0("rerooted_", 
                           "_node", mynode, "_", myposition, ".tree"), 
             tree.names = T)
  # Print info #
  cat("..'rerooted_", 
      "_node", mynode, "_", myposition, ".tree' is ready!\n", sep = "")
  
  # Creates new pdf to check the result:
  check_node_position(t, paste0("rerooted_", 
                               "_node", as.numeric(opt$root), "_", myposition, ".pdf"))
  
  cat("..'rerooted_", 
      "_node", mynode, "_", myposition, ".pdf' is ready!\n", sep = "")
  return(t)
}

############### WORKING ###############

## Save command into a log file
optdf <- as.data.frame((opt))
optdf <- optdf[, optdf!=F]
mycommand <- paste0("gitana.R ", paste0("--", colnames(optdf), " '", optdf[1,], "'", sep = "", collapse = " "))
cat(mycommand, file = "gitana.log")

##### READ TREE #####
## Read file with the path and/or name of the trees: 
mytrees <- read.table(opt$tree, header = F, sep = "\t", quote = "")[, 1]
cat("..Total of input trees: ", length(mytrees), ".\n", sep = "")
cat("..", mytrees[1], " will set the tree topology\n", sep ="") # 'mytrees[1]' sets the topology.  

##### TREE ONE #####
## Tree one, or base tree, read: 
treelist <- list()
if (opt$nexus){
  treelist[[1]] <- read.nexus(mytrees[1])
} else {
  treelist[[1]] <- read.tree(mytrees[1])
} 
tree <- treelist[[1]] # 'phylo' object

# Print info #
cat("...Input tree 1: ", mytrees[1], "\n", sep = "")
nodes <- tree$node.label # Save node values in a different vector for later. 

# If '--noedit' is called, it is executed and the script shuts down. 
if (opt$noedit) {
  check_node_position(tree, "noedit.pdf")
  # Print info #
  cat("..noedit.pdf is ready!\n", sep="")
  quit()
}

#### Root #### 
## New root:
if (!is.null(opt$root)) {
  if (!is.null(opt$position)) {
    tree <- myreroot(t = tree, mynode = as.numeric(opt$root), myposition = as.numeric(opt$position))
  } else {
    tree <- myreroot(t = tree, mynode = as.numeric(opt$root), 
                     myposition = (tree$edge.length[which(tree$edge[,2] == as.numeric(opt$root))]/2)) # Select the branch distance and divide by 2 
  }

  quit()
}
## Unroot:
if (opt$unroot) {
  tree <- ape::unroot(tree)
  write.tree(tree, file = paste0("unrooted_", 
                                 gsub("\\.tree|\\.tr|\\.tre", "", mytrees[1]), 
                                 ".tree"))
  # Print info #
  cat("..'unrooted_", gsub("\\.tree|\\.tr|\\.tre", "", mytrees[1]), ".tree' is ready!\n", sep="")
  quit()
}


##### EXTRA TREE #####
if (length(mytrees) >= 2){
  for (n in 2:length(mytrees)){
    if (opt$nexus){
      treelist[[n]] <- read.nexus(mytrees[n])
    } else {
      treelist[[n]] <- read.tree(mytrees[n])
    }
    
    cat("...Input tree ", n, ": ", mytrees[n], "\n", sep = "")
  }
}


##### DATA FILE #####
# Read file with taxa info.
# Optional. If not added, tip labels from 'phylo' object are used into the plot. 
# print((!is.null(opt$file) & (opt$layout %in% c("circular", "radial"))
if (!is.null(opt$file) && (opt$layout %in% c("circular", "radial") )) {
  csv <- get_df_circular(tree, opt$file) 	# The function gets the right format done for plotting.
  # Print info #
  cat("...Input file: ", opt$file, "\n", sep ="")
  if (opt$isna) {cat("......Missing information is indicated by 'NA'\n")}
}

if (!is.null(opt$file) & (opt$layout %in% c("rectangular", "slanted") )) {
  csv <- get_df(tree, opt$file) 	# The function gets the right format done for plotting.
  # Print info #
  cat("...Input file: ", opt$file, "\n", sep ="")
  if (opt$isna) {cat("......Missing information is indicated by 'NA'\n")}
}

##### SHARED NODES #####
# Checks shared nodes between input trees.  
# For two or more trees.
if (length(mytrees) >= 2){
  nodos_punto <- shared_nodes(treelist)
}

##### BOOTSTRAP ##### 
# Some trees add text in their node labels (eg.: GTDB-tk trees)
# Firstly, we remove the text. It is separated by ":" from the bootstrap value.
tree$node.label[grep(":", tree$node.label)] <- substr(tree$node.label[grep(":", tree$node.label)], 2, regexpr(":", tree$node.label)-1)

# Bootstrap values are turned into percentage. 
if (!is.null(opt$bootstrap_percentage)){
  if (opt$bootstrap_percentage == 1000){
    if (1 %in% as.numeric(tree$node.label)) { # '"1.000"' (character) could be converted into '1' with the 'as.numeric()'
      tree$node.label <- as.character(as.numeric(tree$node.label)*100)
      } else {
      tree$node.label <- as.character(as.numeric(tree$node.label)/10)
      }
  }
  if (opt$bootstrap_percentage == 1){
    tree$node.label <- as.character(as.numeric(tree$node.label)*100)
  }
}

# By default, the threshold set is 70 %. This function filters by the selected threshold ('--bootstrap_threshold')
tree <- bootstrap(tree, value = opt$bootstrap_threshold)
# Print info #
cat("..Bootstraps <", opt$bootstrap_threshold, " filtered out\n", sep ="")

##### ROTATE #####
# 'rotateNodes' function from 'phytools' library rotates tree nodes. 
# 'opt$ladderize' must be FALSE. Topology cannot be automatically and manually modified at the same time. 
if (!is.null(opt$rotate)) {
  opt$ladderize = FALSE 		# To be sure. 
  div <- divide(opt$rotate, ",")
  for (i in div) {
    if (i == "all") {		 	  # 'all' option rotates all the nodes.  
      tree <- rotateNodes(tree, "all")
    } else{
      tree <- rotateNodes(tree, i)
      }
    }
  }


########## TREE PLOT ##########
# Tree plotting uses 'ggtree' package, based on 'ggplot2' library. 
# Its main feature is the use of layers to create very complex plot from very basic ones. 

## First, we check the options that modify our phylo object ('tree'): "--Cclades", "--Ctip".
### --Cclades
if (!is.null(opt$Cclades)){
  # 'ggplot2' is loaded here to avoid incompatibility problems, as it is not used on most options. 
  library("ggplot2")
  # Node number from 'opt$Cclades': 
  clades <- as.numeric(divide(opt$Cclades, ","))
  # Color selection:
  color <- divide(opt$color, ",")
  # Modification of 'phylo' object, creating a subgroup.
  tree <- groupClade(tree, clades)
}

### --Ctip (same as --Cclades)
if (!is.null(opt$Ctip)){
  library("ggplot2")	
  otu <- as.numeric(divide(opt$Ctip, ","))
  color <- divide(opt$color, ",")
  tree <- groupOTU(tree, otu)
}


### Plot base tree:
# 'layout' for tree structure: rectangular (by default) | slanted | fan | circular | radial | equal_angle | daylight.
# 'ladderize' for stairs-like distribution (TRUE). 
if (!is.null(opt$Cclades) | !is.null(opt$Ctip)){
  tr <- ggtree(tree, aes(color=group), layout = opt$layout, ladderize = opt$ladderize) + 
    scale_colour_manual(values = c("black", color)) + theme(legend.position= "none")	
} else {
  tr <- ggtree(tree, layout = opt$layout, ladderize = opt$ladderize)
}

### Tip labels:
## Add edited labels from the file inserted as an argument
if (!is.null(opt$file)){
    if(opt$layout %in% c("circular", "radial")){tr <- tr %<+% csv + geom_tiplab(aes(label=flab), parse=T, size = opt$fontsize)}
    if(opt$layout %in% c("rectangular", "slanted")){tr <- tr %<+% csv + ggtext::geom_richtext(aes(label=flab), size = opt$fontsize,fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"), hjust = -0.01)}
} else { 
  ## Or just print the labels from the .tree
  tr <- tr + geom_tiplab(size = opt$fontsize)
}

### Shared nodes: 
## Insert layer with previous obtained shared nodes. Only for working with more than one tree file. 
## Shared nodes were saved on 'nodos_puntos'. Original nodes were saved on 'nodes'.
if (length(treelist) >= 2) {tr <- tr + geom_nodepoint(aes(subset=(nodos_punto %in% nodes)))}

### Bar scale: 
## Add bar scale to the bottom left:
if (opt$layout %in% c("circular", "radial")) {tr <- tr + geom_treescale(width = opt$bar, fontsize = opt$nodesize)}
if (opt$layout %in% c("rectangular", "slanted")) {tr <- tr + geom_treescale(x = 0, y= -1, width = opt$bar, fontsize = opt$nodesize)}

### Bootstrap: 
## Show previously filtered bootstrap values: 
# 'nudge_x' and 'nudge_y' for positioning the value over the branch. '0' sets them over the node. 
# For not showing bootstrap, threshold value should be higher than the higher present value.
tr <- tr + geom_nodelab(nudge_x = opt$bootstrap_X, nudge_y = opt$bootstrap_Y,  size = opt$nodesize)

### Tree size:
## Each tree has different size according to its taxa and the relationship between them. It is necessary to adjust tree size on X axis. 
if (!is.null(opt$size)) {tr <- tr + hexpand(as.numeric(opt$size))}
if (!is.null(opt$xlim)) {tr <- tr + xlim_tree(as.numeric(opt$xlim))}

### Scale
## This function also adjust tree size using 'scaleClade' function. 
## Useful for adjusting distance between taxa, or making a collapsed node smaller.
## This function works directly over the plot object ('tr'). 
# 'opt$scale' argument includes proportion (> 1, bigger; <1, smaller), and node position, separated by ','.
# Can be used more than once, separating values by ';'. 
# To apply to the complete tree, select root node.
if (!is.null(opt$scale) | !is.null(opt$scalexy)){
  # Save info:
  if (!is.null(opt$scale)){
    values <- opt$scale
    vertical <- TRUE
  } 
  if (!is.null(opt$scalexy)) {
    values <- opt$scalexy
    vertical <- FALSE
  }
  # Edit tree figure. Cannot edit all nodes at the same time.
  div <- divide(values, ":") 	# Split when more than one application. 
  for (i in div) {
    div2 <- divide(i, ",")		# Split size and node.
    if (div2[1] == "all") {div2[1] <- tree$edge[1,1]} # Select the root node
    tr <- scaleClade(tr, node = as.numeric(div2[1]), scale = as.numeric(div2[2]), vertical_only = vertical) 
  }
}

### Highlight: 
## Highlight clades inside a colored rectangle. The color is selected by the user, separated by ','. 
## Can be used more than once, separating values by ':'. 
if (!is.null(opt$Hclades)) {
  div <- divide(opt$Hclades, ":") 	# Split when more than one application. 
  for (i in div) {
    div2 <- divide(i, ",") 		# Split node, color and extension.
    tr <- tr + geom_hilight(node = as.numeric(div2[1]), fill = div2[2], extend = as.numeric(div2[3]))
  }
  tr$layers <- rev(tr$layers) # Reverse layers. That way, the rectangle is in the bottom layer, and the text on top of it. 
  cat("..Clades highlighted! Want a bigger rectangle? Modify the extend.\n")
}

### Annotation: 
## Select a clade and annotate it at the right side.  
# 'opt$offset' argument adjusts the label position. 
# When more than one, they aligns. 
# Can be used more than once, separating values by ':'.
fontface_values <- data.frame(face = c("plain", "bold", "italic", "bold.italic"), 
                         number = c(1, 2, 3, 4))
if (!is.null(opt$Bclades)) {
  div <- divide(opt$Bclades, ":") # Split when more than one application. 
  for (i in div) {
    div2 <- divide(i, ",")
    tr <- tr + geom_cladelabel(node = as.numeric(div2[1]), label = div2[2], color = div2[3], fontface = fontface_values[fontface_values$face == div2[4], "number"], 
                               align =F, barsize = 1, offset = opt$offset, fontsize = opt$fontsize) 
  }
  cat("..Clade labels added! Are they missplaced? Try setting the right distance with '--offset' (eg.: --offset 0.5)\n")
}

### Collapse: 
## Collapse a selected node. 
## This function has different modes: 'none', 'max', 'min', 'mixed'. 
## Its use is recommended along with 'Bclades' (add labels) and 'scale' (modify the size).
## Can be used more than once, separating values by ':'. 
if (!is.null(opt$collapse)) {
  div <- divide(opt$collapse, ":")
  total_collapse_taxa <- 0
  for (i in div) {
    div2 <- divide(i, ",")

    node_col <- as.numeric(div2[1])
    tip_col <- c()
    # Parse 'tree$edge' to check positions:
    n <- 1 
    while (n <= length(node_col)){
      ed <- tree$edge[(tree$edge[,1] == node_col[n]), 2]
      node_col <- c(node_col, 
                    ed[which(ed > tree$Nnode)])
      tip_col <- c(tip_col, 
                   ed[which(ed <= tree$Nnode)])
      n = n+1 
    }
    # Count how many:
    n_collapse_taxa <- length(tip_col)
    # Collapse the node:
    tr <- ggtree::collapse(tr, as.numeric(div2[1]), mode=div2[2], alpha = 0.5)
    # Add number of taxa within the collapsed node:
    if(opt$hide_taxa_number == F){tr <- tr + geom_cladelabel(node= as.numeric(div2[1]), label= sprintf("%s", n_collapse_taxa))}
    total_collapse_taxa <- total_collapse_taxa + n_collapse_taxa
    
    cat("..Node ", as.numeric(div2[1]), " collapsed. ", n_collapse_taxa, " leaves are included within this node.\n", sep = "")
  }
  
  cat("..All selected nodes collapsed. A total of ", total_collapse_taxa , " taxa are not shown. You can label the collapsed nodes with '--Bclades' (eg.: --Bclades 75,Node75 --offset 0.5)\n", sep = "")
}


########## SAVE ##########
### Plot is saved after the script is done. By default, 'yourTree.pdf'.
## Can be saved in different formats such as .jpg or .png. 
## For bigger trees, the output size must be adjusted. 

# R object:
if (opt$Robject) {
  if (length(treelist) >= 2){
    save(tr, nodos_punto, nodes, file = "gitana.rds")
  } else{
    save(tr, file = "gitana.rds")
  }
  cat("...R object saved: gitana.rds\n")
}

# Plot:
## Select sheet size.
## If default values (no width or height as input, modify A4 according to the number of species
if (opt$layout %in% c("circular", "radial")){
  if ((opt$width == 29.7) && (opt$height == 21)){   # If default values
    if (length(tree$tip.label)>=50 && length(tree$tip.label)<80){ # Between 50 and 80 species
      wandh <- c(40, 40)
    } else if (length(tree$tip.label)>=80 && length(tree$tip.label)<100){ # Between 80 and 100 species
      wandh <- c(60, 60)
    } else if (length(tree$tip.label)>=100){ # More than 100 species
      wandh <- c(80, 80)
    } else {
      opt$height <- 29.7
      wandh <- as.numeric(c(opt$width, opt$height))
    }
    cat(sprintf("..Sheet size was auto-adapted: width %s (cm); height %s (cm)\n", wandh[1], wandh[2]))
  } else {
    wandh <- as.numeric(c(opt$width, opt$height))
  }
}

if (opt$layout %in% c("rectangular", "slanted")){
  if ((opt$width == 29.7) && (opt$height == 21)){   # If default values
    if (length(tree$tip.label)>=50 && length(tree$tip.label)<80){ # Between 50 and 80 species
      wandh <- c(29.7, 40)
    } else if (length(tree$tip.label)>=80 && length(tree$tip.label)<100){ # Between 80 and 100 species
      wandh <- c(29.7, 60)
    } else if (length(tree$tip.label)>=100){ # More than 100 species
      wandh <- c(29.7, 80)
    } else {
      wandh <- as.numeric(c(opt$width, opt$height))
    }
    cat(sprintf("..Sheet size was auto-adapted due to the number of taxa: width %s (cm); height %s (cm)\n", wandh[1], wandh[2]))
  } else {
    wandh <- as.numeric(c(opt$width, opt$height))
  }
}

## Save plot: 
if (!is.null(opt$output)){
  saving(saved_file = opt$output, tr, width = wandh[1], height = wandh[2])
} else {
  saving(saved_file =  "gitanaPlot.pdf", 
         tr, width = wandh[1], height = wandh[2])
}
