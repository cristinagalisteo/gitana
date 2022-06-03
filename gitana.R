#!/usr/bin/Rscript --vanilla --silent
# -*-coding: utf-8 -*-

# @autor: Cristina Galisteo Gómez
# @Date: 2021 
# @Version: 2.1


############
# PACKAGES #
############


library(ggtree)
# Package for visualization and annotation of phylogenetic trees, based on 'ggplot2' library. 

library(phytools)
# Carry on the node comparison on phylogenetic trees. 

library(ape)
# Package for tree topology modifications. 

library(optparse)
# Package for command line. 


#############
# ARGUMENTS #
#############

# Arguments list: 
option_list = list(
### External files read: 
  ## Trees:
  make_option(c("-t", "--treeOne"),   # Essential.
              action="store",
              type="character",
              default = NULL,
              help="Base tree file. Final tree plot presents its topology."
              ),
  make_option(c("-e", "--treeTwo"),   # Optional.
              action="store",
              type="character",
              default = NULL,
              help="Tree 2 file."
              ),
  make_option(c("-n", "--treeThree"), # Optional.
              action="store",
              type="character",
              default = NULL,
              help="Tree 3 file."
              ),
        
  ## Taxa data file:    # Optional. Essential to obtain correct format for. 
  make_option(c("-f", "--file"),
              action="store",
              type="character",
              default = NULL,
              help="Data file with information about the tree taxa. \n\t\tIt must present 4 columns, separated by TABULATION, as specified below: \n\t\t'name', 'accession', 'complete_name' 'strain', where: \n\t\t\t'name' is the name of the taxa as shown on tree file 1. \n\t\t\t'accession' is the accession number of the sequences. \n\t\t\t'complete_name' is the scientist name. It'll be shown in italic. It can have blank spaces. \n\t\t\t'strain' is the name of the strain."
              ),


### Output format. 
  ## Output format selection:
    # If not specified, generate a file by default. 
  make_option(c("-o", "--output_format"),
              action="store",
              type="character",
              default = "yourTree.pdf",
              help="Output name and format (pdf | jpg | png). \n\t\tBy default, 'yourTree.pdf'."
              ),

### Quick visualization 
  ## Quick print:
    # Plot quick print, taking in consideration only the base tree ('--TreeOne'). 
    # Only argument '--ladderize' is taken into account.  
  make_option(c("-q", "--quick_print"),
              action="store_true",
              default=FALSE,
              help="Print a basic version of the tree plot. It shows raw names. Besides, bootstrap values are replaced by node position (from 1 to n). \n\t\tThis option creates a template from where to choose nodes and OTUs for other modification options.\n\t\tIt can be conjugated with '--ladderize'."
              ),


### Features: 
  ## Superindexed 'T' after strain names: 
    # When working with type strain, this option add a superindexed 'T'.  
  make_option(c("-i", "--superindex"),
              action="store_true",
              default=FALSE,
              help="A superindexed 'T' appears behind the strain name. \n\t\tThis 'T' must NOT be added as part of the 'strain' column in the data file.\n\t\tBy default, FALSE."
              ),
  ## Tree structure: 
    # Select layout.
  make_option(c("--layout"),
              action="store",
              type="character",
              default = "rectangular",
              help="Layout. \n\t\tOptions: rectangular (by default) | slanted fan | circular | radial | unrooted | equal_angle | daylight. "
              ), 
  ## Ladderizing: 
    # Reorganize structure with a ladderized effect. 
  make_option(c("-l", "--ladderize"),
              action="store_true",
              default = FALSE,
              help="Tree plot presents an ladderized effect. \n\t\tBy default, FALSE."
              ),
  ## Tip labels position:
  make_option(c("-a", "--align"),
              action="store_true",
              default = FALSE,
              help="Tips labels are aligned at the right. By default, tip labels are shown at the end of the branch."
              ),               
  ## Bootstrap values:
    # Scale change:
  make_option(c("--bootstrap_percentage"),
              action="store",
              default=NULL,
              help="Replace the original scale for % 100. \n\t\t The original percentage must be indicated: 1 | 1000."
              ),  
   # Bootstrap threshold: If 'bootstrap_percentage' used, this value has to be in a 0-100 range.
  make_option(c("-b", "--bootstrap_threshold"),
              action="store",
              type="double",
              default = 70,
              help="Threshold for bootstrap values. By default, 70. \n\t\tWarning! Be aware of the scale of the tree file."
              ),


### Structure modifications:
  ## Root changes:  
    # Node as new root.
  make_option(c("--root_node"),
              action="store",
              type="character",
              default = NULL,
              help="Select a node as new root. Position of the node must be inserted.\n\t\t\tE.g.: --root_node 9"
              ),
    # OTU as new root.
  make_option(c("--root_OTU"),
              action="store",
              type="character",
              default = NULL,
              help="Select an OTU as new root. Raw name of the OTU must be inserted. (It can be checked with '--quick_print').\n\t\t\tE.g.: --root_OTU G_trop"
              ),
    # Unroot tree. 
  make_option(c("--unroot"),
              action="store_true",
              default=FALSE,
              help="Unroot the tree."
              ),
                            
  ## Rotation: 
  make_option(c("-r", "--rotate"),
              action="store",
              type="character",
              default = NULL,
              help="Rotate selected node. For more than one rotation, insert positions separated by ','. \n\t\t\tE.g.: --rotate 17,42,3 or -r 17,42,3 \n\t\tFor rotating all nodes, insert: --rotate all or -r all."
              ),
              
  ## Collapse:
  make_option(c("-c", "--collapse"),
              action="store",
              type="character",
              default = NULL,
              help="Collapse selected nodes. \n\t\t'node' and 'mode' must be inserted, separated by ','. \n\t\t\t'mode' options: 'none', 'max', 'min', 'mixed'. \n\t\t\tE.g.: --collapse 7,min or -c 7,min\n\t\tTo collapse more than one node, separated by '_'.\n\t\t\tE.g.: --collapse 7,min_5,max\n\t\tThis option could be used along 'scale' to modify the size.\n\t\t\tE.g.: --collapse 7,min --scale 0.5,7"
              ),
              
  ## Drop:
   make_option(c("-d", "--drop"), 
              action="store",
              type="character",
              default = NULL,
              help="Drop selected OTU. Insert raw name. (It can be checked with '--quick_print').  \n\t\tTo drop more than one OTU, seaparated by ','. \n\t\t\tE.g.: --drop OTU1,OTU2,OTU7 or -d OTU1,OTU2,OTU7"
              ),


### Highlight:
  ## Color OTUs and clades: 
  make_option(c("--Cotu"),
              action="store",
              type="character",
              default = NULL,
              help="Color branch and name of the selected OTU(s). \n\t\t(It could be checked with a previous 'quick_print').\n\t\t\tE.g.: --Cotu G_trop\n\t\tMultiple values separated by ','. \n\t\tCannot be used at the same time than '--Cclades'."  
              ),           
  make_option(c("--Cclades"),
              action="store",
              type="character",
              default = NULL,
              help="Color branch and name of the selected clade(s). \n\t\t\tE.g.: --Cclade 8\n\t\tMultiple values separated by ','. \n\t\t\tE.g.: --Cclade 8,16\n\t\tCannot be used at the same time than '--Cotu'."  
              ),
  make_option(c("--color"),
              action="store",
              type="character",
              default = NULL,
              help="Choose color for '--Cotu' and '--Cclades'."  
              ),
  ## Frame:
  make_option(c("--Hclades"),
              action="store",
              type="character",
              default = NULL,
              help="Highlight selected clade(s). Add color separated by ','. Add extension separated by ','.\n\t\tIf the extension is too high, nothing appears.\n\t\t\tE.g.: --Hclades 9,red,0.0015\n\t\tMultiples values separated by '_'. \n\t\t\tE.g.: --Hclades 9,red,0.0015_20,green,0.0020"  
              ),    
  ## Labels
    # Label selected clade:             
  make_option(c("--Bclades"),
              action="store",
              type="character",
              default = NULL,
              help="Add labels. Select node and add label, separated by ','. \n\t\t\tE.g.: --Bclades 9,Group1\n\t\tMultiples values separated by '_'. \n\t\t\tE.g.: --Bclades 9,Group1_20,Group2\n\t\tMultiples labels are aligned. \n\t\tOption '--offset' positions it/them. \n\t\tCould be used to label collapsed nodes by '--collapse'. \n\t\t\tE.g.: --colapse 7,min --Bclades 7,Group1"  
              ), 
  make_option(c("--offset"),
              action="store",
              type="double",
              default = 0.1,
              help="Position '--Bclades'. By default, '0.1'. \n\t\tThe higher the value, the futher from the tree."  
              ),               


### Sizes and scales              
  ## Fontsize:  
  make_option(c("--fontsize"),
              action="store",
              type="double",
              default = 6,
              help="Font size. By default, 6."
              ),  
  make_option(c("--nodesize"),
              action="store",
              type="double",
              default = 5,
              help="Bootstrap values font size. By default, 5."
              ),  
  ## Bar width: 
  make_option(c("--bar_width"),
              action="store",
              type="double",
              default = NULL,
              help="Bar size. By default, automatic."
              ),    
  ## Output size: 
  make_option(c("-W", "--output_width"),
              action="store",
              type="double",
              default = 30,
              help="Output file width (cm). By default, A4 width (30 cm).\n\t\tBigger trees need bigger sheet size."
              ),                
  make_option(c("-H", "--output_height"),
              action="store",
              type="double",
              default = 21,
              help="Output file height (cm). By default, A4 height (21 cm).\n\t\tBigger trees need bigger sheet size."
              ),
              
  ## Tree size:  
  make_option(c("-s", "--tree_size"),
              action="store",
              type="double",
              default = 0.22,
              help="Tree size. Lower values expand the tree along the X axis. By default, 0.22. \n\t\tIt's recommended to use bigger sizes for circular layouts."
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
  ## Bootstrap position on X and Y axes:
  make_option(c("-x", "--bootstrap_X"),
              action="store",
              type="double",
              default = -0.0025,
              help="Position on X axis for bootstrap values. By default, -0.0025."
              ),  
  make_option(c("-y", "--bootstrap_Y"),
              action="store",
              type="double",
              default = 0.42,
              help="Position on Y axis for bootstrap values. By default, 0.42."
              )
)  


opt_parser = OptionParser(option_list=option_list, add_help_option=TRUE)
opt = parse_args(opt_parser)


### ERRORS
## This tool needs, at least, one tree to work. Therefore, the following error calls out if none inserted. 
## The other options are optional, so any errors are generated. 
if (is.null(opt$treeOne)){
  print_help(opt_parser)
  stop("At least one tree file must be selected ('--treeOne').", call.=FALSE)
}






#############
# FUNCTIONS #
#############

#
## FILE READ  
##############

get_df <- function(tree, data_file) {
### Return a dataframe with taxa data with format ready for plotting.
## It needs a base tree and a taxa data file. 
## 'data_file' input must follow an specific format: 
## It must have 4 columns, separated by TABULATOR, and WITHOUT header:
##	First column. Later renamed as 'arb'. (This tool was thought for working with results from ARB software). 
##			OTUs as named on tree file. It's the name that would appear in any other tree viewers. 
##	Second column. Later renamed as 'acc'.
##			Accesion numbers of sequences 
##			This tool places them in brackets. 
##	Third column. Later renamed as 'gen_sp'. 
##			Scientist name. Blank spaces can be added. 
##			Will be shown in ITALICS.
##	Fourth column. Later renamed as 'cepa'. 
##			Strain name. Blank spaces can be added. If working with type strain, DO NOT add siperindexed T. It will be added automatically when opt$superindex option = TRUE. 
## From this data, essential for publishing figures, a new column will be added to the dataframe. This new column will be read by 'ggtree' plot functions. 


### File read:
   # No header. Columns separated by tabulator. 
  csv <- read.table(file = data_file, header = F, sep = "\t")
  colnames(csv) <- c("arb", "acc","gen_sp", "cepa") # Creates a new header for the dataframe. 

### Clean strains names:
   # This tool was thought for working with ARB software output. Generally, more characters than necessary are present. Therefore, characters that don't belong to the strain name must be erased. 
  v4 <- as.vector(csv[[4]])  # Strains names are on the fourth column, so the function will work only with it from now on. Square brackets turn it into a vector. 
  for (i in 1:length(v4)) {
   # The following conditionals look for the most frequent undesired characters. If present, they are cut off the chain. 
    if (grepl("^type strain: ", v4[i]) == TRUE) {
      v4[i]<- substr(v4[i], start = nchar("type strain: ")+1, stop = 100) #'stop=100' because strain name is considered to be shorter than that. 
    }
    if (grepl("\\[T]", v4[i]) == TRUE) { # Double bar turns the next character into a NOT SPECIAL one.
      v4[i] <- substr(v4[i], start = 1, stop = (regexpr("\\[T]", v4[i])-3))
    }
    if (grepl("\\,|;|/", v4[i]) == TRUE) {
      v4[i] <- substr(v4[i], start = 1 , stop = (regexpr("\\,|;|/", v4[i])-1))
    }
    if (grepl("\\/", v4[i]) == TRUE) {
      v4[i] <- substr(v4[i], start = 1 , stop = (regexpr("\\/", v4[i])-2))
    }
    if (substr(v4[i], start = nchar(v4[i]) , stop = nchar(v4[i])) == " ") {
      v4[i] <- substr(v4[i], start = 1 , stop = (nchar(v4[i])-1))
    }
  }   
  csv[[4]] <- v4  # Replace with the correct names. 
 
  csv$gen_sp <- paste0("'", csv$gen_sp, " ", "'" ) # A space is added after the scientist name. It will be useful later. 
 
### Order the dataframe to match the tree: 
  orden_df <- csv # Copy dataframe. Overwrite may cause false positives in the following loop.
  for (z in 1:length(tree$tip.label)){	# Read every OTU position.
    if (tree$tip.label[z] == csv$arb[z]) {	# Check if its position on the tree matches its position on the dataframe. 
      next 					#'next' to break the loop and keep going. Values does not appear more than once, so it is not necessary to keep looking.
    }
    else {					# 'else' when the position does not match.  
      t <- 1 					# It should always start by one. 
      for (t in 1:length(tree$tip.label)){ 	# Loop for searching position one by one. There must be as many position as OTUs ('length(tree$tip.label)').
        if (csv$arb[t] == tree$tip.label[z]) { # The conditional chooses the right matching position on the tree.
          # t is the right position (meets the condicional)        
          orden_df[z,] <- csv[t,] 		# Replace the unmatched column (z) with the one that matchs (t) from the original dataframe (csv), in the copied dataframe (orden_df).
          next 				#'next' to break the loop and keep going. 
        }
      }
    }
  }
  csv <- orden_df 				# New dataframe overwrites old dataframe. We have made sure dataframe values match the order of the tree. 

### Superindex:
   # When working with type strains, superindexed 'T'. 
  if (opt$superindex){ 			  # If TRUE, place superindexed 'T'
    cepacc <- paste0("'", csv$cepa, "'" ,
                   "^'T'*",             	  # Place a superindexed 'T' behind the strain name. "^" indicates that the following text (between quotation marks) has to be superindexed. "*" indicates that the text has to have regular format again.
                   "'", " ", "'",       	  # Place a space to separate strain name from accession number. 
                   "(", "'", csv$acc, "'", ")") # Place the accession number in brackets. 
  }
  else {
    cepacc <- paste0("'", csv$cepa, " '" ,
                   "(", "'", csv$acc, "'", ")")  
  }
   
### Create a new column for 'ggtree'. 
  etiqueta = paste0('italic(', csv$gen_sp, ')~', cepacc)  # The columns containts scientist name (italics), strain name (with or without superindex) and accesion number in brackets. 
  csv <- cbind.data.frame(csv, etiqueta)		    

### Return
  return(csv)
}



#
## SHARED NODES
#################

shared_nodes <- function(tree, tree2=NULL, tree3=NULL) {
### Return a vector with only the values of those nodes shared on every tree. The unshared nodes present a 'NA' value. 
## This function is useful when more than one tree. Therefore, at least '--tree' and '--tree2' has to be added. 
## These nodes will appear with a black point in the plot, for the reader to know they are present in all the studied algorithms. 
  
   # First of all, original bootstrap values are saved on a new variable. They'll be needed for the plot. 
  nodos_todos <- tree$node.label 

   # Compare base tree (tree) with the other two, separately, using 'matchNodes' function from 'phytools' library. 
  mtt2 <- matchNodes(tree, tree2, method = "descendants")
  if (!is.null(tree3)){
    mtt3 <- matchNodes(tree, tree3, method = "descendants") 
    }
 
   # Save matching positions between tree 1 and tree 2. 
  posiciones_t2 <- c()
  for (i in 1:length(mtt2[,2])){		# Parse it. Column two is the one with tree 2 data. 
    if (!is.na(mtt2[i,2])) {			# If the value is not NA, it will be saved on a new vector ('posiciones_t2').
      posiciones_t2 <- c(posiciones_t2, i)
    }
  }
 
   # Save matching positions between tree 1 and tree 3.
  if (!is.null(tree3)){
    posiciones_t3 <- c()
    for (i in 1:length(mtt3[,2])){
      if (!is.na(mtt3[i,2])) {
        posiciones_t3 <- c(posiciones_t3, i)
      }
    }
  }
 
   # Shared positions of tree 1 and tree 2, and of tree 1 and tree 3, are compared ('posiciones_t2' y 'posiciones_t3').
   # Those are the shared position between all trees. 
  
  if (!is.null(tree3)){
    posicion_nodos <- intersect(posiciones_t2, posiciones_t3)
    posicion_nodos <- posicion_nodos +1 	 # The right position is the following one. That is why plus 1 is added. 
  }
  else{
   # If tree 3 is not present, then shared nodes between tree 1 and tree 2 are the shared position between all trees: 
    posicion_nodos <- posiciones_t2 +1 
  }
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



#
## BOOTSTRAP
##############

bootstrap <- function(tree, value) {
### Return the base tree with 'node.label' values equal or over the selected threshold ('--bootstrap_threshold'). 
## Most phylogenetic trees only represent bootstrap values over a chosen percentage. 
## Therefore, it is interesting to have a filter function.  
## This function needs a tree and a threshold. 
 
  b <- (as.numeric(tree$node.label)) 	# Turn "tree$node.label" into a numeric vector. 
  d <- c() 				# Create a empty vector in which only values equal or over the threshold will be saved. 
 
   # Check all values on 'b'. 'NA' are kept. If equal or over the threshold, the value is saved. If lower, 'NA' is saved. 
   # Final vector 'd' must have the same length than original vector 'b'. 
  for (i in b) {
    if (is.na(i)){
      d <- c(d, NA)
      next
      }
    if (i >= value) {
      d <- c (d,i)
      }
    else { 
      d <- c(d,NA)
      }
  }

   # Older information is replaced with values over the threshold ('d') 
  tree$node.label <- as.character(d)

### Return 
  return(tree)
}



#
## SAVING
###########

saving <- function(saved_file = "yourTree.pdf", tr, width, height) {
### Save the tree plot.  
## By default, it will be saved as 'yourTree.pdf'
## This function needs a plot, an output (with format), and an output size (width, height, cm). 

  ggsave(saved_file, tr, width = opt$output_width, height = opt$output_height, units = "cm", limitsize = FALSE)
}



#
## OTHERS
###########

# Check tree and its node positions:
check_node_position <- function(arbol){
### Save a plot based only on '--treeOne' in which bootstrap values are replaced by node position. 
## The function needs just a tree to work.  
## Later tree modifications may need a node position to work into. This option offers the user that information. 
## The only arguments that work along with this function is '--ladderize', for better localization of the desired node, and '-H' and '-W' for sheet size.

   # Replace bootstrap values with numeration. 
   # First plot and then save on the working directory. 
  tree$node.label <- seq(1:length(tree$node.label))
  tr <- ggtree(tree, ladderize=opt$ladderize) + geom_tiplab(size = opt$fontsize) + geom_nodelab()
  saving(saved_file = "quickTree.pdf", tr)
}


# Choosing nodes:
### Return real node position. 
## 'as.numeric()' converts character strains. 
my_node <- function(chosen_node){
  node_value <- as.numeric(chosen_node) + 1 + length(tree$node.label)

### Return: 
  return(node_value)
}


# Split arguments:
### Convert a character chain into different vectors. 
## Separator must be specified.
divide <- function(chain, sep){
  values <- unlist(strsplit(as.character(chain), sep))
  
### Return:
  return(values)
}






###########
# WORKING #
###########

#
## TREES
############
# First, input tree is read (maximum 3). 
# At least one must be added to plot a phylogenetic tree. '--treeOne' sets the topology.  

## Tree one, or base tree, read: 
tree <- read.tree(opt$treeOne) # 'phylo' object
nodes <- tree$node.label # Save node values in a different vector for later. 

# If '--quick_print' is called, it is executed and the script shuts down. 
if (opt$quick_print) {
  check_node_position(tree)
  quit()
  }
  

## Other trees read: 
if (!is.null(opt$treeTwo)){ 
  tree2 <- read.tree(opt$treeTwo)
  }
    
if (!is.null(opt$treeThree)){ 
  tree3 <- read.tree(opt$treeThree)
  } else {tree3 = opt$treeTree
  }


#
## TXT
########
# Taxa data input read.
# Optional. If not added, tip labels from 'phylo' object are used into the plot. 
if (!is.null(opt$file)) {
  data_file <- opt$file
  csv <- get_df(tree, data_file) 	# The function gets the right format done for plotting.  
}



#
## SHARED NODES
#################
# Compare which nodes are shared between all the trees.  
# Not needed if only one input tree.
if (!is.null(opt$treeTwo)){ 		  
  nodos_punto <- shared_nodes(tree, tree2, tree3)
} 



#
## BOOTSTRAP
##############

# Bootstrap values are turned into percentage. 
if (!is.null(opt$bootstrap_percentage)){
  if (opt$bootstrap_percentage == 1000){
    if (max(as.numeric(tree$node.label)[2:length(tree$node.label)] == 1)) { # '"1.000"' (character) could be converted into '1' with the 'as.numeric()'
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






#################
# MODIFICATIONS #
#################

# For options in which the node needs to be chosen, my_node() function is applied.

#
## OUTGROUP CHANGE
####################
# 'root' and 'unroot' functions from 'ape' package modify tree root. 

if (!is.null(opt$root_node)) {
  tree <- root(tree, node = my_node(opt$root_node))
  }
if (!is.null(opt$root_OTU)) {
  tree <- root(tree, outgroup = (opt$root_OTU))
  }
if (opt$unroot) {
  tree <- unroot(tree)
  }



#
## ROTATE
###########
# 'rotateNodes' function from 'phytools' library rotates tree nodes. 
# 'opt$ladderize' must be FALSE. Topology cannot be automatically and manually modified at the same time. 

if (!is.null(opt$rotate)) {
  opt$ladderize = FALSE 		# To be sure. 
  div <- divide(opt$rotate, ",")
  for (i in div) {
    if (i == "all") {			# 'all' option rotates all the nodes.  
      tree <- rotateNodes(tree, "all")
    } else{
      tree <- rotateNodes(tree, my_node(i))
      }
    }
  }


#
## OTU DELETE
###############
# One or more OTUs can be deleted with 'drop.tip' function from 'ape' package.
if (!is.null(opt$drop)) {
  div <- divide(opt$drop, ",")
    for (i in div) {
    tree <- drop.tip(tree, i)
    }
  }






#############
# TREE PLOT #
#############

# Tree plotting uses 'ggtree' package, based on 'ggplot2' library. Its main feature is the use of layers to create very complex plot from very basic ones. 


### Input data file: 
## This file formats tip labels. Scientist name in italics, strain name, accesion number in brackets. All this in the 'etiqueta' column created by 'get_df()' 
## When this file it is not present, name will be plotted as showed in the tree file. 
   # 'layout' for tree structure: rectangular (by default) | slanted | fan | circular | radial | equal_angle | daylight.
   # 'ladderize' for stairs-like distribution (TRUE). 
   # 'align' tip labels aligned at the right side (TRUE). 

if (!is.null(opt$file)){
  tr <- ggtree(tree, layout = opt$layout, ladderize = opt$ladderize) %<+% csv + 
  geom_tiplab(aes(label=etiqueta), align = opt$align, parse=T, size = opt$fontsize) 		# 'parse=TRUE' allows modifications
  } else {
  tr <- ggtree(tree,  layout = opt$layout, ladderize = opt$ladderize) +
  geom_tiplab(align = opt$align, size = opt$fontsize) 
  }


### Color clades:
## Color branches that belong to the same clade. Can be used in one or more clades. 
## 'groupClade' function from 'tidytree' library, loaded by 'ggtree'. 

if (!is.null(opt$Cclades)){
   # 'ggplot2' is loaded here to avoid incompatibility problems, as it is not used on most options. 
  library("ggplot2")	
   # First, values listed on 'opt$Cclades' are splited. After, 'my_node()' function is applied. 
  clades <- as.numeric(divide(opt$Cclades, ","))
  clades <- sapply(clades, my_node)
  color <- divide(opt$color, ",")
  # 'groupClade' modifies 'phylo' object, creating a subgroup. Therefore, plot has to be plotted again, with or without 'opt$file'.
  tree <- groupClade(tree, clades)
  if (!is.null(opt$file)){
    tr <- ggtree(tree, aes(color=group), layout = opt$layout, ladderize = opt$ladderize) %<+% csv + 
    geom_tiplab(aes(label=etiqueta), align = opt$align, parse=T, size = opt$fontsize) + 
    scale_colour_manual(values = c("black", color)) + theme(legend.position= "none")	# Keep non selected branches in black color. 
    } else {
    tr <- ggtree(tree, aes(color=group), layout = opt$layout, ladderize = opt$ladderize) + 
    geom_tiplab(align = opt$align, size = opt$fontsize) +
    scale_colour_manual(values = c("black", color)) + theme(legend.position= "none")
    }
  }


### Coloring OTUs: 
## Similar to previous option. In this case, OTUs are selected instead of clades. 
## It is incompatible with '--Cclades'. Both work the same way so one destroys the other when used at the same time. (one subgroup replaces the other). 

if (!is.null(opt$Cotu)){
  library(ggplot2)	
  otu <- divide(opt$Cotu, ",") 
  tree <- groupOTU(tree, otu)
  color <- divide(opt$color, ",")
  if (!is.null(opt$file)){
    tr <- ggtree(tree, aes(color=group), layout = opt$layout, ladderize = opt$ladderize) %<+% csv + 
    geom_tiplab(aes(label=etiqueta), align = opt$align, parse=T, size = opt$fontsize) +
    scale_colour_manual(values = c("black", color)) + theme(legend.position= "none")
    } else {
    tr <- ggtree(tree, aes(color=group), layout = opt$layout, ladderize = opt$ladderize) + 
    geom_tiplab(align = opt$align, size = opt$fontsize) +
    scale_colour_manual(values = c("black", color)) + theme(legend.position= "none")
    }
  }


### Shared nodes: 
## Insert layer with previous obtained shared nodes. Only for working with more than one tree file. 
## Shared nodes were saved on 'nodos_puntos'. Original nodes were saved on 'nodes'.

if (!is.null(opt$treeTwo)) {  
  tr <- tr + geom_nodepoint(aes(subset=(nodos_punto %in% nodes)))  
  }  


### Bar scale: 
## Add bar scale to the bottom left: 

tr <- tr + geom_treescale(x = 0, y= -1, width = opt$bar_width, fontsize = opt$nodesize) 


### Bootstrap: 
## Show previously filtered bootstrap values: 
   # 'nudge_x' and 'nudge_y' for positioning the value over the branch. '0' sets them over the node. 
   # For not showing bootstrap, threshold value should be higher than the higher present value.

tr <- tr + geom_nodelab(nudge_x = opt$bootstrap_X, nudge_y = opt$bootstrap_Y,  size = opt$nodesize)


### Tree size:
## Each tree has different size according to its taxa and the relationship between them. It is necessary to adjust tree size on X axis. 

tr <- tr + xlim(0, opt$tree_size)


### Scale Y axis: 
## This function also adjust tree size using 'scaleClade' function. 
## Useful for adjusting distance between taxa, or making a collapsed node smaller.
## This function works directly over the plot object ('tr'). 
   # 'opt$scale' argument includes proportion (> 1, bigger; <1, smaller), and node position, separated by ','.
   # Can be used more than once, separating values by '_'. 
   # To apply to the complete tree, select node 1.

if (!is.null(opt$scale)) {
  div <- divide(opt$scale, "_") 	# Split when more than one application. 
  for (i in div) {
    div2 <- divide(i, ",")		# Split size and node. 
    tr <- scaleClade(tr, node = my_node(div2[2]), scale = as.numeric(div2[1]), vertical_only = TRUE) 
    }
  }


### Scale X and Y axes:
## Same that the previous option, but works also on X axis.

if (!is.null(opt$scalexy)) {
  div <- divide(opt$scalexy, "_")    
  for (i in div) {
    div2 <- divide(i, ",")
    tr <- scaleClade(tr, node = my_node(div2[2]), scale = as.numeric(div2[1]), vertical_only = FALSE) 
    }
  }


### Highlight: 
## Highlight clades inside a colored rectangle. The color is selected by the user, separated by ','. 
## Can be used more than once, separating values by '_'. 

if (!is.null(opt$Hclades)) {
  div <- divide(opt$Hclades, "_") 	# Split when more than one application. 
  for (i in div) {
    div2 <- divide(i, ",") 		# Split node and color.
    tr <- tr + geom_hilight(node = my_node(as.numeric(div2[1])), fill = div2[2], extend = as.numeric(div2[3]))
    }
  tr$layers <- rev(tr$layers) # Reverse layers. That way, the rectangle is in the bottom layer, and the text on top of it. 
  }


### Annotation: 
## Select a clade and annotate it at the right side.  
   # 'opt$offset' argument adjusts the label position. 
   # When more than one, they aligns. 
   # Can be used more than once, separating values by '_'.

if (!is.null(opt$Bclades)) {
  div <- divide(opt$Bclades, "_") # Split when more than one application. 
  for (i in div) {
    div2 <- divide(i, ",")
    tr <- tr + geom_cladelabel(node = my_node(as.numeric(div2[1])), label = div2[2], align =T, barsize = 1, offset = opt$offset, fontsize = opt$fontsize) 
    }
 }


### Collapse: 
## Collapse a selected node. 
## This function has different modes: 'none', 'max', 'min', 'mixed'. 
## Its use is recommended along with 'Bclades' (add labels) and 'scale' (modify the size).
## Can be used more than once, separating values by '_'. 

if (!is.null(opt$collapse)) {
  div <- divide(opt$collapse, "_")
  for (i in div) {
    div2 <- divide(i, ",")
	print(div2)
    tr <- collapse(tr, my_node(as.numeric(div2[1])), mode=div2[2])
    }
  }






########
# SAVE #
######## 

### Plot is saved after the script is done. By default, 'yourTree.pdf'.
## Can be saved in different formats such as .jpg or .png. 
## For bigger trees, the output size must be adjusted. 

saving(saved_file = opt$output_format, tr, width = opt$output_width, height = opt$output_height)

