###Phylo Data####
#read in the 10000 trees
FullTree <- read.nexus(here('./Data/Input/10.cal.tree.nex'))
#se the seed for reproduciblility
set.seed(11)
#sample out a tree
SampleFullTree <- sample(FullTree, 1)
SampleFullTree <- SampleFullTree$UNTITLED
#check if tips are binary
is.binary(SampleFullTree)
#check where the tree and dataset differ
namedrops <- name.check(SampleFullTree, GSH_df, GSH_df$Binomial)
#drop the tips that are in the tree and not in the data (all the rays)
SampleFullTree <- drop.tip(SampleFullTree, namedrops$tree_not_data)
#drop the species in the dataset that are not in the tree
GSH_df_full <- subset(GSH_df, GSH_df$Binomial %nin% namedrops$data_not_tree) 


####models####
pglsMod1 <- gls(Log10CFAR ~ Log10MeanGSH_centered, 
                correlation = corPagel(phy = SampleTree, value = 0.5, fixed = F, form = ~Binomial), 
                data = GSH_df_pgls, method = "ML")

summary(pglsMod1)
