#Section 1A
normalize <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

df <- data.frame(a=1:10, b=seq(200,400,length=10), c=11:20, d=NA)
df[] <- lapply(df, normalize)
print(df)
#Section 1B
#install.packages("bio3d")
library(bio3d)

analyze_protein <- function(pdb_code) {
  library(bio3d)
  
  # Read PDB file
  pdb <- read.pdb(pdb_code)
  
  # Trim to chain A, C-alpha atoms
  pdb_chain <- trim.pdb(pdb, chain="A", elety="CA")
  
  # Extract B-factors
  bfactor <- pdb_chain$atom$b
  
  # Plot
  plotb3(bfactor, sse=pdb_chain, typ="l", ylab="Bfactor")
  
  return(bfactor)
}

# Example usage:
s1.b <- analyze_protein("4AKE")
s2.b <- analyze_protein("1AKE")
s3.b <- analyze_protein("1E4Y")

#Q1) A list object of class "pdb"
#Q2) Extracts subset of atoms from PDB structure based on specific criteria like chain ID and element type.
#Q3) Parameter: sse = NULL; grey rectangles represent alpha helices and black rectangles represent beta sheets
#Q4) Overlay plot showing all 3 proteins on the same axes with different colors and legend
#Q5) The proteins that cluster together first are most similar in their B-factor trends. This can be quantified using hierarchical clustering with dist() and hclust()
#Q6)

analyze_protein_general <- function(pdb_code, chain="A", plot=TRUE) {
  # This function analyzes protein drug interactions by reading in any 
  # protein PDB data and outputting a plot for the specified protein
  #
  # Inputs:
  #   - pdb_code: A 4-letter PDB code (e.g., "4AKE") or file path
  #   - chain: The protein chain to analyze (default: "A")
  #   - plot: whether to generate a plot (default: TRUE)
  #
  # Output:
  #   - Returns a numeric vector of B-factor values for the protein
  
  library(bio3d)
  
  # Read PDB file
  pdb <- read.pdb(pdb_code)
  
  # Trim to specified chain and C-alpha atoms
  pdb_chain <- trim.pdb(pdb, chain=chain, elety="CA")
  
  # Extract B-factors
  bfactor <- pdb_chain$atom$b
  
  # Generate plot if requested
  if (plot) {
    plotb3(bfactor, sse=pdb_chain, typ="l", ylab="Bfactor", 
           main=paste("B-factor Plot for", pdb_code))
  }
  
  # Return B-factor values
  return(bfactor)
}

# Example usage showing the function works:
result1 <- analyze_protein_general("4AKE")
result2 <- analyze_protein_general("1AKE")
result3 <- analyze_protein_general("1E4Y")

