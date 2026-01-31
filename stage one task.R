gc_content <- function(sequence) {
  # Convert sequence to uppercase to handle mixed cases
  sequence <- toupper(sequence)
  
  # Split sequence into individual nucleotides
  nucleotides <- strsplit(sequence, "")[[1]]
  
  # Count number of G and C
  gc_count <- sum(nucleotides %in% c("G", "C"))
  
  # Total length of sequence
  total_length <- length(nucleotides)
  
  # Calculate GC percentage
  gc_percentage <- (gc_count / total_length) * 100
  
  return(gc_percentage)
}


aa_weights <- c(
  A = 89.09,
  R = 174.20,
  N = 132.12,
  D = 133.10,
  C = 121.15,
  E = 147.13,
  Q = 146.15,
  G = 75.07,
  H = 155.16,
  I = 131.18,
  L = 131.18,
  K = 146.19,
  M = 149.21,
  F = 165.19,
  P = 115.13,
  S = 105.09,
  T = 119.12,
  W = 204.23,
  Y = 181.19,
  V = 117.15
)
protein_weight <- function(protein = "EBENEZER") {
  
  # Convert protein sequence to uppercase
  protein <- toupper(protein)
  
  # Split protein into individual amino acids
  amino_acids <- strsplit(protein, "")[[1]]
  
  # Check for non-protein characters
  if (!all(amino_acids %in% names(aa_weights))) {
    return(0)
  }
  
  # Sum molecular weights
  total_weight_da <- sum(aa_weights[amino_acids])
  
  # Convert Daltons to Kilodaltons
  total_weight_kda <- total_weight_da / 1000
  
  return(total_weight_kda)
}
