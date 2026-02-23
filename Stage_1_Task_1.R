GC_calculator <- function(input_gene) {
    # convert everything to uppercase
  input_gene <- toupper(input_gene)
  
  input_gene <- strsplit(input_gene, split="")[[1]]
  
  gc_counter <- 0
  
  for (nuc in input_gene){
    if (nuc == 'G' | nuc == 'C'){
      gc_counter <- gc_counter + 1
    }
  }
  
  return ((gc_counter / length(input_gene)) * 100)
}

GC_calculator("GCATTTAT")
GC_calculator("gcaTTTAT")