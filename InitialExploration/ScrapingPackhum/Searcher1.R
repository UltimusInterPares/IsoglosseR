GrkToLat <- function(text) {
  # The Standard Ones
  text <- gsub("α", "a", text, ignore.case = F, perl = T)
  text <- gsub("Α", "A", text, ignore.case = F, perl = T)
  
  text <- gsub("β", "b", text, ignore.case = F, perl = T)
  text <- gsub("Β", "B", text, ignore.case = F, perl = T)
  
  text <- gsub("γ", "g", text, ignore.case = F, perl = T)
  text <- gsub("Γ", "G", text, ignore.case = F, perl = T)
  
  text <- gsub("δ", "d", text, ignore.case = F, perl = T)
  text <- gsub("Δ", "D", text, ignore.case = F, perl = T)
  
  text <- gsub("ε", "e", text, ignore.case = F, perl = T)
  text <- gsub("Ε", "E", text, ignore.case = F, perl = T)
  
  text <- gsub("ζ", "z", text, ignore.case = F, perl = T)
  text <- gsub("Ζ", "Z", text, ignore.case = F, perl = T)
  
  text <- gsub("η", "h", text, ignore.case = F, perl = T)
  text <- gsub("Η", "H", text, ignore.case = F, perl = T)
  
  text <- gsub("θ", "q", text, ignore.case = F, perl = T)
  text <- gsub("Θ", "Q", text, ignore.case = F, perl = T)
  
  text <- gsub("ι", "i", text, ignore.case = F, perl = T)
  text <- gsub("Ι", "I", text, ignore.case = F, perl = T)
  
  text <- gsub("κ", "k", text, ignore.case = F, perl = T)
  text <- gsub("Κ", "K", text, ignore.case = F, perl = T)
  
  text <- gsub("λ", "l", text, ignore.case = F, perl = T)
  text <- gsub("Λ", "L", text, ignore.case = F, perl = T)
  
  text <- gsub("μ", "m", text, ignore.case = F, perl = T)
  text <- gsub("Μ", "M", text, ignore.case = F, perl = T)
  
  text <- gsub("ν", "n", text, ignore.case = F, perl = T)
  text <- gsub("Ν", "N", text, ignore.case = F, perl = T)
  
  text <- gsub("ξ", "c", text, ignore.case = F, perl = T)
  text <- gsub("Ξ", "C", text, ignore.case = F, perl = T)
  
  text <- gsub("ο", "o", text, ignore.case = F, perl = T)
  text <- gsub("Ο", "O", text, ignore.case = F, perl = T)
  
  text <- gsub("π", "p", text, ignore.case = F, perl = T)
  text <- gsub("Π", "P", text, ignore.case = F, perl = T)
  
  text <- gsub("ρ", "r", text, ignore.case = F, perl = T)
  text <- gsub("Ρ", "R", text, ignore.case = F, perl = T)
  
  text <- gsub("(σ|ς)", "s", text, ignore.case = F, perl = T)
  text <- gsub("Σ", "S", text, ignore.case = F, perl = T)
  
  text <- gsub("τ", "t", text, ignore.case = F, perl = T)
  text <- gsub("Τ", "T", text, ignore.case = F, perl = T)
  
  text <- gsub("υ", "u", text, ignore.case = F, perl = T)
  text <- gsub("Υ", "U", text, ignore.case = F, perl = T)
  
  text <- gsub("φ", "f", text, ignore.case = F, perl = T)
  text <- gsub("Φ", "F", text, ignore.case = F, perl = T)
  
  text <- gsub("χ", "x", text, ignore.case = F, perl = T)
  text <- gsub("Χ", "X", text, ignore.case = F, perl = T)
  
  text <- gsub("ψ", "y", text, ignore.case = F, perl = T)
  text <- gsub("Ψ", "Y", text, ignore.case = F, perl = T)
  
  text <- gsub("ω", "w", text, ignore.case = F, perl = T)
  text <- gsub("Ω", "W", text, ignore.case = F, perl = T)
  
  # The Weird Ones
  text <- gsub("ϝ", "v", text, ignore.case = F, perl = T)
  text <- gsub("Ϝ", "V", text, ignore.case = F, perl = T)
  
  #text <- gsub("ϙ", "", text, ignore.case = F, perl = T)
  #text <- gsub("Ϙ", "", text, ignore.case = F, perl = T)
  
  text <- gsub("ϳ", "j", text, ignore.case = F, perl = T)
  
  #text <- gsub("ϟ", "", text, ignore.case = F, perl = T)
  #text <- gsub("Ϟ", "", text, ignore.case = F, perl = T)
  
  return(text)
}

LatToGrk <- function(text) {
  # The Standard Ones
  text <- gsub("a", "α", text, ignore.case = F, perl = T)
  text <- gsub("A", "Α", text, ignore.case = F, perl = T)
  
  text <- gsub("b", "β", text, ignore.case = F, perl = T)
  text <- gsub("B", "Β", text, ignore.case = F, perl = T)
  
  text <- gsub("g", "γ", text, ignore.case = F, perl = T)
  text <- gsub("G", "Γ", text, ignore.case = F, perl = T)
  
  text <- gsub("d", "δ", text, ignore.case = F, perl = T)
  text <- gsub("D", "Δ", text, ignore.case = F, perl = T)
  
  text <- gsub("e", "ε", text, ignore.case = F, perl = T)
  text <- gsub("E", "Ε", text, ignore.case = F, perl = T)
  
  text <- gsub("z", "ζ", text, ignore.case = F, perl = T)
  text <- gsub("Z", "Ζ", text, ignore.case = F, perl = T)
  
  text <- gsub("h", "η", text, ignore.case = F, perl = T)
  text <- gsub("H", "Η", text, ignore.case = F, perl = T)
  
  text <- gsub("q", "θ", text, ignore.case = F, perl = T)
  text <- gsub("Q", "Θ", text, ignore.case = F, perl = T)
  
  text <- gsub("i", "ι", text, ignore.case = F, perl = T)
  text <- gsub("I", "Ι", text, ignore.case = F, perl = T)
  
  text <- gsub("k", "κ", text, ignore.case = F, perl = T)
  text <- gsub("K", "Κ", text, ignore.case = F, perl = T)
  
  text <- gsub("l", "λ", text, ignore.case = F, perl = T)
  text <- gsub("L", "Λ", text, ignore.case = F, perl = T)
  
  text <- gsub("m", "μ", text, ignore.case = F, perl = T)
  text <- gsub("M", "Μ", text, ignore.case = F, perl = T)
  
  text <- gsub("n", "ν", text, ignore.case = F, perl = T)
  text <- gsub("N", "Ν", text, ignore.case = F, perl = T)
  
  text <- gsub("c", "ξ", text, ignore.case = F, perl = T)
  text <- gsub("C", "Ξ", text, ignore.case = F, perl = T)
  
  text <- gsub("o", "ο", text, ignore.case = F, perl = T)
  text <- gsub("O", "Ο", text, ignore.case = F, perl = T)
  
  text <- gsub("p", "π", text, ignore.case = F, perl = T)
  text <- gsub("P", "Π", text, ignore.case = F, perl = T)
  
  text <- gsub("r", "ρ", text, ignore.case = F, perl = T)
  text <- gsub("R", "Ρ", text, ignore.case = F, perl = T)
  
  text <- gsub("s ", "ς ", text, ignore.case = F, perl = T)
  text <- gsub("s", "σ", text, ignore.case = F, perl = T)
  text <- gsub("S", "Σ", text, ignore.case = F, perl = T)
  
  text <- gsub("t", "τ", text, ignore.case = F, perl = T)
  text <- gsub("T", "Τ", text, ignore.case = F, perl = T)
  
  text <- gsub("u", "υ", text, ignore.case = F, perl = T)
  text <- gsub("U", "Υ", text, ignore.case = F, perl = T)
  
  text <- gsub("f", "φ", text, ignore.case = F, perl = T)
  text <- gsub("F", "Φ", text, ignore.case = F, perl = T)
  
  text <- gsub("x", "χ", text, ignore.case = F, perl = T)
  text <- gsub("X", "Χ", text, ignore.case = F, perl = T)
  
  text <- gsub("y", "ψ", text, ignore.case = F, perl = T)
  text <- gsub("Y", "Ψ", text, ignore.case = F, perl = T)
  
  text <- gsub("w", "ω", text, ignore.case = F, perl = T)
  text <- gsub("W", "Ω", text, ignore.case = F, perl = T)
  
  # The Weird Ones
  text <- gsub("v", "ϝ", text, ignore.case = F, perl = T)
  text <- gsub("V", "Ϝ", text, ignore.case = F, perl = T)
  
  text <- gsub("j", "ϳ", text, ignore.case = F, perl = T)
  
  return(text)
}


Search <- function(df, target, hyp1, hyp2) {
  target1 <- gsub("[CV]", GrkToLat(hyp1), GrkToLat(target))
  target2 <- gsub("[CV]", GrkToLat(hyp2), GrkToLat(target))
  
  df1 <- df %>% filter(str_detect(GrkToLat(text), target1) == T)
  df2 <- df %>% filter(str_detect(GrkToLat(text), target2) == T)
  
  print(c("hyp1:", target, hyp1))
  print(df1)
  print(c("hyp2:", target, hyp2))
  print(df2)
  
}

