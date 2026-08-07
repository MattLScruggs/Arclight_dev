# =====================================================================
# 1. SETUP PARAMETERS & SCORES
# =====================================================================

# Sequence length (3 words) and unique states (1 = Noun, 2 = Verb)
states <- c("Noun", "Verb")
N <- length(states)
T_len <- 3

# Node Potentials (Emissions)
# Rows = States, Columns = Time step (Word 1, Word 2, Word 3)
# These represent unnormalized scores for how much a word looks like a Noun or Verb
node_pot <- matrix(c(
  3.0, 0.5,  # Word 1: strongly favors Noun
  1.0, 4.0,  # Word 2: strongly favors Verb
  2.5, 1.0   # Word 3: favors Noun
), nrow = N, ncol = T_len)
rownames(node_pot) <- states

# Edge Potentials (Transitions)
# Matrix mapping: Rows = Previous State (t-1), Columns = Current State (t)
edge_pot <- matrix(c(
  1.5, 3.0,  # Noun -> Noun (1.5), Noun -> Verb (3.0)
  0.5, 0.8   # Verb -> Noun (0.5), Verb -> Verb (0.8)
), nrow = N, ncol = N, byrow = TRUE)
rownames(edge_pot) <- colnames(edge_pot) <- states


# =====================================================================
# 2. BRUTE-FORCE ALL PATHS (To find Global Normalization Z)
# =====================================================================

# Generate every possible sequence permutation (2^3 = 8 combinations)
all_paths <- expand.grid(W1 = 1:N, W2 = 1:N, W3 = 1:N)
all_paths$unnormalized_score <- 0

# Calculate the global exponential score for every path
for(i in 1:nrow(all_paths)) {
  p <- as.integer(all_paths[i, 1:3])
  
  # Sum of state features
  state_score <- node_pot[p[1], 1] + node_pot[p[2], 2] + node_pot[p[3], 3]
  
  # Sum of transition features
  trans_score <- edge_pot[p[1], p[2]] + edge_pot[p[2], p[3]]
  
  # Total potential is exp(state_features + transition_features)
  all_paths$unnormalized_score[i] <- exp(state_score + trans_score)
}

# Z(X) is the global sum of all sequence scores combined
Z_X <- sum(all_paths$unnormalized_score)
all_paths$probability <- all_paths$unnormalized_score / Z_X

# Display probabilities for all possible sequences
cat("--- ALL SEQUENCE PROBABILITIES ---\n")
for(i in 1:nrow(all_paths)) {
  path_names <- states[as.integer(all_paths[i, 1:3])]
  cat(paste(path_names, collapse = " -> "), ": ", round(all_paths$probability[i], 4), "\n")
}


# =====================================================================
# 3. GLOBAL INFERENCE VIA VITERBI ALGORITHM
# =====================================================================

# Viterbi DP matrix to store max log-potentials
viterbi <- matrix(0, nrow = N, ncol = T_len)
rownames(viterbi) <- states

# Backpointer matrix to track paths
backpointer <- matrix(0, nrow = N, ncol = T_len)

# Initialization step (t = 1)
viterbi[, 1] <- node_pot[, 1]

# Recursion step (t = 2 to T)
for (t in 2:T_len) {
  for (curr in 1:N) {
    # Calculate scores from all previous states to current state
    candidate_scores <- viterbi[, t-1] + edge_pot[, curr] + node_pot[curr, t]
    
    # Store maximum score and index of the best transition
    viterbi[curr, t] <- max(candidate_scores)
    backpointer[curr, t] <- which.max(candidate_scores)
  }
}

# Backtracking step to find the optimal path
best_path_idx <- integer(T_len)
best_path_idx[T_len] <- which.max(viterbi[, T_len])

for (t in (T_len-1):1) {
  best_path_idx[t] <- backpointer[best_path_idx[t+1], t+1]
}

cat("\n--- DECODING RESULT ---\n")
cat("The globally optimal sequence is:", paste(states[best_path_idx], collapse = " -> "), "\n")
