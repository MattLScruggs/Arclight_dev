setwd("~/Arclight/R code and data")
library(tidyverse)

########## helper functions #############

#### make text data frame 

make_text_df<-function(directory_name='book_test'){
  
  directory_path<-paste0(getwd(),"/",directory_name)
  
  docs<-list.files(path=directory_path)
  
  clean_df<-data.frame(matrix(nrow=length(docs), ncol=2))
  colnames(clean_df)<-c("Doc","Text")
  
  for (i in 1:length(docs)){
    dtr<-docs[i]
    loc<-paste0(directory_path,"/",dtr)
    txt<-paste(readLines(loc), collapse = " ")
    clean_df[i,1]<-dtr
    clean_df[i,2]<-txt%>%
      str_replace_all("\\s+NA\\s+","")%>%
      str_replace_all("REFERENCES[:]?\\s+.*","")%>%
      str_replace_all("References[:]?\\s+.*","")
  }
  
  return(clean_df)
  
}

#### split source text into sentences and label blocks 

split_sentences<-function(input_text, id_col, block_size){
  ###testing block
  #input_text<-text_col
  #id_col<-doc_col
  
  sent_patt<-"(?<=[.?!])\\s+(?=[A-Z])"
  #sent_patt<-"[.?!:]\\s+(?=[A-Z])"
  noise_patt<-"[.?!:][0-9]+[,-]?[0-9]*"
  
  
  new_df<-data.frame()
  
  for (i in 1:length(input_text)){
    doc_id<-id_col[i]
    abs_text<-input_text[i]
    
    
    
    doc_df<-abs_text%>%
      #str_replace_all('\\\\','"')%>%
      str_split(sent_patt)%>%
      unlist()%>%
      as.data.frame()%>%
      mutate(doc = doc_id)
    
    new_df<-rbind(new_df, doc_df)
  }
  
  new_df$line<-paste0("line_",seq_along(1:nrow(new_df)))
  names(new_df)[names(new_df)== "."]<-"sentence"
  new_df$row_nr<-seq_along(1:nrow(new_df))
  new_df$block<-paste0("block_",floor(new_df$row_nr/block_size))
  #new_df$sentence<-apply(new_df$sentence,1,function(x) paste0(x,"."))
  
  return(new_df)
}

#### Make TFIDF matrix 

make_tfidf_quickly<-function(sentence_col, block_col){
  
  #### split out words and create key value triplets
  punct_patt<-"[^A-Za-z0-9\\s]+"
  
  word_list <- strsplit(sentence_col, "\\s+")
  sentence_id <- rep(block_col, lengths(word_list))   
  words       <- unlist(word_list, use.names = FALSE)%>%
    str_to_lower()%>%
    str_replace_all(punct_patt, "")
  
  stop_words<-read.csv("custom_stop_words.csv") #must be located in same working directory
  stop_words_list<-unique(stop_words$word)%>%append("", after=0)
  stop_index<-which(words %in% stop_words_list)
  
  clean_words<-words[-stop_index]
  revised_blocks<-sentence_id[-stop_index]
  
  word_tbl<- table(revised_blocks, clean_words)
  
  nz<-which(word_tbl >0, arr.ind = TRUE)
  
  value<-word_tbl[nz] #count of word in given block
  row_idx   <- rownames(word_tbl)[nz[,1]] #non-zero blocks
  col_idx <- colnames(word_tbl)[nz[,2]]  #non-zero words
  
  
  #number each unique term
  terms<- sort(unique(col_idx))            
  term_id<- setNames(seq_along(terms), terms)
  col_num<- term_id[col_idx]             
  
  #calc doc freq- how many blocks does each term appear in?
  df <- tabulate(col_num, nbins = length(terms))
  
  
  N   <- length(unique(row_idx))
  idf <- log((N + 1) / (df + 1)) + 1
  block_names<-unique(row_idx)
  
  tfidf_val <- value * idf[col_num] #each non-zero term gets an idf value
  
  tfidf_mat <- matrix(0,
                      nrow = N,
                      ncol = length(terms),
                      dimnames = list(block_names, terms))
  idx<-cbind(match(row_idx, block_names),
             match(col_idx, terms))
  
  
  tfidf_mat[idx]<-tfidf_val
  
  vocab<-terms
  
  idf_data<-idf
  names(idf_data)<-terms
  
  return(list(tfidf=tfidf_mat,vocab=terms,idf=idf_data))
  
  
}

#### Revised Embedding Functions 

power_iter_custom <- function(mult_fun, dim, max.it = 100, eps = 1e-10) {
  # Initialise a random unit vector of length 'dim'
  b <- rnorm(dim)
  b <- b / sqrt(sum(b^2))
  
  for (k in seq_len(max.it)) {
    # ---- custom multiplication ----
    b_new <- mult_fun(b)                 # returns a vector of length 'dim'
    
    # Normalise
    norm_b <- sqrt(sum(b_new^2))
    if (norm_b == 0) break               # zero eigenvalue reached
    b_new <- as.vector(b_new / norm_b)
    
    # Convergence test
    if (sqrt(sum((b_new - b)^2)) < eps) break
    b <- b_new
  }
  
  list(vec = b,          # unit eigenvector
       norm = norm_b,    # |λ|  (Rayleigh quotient)
       it   = k)
}

make_deflated_mult <- function(A, prev_vs, prev_lambdas) {
  # prev_vs   : list of previously extracted eigenvectors (each length p)
  # prev_lambdas : numeric vector of corresponding eigenvalues (λ = σ²)
  function(x) {
    # Core term: Aᵀ (A x)
    y <- A %*% x                # size m
    out <- as.vector(t(A) %*% y)  # size p
    
    # Subtract all rank‑1 contributions accumulated so far
    if (length(prev_vs) > 0) {
      for (j in seq_along(prev_vs)) {
        vj   <- prev_vs[[j]]
        lamj <- prev_lambdas[j]
        # λ_j * v_j * (v_jᵀ x)  =  (λ_j * (v_jᵀ x)) * v_j
        coeff <- lamj * sum(vj * x)   # scalar = λ_j * (v_jᵀ x)
        out   <- out - coeff * vj
      }
    }
    out
  }
}

extract_svd_power <- function(A, k.max = 5, max.it = 100, eps = 1e-12) {
  p <- ncol(A)                         # dimension of the Gram matrix C
  # Containers
  Vlist <- vector("list", k.max)       # right singular vectors (loadings)
  Ulist <- vector("list", k.max)       # left singular vectors (scores)
  sigma_vals <- numeric(k.max)
  
  # Keep track of previous eigenvectors/eigenvalues for deflation
  prev_vs   <- list()
  prev_lams <- numeric()
  
  for (j in seq_len(k.max)) {
    # Build the multiplication routine for the *current* deflated operator
    mult_fun <- make_deflated_mult(A, prev_vs, prev_lams)
    
    # Power iteration on the implicit C
    pw <- power_iter_custom(mult_fun = mult_fun,
                            dim = p,
                            max.it = max.it,
                            eps = eps)
    
    vj   <- pw$vec                     # loading (unit)
    lamj <- pw$norm                    # λ_j = σ_j²
    sigma_j <- sqrt(lamj)              # singular value
    
    # Left singular vector (scores)
    uj <- as.vector(A %*% vj) / sigma_j
    
    # Store results
    Vlist[[j]]   <- vj
    Ulist[[j]]   <- uj
    sigma_vals[j] <- sigma_j
    
    # Update deflation bookkeeping
    prev_vs[[j]]   <- vj
    prev_lams[j]   <- lamj
    
    # Optional early stop if singular value becomes tiny
    if (sigma_j < 1e-8) break
  }
  
  # Assemble matrices (columns = components)
  V_mat <- do.call(cbind, Vlist)
  U_mat <- do.call(cbind, Ulist)
  
  list(u = U_mat, d = sigma_vals, v = V_mat)
}





#### ---- ####

#### create vector DB function ####

create_vector_database<-function(text_input_type = c("table","directory"), location, block_size){

if (text_input_type=="table"){
  raw_text_df<-read.table(location,sep="|",  
                          header=TRUE, 
                          encoding = "UTF-8",
                          strip.white=TRUE,
                          quote = "\"",
                          allowEscapes = FALSE)
} else if (text_input_type=="directory"){
  raw_text_df<-make_text_df(location)
}

sentence_df<-split_sentences(input_text = raw_text_df$Text, id_col=raw_text_df$Doc, block_size=block_size)

tfidf_data<-make_tfidf_quickly(sentence_col=sentence_df$sentence, block_col=sentence_df$block)

tfidf_mat<-tfidf_data$tfidf
idf_data<-tfidf_data$idf

# how fine-grained do we want this?
k.max<-ceiling(log2(nrow(tfidf_mat)))

svd_test<-extract_svd_power(tfidf_mat,k.max=k.max)

loading_scores<-as.data.frame(svd_test$v)
plot(svd_test$d, type='l')
pc_df<-as.data.frame(svd_test$u %*% diag(svd_test$d))
pc_list<-paste0("PC",seq_along(1:length(svd_test$d)))

colnames(loading_scores)<-pc_list
rownames(loading_scores)<-colnames(tfidf_mat)

rownames(pc_df)<-rownames(tfidf_mat)
colnames(pc_df)<-pc_list

return(list(idf_data=idf_data, sentence_df=sentence_df, loading_scores=loading_scores, pc_df=pc_df))

}



#### query function ####

ask_query<-function(query, vector_database){
  
  ##Testing block
  #vector_database<-sml_db
  #query="what's the best strategy for dealing with multiple attackers?"
  
  punct_patt<-"[^A-Za-z0-9\\s]+"
  
  word_list <- strsplit(query, "\\s+")
  words<- unlist(word_list, use.names = FALSE)%>%
    str_to_lower()%>%
    str_replace_all(punct_patt, "")
  
  stop_words<-read.csv("custom_stop_words.csv") #must be located in same working directory
  stop_words_list<-unique(stop_words$word)%>%append("", after=0)
  stop_index<-which(words %in% stop_words_list)
  
  clean_words<-words[-stop_index]
  
  word_tbl<- table(clean_words)
  
  nz<-which(word_tbl >0, arr.ind = TRUE)
  
  value<-word_tbl[nz]
  
  idf_values<-as.numeric(vector_database$idf_data[names(vector_database$idf_data) %in% names(value)])
  
  load_matrix<-as.matrix(vector_database$loading_scores[rownames(vector_database$loading_scores) %in% names(value),])
  
  dot_prod<-idf_values %*% load_matrix
  
  dot_norm<-sum(dot_prod^2)
  
  sim_scores<-apply(vector_database$pc_df,1,function(x) {
    (dot_prod %*% x)/(sqrt(dot_norm)*sqrt(sum(x^2)))
  })
  
  threshold<-min(mean(sim_scores)+2*sd(sim_scores),0.85)
  
  rtn_blocks<-names(sim_scores)[which(sim_scores>=threshold)]
  
  rtn_df<-data.frame(matrix(NA, nrow=length(rtn_blocks), ncol=4))
  colnames(rtn_df)<-c("Block","Score","Source","Passage")
  
  
  
  for (i in 1:length(rtn_blocks)){
    rtn_df[i,1]<-rtn_blocks[i]
    rtn_df[i,2]<-sim_scores[names(sim_scores)==rtn_blocks[i]]
    doc_out<-vector_database$sentence_df$doc[which(vector_database$sentence_df$block==rtn_blocks[i])]
    rtn_df[i,3]<-doc_out[1]
    lines<-vector_database$sentence_df$sentence[which(vector_database$sentence_df$block==rtn_blocks[i])]
    rtn_df[i,4]<-paste(as.character(lines), collapse=" ")
  }
  
  rtn_df<-rtn_df[order(rtn_df$Source, rtn_df$Block, rtn_df$Score),]
  return(rtn_df)
}


#### Try it out ####
system.time(sml_db<-create_vector_database(text_input_type = "table",
                                           "other books.txt", 
                                           block_size =5))

qry_test<-""

answer<-ask_query(qry_test,sml_db)

write.table(answer, 
            file='who_test_log2.txt',
            sep = "|",
            row.names=FALSE,
            quote=TRUE)
