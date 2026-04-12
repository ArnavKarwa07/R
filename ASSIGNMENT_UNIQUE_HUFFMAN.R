

create_node <- function(char = NULL, freq = 0L, left = NULL, right = NULL) {
  list(char = char, freq = as.integer(freq), left = left, right = right)
}

# Manual sorted insertion by frequency (acts like a simple priority queue)
insert_sorted <- function(queue, node) {
  if (length(queue) == 0) {
    return(list(node))
  }
  
  pos <- 1L
  while (pos <= length(queue) && queue[[pos]]$freq <= node$freq) {
    pos <- pos + 1L
  }
  
  if (pos == 1L) {
    return(c(list(node), queue))
  }
  
  if (pos > length(queue)) {
    return(c(queue, list(node)))
  }
  
  c(queue[1:(pos - 1L)], list(node), queue[pos:length(queue)])
}

build_frequency_table <- function(text) {
  chars <- strsplit(text, split = "", fixed = TRUE)[[1]]
  freq <- list()
  
  for (ch in chars) {
    if (is.null(freq[[ch]])) {
      freq[[ch]] <- 1L
    } else {
      freq[[ch]] <- freq[[ch]] + 1L
    }
  }
  
  freq
}

build_huffman_tree <- function(text) {
  freq_table <- build_frequency_table(text)
  keys <- names(freq_table)
  
  if (length(keys) == 0) {
    stop("Input text is empty.")
  }
  
  queue <- list()
  for (k in keys) {
    queue <- insert_sorted(queue, create_node(char = k, freq = freq_table[[k]]))
  }
  
  # Special case: only one distinct character
  if (length(queue) == 1) {
    only <- queue[[1]]
    return(create_node(char = NULL, freq = only$freq, left = only, right = NULL))
  }
  
  while (length(queue) > 1) {
    left <- queue[[1]]
    right <- queue[[2]]
    queue <- queue[-c(1, 2)]
    
    parent <- create_node(
      char = NULL,
      freq = left$freq + right$freq,
      left = left,
      right = right
    )
    
    queue <- insert_sorted(queue, parent)
  }
  
  queue[[1]]
}

build_code_table <- function(node, prefix = "", table = list()) {
  if (is.null(node$left) && is.null(node$right)) {
    # For a single-character input, assign "0"
    table[[node$char]] <- ifelse(nchar(prefix) == 0, "0", prefix)
    return(table)
  }
  
  if (!is.null(node$left)) {
    table <- build_code_table(node$left, paste0(prefix, "0"), table)
  }
  
  if (!is.null(node$right)) {
    table <- build_code_table(node$right, paste0(prefix, "1"), table)
  }
  
  table
}

tree_depth <- function(node) {
  if (is.null(node)) {
    return(0L)
  }
  if (is.null(node$left) && is.null(node$right)) {
    return(1L)
  }
  1L + max(tree_depth(node$left), tree_depth(node$right))
}

collect_leaf_positions <- function(node, depth = 1L, x_counter = 1L, positions = list()) {
  if (is.null(node)) {
    return(list(positions = positions, x_counter = x_counter))
  }
  
  if (is.null(node$left) && is.null(node$right)) {
    key <- paste0("leaf_", depth, "_", x_counter)
    positions[[key]] <- list(node = node, x = as.numeric(x_counter), y = -as.numeric(depth))
    return(list(positions = positions, x_counter = x_counter + 1L))
  }
  
  left_out <- collect_leaf_positions(node$left, depth + 1L, x_counter, positions)
  right_out <- collect_leaf_positions(node$right, depth + 1L, left_out$x_counter, left_out$positions)
  right_out
}

assign_internal_positions <- function(node, depth = 1L, leaf_positions, idx = 1L, out = list()) {
  if (is.null(node)) {
    return(list(out = out, idx = idx))
  }
  
  if (is.null(node$left) && is.null(node$right)) {
    # Find this exact leaf in precomputed positions
    keys <- names(leaf_positions)
    for (k in keys) {
      if (identical(leaf_positions[[k]]$node, node)) {
        out[[paste0("node_", idx)]] <- list(node = node, x = leaf_positions[[k]]$x, y = leaf_positions[[k]]$y)
        return(list(out = out, idx = idx + 1L))
      }
    }
  }
  
  left_out <- assign_internal_positions(node$left, depth + 1L, leaf_positions, idx, out)
  right_out <- assign_internal_positions(node$right, depth + 1L, leaf_positions, left_out$idx, left_out$out)
  
  # Internal node x is midpoint of children x values
  left_x <- NA_real_
  right_x <- NA_real_
  if (!is.null(node$left)) {
    for (k in names(right_out$out)) {
      if (identical(right_out$out[[k]]$node, node$left)) {
        left_x <- right_out$out[[k]]$x
        break
      }
    }
  }
  if (!is.null(node$right)) {
    for (k in names(right_out$out)) {
      if (identical(right_out$out[[k]]$node, node$right)) {
        right_x <- right_out$out[[k]]$x
        break
      }
    }
  }
  
  node_x <- if (!is.na(left_x) && !is.na(right_x)) {
    (left_x + right_x) / 2
  } else if (!is.na(left_x)) {
    left_x
  } else {
    right_x
  }
  
  right_out$out[[paste0("node_", right_out$idx)]] <- list(
    node = node,
    x = node_x,
    y = -as.numeric(depth)
  )
  
  list(out = right_out$out, idx = right_out$idx + 1L)
}

find_node_coords <- function(positions, target_node) {
  for (k in names(positions)) {
    if (identical(positions[[k]]$node, target_node)) {
      return(c(x = positions[[k]]$x, y = positions[[k]]$y))
    }
  }
  c(x = NA_real_, y = NA_real_)
}

plot_huffman_tree <- function(tree, file_name = "huffman_tree.png", show_plot = interactive()) {
  depth <- tree_depth(tree)
  leaf_out <- collect_leaf_positions(tree)
  positioned <- assign_internal_positions(tree, 1L, leaf_out$positions)$out
  
  max_x <- max(vapply(positioned, function(p) p$x, numeric(1)), na.rm = TRUE)
  
  draw_tree <- function() {
    par(mar = c(2, 2, 4, 2))
    plot(
      NA,
      xlim = c(0, max_x + 1),
      ylim = c(-(depth + 1), -0.5),
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "",
      bty = "n",
      main = "Huffman Tree"
    )
    
    for (k in names(positioned)) {
      node_info <- positioned[[k]]
      node <- node_info$node
      parent_xy <- c(node_info$x, node_info$y)
      
      if (!is.null(node$left)) {
        left_xy <- find_node_coords(positioned, node$left)
        segments(parent_xy[1], parent_xy[2], left_xy["x"], left_xy["y"], lwd = 1.5)
        text((parent_xy[1] + left_xy["x"]) / 2, (parent_xy[2] + left_xy["y"]) / 2 + 0.15, "0", col = "blue")
      }
      
      if (!is.null(node$right)) {
        right_xy <- find_node_coords(positioned, node$right)
        segments(parent_xy[1], parent_xy[2], right_xy["x"], right_xy["y"], lwd = 1.5)
        text((parent_xy[1] + right_xy["x"]) / 2, (parent_xy[2] + right_xy["y"]) / 2 + 0.15, "1", col = "darkgreen")
      }
    }
    
    for (k in names(positioned)) {
      node_info <- positioned[[k]]
      node <- node_info$node
      x <- node_info$x
      y <- node_info$y
      
      is_leaf <- is.null(node$left) && is.null(node$right)
      point_col <- if (is_leaf) "tomato" else "lightblue"
      
      points(x, y, pch = 21, bg = point_col, cex = 1.8)
      
      label <- if (is_leaf) {
        leaf_char <- ifelse(node$char == " ", "[space]", node$char)
        paste0(leaf_char, " (", node$freq, ")")
      } else {
        paste0("* (", node$freq, ")")
      }
      text(x, y + 0.35, labels = label, cex = 0.85)
    }
  }
  
  if (!is.null(file_name) && nchar(file_name) > 0) {
    png(filename = file_name, width = 1400, height = 900)
    draw_tree()
    dev.off()
  }
  
  if (show_plot && interactive()) {
    dev.new(width = 12, height = 8)
    draw_tree()
  }
  
  if (!is.null(file_name) && nchar(file_name) > 0) {
    return(normalizePath(file_name, winslash = "/", mustWork = FALSE))
  }
  invisible(NULL)
}

plot_compression_comparison <- function(report, file_name = "compression_comparison.png", show_plot = interactive()) {
  draw_bar <- function() {
    values <- c(report$original_bits, report$compressed_bits)
    names(values) <- c("Original (ASCII)", "Huffman Encoded")
    cols <- c("gray70", "steelblue")
    
    bp <- barplot(
      values,
      col = cols,
      border = NA,
      ylim = c(0, max(values) * 1.2),
      main = "Compression Comparison",
      ylab = "Bits"
    )
    
    text(bp, values, labels = as.character(values), pos = 3, cex = 0.9)
    legend(
      "topright",
      legend = c(
        sprintf("Ratio: %.4f", report$compression_ratio),
        sprintf("Saved: %.2f%%", report$space_saved_percent)
      ),
      bty = "n"
    )
  }
  
  if (!is.null(file_name) && nchar(file_name) > 0) {
    png(filename = file_name, width = 1200, height = 800)
    par(mar = c(5, 5, 4, 2))
    draw_bar()
    dev.off()
  }
  
  if (show_plot && interactive()) {
    dev.new(width = 10, height = 7)
    par(mar = c(5, 5, 4, 2))
    draw_bar()
  }
  
  if (!is.null(file_name) && nchar(file_name) > 0) {
    return(normalizePath(file_name, winslash = "/", mustWork = FALSE))
  }
  invisible(NULL)
}

huffman_encode <- function(text, code_table) {
  chars <- strsplit(text, split = "", fixed = TRUE)[[1]]
  bits <- character(length(chars))
  
  for (i in seq_along(chars)) {
    bits[i] <- code_table[[chars[i]]]
  }
  
  paste0(bits, collapse = "")
}

huffman_decode <- function(bit_string, tree) {
  if (nchar(bit_string) == 0) {
    return("")
  }
  
  bits <- strsplit(bit_string, split = "", fixed = TRUE)[[1]]
  out <- character(0)
  current <- tree
  
  for (b in bits) {
    if (b == "0") {
      current <- current$left
    } else if (b == "1") {
      current <- current$right
    } else {
      stop("Bit string contains symbols other than 0 and 1.")
    }
    
    if (is.null(current$left) && is.null(current$right)) {
      out <- c(out, current$char)
      current <- tree
    }
  }
  
  paste0(out, collapse = "")
}

compression_report <- function(original_text, encoded_bits) {
  original_bits <- nchar(original_text) * 8L
  compressed_bits <- nchar(encoded_bits)
  ratio <- compressed_bits / original_bits
  savings <- (1 - ratio) * 100
  
  list(
    original_bits = original_bits,
    compressed_bits = compressed_bits,
    compression_ratio = ratio,
    space_saved_percent = savings
  )
}

# -------------------------------
# Demo
# -------------------------------
sample_text <- "huffman coding is a classic greedy algorithm for lossless data compression"

tree <- build_huffman_tree(sample_text)
code_table <- build_code_table(tree)
encoded <- huffman_encode(sample_text, code_table)
decoded <- huffman_decode(encoded, tree)
report <- compression_report(sample_text, encoded)
tree_plot_path <- plot_huffman_tree(tree, "huffman_tree.png", show_plot = TRUE)
comparison_plot_path <- plot_compression_comparison(report, "compression_comparison.png", show_plot = TRUE)

cat("Original Text:\n", sample_text, "\n\n", sep = "")
cat("Code Table (character -> bit code):\n")
for (k in names(code_table)) {
  display_key <- ifelse(k == " ", "[space]", k)
  cat(display_key, "->", code_table[[k]], "\n")
}

cat("\nEncoded Bit Length:", report$compressed_bits, "bits\n")
cat("Original Bit Length (ASCII):", report$original_bits, "bits\n")
cat(sprintf("Compression Ratio: %.4f\n", report$compression_ratio))
cat(sprintf("Space Saved: %.2f%%\n\n", report$space_saved_percent))

cat("Decoded Text:\n", decoded, "\n\n", sep = "")
cat("Decoding Correct:", identical(sample_text, decoded), "\n\n")
cat("Tree plot saved as:", tree_plot_path, "\n")
cat("Comparison plot saved as:", comparison_plot_path, "\n\n")

cat("Time Complexity:\n")
cat("1) Frequency table build: O(n)\n")
cat("2) Queue insertions + tree construction (this simple queue is linear insert): O(k^2)\n")
cat("3) Encoding: O(n)\n")
cat("4) Decoding: O(m), where m = number of encoded bits\n\n")

cat("Space Complexity:\n")
cat("1) Frequency table: O(k)\n")
cat("2) Huffman tree: O(k) nodes\n")
cat("3) Encoded output: O(m)\n")
cat("Overall auxiliary space: O(k + m)\n")
cat("(n = text length, k = number of distinct characters)\n")
