
# == title
# plot Chord Diagram
#
# == param
# -mat a table which represents as a numeric matrix
# -grid.col colors for grids
# -transparency Transparency of link/ribbon colors, 0 means no transparency and 1 means complete transparency.
# -col colors for links. It can be a matrix which corresponds to ``mat``, or a function which generate colors 
#      according to values in ``mat``, or a single value which means colors for all links are the same.
# -row.col if ``col`` is not set, colors correspond to rownames
# -column.col if ``col`` is not set, colors correspond to rownames
# -directional whether links have direction. The direction is from rows to columns
# -symmetric whether the matrix is symmetric. If the value is set to ``TRUE1``, only
#            lower triangular matrix without the diagonal will be used.
# -order order of sectors
# -preAllocateTracks
# -annotationTrack
#
# == details
# http://circos.ca/intro/tabular_visualization/
chordDiagram = function(mat, grid.col = NULL, transparency = 0.5,
	col = NULL, row.col = NULL, column.col = NULL, directional = FALSE,
	symmetric = FALSE, order = NULL, preAllocateTracks = NULL,
	annotationTrack = c("name", "grid")) {
	
	if(symmetric) {
		if(nrow(mat) != ncol(mat)) {
			stop("`mat` should be a square matrix.\n")
		}
		
		for(i in 1:10) {
			n = sample(nrow(mat), 2)
			ir = n[1]
			ic = n[2]
			if(abs(mat[ir, ic] - mat[ic, ir]) > 1e-8) {
				stop("Is `mat` really a symmetric matrix?\n")
			}
		}
		mat[upper.tri(mat, diag = TRUE)] = 0
	}
	
	if(!is.null(order)) {
		if(is.null(rownames(mat)) || is.null(colnames(mat))) {
			stop("Since you specified `order`, your matrix should have rowname and colname.\n")
		}
		if(!setequal(order, union(rownames(mat), colnames(mat)))) {
			stop("Elements in `order` should be same as in `union(rownames(mat), colnames(mat))`.\n")
		}
	}
	
	ri = apply(mat, 1, function(x) any(abs(x) > 1e-8))
	ci = apply(mat, 2, function(x) any(abs(x) > 1e-8))
	
	mat = mat[ri, ci]
	if(is.matrix(col)) {
		col = col[ri, ci]
	}
	if(length(row.col) > 1) {
		row.col = row.col[ri]
	}
	if(length(column.col) > 1) {
		column.col = column.col[ci]
	}
	
	if(is.null(rownames(mat))) {
		rownames(mat) = paste0("R", seq_len(nrow(mat)))
	}
	if(is.null(colnames(mat))) {
		colnames(mat) = paste0("C", seq_len(ncol(mat)))
	}
	
	if(!is.null(order)) {
		order = order[order %in% union(rownames(mat), colnames(mat))]
	}

	rs = rowSums(abs(mat))
	cs = colSums(abs(mat))

	nn = union(names(rs), names(cs))
	xlim = numeric(length(nn))
	names(xlim) = nn
	
	if(!is.null(order)) {
		xlim = xlim[order]
	}
	
	xlim[names(rs)] = xlim[names(rs)] + rs
	xlim[names(cs)] = xlim[names(cs)] + cs
	
	factors = names(xlim)
	factors = factor(factors, levels = factors)
	xlim = cbind(rep(0, length(factors)), xlim)
	
	n = length(factors)
	if(is.null(grid.col)) {
		grid.col = rgb(cbind(runif(n), runif(n), runif(n)))
		names(grid.col) = factors
	}
	
	## make a color matrix based on settings
	if(!is.null(col)) {
		if(is.function(col)) {
			col = col(mat)
		} else if(is.matrix(col)) {
		
		} else if(length(col) == 1) {
			col = rep(col, length(mat))
		}
	} else if(!is.null(row.col)) {
		if(length(row.col) == 1) {
			row.col = rep(row.col, nrow(mat))
		}
		col = rep(row.col, ncol(mat))
	} else if(!is.null(column.col)) {
		if(length(column.col) == 1) {
			column.col = rep(column.col, ncol(mat))
		}
		col = rep(column.col, each = nrow(mat))
	} else {
		col = rep(grid.col[rownames(mat)], ncol(mat))
	}
	
	col = rgb(t(col2rgb(col)), maxColorValue = 255, alpha = (1 - transparency)*255)
	
	dim(col) = dim(mat)
	colnames(col) = colnames(col)
	rownames(col) = rownames(col)

	circos.par(cell.padding = c(0, 0, 0, 0))
    circos.initialize(factors = factors, xlim = xlim)
	
	# pre allocate track
	if(!is.null(preAllocateTracks)) {
		pa = parsePreAllocateTracksValue(preAllocateTracks)
		for(i in seq_along(pa)) {
			va = pa[[i]]
			circos.trackPlotRegion(ylim = va$ylim, track.height = va$track.height,
				bg.col = va$bg.col, bg.border = va$bg.border, bg.lty = va$bg.lty, bg.lwd = va$bg.lwd)
		}
	}
	if(any(annotationTrack %in% "name")) {
		circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA,
			panel.fun = function(x, y) {
				xlim = get.cell.meta.data("xlim")
				current.sector.index = get.cell.meta.data("sector.index")
				i = get.cell.meta.data("sector.numeric.index")
				theta = mean(get.cell.meta.data("xplot")) %% 360
				if(theta < 90 || theta > 270) {
					text.direction = "vertical_right"
					text.adj = c(0, 0.5)
				} else {
					text.direction = "vertical_left"
					text.adj = c(1, 0.5)
				}
				circos.text(mean(xlim), 0.5, labels = current.sector.index,
					direction = text.direction, adj = text.adj)
			}, track.height = 0.05)
    }
	if(any(annotationTrack %in% "grid")) {
		circos.trackPlotRegion(ylim = c(0, 1), factors = factors, bg.border = NA, 
			track.height = 0.05, panel.fun = function(x, y) {
				xlim = get.cell.meta.data("xlim")
				current.sector.index = get.cell.meta.data("sector.index")
				circos.rect(xlim[1], 0, xlim[2], 1, col = grid.col[current.sector.index], border = grid.col[current.sector.index])
			})
	}
    # links
    rn = rownames(mat)
	cn = colnames(mat)
    sector.sum.row = numeric(length(factors))
    sector.sum.col = numeric(length(factors))
	names(sector.sum.row) = factors
	names(sector.sum.col) = factors
	sector.sum.col[ names(rs) ] = rs
    for(i in seq_along(rn)) {
		for(j in rev(seq_along(cn))) {
			if(abs(mat[i, j]) < 1e-8) {
				next
			}
			rou = circlize:::get.track.end.position(circlize:::get.current.track.index())
            sector.index1 = rn[i]
            sector.index2 = cn[j]
            circos.link(sector.index1, c(sector.sum.row[ rn[i] ], sector.sum.row[ rn[i] ] + abs(mat[i, j])),
                        sector.index2, c(sector.sum.col[ cn[j] ], sector.sum.col[ cn[j] ] + abs(mat[i, j])),
                        col = col[i, j], rou = ifelse(directional, rou - 0.02, rou), border = NA)
			if(directional) {
				d1 = circlize(c(sector.sum.row[ rn[i] ], sector.sum.row[ rn[i] ] + abs(mat[i, j])), c(0, 0), sector.index = sector.index1)
				draw.sector(start.degree = d1[1, 1], end.degree = d1[2, 1], rou1 = rou, rou2 = rou - 0.02, col = col[i, j], border = "white", lwd = 0.2)
			}
            sector.sum.row[ rn[i] ] = sector.sum.row[ rn[i] ] + abs(mat[i, j])
			sector.sum.col[ cn[j] ] = sector.sum.col[ cn[j] ] + abs(mat[i, j])
        }
    }
}

# returns a list, each list containing settings for each new track
parsePreAllocateTracksValue = function(preAllocateTracks) {

}