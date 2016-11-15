  
library(shiny)
require(mvtnorm)
require(pander)
require(markdown)
require(stringr)
library(ReporteRs)

source('source-code/functions.R')

shinyServer(function(input, output) {
  
  output$htmltable <- renderUI({
    # define CSS tags
    input$recomp | input$recomp2
    
    css <- "#bggrn {background-color: #A6D785;}"
    
    n <- compute_n(input$n)
    
    out <- matrix(5, nr = length(n), nc = 9)
    colnames(out) <- colnames_gen_full(input)
    
  
    for (ii in 1:length(n)){
      out[ii, ] <- evalss(cv = isolate(input$cv),
                          dropout = isolate(input$dropout), 
                          n = n[ii], 
                          alpha = isolate(input$alpha)/2,
                          n.sim = isolate(input$nsim),
                          minpp = isolate(input$minpp), 
                          tarpp = isolate(input$tarpp),
                          th.stop = isolate(input$th.stop),
                          th.go = isolate(input$th.go),
                          power.shade = isolate(input$power.ctrl))
      
      if(isolate(input$add.interim) == "yes"){
   
        h0 <- joint_fp_fn(int.corr = isolate(input$int.corr), 
                          cv = isolate(input$cv), 
                          cv.int = isolate(input$cv.int), 
                          nsim = isolate(input$nsim), 
                          effect.size = 0, 
                          n.full = n[ii], 
                          n.int = isolate(input$n.int), 
                          dropout = isolate(input$dropout.int),
                          dropout.int = isolate(input$dropout.int),
                          th.stop.int = isolate(input$th.stop.int), 
                          th.go.int = isolate(input$th.go.int), 
                          th.stop = isolate(input$th.stop), 
                          th.go = isolate(input$th.go),
                          comp = "fp")
        
        h1 <- joint_fp_fn(int.corr = isolate(input$int.corr), 
                          cv = isolate(input$cv), 
                          cv.int = isolate(input$cv.int), 
                          nsim = isolate(input$nsim), 
                          effect.size = isolate(input$tarpp), 
                          n.full = n[ii], 
                          n.int = isolate(input$n.int), 
                          dropout = isolate(input$dropout.int),
                          dropout.int = isolate(input$dropout.int),
                          th.stop.int = isolate(input$th.stop.int), 
                          th.go.int = isolate(input$th.go.int), 
                          th.stop = isolate(input$th.stop), 
                          th.go = isolate(input$th.go),
                          comp = "fn")
        
        out[ii, c(6, 8)] <- h0
        out[ii, c(7, 9)] <- h1
        
        out[ii, 4] <- joint_fp_fn(int.corr = isolate(input$int.corr), 
                                  cv = isolate(input$cv), 
                                  cv.int = isolate(input$cv.int), 
                                  nsim = isolate(input$nsim), 
                                  effect.size = isolate(input$minpp), 
                                  n.full = n[ii], 
                                  n.int = isolate(input$n.int), 
                                  dropout = isolate(input$dropout), 
                                  dropout.int = isolate(input$dropout.int),
                                  th.stop.int = isolate(input$th.stop.int), 
                                  th.go.int = isolate(input$th.go.int), 
                                  th.stop = isolate(input$th.stop), 
                                  th.go = isolate(input$th.go),
                                  alpha = isolate(input$alpha),
                                  comp = "pwr")    
        
        out[ii, 5] <-  joint_fp_fn(int.corr = isolate(input$int.corr), 
                                   cv = isolate(input$cv), 
                                   cv.int = isolate(input$cv.int), 
                                   nsim = isolate(input$nsim), 
                                   effect.size = isolate(input$tarpp), 
                                   n.full = n[ii], 
                                   n.int = isolate(input$n.int), 
                                   dropout = isolate(input$dropout), 
                                   dropout.int = isolate(input$dropout.int),
                                   th.stop.int = isolate(input$th.stop.int), 
                                   th.go.int = isolate(input$th.go.int), 
                                   th.stop = isolate(input$th.stop), 
                                   th.go = isolate(input$th.go),
                                   alpha = isolate(input$alpha),
                                   comp = "pwr")   
      }
      
    }
    
    tab <- round(out, 2)
    
    if (input$shading.switch == 1){    
      tab[which(tab[, 2] <= isolate(input$minpp)), 2] <- paste(tab[which(tab[, 2] <= isolate(input$minpp)), 2], "#bggrn") # mdd
      tab[which(tab[, 3] <= isolate(input$tarpp)), 3] <- paste(tab[which(tab[, 3] <= isolate(input$tarpp)), 3], "#bggrn") # es80
      tab[which(tab[, 4] >= isolate(input$power.ctrl)), 4] <- paste(tab[which(tab[, 4] >= isolate(input$power.ctrl)), 4], "#bggrn") # power @ mpp
      tab[which(tab[, 5] >= isolate(input$power.ctrl)), 5] <- paste(tab[which(tab[, 5] >= isolate(input$power.ctrl)), 5], "#bggrn") # power @ tpp
      tab[which(tab[, 6] <= isolate(input$fp.ctrl)), 6] <- paste(tab[which(tab[, 6] <= isolate(input$fp.ctrl)), 6], "#bggrn")       # fp @ stop
      tab[which(tab[, 7] <= isolate(input$fn.ctrl)), 7] <- paste(tab[which(tab[, 7] <= isolate(input$fn.ctrl)), 7], "#bggrn")       # 
      tab[which(tab[, 8] <= isolate(input$fp.ctrl)), 8] <- paste(tab[which(tab[, 8] <= isolate(input$fp.ctrl)), 8], "#bggrn")
      tab[which(tab[, 9] <= isolate(input$fn.ctrl)), 9] <- paste(tab[which(tab[, 9] <= isolate(input$fn.ctrl)), 9], "#bggrn")
    }
    
    tab <- filter_cols(input, tab)

    # generate html table with pander package and markdown package
    htmltab <- markdownToHTML(
      text=pandoc.table.return(
        tab, 
        style="rmarkdown", split.tables=Inf
      ), 
      fragment.only=TRUE
    ) 
    colortable(htmltab, css)
  })
  
  output$interim.table <- renderUI({
    
    input$recomp | input$recomp2
    css <- "#bggrn {background-color: #A6D785;}"

    if(isolate(input$add.interim) == "yes"){
      
      n <- isolate(input$n.int)
      
      out <- matrix(nr = length(n), nc = 9)
      colnames(out) <- colnames_gen_int(input)
      
      for (ii in 1:length(n)){
        out[ii, ] <- evalss(cv = isolate(input$cv.int),
                            dropout = isolate(input$dropout), 
                            n = n[ii], 
                            alpha = isolate(input$alpha.int)/2,
                            n.sim = isolate(input$nsim),
                            minpp = isolate(input$minpp), 
                            tarpp = isolate(input$tarpp),
                            th.stop = isolate(input$th.stop.int),
                            th.go = isolate(input$th.go.int),
                            power.shade = isolate(input$power.ctrl))
      }
      
      tab <- round(out, 2)
      
      if (input$shading.switch == 1){    
        tab[which(tab[, 2] <= isolate(input$th.stop)), 2] <- paste(tab[which(tab[, 2] <= isolate(input$th.stop.int)), 2], "#bggrn") # mdd
        tab[which(tab[, 3] <= isolate(input$tarpp)), 3] <- paste(tab[which(tab[, 3] <= isolate(input$tarpp)), 3], "#bggrn") # es80
        tab[which(tab[, 4] >= (input$power.ctrl)), 4] <- paste(tab[which(tab[, 4] >= isolate(input$power.ctrl)), 4], "#bggrn") # power @ mpp
        tab[which(tab[, 5] >= (input$power.ctrl)), 5] <- paste(tab[which(tab[, 5] >= isolate(input$power.ctrl)), 5], "#bggrn") # power @ tpp
        tab[which(tab[, 6] <= (input$fp.ctrl)), 6] <- paste(tab[which(tab[, 6] <= isolate(input$fp.ctrl)), 6], "#bggrn")       # fp @ stop
        tab[which(tab[, 7] <= (input$fn.ctrl)), 7] <- paste(tab[which(tab[, 7] <= isolate(input$fn.ctrl)), 7], "#bggrn")       # 
        tab[which(tab[, 8] <= (input$fp.ctrl)), 8] <- paste(tab[which(tab[, 8] <= isolate(input$fp.ctrl)), 8], "#bggrn")
        tab[which(tab[, 9] <= 0.5), 9] <- paste(tab[which(tab[, 9] <= 0.5), 9], "#bggrn")
      }
      
      tab <- filter_cols(input, tab)
      
      # generate html table with pander package and markdown package
      htmltab <- markdownToHTML(
        text=pandoc.table.return(
          tab, 
          style="rmarkdown", split.tables=Inf
        ), 
        fragment.only=TRUE
      ) 
      colortable(htmltab, css)
      
    }
    
    
  })

  output$interim.title <- renderUI({
    
    input$recomp | input$recomp2
    
    if (isolate(input$add.interim) == 'yes'){
      title <- "Interim"
    }else{
      title <- ""
    }
    
    h3(title)
  })
  
  
  output$ss.rept <- downloadHandler(
      filename = function() {
        paste0(input$study.title, "_", Sys.Date(), '.pptx')
      },
      content = function(file) {
        out <- pptx( )
        
        # title slide
        out <- addSlide(out, slide.layout = "Title Slide")
        out <- addTitle( out, input$study.title ) #set the main title
        out <- addSubtitle( out , input$study.author) #set the sub-title
        
        if (input$add.interim == "yes"){
          
          # interim assumptions slide
          out <- addSlide(out, slide.layout = "Title and Content")
          out <- addTitle(out, "Interim Assumptions")
          
          assmp.txt <- c(paste0("Target PP = ", input$tarpp),
                         paste0("Minimum PP = ", input$minpp),          
                         paste0("Coefficient of Variation (CV) = ", input$cv.int),
                         paste0("Correlation with End-of-Study Read Out = ", input$int.corr),
                         paste0("Significance testing using two-sided t-test with alpha = ", input$alpha.int),
                         paste0("Stop Threshold = ", input$th.stop.int),
                         paste0("Go Threshold = ", input$th.go.int)
          )
          
          out <- addParagraph(out, value = assmp.txt)
          
          # interim results slide          
          out <- addSlide(out, slide.layout = "Title and Content")
          out <- addTitle(out, "Interim Results")
          
          out.mat <- evalss(cv = isolate(input$cv.int),
                            dropout = isolate(input$dropout), 
                            n = isolate(input$n.int), 
                            alpha = isolate(input$alpha.int)/2,
                            n.sim = isolate(input$nsim),
                            minpp = isolate(input$minpp), 
                            tarpp = isolate(input$tarpp),
                            th.stop = isolate(input$th.stop.int),
                            th.go = isolate(input$th.go.int),
                            power.shade = isolate(input$power.ctrl))
          
          out.mat <- as.data.frame(t(as.data.frame(round(out.mat, 2))))
          colnames(out.mat) <- colnames_gen_full(input)
          tab.flex <- FlexTable(filter_cols(input, out.mat))
          out <- addFlexTable(out, tab.flex)
        }
        
        
        # full study assumptions slide
        out <- addSlide(out, slide.layout = "Title and Content")
        out <- addTitle(out, "Full Study Assumptions")
        
        assmp.txt <- c(paste0("Target PP = ", input$tarpp),
                       paste0("Minimum PP = ", input$minpp),          
                       paste0("Coefficient of Variation (CV) = ", input$cv),
                       paste0("Dropout Rate = ", input$dropout),
                       paste0("Significance testing using two-sided t-test with alpha = ", input$alpha),
                       paste0("Stop Threshold = ", input$th.stop),
                       paste0("Go Threshold = ", input$th.go)
                       )
        
        out <- addParagraph(out, value = assmp.txt)
        
        # full study results slide
        out <- addSlide(out, slide.layout = "Title and Content")
        out <- addTitle(out, "Full Study Results")
        
        n <- compute_n(input$n)
        
        out.mat <- matrix(nr = length(n), nc = 9)
        colnames(out.mat) <- colnames_gen_full(input)
        
        
        for (ii in 1:length(n)){
          out.mat[ii, ] <- evalss(cv = isolate(input$cv),
                              dropout = isolate(input$dropout), 
                              n = n[ii], 
                              alpha = isolate(input$alpha)/2,
                              n.sim = isolate(input$nsim),
                              minpp = isolate(input$minpp), 
                              tarpp = isolate(input$tarpp),
                              th.stop = isolate(input$th.stop),
                              th.go = isolate(input$th.go),
                              power.shade = isolate(input$power.ctrl))
        }
        
        out.mat <- round(out.mat, 2)
        tab <- filter_cols(input, out.mat)
        tab.df <- as.data.frame(tab)
        tab.flex <- FlexTable(tab.df)
        
        if (colnames(out.mat)[2] %in% tab.flex$col_id & input$shading.switch == 1){  # mdd
          col.ind <- which(tab.flex$col_id == colnames(out.mat)[2])
          tab.flex <- setFlexTableBackgroundColors(tab.flex, j = col.ind,
                                                   i = which(out.mat[, 2] <= isolate(input$th.stop)), 
                                                   colors = "#A6D785")
        }
        
        if (colnames(out.mat)[3] %in% tab.flex$col_id & input$shading.switch == 1){  # es80
          col.ind <- which(tab.flex$col_id == colnames(out.mat)[3])
          tab.flex <- setFlexTableBackgroundColors(tab.flex, j = col.ind,
                                                   i = which(out.mat[, 3] <= isolate(input$tarpp)), 
                                                   colors = "#A6D785")
        }
        
        if (colnames(out.mat)[4] %in% tab.flex$col_id & input$shading.switch == 1){  # power @ mpp
          col.ind <- which(tab.flex$col_id == colnames(out.mat)[4])
          tab.flex <- setFlexTableBackgroundColors(tab.flex, j = col.ind,
                                                   i = which(out.mat[, 4] >= isolate(input$power.ctrl)), 
                                                   colors = "#A6D785")
        }
        
        if (colnames(out.mat)[5] %in% tab.flex$col_id & input$shading.switch == 1){  # power at tpp
          col.ind <- which(tab.flex$col_id == colnames(out.mat)[5])
          tab.flex <- setFlexTableBackgroundColors(tab.flex, j = col.ind,
                                                   i = which(out.mat[, 5] >= isolate(input$power.ctrl)), 
                                                   colors = "#A6D785")
        }
        
        if (colnames(out.mat)[6] %in% tab.flex$col_id & input$shading.switch == 1){  # fpr at stop
          col.ind <- which(tab.flex$col_id == colnames(out.mat)[6])
          tab.flex <- setFlexTableBackgroundColors(tab.flex, j = col.ind,
                                                   i = which(out.mat[, 6] <= isolate(input$fp.ctrl)), 
                                                   colors = "#A6D785")
        }
        
        if (colnames(out.mat)[7] %in% tab.flex$col_id & input$shading.switch == 1){  # es80
          col.ind <- which(tab.flex$col_id == colnames(out.mat)[7])
          tab.flex <- setFlexTableBackgroundColors(tab.flex, j = col.ind,
                                                   i = which(out.mat[, 7] <= isolate(input$fn.ctrl)), 
                                                   colors = "#A6D785")
        }
        
        if (colnames(out.mat)[8] %in% tab.flex$col_id & input$shading.switch == 1){  # es80
          col.ind <- which(tab.flex$col_id == colnames(out.mat)[8])
          tab.flex <- setFlexTableBackgroundColors(tab.flex, j = col.ind,
                                                   i = which(out.mat[, 8] <= isolate(input$fp.ctrl)), 
                                                   colors = "#A6D785")
        }
        
        if (colnames(out.mat)[9] %in% tab.flex$col_id & input$shading.switch == 1){  # es80
          col.ind <- which(tab.flex$col_id == colnames(out.mat)[9])
          tab.flex <- setFlexTableBackgroundColors(tab.flex, j = col.ind,
                                                   i = which(out.mat[, 9] <= isolate(input$fn.ctrl)), 
                                                   colors = "#A6D785")
        }
 
        out <- addFlexTable(out, tab.flex)
        writeDoc(out , file)

        
      }
    )
    
})

