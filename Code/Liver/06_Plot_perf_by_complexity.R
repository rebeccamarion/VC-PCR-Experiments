#################################
#### Plot perf vs complexity ####
#################################

rm(list = ls())

library(ggplot2)
library(gridExtra)
library(data.table)
library(RColorBrewer)

data.name <- "Liver"
crit.calib <- "MSEP" # crit used to select hps
plot.crits <- c("MSEP")
plot.crit.names <- c("MSEP")
plot.error.bars <- F
s.true <- NULL
line.size <- 1.4
point.size <- 3
legend.text.size <- 15
legend.line.size <- 1.2
error.bar.line.size <- 1.6
xlims <- c(0, 30)

source("../../Scripts/Set_file_paths.R")
source("../../Scripts/Load_all_functions.R")

file.names <- list.files(paste0(data.path, "Eval/"))
file.names <- file.names[grep("eval_dfs", file.names)]


 
  load(file = paste0(data.path, "Eval_stats/",  "plot_data_all.RData"))

  data <- plot.data[grep(crit.calib, plot.data$crit), ]
  

  ylims <- lapply(plot.crits, function(x) c(0, max(data[, x], na.rm = T)))
  names(ylims) <- plot.crits
  
  # Reorder levels of method names
  method.names <- paste(data$method)
  unique.names <- unique(method.names)
  unique.names <- unique.names[c(1, 5, 3, 2, 4, 8:6)]
  #unique.names <- unique.names[(length(unique.names):1)]
  data$method <- factor(method.names, levels = unique.names)
  
  data$config <- paste0("Config ", data$data.index)
  lty.index <- rep(1, nrow(data))
  lty.index[grep("CRL", data$method)] <- 2
  lty.index[grep("VC-PCR", data$method)] <- 3
  data$lty.index <- factor(lty.index)
  
  col.vals.spec <- brewer.pal(11, "Spectral")
  col.vals2 <- col.vals.spec[c(1,3:4)] # VC-PCR
  col.vals3 <- col.vals.spec[9:11] # two-step
  col.vals1 <- brewer.pal(9, "Set1") # embedded methods
  col.vals <- c(col.vals1[c(8:9)],  col.vals3, col.vals2[1:3])
  lty.vals <- c(rep("dotted", 2), rep("dotdash", 3),  rep("solid", 3))
  
  data.old <- data
  
  
  for (crit.index in 1:length(plot.crits)){
    
    crit <- plot.crits[crit.index]
    data <- data.old
    stat <- paste0(data$crit)
    stat[which(stat == paste0(crit))] <- "mean"
    stat[which(stat == paste0("sd.", crit))] <- "sd"
    data$stat <- factor(stat)
    data <- data[which(data$stat == "mean"), ]
    
    
    crit.name <- plot.crit.names[crit.index]
    crit.file.name <- paste0(crit.name, collapse = "_")
    file.name <- paste0("../../Plots/", data.name, "/Performance/", "perf_by_sparsity_", crit.file.name, ".pdf")
   
    p1 <- ggplot(data = data[!is.na(data[, crit]), ],
                 aes_string(x = "s", y = crit, group = "method", lty = "method")) +
            
            geom_line(aes(color=method), size = line.size, lineend = "round") + 
            ylim(ylims[[crit.index]]) +
            coord_cartesian(xlim = xlims)+
            scale_linetype_manual(values = lty.vals) +
            scale_color_manual(values=col.vals)+
            guides(color = guide_legend(nrow=3,byrow=F, reverse = T), 
                   lty = guide_legend(nrow=3,byrow=F, reverse = T, override.aes = list(size = legend.line.size)))+
            #facet_wrap(~ stat) +
            theme_bw(base_size = 18) +
            labs(y = crit.name) +
            theme(legend.position="bottom", 
                  legend.text = element_text(size = legend.text.size),
                  legend.key.width = unit(0.7,"cm"))

    if (!is.null(s.true)){
      p1 <- p1 + geom_vline(xintercept = s.true)
    }
    
    if (plot.error.bars){
      p1 <- p1 +
        geom_errorbar(aes_string(ymin=paste0(crit, ".sd.min"), 
                                 ymax=paste0(crit, ".sd.max"), 
                                 color = "method"),
                      width=0, show.legend = F, size = error.bar.line.size, lty = 1)
    }
    
    pdf(file = file.name, width = 10, height = 6)
    print(p1)
    dev.off()
    
  }



# wh <- which(plot.data$method == "VC-PCR-Ridge" &
#             plot.data$data.index == 1 &
#             plot.data$crit == "MSEP")
# sub <- plot.data[wh, ]
# plot(sub$MSEP ~ sub$s)
# head(sub)
