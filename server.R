library(shiny)
library(ggplot2)
library(ggsci)
library(ggpubr)

load("data/RUSP.SN.RData")

race.all <- c("Asian", "Black", "Hispanic", "White")

shinyServer(function(input, output) {
  
  output$plot <- renderPlot({
    gp <- NULL
    if(input$tabset == "show"){
      title = NULL
      meta <- NULL
      if(input$tabShow=="single"){
        idx <- which(meta.name==input$analytes) + 7
        meta <- control.clean[,idx]
        title =  paste0(input$analytes, " (\u03BCmol/L)")
      } else {
        numerator <- rep(0, nrow(control.clean))
        denominator <- numerator
        for(num.sel in input$numerator){
          numerator <- numerator + control.clean[, which(meta.name==num.sel)+7]
        }
        for(denom.sel in input$denominator){
          denominator <- denominator + control.clean[, which(meta.name==denom.sel) + 7]
        }
        denominator[denominator==0] <- 0.01
        meta <- numerator/denominator
        t1 <- paste(input$numerator, collapse = "+")
        if(length(input$numerator) > 1){
          t1 <- paste0("(", t1, ")")
        }
        t2 <- paste(input$denominator, collapse = "+")
        if(length(input$denominator) > 1){
          t2 <- paste0("(", t2, ")")
        }
        title = paste0(t1, "/", t2)
      }
      
      idx.sel <- rep(TRUE, nrow(control.clean))
      
      if(as.numeric(input$race) > 1){
        idx.sel <- idx.sel & control.clean$race == race.all[as.numeric(input$race)-1]
      }
      
      if(as.numeric(input$sex) == 2){
        idx.sel <- idx.sel & control.clean$sex == "M"
      } else if(as.numeric(input$sex) == 3){
        idx.sel <- idx.sel & control.clean$sex == "F"
      }
      
      if(as.numeric(input$aac)==2){
        idx.sel <- idx.sel & control.clean$Age_hr < 24
      } else if(as.numeric(input$aac)==3){
        idx.sel <- idx.sel & control.clean$Age_hr >= 24 & control.clean$Age_hr <=48
      } else if(as.numeric(input$aac)==4){
        idx.sel <- idx.sel & control.clean$Age_hr > 48
      }
      
      if(as.numeric(input$tpn)==2){
        idx.sel <- idx.sel & control.clean$TPN_HYPERAL==0
      } else if(as.numeric(input$tpn)==3){
        idx.sel <- idx.sel & control.clean$TPN_HYPERAL==1
      }
      
      val <- matrix(0, 5, 5)
      num <- matrix(0, 5, 5)
      for(i in 1:5){
        for(j in 1:5){
          idx.tmp <- idx.sel & idx.BW==i & idx.GA==j
          val[i,j] <- mean(meta[idx.tmp], na.rm = TRUE)
          num[i,j] <- sum(idx.tmp, na.rm = TRUE)
        }
      }
      
      dplot <- data.frame(
        GA = factor(rep(c("28-36", "37-38", "39-40", "41", "42"), each = 5), 
                    levels = c("28-36", "37-38", "39-40", "41", "42")),
        BW = factor(rep(c("1000-2499", "2500-3000", "3001-3500", "3501-4000","4001-5000"), 5),
                    levels =c("1000-2499", "2500-3000", "3001-3500", "3501-4000","4001-5000")),
        val = as.vector(val),
        text = paste0(format(round(as.vector(val), 2), nsmall = 2), "\n(n=",
                      num, ")")
      )
      
      mx <- max(as.vector(val), na.rm = TRUE)
      mn <- min(as.vector(val), na.rm = TRUE)
      
      gp.heat <- ggplot(dplot, aes(GA, BW)) + 
        geom_raster(aes(fill = val)) + 
        geom_text(aes(label = text), size=4) + 
        labs(x = "Gestational Age (week)", y = "Birth Weight (g)", title = title) + 
        theme_classic() + 
        scale_fill_gradientn(colours = c("#00A1D5FF", "#FFFFFFFF", "#B24745FF"), limits = c(mn, mx)) +
        geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5), size = 0.6, fill = NA, color="black") +
        theme(legend.title = element_blank(), text = element_text(size = 18), legend.position = "right")
      
      
      idx.sel.All <- idx.sel
      idx.sel.GA <- idx.sel & idx.GA==3
      
      dplot <- data.frame(
        meta = c(meta[which(idx.sel.All)], meta[which(idx.sel.GA)]),
        BW = c(control.clean$BW[which(idx.sel.All)], 
               control.clean$BW[which(idx.sel.GA)]),
        GA = c(rep("All", sum(idx.sel.All, na.rm = TRUE)),
               rep("GA: 39-40", sum(idx.sel.GA, na.rm = TRUE)))
      )
      
      
      gp.smooth <- ggplot(dplot) + 
        geom_smooth(aes(x=BW, y=meta, color=GA, fill = GA)) +
        labs(x="Birth Weight (g)", y=title) +
        scale_color_jama() + 
        theme_light() + 
        theme(legend.title = element_blank(),
              text = element_text(size=18),
              legend.position = c(0.75, 0.25))
      
      gp <- ggarrange(
        gp.heat, gp.smooth, NULL,
        ncol = 1, nrow = 3)
    } else if(input$tabset == "compare"){
      
    } else if(input$tabset == "about"){
      
    }
    gp
  }, width = 500, 
  height = 1200)
  
  
})
