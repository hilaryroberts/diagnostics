require(ggplot2)


diagnose <- function(diagnostics, scores, response, segs, title, rsltn = 1000, theme = 'none', diagonal = FALSE) {
    diagnostics <- substitute(diagnostics)
    if (!is.symbol(diagnostics)) {
        xfun <- deparse(diagnostics[[3]])
        yfun <- deparse(diagnostics[[2]])
    } else if (deparse(diagnostics) %in% c('ROC', 'ReceiverOperatingCharacteristic')) {
        xfun = 'FalsePositiveRate'
        yfun = 'TruePositiveRate'
        if (missing(diagonal)) diagonal = TRUE
    } else if (deparse(diagnostics) == 'PR') {
        xfun = 'Recall'
        yfun = 'Precision'
    } else stop('Diagnostic not recognised')
    
    
    TruePositiveRate <- function(x) mean(x[response], na.rm = TRUE)
    Sensitivity <- Recall <- TPR <- TruePositiveRate
    TrueNegativeRate <- function(x) mean(!x[!response], na.rm = TRUE)
    Specificity <- TNR <- TrueNegativeRate
    FalseNegativeRate <- function(x) mean(!x[response], na.rm = TRUE)
    MissRate <- FNR <- FalseNegativeRate
    FalsePositiveRate <- function(x) mean(x[!response], na.rm = TRUE)
    FallOut <- FPR <- FalsePositiveRate
    
    Precision <- function(x) mean(response[x], na.rm = TRUE)
    PPV <- PositivePredictiveValue <- Precision
    FalseDiscoveryRate <- function(x) mean(!response[x], na.rm = TRUE)
    FDR <- FalseDiscoveryRate
    FalseOmissionRate <- function(x) mean(response[!x], na.rm = TRUE)
    FOR <- FalseOmissionRate
    NegativePredictiveValue <- function(x) mean(!response[!x], na.rm = TRUE)
    NPV <- NegativePredictiveValue
    
    PositiveLikelihoodRatio <- function(x) TPR(x)/FPR(x)
    PLR <- PositiveLikelihoodRatio
    NegativeLikelihoodRatio <- function(x) FPR(x)/TPR(x)
    NLR <- NegativeLikelihoodRatio
    DiagnosticOddsRatio <- function(x) PLR(x)/NLR(x)
    DOR <- DiagnosticOddsRatio
    

    Accuracy <- function(x) mean(x == response, na.rm = TRUE)
    Cutoff <- function(x) 1
    Threshold <- Cutoff       
            
            
    get_rates <- function(scores, response){
        xs <- c()
        ys <- c()
        for (i in 0:rsltn) {
            interval <- 1/rsltn
            thresh <- interval * i
            preds <- scores > thresh
            xs[i] <- do.call(xfun, list(preds))
            if (xfun %in% c('Threshold', 'Cutoff')) xs[i] <- thresh
            ys[i] <- do.call(yfun, list(preds))
            if (yfun %in% c('Threshold', 'Cutoff')) ys[i] <- thresh
        }
        plotframe <-  data.frame(xs, ys)
        colnames(plotframe) <- c(xfun, yfun)
        plotframe <- plotframe[!apply(plotframe, MARGIN = 1, FUN = function(x) any(is.na(x))),]
        plotframe
    }
    
    if (missing(segs)) {
        plotdata <- get_rates(scores, response = response)
        outplot <- ggplot(plotdata, aes_string(xfun, yfun)) +
            geom_path(size = 2) +
            coord_cartesian(xlim = c(0,1),ylim = c(0,1))
    } else {
        sscores <- split(scores, segs)
        sresp <- split(response, segs)
        plotdata <- mapply(sscores, sresp, FUN = get_rates, SIMPLIFY = FALSE)
        plotdata <- do.call(rbind, plotdata)
        plotdata$segment <- gsub('\\.[0-9]*$', '', row.names(plotdata))
        outplot <- ggplot(plotdata, aes_string(xfun, yfun, group = 'segment', color = 'segment')) +
            geom_line(size = 2) +
            coord_cartesian(xlim = c(0,1),ylim = c(0,1))
    }

    if (diagonal) outplot <- outplot + geom_abline()
    if (max(plotdata[xfun], na.rm = TRUE) > 1 | min(plotdata[xfun], na.rm = TRUE) < 0) outplot$coordinates$limits$x <- NULL
    if (max(plotdata[yfun], na.rm = TRUE) > 1 | min(plotdata[yfun], na.rm = TRUE) < 0) outplot$coordinates$limits$y <- NULL
    if (!missing(title)) outplot <- outplot + ggtitle(title)
    if (theme == 'neo') {
        if (missing(segs)) outplot$layers[[1]]$aes_params$colour <- deep
        if (diagonal) outplot$layers[[2]]$aes_params$colour <- high
        outplot <- outplot + neo
    } else if (theme != 'none') {
        outplot <- outplot + theme
    }

    outplot
    
}
