######################################################################
# functions
#   customised histogram plotting function
plotHist <- function(dFrame, varName, xLab, pTitle, breakSeq){
    opar <- par(no.readonly = TRUE)
    par(las = 2)
    hist(
        dFrame[,varName],
        breaks = breakSeq,
        xlab = xLab,
        main = pTitle,
        col = rgb(0, 1, 0),
        border = 'white',
        xaxt = 'n'
    )
    abline(v = median(dFrame[,varName]), lty = 2)
    legend('topright', 'median', lty = 2, bty = 'n')
    axis(1, at = breakSeq)
    par(opar)
}

plotPredAct <- function(predicted, actual, pTitle, tickVector, tickLabels){
    opar <- par(no.readonly = TRUE)
    par(pty = 's')
    plot(
        predicted ~ actual,
        ylab = 'predicted',
        xlab = 'actual',
        main = pTitle,
        xaxt = 'n',
        yaxt = 'n',
        xlim = c(0, max(tickVector)),
        ylim = c(0, max(tickVector))
    )
    axis(1, at = tickVector, tickLabels)
    axis(2, at = tickVector, tickLabels)
    abline(a = 0, b = 1, col = 'red', lty = 2)
    par(opar)
}

