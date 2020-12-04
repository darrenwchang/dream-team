using Plots

function plotDiagonal(xmin, xmax)
    xsamples = [xmin, xmax]
    plot!(xsamples, xsamples, color = :red, linestyle = :dash, label = "model")
end

function plotdata(x, y, xname, yname; margin = 0.05, plotDiag = true, zeromin = false)
    scatter(x, y, label = "data")
    xlabel!(xname)
    ylabel!(yname)
    range_y = maximum(y) - minimum(y)
    range_x = maximum(x) - minimum(x)
    if plotDiag
        plotDiagonal(minimum(x) - margin*range_x, maximum(x) + margin*range_x)
    end
    if zeromin
        ylims!(0.0, maximum(y) + margin*range_y)
        xlims!(0.0, maximum(x) + margin*range_x)
    else
        ylims!(minimum(y) - margin*range_y, maximum(y) + margin*range_y)
        xlims!(minimum(x) - margin*range_x, maximum(x) + margin*range_x)
    end
end

function plot_pred_true(test_pred, test_y, max_points = 1000)
    plotdata(test_pred[1:max_points], test_y[1:max_points], 
    "Predicted (Projected Points)",
    "True (Actual Points)")
end