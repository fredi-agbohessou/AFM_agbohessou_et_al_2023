# Uncertainty analysis  (DynACof)
# To what extent are greenhouse-gas emissions offset by trees in a Sahelian silvopastoral system? (Agbohessou et al 2023)
# y. agbohessou

empty!(names(Main))

using DynACof, CSV, CairoMakie, DataFrames, Query, Random, Distributions 
using Statistics
# Define the dynacof_uncertainty function
function dynacof_uncertainty(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
    # read tree parameters file
    file = "B.aegyptiaca/tree.jl"
    sc=read(file,String)

    # assign new values to the selected parameters
    mc = replace(sc,r"DELM_Tree\s*=\s*[\d.]+" => "DELM_Tree = $x1")
    mc = replace(mc,r"LAI_max_Tree\s*=\s*[\d.]+" => "LAI_max_Tree = $x2")
    mc = replace(mc,r"pruningIntensity_Tree\s*=\s*[\d.]+" => "pruningIntensity_Tree = $x3")
    mc = replace(mc,r"m_FRoot_Tree\s*=\s*[\d.]+" => "m_FRoot_Tree = $x4")
    mc = replace(mc,r"kres_max_Tree\s*=\s*[\d.]+" => "kres_max_Tree = $x5")
    mc = replace(mc,r"Res_max_Tree\s*=\s*[\d.]+" => "Res_max_Tree = $x6")
    mc = replace(mc,r"Kh\s*=\s*[\d.]+" => "Kh = $x7")
    mc = replace(mc,r"KhExp\s*=\s*[\d.]+" => "KhExp = $x8")
    mc = replace(mc,r"Kc\s*=\s*[\d.]+" => "Kc = $x9")
    mc = replace(mc,r"KcExp\s*=\s*[\d.]+" => "KcExp = $x10")

    # Write new tree parameters tree file
    open("B.aegyptiaca/tree3.jl", "w") do file
        write(file, mc)
    end

    # Execute the model using the new file for tree parameters
    Sim, Meteo, Parameters= dynacof(period=["1990-01-01", "2021-12-31"],
    input_path = "B.aegyptiaca", file_name= (constants= "constants.jl",
    site="site.jl",meteo="meteorology.txt",soil="soil.jl", 
    coffee="coffee.jl",tree="tree3.jl"))
    # subsetting
    Sim0 = Sim[!,[:date,:year,:Ra_Tree, :GPP_Tree]]
    Sim1 = Sim0[(Sim0.year .>= 2012) .& (Sim0.year .<= 2020), :]
    # compute annual sums 
    Sim2 = combine(groupby(Sim1, :year), :GPP_Tree => sum => :GPP_Tree_sum)
    # pivot wider Sim2
    Sim3 = stack(Sim2, Not(:year))
    Sim3 = unstack(Sim3, :year, :value)
    return (Sim3)
end

N = 1000
# Define uncertainty distributions for input parameters
Random.seed!(123)  # Set a random seed for reproducibility
x = randn(N)

# parms val (min-max) x +-20%
#x1 = DELM_Tree 778.5 (622.8-934.2)
xx1 = (x .- minimum(x)) ./ (maximum(x) - minimum(x)) .*(934.2 - 622.8) .+ 622.8
#x2 = LAI_max_Tree  0.17 (0.136-0.204)
xx2 = (x .- minimum(x)) ./ (maximum(x) - minimum(x)) .*(0.204 - 0.136) .+ 0.136
#x3 = pruningIntensity_Tree 0.55 (0.44-0.66)
xx3 = (x .- minimum(x)) ./ (maximum(x) - minimum(x)) .*(0.66 - 0.44) .+ 0.44
#x4= m_FRoot_Tree 0.005 (0.004-0.006)
xx4 = (x .- minimum(x)) ./ (maximum(x) - minimum(x)) .*(0.006 - 0.004) .+ 0.004
#x5 = kres_max_Tree 1.2 (0.96 1.44)
xx5 = (x .- minimum(x)) ./ (maximum(x) - minimum(x)) .*(1.44 - 0.96) .+ 0.96
#x6= Res_max_Tree 500.0 (400,600)
xx6 = (x .- minimum(x)) ./ (maximum(x) - minimum(x)) .*(600 - 400) .+ 400
#x7= Kh 0.46 (0.368 - 0.552)
xx7 = (x .- minimum(x)) ./ (maximum(x) - minimum(x)) .*(0.552 - 0.368) .+ 0.368
#x8=KhEx 0.5 (0.4-0.6)
xx8 = (x .- minimum(x)) ./ (maximum(x) - minimum(x)) .*(0.6 - 0.4) .+ 0.4
#x9= Kc 8.0 (6.4-9.6)
xx9 = (x .- minimum(x)) ./ (maximum(x) - minimum(x)) .*(9.6 - 6.4) .+ 6.4
#x10=KcExp 0.45 (0.36-0.54)
xx10 = (x .- minimum(x)) ./ (maximum(x) - minimum(x)) .*(0.54 - 0.36) .+0.36
plot(xx10)


# Initialize an empty DataFrame to store model outputs
model_outputs = DataFrame()

# Run Monte Carlo simulation
for i in 1:N
    # Sample parameter values from the distributions
    x1=round(xx1[i],digits=3)  
    x2=round(xx2[2],digits=3) 
    x3=round(xx3[i],digits=3)  
    x4=round(xx4[i],digits=3) 
    x5=round(xx5[i],digits=3)  
    x6=round(xx6[i],digits=3)  
    x7=round(xx7[i],digits=3)  
    x8=round(xx8[i],digits=3)  
    x9=round(xx9[i],digits=3)  
    x10=round(xx10[i],digits=3)  
    
    # Run the process-based model
    out = dynacof_uncertainty(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
  
    # Store the model output
    model_outputs= vcat(model_outputs, out)
end

println(first(model_outputs,50))

CSV.write("B.aegyptiaca/Uncertainty_GPP_Out_BA.csv",model_outputs)

model_outputs = CSV.File("Uncertainty_GPP_Out_BA.csv")


