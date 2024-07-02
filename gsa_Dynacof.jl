# Global sensitivity analysis using Sobol Method (DynACof)
# To what extent are greenhouse-gas emissions offset by trees in a Sahelian silvopastoral system? (Agbohessou et al 2023)
# GlobalSensitivity pkg on Github https://github.com/SciML/GlobalSensitivity.jl
# y. agbohessou
empty!(names(Main))

# loading required packages 
using GlobalSensitivity, Statistics, CairoMakie, QuasiMonteCarlo, OrdinaryDiffEq, Plots, CSV, DataFrames, DynACof, DelimitedFiles

# Serial execution
# Define the dynacof_gsa function
function dynacof_gsa(x)
    # read tree parameters file
    file = "B.aegyptiaca/tree.jl"
    sc=read(file,String)
    x1=x[1]
    x2=x[2]
    x3=x[3]
    x4=x[4]
    x5=x[5]
    x6=x[6]
    x7=x[7]
    x8=x[8]
    x9=x[9]
    x10=x[10]

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
    # compute annual sums (for Ra and GPP)
    Sim2 = combine(groupby(Sim1, :year), :Ra_Tree => sum => :Ra_Tree_sum)
    Out_mean= Statistics.mean(Sim2.Ra_Tree_sum)
    return (Out_mean)
end

n = 10*10  # 10 * number of parameters

# parameters bounds 
# parms val (min-max) x +-20%
#x1 = DELM_Tree 778.5 (622.8-934.2)
#x2 = LAI_max_Tree  0.17 (0.136-0.204)
#x3 = pruningIntensity_Tree 0.55 (0.44-0.66)
#x4= m_FRoot_Tree 0.005 (0.004-0.006)
#x5 = kres_max_Tree 1.2 (0.96 1.44)
#x6= Res_max_Tree 500.0 (400,600)
#x7= Kh 0.46 (0.368 - 0.552)
#x8=KhEx 0.5 (0.4-0.6)
#x9= Kc 8.0 (6.4-9.6)
#x10=KcExp 0.45 (0.36-0.54)

# Define parameter bounds
lb = [622.8, 0.136, 0.44, 0.004, 0.96, 400.0, 0.368, 0.4, 6.4, 0.36] # lower bound
ub = [934.2, 0.204, 0.66, 0.006, 1.44, 600.0, 0.552, 0.6, 9.6, 0.54] # upper bound

sampler = SobolSample()
A, B = QuasiMonteCarlo.generate_design_matrices(n, lb, ub, sampler)

# run global sensitivity analysis
res1 = gsa(dynacof_gsa, Sobol(order = [0, 1, 2]), A, B)
#number of run = n*(k+2) ====> 10*10*(10+2) = 1200 (k= number of parms) 

# export results as CSV file
res= hcat(res1.ST, res1.S1)
res2=DataFrame(res,Symbol.(["ST", "S1"])) 
res2.parms= ["DELM_Tree","LAI_max_Tree","pruningIntensity_Tree","m_FRoot_Tree",
             "kres_max_Tree","Res_max_Tree","Kh","KhEx","Kc","KcExp"]

println(res2)
writedlm("gsa_Ra_Tree.csv", Iterators.flatten(([names(res2)], eachrow(res2))), ',')

# plot the first order and total order Sobol Indices for the parameters.
p1 = bar(res2.parms,res2.ST,title="Total Order Indices Ra_tree",legend=false)
p2 = bar(res2.parms,res2.S1,title="First Order Indices Ra_tree",legend=false)

