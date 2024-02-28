# The lpSolveAPI package provides an R interface to 'lp_solve', a MILP solver
# with support for pure linear, integer/binary, semi-continuous and SOS models.
library(lpSolveAPI)

# Question-2
# a) Formulate an LP model for the factory that maximizes the profit, while
#    satisfying the demand and the cotton and wool proportion constraints.
# b) Solve the model using R/R Studio. Find the optimal profit and optimal
#    values of the decision variables.

# Creating a LP model for maximizing profit with 9 variables and 9 constraints
Factory_Model <- make.lp(9, 9)

# Setting the objective function "Maximize"
lp.control(Factory_Model, sense = "maximize")

# Setting the objective function
set.objfn(Factory_Model, c(15, 10, 25, 11, 6, 21, 15, 10, 25))

# Demand constraints
set.row(Factory_Model, 1, rep(1, 3), indices = c(1:3))
set.row(Factory_Model, 2, rep(1, 3), indices = c(4:6))
set.row(Factory_Model, 3, rep(1, 3), indices = c(7:9))

# Minimum cotton proportion constraints
set.row(Factory_Model, 4, c(0.5,-0.5,-0.5), indices = c(1:3))
set.row(Factory_Model, 5, c(0.4,-0.6,-0.6), indices = c(4:6))
set.row(Factory_Model, 6, c(0.5,-0.5,-0.5), indices = c(7:9))

# Minimum wool proportion constraints
set.row(Factory_Model, 7, c(-0.4, 0.6,-0.4), indices = c(1:3))
set.row(Factory_Model, 8, c(-0.4, 0.6,-0.4), indices = c(4:6))
set.row(Factory_Model, 9, c(-0.3, 0.7,-0.3), indices = c(7:9))

# Setting the right hand side values of the constraint
set.rhs(Factory_Model, c(4200, 3200, 3500, 0, 0, 0, 0, 0, 0))

# Setting constraint type
set.constr.type(Factory_Model,
                c("<=", "<=", "<=", ">=", ">=", ">=", ">=", ">=", ">="))

# Setting type of a decision variable
set.type(Factory_Model, c(1:9), "real")

# Setting lower and upper boundaries
set.bounds(Factory_Model,
           lower = rep(0, 9),
           upper = rep(Inf, 9))

# Setting proper names of constraints and decision variables
constraint_names = c(
  "Bloom Demand",
  "Amber Demand",
  "Leaf Demand",
  "Bloom-Cotton Prop",
  "Amber-Cotton Prop",
  "Leaf-Cotton Prop",
  "Bloom-Wool Prop",
  "Amber-Wool Prop",
  "Leaf-Wool Prop"
)

variable_names = c(
  "X11-CB",
  "X21-WB",
  "X31-NB",
  "X12-CA",
  "X22-WA",
  "X32-NA",
  "X13-CL",
  "X23-NL",
  "X33-NL"
)


#Adding them to the model
dimnames(Factory_Model) = list(constraint_names, variable_names)

solve(Factory_Model)

# Retrieving the value of the objective function from a lp model object
objvalue <- get.objective(Factory_Model)
objvalue # 141850

# Retrieving the values of the decision variables from a lp model object
solution <- get.variables(Factory_Model)
solution # 2100 1680 420 1920 1280 0 1750 1050 700


######################################################################

######################################################################

# Question-3
# (d) Construct a linear programming model for Company Sky in this game.
# (e) Produce an appropriate code to solve the linear programming model in part.
# (f) Solve the game for Sky using the linear programming model and the code you
#     constructed in parts (d) and (e). Interpret your solution.

# Creating a LP model for maximizing profit with 6 variables and 0 constraints
Bidding_Model <- make.lp(0, 6)

# Setting the objective function "Maximize"
lp.control(Bidding_Model, sense = "maximize")

# Setting the objective function
set.objfn(Bidding_Model, c(0, 0, 0, 0, 0, 1))

# If Giant chooses strategy 1, the expected payoff constraint is:
add.constraint(Bidding_Model, c(1, -1, -1, -1, -1, 1), "<=", 0)

# If Giant chooses strategy 2, the expected payoff constraint is:
add.constraint(Bidding_Model, c(1, 1, -1, -1, -1, 1), "<=", 0)

# If Giant chooses strategy 3, the expected payoff constraint is:
add.constraint(Bidding_Model, c(1, 1, 1, -1, -1, 1), "<=", 0)

# If Giant chooses strategy 4, the expected payoff constraint is:
add.constraint(Bidding_Model, c(1, 1, 1, 1, -1, 1), "<=", 0)

# If Giant chooses strategy 5, the expected payoff constraint is:
add.constraint(Bidding_Model, c(-1, -1, -1, -1, 1, 1), "<=", 0)

# Sum of the total probabilities constraint:
add.constraint(Bidding_Model, c(1, 1, 1, 1, 1, 0), "=", 1)

# Setting type of a decision variable
set.type(Bidding_Model, c(1:6), "real")

# Setting lower and upper boundaries
set.bounds(Bidding_Model,
           lower = c(0, 0, 0, 0, 0, -Inf),
           upper = rep(Inf, 6))

solve(Bidding_Model)

# Retrieving the value of the objective function from a lp model object
bidding_objvalue <- get.objective(Bidding_Model)
bidding_objvalue # 0

# Retrieving the values of the decision variables from a lp model object
bidding_solution <- get.variables(Bidding_Model)
bidding_solution # 0.5 0.0 0.0 0.0 0.5 0.0
