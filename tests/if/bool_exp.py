# TODO: Optimization Opportunity
# Indeed, explicate pass works like partial evaluator such that
# print(1 if (not False) and True else 0) will be optimized 
# to print(1)
# but ...
# x = (not False) and True
# print(1 if x else 0) 
# does not!

# x = (not False) and True
print(1 if (not False) and True else 0)