# The Efficiency of Functional Programs
## Strict evaluation
Operator ($!) takes argument f and x: forces x to be evaluated fully before f is applied.

Operator (!) precedes a constructor makes that constructor strict.
```haskell
-- All head of a list must be evaluated before its needed.
data List a = Cons !a List a | Nil

--Forces the tail of the list to be fully evaluated.
data List a = Cons a !(List a) | Nil
```
## Step-counting Analysis (Runtime)
1. For each function f, derived a step-counting version Tf applied to the same arguments:
- { f a1 a2 .. an = e} => {Tf a1 a2 .. an = 1 + T(e)}	, where T(e) is the cost of evaluating expression e; cost of the function call f is 1 unit of time + number of step required to evaluate function body.
- T(c) => 0	, where c is a constant
- T(v) => 0	, where v is a variable
- T(if a then b else c) => T(a) + (if a then T(b) else T(c))
- T(p a1 a2 .. an) => T(a1) + T(a2) .. + T(an)	    , where p is a primitive function call
- T(f a1 a2 .. an) => T(a1) + T(a2) .. + T(an) + Tf(a1 a2 .. an)
2. For recursive functions, find the structural property that complexity depends upon, the size.
3. Derived a closed expression in terms of input size based on step-counting functions T. Appendix B4
## Space Efficiency
Two types of space analysis:
- __Accumulated space analysis__: Cost of total of space units used if no garbage collection occurred.
- __Largest space analysis__: Cost is equal the largest amount of space units used during the reduction sequence. Garbage collection can be used.

Space leaks:
- Memory space leaks away invisibly.
- Memory used when it could've been avoided.
- Memory remained referenced although it could've been garbage collected.
## Program Transformation
__Burstall-Darlington Transformation system:__

Transforming recursive programs to more time efficient versions with a series of equal-to-equal substitutions: 
1. __Unfolding__: Substituting a function call for its body.
2. __Definition__: Introducing new equations based on known equations.
3. __Instantiation__: Creates a specialization of a given equation, by giving values to some variables.
4. __Folding__: Replace occurrence of right-hand side by the appropriate left-hand side definition.
5. __Abstraction__: Introduces local definition.
6. Applying known laws for operators.

__Tail recursivity optimization:__

Increases space efficiency by using an accumulating parameter.

Requirements:
- The function transformed must be tail-recursive.
- All parameters of a recursive call must be evaluated strictly, with ($!) operator. 
