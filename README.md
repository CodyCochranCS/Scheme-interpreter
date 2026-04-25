# Scheme Interpreter

An attempt at making a parser and interpreter for Scheme, as practice for Haskell.

While there were many parsing libraries I could have used, I wanted to see how difficult it would be to build one from scratch.

## Current features

__**Parsable types**__:
- Integers
- Strings
- Lambdas
- Lists / pairs

__**Primitive functions**:__
- (*op* *integer1* *integer2*) : Comparison and arithmetic on Integers. Currently only implemented for two integers (non-variadic). Currently supported *op*s:
  - + : Add two integers
  - - : Subtract two integers
  - * : Multiply two integers
  - = : Check two integers for numeric equality
  - <= : Check if *integer1* is less than *integer2*
- (cons *a* *b*) : Create the cons pair (*a* . *b*)
- (car *pair*) : Gets the first value of a cons pair (or head of a list)
- (cdr *pair*) : Gets the second value of a cons pair (or tail of a list)
- (set-car! *pair* *value*) : Sets the first value of a cons pair (or head of a list)
- (set-cdr! *pair* *value*) : Sets the second value of a cons pair (or tail of a list)
- (read) : Runs the parser and creates a Scheme expression from the text. This can then be evaluated using the **eval** function.
- (write *expr*) : Prints *expr* in a form that would be valid to parse. (Strings are printed with quotation symbols.)
- (display *expr*) : Prints *expr*, but removes quotations from Strings.
- (newline) : Returns the newline character
- (values *val* ...) : Returns multiple values. Currently multiple values cannot be bound to anything, but the REPL will print each value on a new line if a function returns them.
- (apply *function* *arg_list*) : Calls *function* and supplies *arg_list* as arguments.
- (eval *expr* *env*) : Evaluates *expr*, providing *env* as the environment for it to use.

__**Special forms**__:
- (define *name* *value*) : Adds the identifier *name* to the local environment and binds *value* to it. 
- (set! *name* *value*) : Changes the binding of the most locally scoped identifier *name* to *value*.
- (if *condition* *ifTrue* *ifFalse*) : If *condition* is **false** *ifFalse* is returned, otherwise *ifTrue* is returned.
- (quote *expression*) : Returns *expression* unevaluated. Can be parsed as: '*expression*
- (lambda *params* *exprs* ...) : Creates a new function that, when called, creates a new local scope and binds the passed arguments to the name or names provided in *params* before evaluating *exprs* from left to right.
  - If *params* is a list of symbols, then each argument is bound to each symbol in the order that they are provided.
  - If *params* is a single symbol, then it gets bound a list containing the input arguments.
  - If *params* is a dotted list, then the last parameter gets bound a list containing all remaining arguments that weren't bound by the previous parameters.
- (call/cc *unary_function*) : Calls *unary_function*, passing the current continuation into it and returning the result of *unary_function*. If the continuation is ever called with an argument (either within *unary_function* or elsewhere if the continuation was saved to a variable using **define** or **set!**), execution will immediately return to the point where **call/cc** was made, but the continuation argument will be used instead.
- (get-environment) : Convenience method; gets the environment. Useful for the **eval** function, which requires an environment.

__**Evaluation features**__
- Recursion: Functions can call the name that they get bound to.
- Tail call recursion: If the final statement in a lambda body is a function, then the current environment frame will be replaced. (Performance tests seem to indicate that resource use is still high with deep recursions despite this. This is currently a bug that I will need to investigate.)
- Continuations: call/cc can be used to return to an earlier point in the evaluation.

Examples:
```
=> (define x 5)
=> x
5
=> (<= x 3)
#f
=> (define factorial
     (lambda (x)
       (define factorial_helper
         (lambda (x acc)
           (if (= x 0)
               acc
               (factorial_helper (- x 1) (* x acc)))))
       (helper x 1)))
=> (set! x (factorial x))
=> x
120
=> factorial
<Lambda_function>
=> (eval '(+ 3 4) (get-environment))
7
```


## Future plans

__**Prepare for macros**__
- Add "syntax objects" as a Scheme value type.
- Add syntax object wrapping / unwrapping functions. Insert between parser and evaluator.
  - Recognize **syntax** keyword. Do not unwrap (ie treat as **quoted** special form).

__**Add simplified macro expander**__
- Include hard-coded syntax transformers. (No ability to define syntaxes yet.)
  - Syntax transformers are just lambdas that transform a syntax object into a syntax object.
  - Hard-coded transformers will be simple: identity, or return constant syntax output.
- Expander looks up first identifier in expansion-time environment
  - If found:
    - Run evaluator on transformer, passing in rest of expression as syntax object.
    - Run expander on returned syntax object.
  - If not found:
    - Run expander on each subexpression.

__**Add syntax defining capability**__
- Add "define-syntax" to expander.
  - Run the expander on the body first.
  - Unwrap the syntax object of the body.
  - Save the definition to the expansion environment.

__**Add syntax pattern matching**__
- Create **syntax-case** special form
  - (syntax-case *stx* *constants* (*template* *return_val*) ...)
  - Test input syntax *stx* against *template* in each subexpression by walking both and binding patterns from the expression to names in the template.
  - On the first matching *template* pattern, modify the *return_val* syntax with pieces of the pattern.

__**Add variadic pattern matches**__
- Add handling for "..." patterns.
- This is going to be a nightmare.

__**Add Scheme macro definitions**__
- Add common macros: let, let*, letrec, or, and (plus others...?)

__**Add parsing for remaining types**__
- Fraction, complex numbers, floating points, chars

__**Add proper Scheme primitive functions**__
- Arithmetic, comparisons, list functions, string functions, etc.

