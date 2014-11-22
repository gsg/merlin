- Add a global notion of cursor context (inside a type, an expression, a
  pattern, …)
- Insert a placeholder when completing without prefix, use the type of the
  placeholder to guide completion?
- Detect applications during completion, remind type of the function
- Compute dependency graph between cmis

- Add support for ppx
- Find proper API for incremental parser
  -> goto table should not be manipulated explicitly
  -> exceptions from semantic action should be caught and treated differently
  -> relying on special handling of stack bottom is not good either
     Bottom handling code improved, but we should make sure it s stable.
- Test:
  -> completion
  -> locate
  -> type enclosing / type expr
  -> occurrences
- Module constraint relaxation is wrong on functors argument. 
  Check how typer behaves with incorrect functors.

IDEA (later)
- perf: send batches of token rather than entering/exiting the whole
  parser barrier for each token
