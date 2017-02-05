#WIP
We have two libraries that have some common territory.
- `refined`: enables compile-time constraints on parameters by refining the type.
- `singleton-ops`: enables compile-time operations and constraints on type arguments.

When is it best to use either? When is it best to use both?

---
####Just place ideas and thoughts here:
- @soronpo: The question can be asked in a larger scope, IMO. Now that SIP-23 enables literal types, when to use a type argument and when to use a parameter, assuming they are both known at compile-time.
- @soronpo: When your argument or parameter expresses a structure (e.g. length of a fixed-sized vector), use a type argument. When your argument or parameter expresses a value (e.g. values of the vector) use a parameter.
- @soronpo: **Example**. Say we want to describe a fixed sized vector structure that accepts only positive integer values. It will look like:
```
trait FixedSizeVector[Length] {
    //PositiveInt is a refined Integer parameter to accept only positive literals
    //Index[Length] is a user custom refined value to allow 0..Length-1 values.
    def assignValue(index : Index[Length], newValue : PositiveInt) : FixedSizeVector[Length]
}
object FixedSizeVector {
    //Length is a type argument constrained by singleton-ops
    implicit def apply[Length](implicit check: Require[Length > 0]) : FixedSizeVector[Length] = new FixedSizeVector[Length]
}
```
Length could have just as easily been a `PositiveInt` parameter.
However, in such a case, it would have been harder to limit `index` with `refined`. An implicit would have been required for the `Index` definition. 
- @soronpo: The example above demonstrates that whenever we have relational constraints between two parameters, it is better for that relation to be a type argument.
- @soronpo: Expression type argument literals for structure helps if we want to define implicit conversions between equivalent structures. **Example:**
```
trait Matrix[ColumnLength,RowLength]
```
We can have an implicit to convert `Matrix[ColumnLength,1] <--> FixedSizeVector[ColumnLength]`.
