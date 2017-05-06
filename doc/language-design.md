# Nomad Language Design

## Design Decisions

### Primitive Types
* int: native signed integer (so 32 bits wide on a 32 bit system, 64 bits on a 64 bit system on)
* uint: native unsigned integer
* float: 64 bit floating point number
* char: [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value)  
* string: UTF-8 unicode string, stored as a length prefixed array in memory
* bool: Boolean type, possible values true or false

### Operators
* **+,** **-,**, **\*,** **/,**: applies to int, uint, float
* **%**: remainder applies to int and uint
* **<,** **>,** **>=,** **<=,**: numeric comparisons, applies to int, uint, float, char
* **==,** **!=,**: equals and not equals, applies to int, uint, float, char, string, bool,
* **&&,** **||,** **!,**: boolean _and_, _or_ and _not_ operators 
