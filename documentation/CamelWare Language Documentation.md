# CamelWare
Ocaml, bit by bit

Documentation of the `CamelWare` language

## Table of Contents
1. [Overview](#overview)

## Overview
`CamelWare` is a functional hardware description language for digital logic. It provides syntactical constructs that make it easy to compactly describe the construction of small to medium scale digital circuits and an interactive simulator for examining their behavior.

### Keywords
1. `input`
2. `output`
2. `register`
3. `falling`
4. `rising`
5. `fun`
6. `let ... in ...`
7. `if ... then ... else ...`

## Structure of a `CamelWare` program
A `CamelWare` program consists of a set of component definitions. These fall into four categories:

* Registers
* Inputs
* Outputs
* Subcircuits

The order of definitions does not matter to the compiler except in that if multiple definitions are given the same name then only the last one will be used.

## Registers, Inputs, and Outputs

The definition syntax has the same general form for registers, and outputs. It is:

```
definition := type name[length] = expression

type := [rising | falling] register | output
```

Registers and outputs are assigned a transition function (this is the `expression` after the `=`). Whenever it comes time to compute their next value this is evaluated. Rising registers become the result of applying their transition function to the current state (ie they step) when the clock changes from `0` to `1`. Falling registers step when the clock changes from `1` to `0`. Outputs reevaluate their transition function whenever anything else in the circuit changes (This essentially simulates instantaneous logic). Although you can specify `rising` if you wish, registers are assumed to be rising by default - you must add the optional `falling` keyword to their declaration to make them so.

Defining an input is simply:

```
input name[length]
```

## Transition Functions
`CamelWare` models circuits at the register transfer level. That is, circuit definitions don't consist of lists of individual gates - instead they consist of lists of registers and how they relate to each other. Each register is defined with a transition function, which specifies how its next value is computed each time it steps. A transition function can be thought of as implicitly defining the wires and gates neccesary to actually implement the computation in question in hardware. Each transition function is simply a logical expression defined in terms of the registers and inputs in the current state of the circuit

If the result of a transition function is longer than the length of the register to which it is to be assigned then it is implicitly truncated. If it is shorter it is zero extended.

## Constants

### Syntax
`CamelWare` allows referring to constants as hexadecimal, binary, and decimal numbers. The syntax for these is:
```
constant := binary | hexadecimal | decimal
binary := [length]`b bin_digit+
hexadecimal := [length]`x hex_digit+
decimal := [length]`d [-] dec_digit+
```

If the length is left unspecifed then the maximum possible length (32 bits) will be assumed. If the length is larger than the number of digits actually specified then the remaining digits will be assumed to be zero. If it is smaller then the most significant digits will be cut off to make it fit. Only decimal constants are allowed to be explicitly
negative. Binary and hexadecimal constants correspond more directly to the bits and can be made implicitly negative by specifying their MSB to be 1.

#### Example Usage
*[length]'[base][value] where base is in b for binary, d for decimal, x for hexidecimal and 0 < length <= 32*
```
2'b10
32'b000000000000000000000111010111000
32'd10
10'd1
4'x4
32'x314AFF0E
5'd-4
```

## Manipulating Values

### Array Access
Referring to the current value of registers and inputs is easy - you simply use their name in your transition function. It is illegal to refer to the current value of an output. Since each register consists of an array of bits, it is frequently desirable to access a specific subsequence. The syntax for this is:
```
nth_bit := expression[index]
subsequence := expression[index1 - index2]
```
Accessing subsequence `A[from - to]` gives bits `from` through `to` inclusive of `A`. All values are zero indexed with the indices in order of increasing significance.

### Concatenation
It is also possible to concatenate values together. We do this by surrounding a comma delineated list of expressions we wish to concatenate with the brace symbols `{` and `}`.
```
concatenate := {expression1, expression2, ...}
```

The first expression in the list becomes the most significant subsequence of the resulting value.

### Example Usage
```
(5'b00010)[0] -->* 1'b0
(5'b00010)[1] -->* 1'b1
(5'b00010)[2] -->* 1'b0
(5'b00010)[3] -->* 1'b0
(5'b00010)[4] -->* 1'b0
(5'b00010)[0-2] -->* 3'b010
(5'b00010)[1-3] -->* 3'b001
{2'b11, 3'b000} -->* 5'b00011
```

## Operators

### Basic Hardware Gates
`CamelWare` supports a number of operators which correspond to basic hardware gates. These are:

1. `~` bitwise not
2. `&` bitwise and
3. `|` bitwise or
4. `^` bitwise xor
5. `~&` bitwise nand
6. `~|` bitwise nor
7. `~^` bitwise nxor

These can be applied as follows:
```
bitwise_operation := expression1 bitwise_operator expression2
bitwise_operator := & | | | ^ | ~& | ~| | ~^
bitwise_negation := ~expression
```

When the two operands of a gate are not of equal length, the shorter of the two is zero extended to reach the length of the other. The output of the expression is of length `max (length expression1) (length expression2)`.

#### Example Usage
```
3'b001 | 3'b010 -->* 3'b011
3'b001 & 3'b010 -->* 3'b000
~3'b001 -->* 3'b110
```

### Reduction
`CamelWare` supports a `Verilog` inspired "reduction" syntax. This allows all of the bits of an expression to be reduced to a single value using a gate.
```
reduction := bitwise_operator expression
```
For example `|(3'b001)` is equivalent to `1'b0 | 1'b0 | 1'b1` which evaluates to `1'b1`. `&(1'b001)` on the other hand would evaluate to `1'b0`.

#### Example Usage
```
^(3'b111) -->* 1'b1
&(3'b101) --->* 1'b0
```

### Logical Operators
Traditionally, logical operators obey the following semantics: all non zero values are treated as `true`, and zero is treated as `false`. `CamelWare` supports this by means of following operators:

1. `!` logical not
2. `&&` logical and
3. `||` logical or

Logical operations obey the same length extension rules as bitwise operations on inputs of different sizes. The output of a logical operation is either `1'b0` or `1'b1` depending on the result.

#### Example Usage
```
!5'b10011 -->* 1'b0
!5'b00000 -->* 1'b1
5'b10011 && 5'b11000 -->* 1'b1
5'b00000 && 5'b11000 -->* 1'b0
```

### Arithmetic Operators
`CamelWare` supports twos complement arithmetic operations. These are:

1. `-` arithmetic negation
2. `+` addition
3. `-` subtraction

When the two operands of an arithmetic operator are not of the same length the shorter of the two is sign extended. Just as with bitwise operations, the result is of length `max (length expression1) (length expression2)`.

#### Example Usage
```
32'd1 + 32'd2 -->* 32'd3
-3'b001 -->* 3'b111
```

### Relations
To compare twos complement values we use the following operators

1. `<` less than
2. `<=` less than or equals
3. `>` greater than
4. `>=` greater than or equals
5. `==` equals
6. `!=` not equals

Just as with arithmetic, before comparison the two operands are sign extended to match. The output is of length 1.

## If Then Else

### Syntax
To conditionally evaluate an expression the appropriate syntax is:
```
if_then_else := if condition then expression1 else expression2
```
In hardware terms this is equivalent to a 2 to 1 multiplexer with `condition` as the selector bit. To support the logical semantics where all nonzero values are treated as true, if `condition` is longer than 1 bit then it is first reduced with `|` before being fed into the mux. The output of an `if then else` is of length `max (length expression1) (length expression2)` with the shorter of the two expressions zero extended if neccessary.

### Example Usage
```
if 1'b0 then 3'b010 else 3'b001 -->* 3'b001
if 1'b1 then 3'b010 else 3'b001 -->* 3'b010
```

## Let Expressions

### Syntax
To avoid lengthy and cumbersome repeated subexpressions, `CamelWare` supports an `OCaml` like `let ... in` syntax. This is:
```
let variable = expression1 in expression2
```
In `expression2`, `variable` is bound to the result of evaluating `expression1` and can be referenced by name.

### Example Usage
```
let x = 3'b010 in x -->* 3'b010
let x = 3'b010 in 1'b1 -->* 1'b1
```

## Subcircuit Definitions
In order to allow for code reuse `CamelWare` allows for defining subcircuits. These are essentially functions with multiple inputs but only one output. The syntax is:
```
subcircuit := fun function_name (arg1[length1], ...)[output_length] = expression
```
To apply it:
```
application := function_name(expression1, ...)
```
