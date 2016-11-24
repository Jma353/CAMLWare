# CamelWare
Ocaml, bit by bit

Documentation of the CamelWare language

______________
##### 1. Keywords
##### 2. Supported Values
##### 3. Registers, Inputs, and Outputs
##### 4. Basic Hardware Gates 
##### 5. Comparators 
##### 6. Arithmetic Operations
##### 7. Plexers
##### 8. Defining Subcircuits 
##### 9. CamelWare, Learn By Examples 
______________
### 1. Keywords
1. input 
2. output
2. register
3. falling
4. rising
5. fun
6. let ... in 
7. if ... then ... else 

### 2. Supported values
CamelWare supports inputs and outputs of hexidecimal, decimal, and binary. To define a constant of of these types, one specifies the length of the bitstream that will hold the values (CamelWare supports up to 32 bits), the base system, and the value of that constant in the base system. 
*[length]'[base][value] where base is in b for binary, d for decimal, h for hexidecimal and 0 < length <= 32*
```
2'b10
32'b000000000000000000000111010111000
32'd10
10'd1
4'x4
32'x384972311
```
### 3. Registers, Inputs, and Outputs 

1. **[falling, rising] register** [ rising register, falling register, register ]
3. **input** [ input ] 
4. **output** [ output ]
5. **substream** [ [  ] ]
6. **concat** [ { } ]

**name** [token]
*defintion*
example usage - meta syntax [ -->* means steps to ]

**[falling, rising] register** [ rising register, falling register, register ]
*[ falling register A[length] ] defines a register referred to by A that recomputes its values on the falling edge of the clock with bit # length
[ rising register A[length] ] defines a register referred to by A that recomputers its values on the rising edge of the clock with bit # length
[ register A[length] ] defines a rising register referred to by A with bit # length*
```
register A[10] = 10'd10
rising register B[32] = 32'd19283
falling register C[1] = 1'b1
register D[1] = C
```

**input** [ input ] 
*[ input A[length] ] defines an input referred to by A with bit # length.
Inputs are assigned values by the programmer via the gui*
```
input A[10]
input B[32]
```
**output** [ output ]
*[ output A[length] ] defines an output referred to by A with bit # length.*
```
input A[10] 
output B[32] = A
```

**substream** [ [  ] ]
*[ A[n1-n2] ] refers to the substream of A from bits n1 to bits n2
0 < n1,n2 < length A and n2 >= n1*
```
register A[4] = 4'b1101
register B[2] = A[0-1] -->* 2'b01
register C[1] = A[3] -->* 1'b1
```

**concat** [ { } ]
*[ {A, B} ] concats register A to register B creating a value of twice their length*
```
register A[4] = 4'b1101
register B[4] = 4'b1111
register C[7] = {A[0-2],B} -->* 7'b1011111
```
### 4. Basic Hardware Gates 
1. **logical and** [ && ] 
2. **logical or** [ || ]
3. **logical not** [ ! ]
4. **and** [ & ]
5. **or** [ | ]
6. **xor** [ ^ ]
6. **not** [ ~ ]
7. **nand** [ ~& ]
8. **nor** [ ~| ]
9. **xnor** [ ~^ ]

**name** [token]
*defintion*
example usage


**logical and** [ && ]
*[ A && B ] is true (1'b1) iff reducing A with the AND gate and B with the AND gate both result in true (1'b1)*
```
register A[4] = 2'b00
register B[4] = 2'b11
register C[4] = 2'b11
A && B -->* 1'b0
B && C -->* 1'b1
register D[32] = A && B -->* 32'b00000000000000000000000000000000 
```

**logical or** [ || ]
*[ A || B ] is true (1'b1) iff either reducing A with the OR gate or B with the OR gate both result in true (1'b1)*
```
register A[2] = 2'b00
register B[2] = 2'b11
register C[2] = 2'b00
A || B -->* 1'b1
A || C -->* 1'b0
register D[32] = A || B -->* 32'b00000000000000000000000000000001 
```

**logical not** [ ! ]
*[ !A ] is true (1'b1) if A consists of all zero bits else false (0'b0)*
```
register A[2] = 2'b00
register B[2] = 2'b01
!A -->* 1'b1
!B -->* 1'b0
register D[32] = !B -->* 32'b00000000000000000000000000000000
```
**not** [~]
*[ ~A ] is the same number of bits as A with each bit being the opposite as its value in A*
```
register A[4] = 4'b0001
register B[4] = 4'b1101
~A -->* 4'b1110
~B -->* 4'b0010
register D[32] = !B -->* 32'b00000000000000000000000000001101
```
**and** [ & ]
*[ A & B ] is the AND gate applied to each bit in A with each bit in B*
```
register A[4] = 4'b0001
register B[4] = 4'b1101
A & B -->* 4'b0001
register D[32] = A & B -->* 32'b00000000000000000000000000000001
```

**or** [ | ] 
*[ A | B ] is the OR gate applied to each bit in A with each bit in B*
```
register A[4] = 4'b0001
register B[4] = 4'b0101
A & B -->* 4'b0101
register D[32] = A | B -->* 32'b00000000000000000000000000000101
```
**xor** [ ^ ]
*[ A ^ B ] is the XOR gate applied to each bit in A with each bit in B*
```
register A[4] = 4'b0001
register B[4] = 4'b0101
A ^ B -->* 4'b0100
register D[32] = A | B -->* 32'b00000000000000000000000000000100
```

**nand** [ ~& ]
*[ A ~& B ] is the NAND gate applied to each bit in A with each bit in B*
```
register A[4] = 4'b0001
register B[4] = 4'b0101
A ~& B -->* 4'b1110
register D[32] = A ~& B -->* 32'b00000000000000000000000000001110
```

**nor** [ ~|]
*[ A ~| B ] is the NOR gate applied to each bit in A with each bit in B*
```
register A[4] = 4'b0001
register B[4] = 4'b0101
A ~| B -->* 4'b1010
register D[32] = A ~| B -->* 32'b00000000000000000000000000001010
```
**xnor** [ ~^ ]
*[ A ~^ B ] is the XNOR gate applied to each bit in A with each bit in B*
```
register A[4] = 4'b0001
register B[4] = 4'b0101
A ~^ B -->* 4'b1011
register D[32] = A ~^ B -->* 32'b00000000000000000000000000001011
```


### 5. Comparators 
1. **equals** [ == ]
2. **not equals** [ != ]
3. **greater than or equal to** [ >= ]
4. **less than or equal to** [ <= ]
5. **less than** [ < ]
6. **greater than** [ > ]


**name** [token]
*defintion*
example usage - meta syntax [ -->* means steps to ]

**equals** [ == ]
*[A == B] is true (1'b1) iff every bit in A is the same value as every bit in B*
```
register A[32] = 32'd10
register B[32] = 32'd13
register C[32] = 32'd10
A == B -->* 1'b0
A == C -->* 1'b1
register D[32] = A == C -->* 32'b00000000000000000000000000000001 
#zero extended to match registers length
```

**not equals** [ != ]
*[A != B] is true (1'b1) iff at least one bit in A is not the same value as every bit in B*
```
register A[2] = 2'b01
register B[2] = 2'b00
register C[2] = 2'b01
A != B -->* 1'b1
A != C -->* 1'b0
register D[32] = A != B -->* 32'b00000000000000000000000000000001 
```

**greater than or equal to** [ >= ]
*[A >= B] is true (1'b1) iff two's complement interpretation of A is greater than or equal to B*
```
register A[32] = 32'd10
register B[32] = 32'd9
register C[32] = 32'd11
A >= B -->* 1'b1
A >= C -->* 1'b0
register D[32] = A >= B -->* 32'b00000000000000000000000000000001 
```

**less than or equal to** [ <= ]
*[A <= B] is true (1'b1) iff two's complement interpretation of A is less than or equal to B*
```
register A[32] = 32'd10
register B[32] = 32'd9
register C[32] = 32'd11
A <= B -->* 1'b0
A <= C -->* 1'b1
register D[32] = A <= B -->* 32'b00000000000000000000000000000000
```

**less than** [ < ]
*[A < B] is true (1'b1) iff two's complement interpretation of A is less than  B*
```
register A[32] = 32'd8
register B[32] = 32'd9
register C[32] = 32'd10
A < B -->* 1'b1
A < C -->* 1'b0
register D[32] = A < B -->* 32'b00000000000000000000000000000001
```

**greater than** [ > ]
*[A > B] is true (1'b1) iff two's complement interpretation of A is greater than  B*
```
register A[32] = 32'd8
register B[32] = 32'd9
register C[32] = 32'd4
A > B -->* 1'b0
A > C -->* 1'b1
register D[32] = A > B -->* 32'b00000000000000000000000000000000
```

### 6. Arithmetic Operations 

1. **shift left logically** [ << ]
2. **shift right logically** [ >> ]
3. **shift right arithmatically** [ >>> ]
4. **plus** [ + ]
5. **minus** [ - ] 

**name** [token]
*defintion*
example usage - meta syntax [ -->* means steps to ] 

**shift left logically** [ << ]
*[A << B] is A shifted left by the unsigned number B represents*
```
register A[4] = 4'b0011
register B[4] = 4'b0010
A << B -->* 4'b1100
register D[32] = A << B -->* 32'b00000000000000000000000000001100
```
**shift right logically** [ >> ]
*[A >> B] is A shifted right with zeros shifted in by the unsigned number B represents*
```
register A[4] = 4'b1011
register B[4] = 4'b0010
A >> B -->* 4'b0010
register D[32] = A << B -->* 32'b00000000000000000000000000000010
```
**shift right arithmatically** [ >>> ]
*[A >>> B] is A shifted right with the sign of A shifted in by the unsigned number B represents*
```
register A[4] = 4'b1011
register B[4] = 4'b0010
A >>> B -->* 4'b1110
register D[32] = A << B -->* 32'b00000000000000000000000000001110
```

**plus** [ + ]
*[A + B] is the two's complement addition of A and B*
```
register A[4] = 4'b1011
register B[4] = 4'b0010
A + B -->* 4'b1101
register D[32] = A + B -->* 32'b00000000000000000000000000001101
```

**minus** [ - ] 
*[A + B] is the two's complement subtraction of B from A*
```
register A[4] = 4'b0011
register B[4] = 4'b0010
A - B -->* 4'b0001
register D[32] = A - B -->* 32'b00000000000000000000000000000001
```

### 7. Plexers

1. **multiplexer** [ if ... then ... else ... ]

**name** [token]
*defintion*
example usage - meta syntax [ -->* means steps to ] 

**multiplexer** [ if ... then ... else ... ]
*[if ___ then A else B] is the multiplexed A, B inputs with the selector __ represents. If __ evaluates to true (0'b1) A is the value of the operation else B is the value of the operation.*
```
register A[4] = 4'b0011
register B[4] = 4'b0010
register C[4] = 4'b0010
if A < B then C[0] else C[1] -->* 1'b1
register D[4] = if A < B then C[0] else C[1] -->* 32'b00000000000000000000000000000001
```
### 8. Defining Subcircuits

1. **subcircuit** [ fun f(x1,x2,...,xn) = ... ] 

**name** [token]
*defintion*
example usage - meta syntax [ -->* means steps to ] 

**subcircuit** [ fun f(x1,x2,...,xn) = ... ] 
*[ fun f(x1,x2,...,xn) = ... ]  represents the subcircuit defined by f that can be used by expressing f on registers or inputs*
```
register A[4] = 4'b0101
register B[4] = 4'b1001
fun f(x,y) = x | y
fun h(u,v) = u && v
register D[4] = f(A,B) -->* 4'b1101
register F[4] = h(B,A) -->* 4'b0001
```

### 9. CamelWare, learn by Examples





