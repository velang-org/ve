# Arrays in the Velang Language

This directory contains examples demonstrating the use of arrays in the Velang language.

## Examples Structure

### 1. Array Basics
**File**: [01_array_basics.ve](./01_array_basics.ve)

Demonstrates basic operations on arrays:
- Creation of arrays
- Accessing array elements
- Assignment to array elements

```bash
ve examples/arrays/01_array_basics.ve
```

### 2. Array Operations
**File**: [02_array_operations.ve](./02_array_operations.ve)

Demonstrates operations performed on arrays:
- Iterating through arrays using loops
- Summing array elements
- Calculating averages
- Finding minimum and maximum values

```bash
ve examples/arrays/02_array_operations.ve
```

### 3. Array Functions
**File**: [03_array_functions.ve](./03_array_functions.ve)

Demonstrates advanced operations:
- Passing arrays to functions
- Processing arrays within functions
- Manipulating array data with functions

```bash
ve examples/arrays/03_array_functions.ve
```

## Key Features

### Declaration and Initialization
```
// Array declaration with initial values
let numbers = [1, 2, 3, 4, 5];
```

### Element Access
```
// Access the first element (index 0)
let first = numbers[0];
```

### Element Modification
```
// Change the third element (index 2)
numbers[2] = 99;
```

## Limitations

The current implementation of arrays in Velang has the following limitations:
- No built-in length property
- No dynamic resizing (arrays have a fixed size)
- Limited array methods (no built-in functions like push, pop, etc.)
- Arrays must have elements of the same type 