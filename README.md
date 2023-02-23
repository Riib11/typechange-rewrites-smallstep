# README

TODO:
- handle polymorphism
  - add type param
  - remove type param
  - change type param
- handle type aliases

## Rewrite Rules

A type boundary is written `Γ ⊢ {a : α}[α ~[ξ]~> α'] : α'`, encodes that the
inner term `Γ ⊢ a : α`, but it's position expects a term `a'` such that `Γ ⊢ a'
: α'`, and the type-change `ξ` encodes a way of changing the actual type `α`
into the expected type `α'`.

In the rewrite system, a type boundary can be annotated with a direction
`δ ∈ { ↑ , ↓ }`.
- A type boundary `Γ ⊢ ↑{a : α}[α ~[ξ]~> α'] : α'` encodes that the rewrite
  system should propogate the type boundary **outwards** such that the type of
  `a` is (immediately) preserved as `α`, and the program around the type
  boundary changes to accomodate that fact. Intuition: "No, you're wrong, I _do_
  actually want `a` to have type `α`, and my program should change around my
  cursor to accomodate that."
- A type boundary `Γ ⊢ ↓{a : α}[α ~[ξ]~> α'] : α'` encodes that the rewrite
  system should propogate the type boundary **inwards** such that the type of
  `a` is changed to `α'`, and the program around the type boundary should be
  (immediately) preserved. Intuition: "Yeah, you're right, `a` _should_ have
  type `α'`, so please change `a` to accomodate that."

### Propogate Inwards (Down)

#### `_ replaced-by _`

To replace the type of an expression with another type, put the expression in a
buffer.
```
↓{a : α}[α ~replace~> α']
~~~>
buf a : α in ? : α'
```

#### `+ _ -> [_]`

To add an argument to an expression, wrap it in a lambda.
```
↓{b : β}[β ~[+ α -> [β]]~> (α -> β)]
~~~>
fun _ : α => b : β
```

#### `- _ -> [_]`

To remove an argument from an expression, wrap it in an application.
```
↓{f : α -> β}[(α -> β) ~[- α -> [ξβ]]~> β']
~~~>
↓{f}[(α -> β) ~[α -> ξβ]~> (α -> β')] (? : α)
```

#### `_ -> _`

```
↓{fun x : α => b : β}[(α -> β) ~[ξα -> ξβ]~> (α' -> β')]
~~~>
fun x : α[ξα] => ↓{b : β}[β ~[ξβ]~> β']
```

#### `_`

```
↓{x : α}[α ~[ξ]~> α']
~~~>
↑{x : α'}[x : α' ~[ξ]~> α]
```

```
↓{a : α}[α ~[ξ]~> α']
~~~>
buf a : α in ? : α'
```

### Propogate Outwards (Up)

#### `replaced-by`

If the body of a lambda requires it's type to be replaced, then require the
lambda expression to have its codomain replaced.
```
fun (x : α) => ↑{b : β}[β ~[β replaced-by β']~> β']
~~~>
↑{fun (x : α) => b : β}[(α -> β) ~[α -> (β replaced-by β')]~> (α -> β')]
```

**propogate up**: *-bod

#### `+ _ -> [_]`

If the applicant of an application requires its argument to be removed, then pop
the argument into a buffer.
```
↑{b : β}[+ α -> [β]] (a : α)
~~~>
buf a : α in b : β
```

**propogate up**: *-bod

#### `- _ -> [_]`

If an applicant requires an arugment to be added, then apply it to an additional
argument.
```
↑{f : α -> β -> γ}[ (α -> β -> γ) ~[ - α -> [β -> γ] ]~> (β -> γ) ] (b : β)
~~~>
(f : α -> β -> γ) (? : α) (b : β)
```

**propogate up**: *-bod

#### `_ -> _`

If the type of the applicant of an application requires an arrow change, then
change the type of the argument via the domain change and require the
application to change via the codomain change.
```
↑{f : α -> β}[(α → β) ~[ξα -> ξβ]~> (α' → β')] a'
~~~>
↑{ (f : α -> β) ↓{a'}[α' ~[ξα]~> α] }[β ~[ξβ]~> β']
```

**propogate up**: *-bod

#### `_`

If the argument of an application requires its type to change, then change the
domain of the applicant.
```
(f : α -> β) ↑{a' : α'}[ α' ~[ξ]~> α ]
~~~>
↓{f : α -> β}[ (α -> β) ~[ ξⁱ -> β ]~> (α' -> β) ] ↑{a' : α'}[ α' ~[ξ]~> α ]
```

If the implementation of a `let` requires its type to change, then change
the signature of the `let` and change the type of the bound variable in the
body.
```
let x : α' = ↑{a}[α ~[ξ]~> α'] in b
let x : ↓{α'}[α' ~[ξⁱ]~> α] = a in ↓{b}[x : α' ~[ξⁱ]~> α]
```

## Context Change

### Propogate Inwards (Down)

```
↓{x : α}[x : α ~[ξ]~> α']
~~~>
↓{x : α}[α ~[ξ]~> α']
```

**propogate down**: all

### Propogate Outwards (Up)

```
let x : α = a in ↑{b}[x : α' ~[ξ]~> α]
~~~>
let x : α[ξⁱ] = ↓{a}[x : α ~[ξⁱ]~> α'] in b
```

```
let x : α = ↑{a}[x : α' ~[ξ]~> α] in b
~~~>
let x : α[ξⁱ] = a in ↓{b}[x : α ~[ξⁱ]~> α'] 
```

```
data D = x(α) in ↑{b}[x : α' ~[ξ]~> α]
~~~>
data D = x(α[ξⁱ]) in b
```

**propogate up**: all; while propogating up, propogates down other kids