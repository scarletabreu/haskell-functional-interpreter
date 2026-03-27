# haskell-functional-interpreter

Intérprete de lenguaje funcional desarrollado en Haskell como proyecto final del curso de Programación Funcional. Aplica tipos algebraicos, funciones puras, manejo de errores con `Either`, recursión estructural y composición funcional.

---

## Estructura del proyecto

```
haskell-functional-interpreter/
├── Dominio.hs    -- tipos algebraicos, entorno y funciones auxiliares
├── Evaluador.hs  -- motor de evaluación puro
├── Main.hs       -- parser, pipeline y entrada/salida
└── README.md
```

---

## Módulos

### `Dominio.hs`
Define todos los tipos del intérprete: `Expr` (AST del lenguaje), `Value` (resultados), `EvalError` (errores como datos), `Env` y `FunEnv` (entornos de variables y funciones). También incluye las funciones `showExpr`, `showValue`, `showError` y las funciones de igualdad `eqExpr`, `eqError`.

### `Evaluador.hs`
Implementa `evaluate :: FunEnv -> Env -> Expr -> Result` de forma completamente pura. Usa recursión estructural sobre `Expr`, pattern matching exhaustivo y `Either` para propagar errores sin excepciones. Los helpers `evalIntBinOp` y `evalArgs` evitan repetición y aplican composición funcional con `foldr`.

### `Main.hs`
Contiene el parser de expresiones (`parseExpr`, `tokenize`) y el pipeline de ejecución (`evalInput`, `runPipeline`) usando composición con `(.)`. El único código con `IO` está en `main` y `readSource`, claramente separado de la lógica pura.

---

## Lenguaje soportado

| Construcción | Ejemplo de entrada |
|---|---|
| Entero | `42` |
| Booleano | `true` |
| Variable | `x` |
| Suma | `(add x y)` |
| Resta | `(sub x y)` |
| Multiplicación | `(mul x y)` |
| División entera | `(div x y)` |
| Igualdad | `(eq x y)` |
| Menor que | `(lt x y)` |
| Condicional | `(if (lt x y) x y)` |
| Ligadura local | `(let x (add x y) (mul x y))` |
| Llamada a función | `(app factorial n)` |

---

## Errores manejados

| Error | Causa |
|---|---|
| `UndefinedVariable` | Variable no encontrada en el entorno |
| `DivisionByZero` | División entre cero |
| `TypeMismatch` | Tipo incorrecto en una operación |
| `UndefinedFunction` | Función no definida |
| `ArityMismatch` | Número incorrecto de argumentos |
| `NonBooleanCondition` | Condición del `if` no es booleana |

---

## Criterios del proyecto aplicados

| Criterio | Cómo se aplica |
|---|---|
| Tipos algebraicos compuestos | `Expr`, `Value`, `EvalError`, `FunDef` en `Dominio.hs` |
| Funciones puras / separación IO | `evaluate` no toca `IO`; solo `main` y `readSource` lo hacen |
| Recursión y pattern matching | `evaluate`, `lookupEnv`, `lookupFun`, `showExpr` |
| Manejo de errores con `Either` | `type Result = Either EvalError Value` |
| Composición funcional | `runPipeline` y `evalInput` usan `(.)` |
| Fold | `evalArgs` y `bindParams` usan `foldr` |
| Documentación | Comentarios en tipos y funciones clave |

---

## Requisitos

- GHC 9.x o superior

## Compilar y ejecutar

```bash
ghc Main.hs -o interprete
./interprete
```

## Ejemplos de uso

```
Ingresa expresion o ':file ruta'
> (add x y)
> (if (lt x y) x y)
> (let x (add x y) (mul x y))
> :file entrada.txt
```