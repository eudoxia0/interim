(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

    This file is part of Interim.

    Interim is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Interim is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Interim.  If not, see <http://www.gnu.org/licenses/>.
*)

structure Compiler :> COMPILER = struct
  open SymTab

  datatype compiler = Compiler of Type.tenv * Function.fenv * Type.renv * string

  val prelude = String.concatWith "\n" [
          "#include <stdbool.h>",
          "#include <inttypes.h>",
          "#include <stdio.h>",
          "#include <stdlib.h>",
          "",
          "#define INTERIM_GROW 8",
          "",
          "typedef struct { void** data; size_t size; size_t cap; } interim_region_t;",
          "",
          "void interim_region_create(interim_region_t* r) {",
          "  r->data = malloc(sizeof(void*) * INTERIM_GROW);",
          "  if (r->data == NULL) {",
          "    exit(-1);",
          "  }",
          "  r->size = 0;",
          "  r->cap  = INTERIM_GROW;",
          "}",
          "",
          "void* interim_region_allocate(interim_region_t* r, size_t size) {",
          "  if (r->size == r->cap) {",
          "    r->cap += INTERIM_GROW;",
          "    r->data = realloc(r->data, r->cap);",
          "    if (r->data == NULL) {",
          "      exit(-1);",
          "    }",
          "  }",
          "  void* datum = malloc(size);",
          "  if (datum == NULL) {",
          "    exit(-1);",
          "  }",
          "  r->data[r->size] = datum;",
          "  r->size++;",
          "  return datum;",
          "}",
          "",
          "void interim_region_free(interim_region_t* r) {",
          "  for (size_t i = 0; i < r->size; i++) {",
          "    free(r->data[i]);",
          "  }",
          "  free(r->data);",
          "  r->size = 0;",
          "  r->cap  = 0;",
          "}",
          "",
          "int interim_print_bool(bool v, bool nl) {",
          "  if (nl) {",
          "    return printf(v ? \"true\\n\" : \"false\\n\");",
          "  } else {",
          "    return printf(v ? \"true\" : \"false\");",
          "  }",
          "}",
          "",
          "bool interim_not(bool v) {",
          "  return !v;",
          "}",
          ""
      ]

  local
    open Function
    open Type
  in
    val emptyCompiler =
        let val interim_not = Function ("interim_not", [Param ("v", PBool)], Bool)
        in
            Compiler (empty, bind ("interim_not", interim_not) empty, empty, prelude)
        end
  end

  fun compilerTypeEnv (Compiler (t, _, _, _)) = t
  fun compilerCode (Compiler (_, _, _, c)) = c

  fun compileAST (Compiler (tenv, fenv, renv, code)) ast =
    (case ast of
         (AST.Defun (func, ast)) =>
         let val fenv' = bind (Function.funcName func, func) fenv
         in
             let val tast = TAST.augment ast
                                         (TAST.mkContext (Function.toStack func)
                                                         tenv
                                                         fenv'
                                                         renv)
             in
                 if (TAST.typeOf tast) <> Function.funcRT func then
                     raise Fail "Return type does not match type of body"
                 else
                     let val code' = Backend.defineFunction func tast
                     in
                         Compiler (tenv, fenv', renv, code ^ (Backend.renderTop code'))
                     end
             end
         end
       | (AST.Defrecord (name, slots)) =>
         let val slots = map (fn (n, t) => Type.Slot (n, t)) slots
         in
             let val ty = Type.Record (name, slots)
             in
                 let val tenv' = bind (name, ty) tenv
                 in
                     let val typedef = Backend.defineStruct name slots
                     in
                         Compiler (tenv', fenv, renv, code ^ (Backend.renderTop typedef))
                     end
                 end
             end
         end
       | (AST.CInclude s) =>
         let val incl = "\n#include <" ^ s ^ ">\n\n"
         in
             Compiler (tenv, fenv, renv, code ^ incl)
         end)

  fun compileString c s =
    let val (sexp, _) = Parser.parseString s
    in
        compileAST c (AST.parseToplevel sexp (compilerTypeEnv c))
    end

  fun compileForms c (form::rest) = let val c' = compileAST c (AST.parseToplevel form (compilerTypeEnv c))
                                    in
                                        compileForms c' rest
                                    end
    | compileForms c nil = c

  fun compileFile c path =
    let val code = "(" ^ (Util.readFileToString path) ^ ")"
    in
        case (Parser.parseString code) of
            (Parser.SList l, _) => compileForms c l
          | _ => raise Fail "Impossible"
    end
end
