local TV = {
  a = {"tyA", "tyVarA"},
  b = {"tyB", "tyVarB"},
  c = {"tyC", "tyVarC"},
  d = {"tyD", "tyVarD"},
  eqA = {"tyEqA", "tyVarEqA", "IsEqType"},
}
local PRIMITIVES
do
  local string_format = string.format
  local unit = {"unit"}
  local bool = {"bool"}
  local int = {"int"}
  local word = {"word"}
  local real = {"real"}
  local char = {"char"}
  local char16 = {"char16"}
  local string = {"string"}
  local string16 = {"string16"}
  local intInf = {"intInf"}
  local exn = {"exn"}
  local exntag = {"exntag"}
  local LuaValue = {"LuaValue"}
  local JSValue = {"JavaScriptValue"}
  local ref = function(payloadTy) return {string_format("refOf (%s)", payloadTy[1])} end
  local list = function(elemTy) return {string_format("listOf (%s)", elemTy[1])} end
  local vector = function(elemTy) return {string_format("vectorOf (%s)", elemTy[1])} end
  local array = function(elemTy) return {string_format("arrayOf (%s)", elemTy[1])} end
  local pair = function(ty1, ty2) return {string_format("pairOf (%s, %s)", ty1[1], ty2[1])} end
  local tuple = function(types)
    local t = {}
    for _, v in pairs(types) do
      table.insert(t, v[1])
    end
    return {string_format("tupleOf [%s]", table.concat(t, ", "))}
  end
  local function1 = function(resultTy, arg1Ty) return {string_format("function1Of (%s, %s)", resultTy[1], arg1Ty[1])} end
  local function2 = function(resultTy, arg1Ty, arg2Ty) return {string_format("function2Of (%s, %s, %s)", resultTy[1], arg1Ty[1], arg2Ty[1])} end
  local function3 = function(resultTy, arg1Ty, arg2Ty, arg3Ty) return {string_format("function3Of (%s, %s, %s, %s)", resultTy[1], arg1Ty[1], arg2Ty[1], arg3Ty[1])} end
  local promptTag = function(ty) return {string_format("promptTagOf (%s)", ty[1])} end
  local subcont = function(ty1, ty2) return {string_format("subcontOf (%s, %s)", ty1[1], ty2[1])} end
  local function Binary(a, b)
    return function(result)
      return { vars = {}, args = {a, b}, result = result }
    end
  end
  local function HomoUnary(a)
    return { vars = {}, args = {a}, result = a }
  end
  local function HomoBinary(a)
    return { vars = {}, args = {a, a}, result = a }
  end
  local function Compare(a)
    return { vars = {}, args = {a, a}, result = bool }
  end

  PRIMITIVES = {
    {
      name = "=",
      srcname = "EQUAL",
      type = { vars = {TV.eqA}, args = {TV.eqA, TV.eqA}, result = bool },
    },
    {
      name = "call2",
      srcname = "call2",
      type = { vars = {TV.a, TV.b, TV.c}, args = {function2(TV.a, TV.b, TV.c), TV.b, TV.c}, result = TV.a },
    },
    {
      name = "call3",
      srcname = "call3",
      type = { vars = {TV.a, TV.b, TV.c, TV.d}, args = {function3(TV.a, TV.b, TV.c, TV.d), TV.b, TV.c, TV.d}, result = TV.a },
    },
    {
      name = "Ref.=",
      srcname = "Ref_EQUAL",
      type = { vars = {TV.a}, args = {ref(TV.a), ref(TV.a)}, result = bool },
    },
    {
      name = "Ref.:=",
      srcname = "Ref_set",
      type = { vars = {TV.a}, args = {ref(TV.a), TV.a}, result = unit },
    },
    {
      name = "Ref.!",
      srcname = "Ref_read",
      type = { vars = {TV.a}, args = {ref(TV.a)}, result = TV.a },
    },
    {
      name = "Bool.=",
      srcname = "Bool_EQUAL",
      type = Compare(bool),
    },
    {
      name = "Bool.not",
      srcname = "Bool_not",
      type = HomoUnary(bool),
    },
    {
      name = "Int.=",
      srcname = "Int_EQUAL",
      type = Compare(int),
    },
    {
      name = "Int.<",
      srcname = "Int_LT",
      type = Compare(int),
    },
    {
      name = "Int.<=",
      srcname = "Int_LE",
      type = Compare(int),
    },
    {
      name = "Int.>",
      srcname = "Int_GT",
      type = Compare(int),
    },
    {
      name = "Int.>=",
      srcname = "Int_GE",
      type = Compare(int),
    },
    {
      name = "Word.=",
      srcname = "Word_EQUAL",
      type = Compare(word),
    },
    {
      name = "Word.+",
      srcname = "Word_PLUS",
      type = HomoBinary(word),
    },
    {
      name = "Word.-",
      srcname = "Word_MINUS",
      type = HomoBinary(word),
    },
    {
      name = "Word.*",
      srcname = "Word_TIMES",
      type = HomoBinary(word),
    },
    {
      name = "Word.~",
      srcname = "Word_TILDE",
      type = HomoUnary(word),
    },
    {
      name = "Word.<",
      srcname = "Word_LT",
      type = Compare(word),
    },
    {
      name = "Word.<=",
      srcname = "Word_LE",
      type = Compare(word),
    },
    {
      name = "Word.>",
      srcname = "Word_GT",
      type = Compare(word),
    },
    {
      name = "Word.>=",
      srcname = "Word_GE",
      type = Compare(word),
    },
    {
      name = "Real.+",
      srcname = "Real_PLUS",
      type = HomoBinary(real),
    },
    {
      name = "Real.-",
      srcname = "Real_MINUS",
      type = HomoBinary(real),
    },
    {
      name = "Real.*",
      srcname = "Real_TIMES",
      type = HomoBinary(real),
    },
    {
      name = "Real./",
      srcname = "Real_DIVIDE",
      type = HomoBinary(real),
    },
    {
      name = "Real.~",
      srcname = "Real_TILDE",
      type = HomoUnary(real),
    },
    {
      name = "Real.<",
      srcname = "Real_LT",
      type = Compare(real),
    },
    {
      name = "Real.<=",
      srcname = "Real_LE",
      type = Compare(real),
    },
    {
      name = "Real.>",
      srcname = "Real_GT",
      type = Compare(real),
    },
    {
      name = "Real.>=",
      srcname = "Real_GE",
      type = Compare(real),
    },
    {
      name = "Char.=",
      srcname = "Char_EQUAL",
      type = Compare(char),
    },
    {
      name = "Char.<",
      srcname = "Char_LT",
      type = Compare(char),
    },
    {
      name = "Char.<=",
      srcname = "Char_LE",
      type = Compare(char),
    },
    {
      name = "Char.>",
      srcname = "Char_GT",
      type = Compare(char),
    },
    {
      name = "Char.>=",
      srcname = "Char_GE",
      type = Compare(char),
    },
    {
      name = "Char16.=",
      srcname = "Char16_EQUAL",
      type = Compare(char16),
    },
    {
      name = "Char16.<",
      srcname = "Char16_LT",
      type = Compare(char16),
    },
    {
      name = "Char16.<=",
      srcname = "Char16_LE",
      type = Compare(char16),
    },
    {
      name = "Char16.>",
      srcname = "Char16_GT",
      type = Compare(char16),
    },
    {
      name = "Char16.>=",
      srcname = "Char16_GE",
      type = Compare(char16),
    },
    {
      name = "String.=",
      srcname = "String_EQUAL",
      type = Compare(string),
    },
    {
      name = "String.<",
      srcname = "String_LT",
      type = Compare(string),
    },
    {
      name = "String.<=",
      srcname = "String_LE",
      type = Compare(string),
    },
    {
      name = "String.>",
      srcname = "String_GT",
      type = Compare(string),
    },
    {
      name = "String.>=",
      srcname = "String_GE",
      type = Compare(string),
    },
    {
      name = "String.^",
      srcname = "String_HAT",
      type = HomoBinary(string),
    },
    {
      name = "String.size",
      srcname = "String_size",
      type = { vars = {}, args = {string}, result = int },
    },
    {
      name = "String.str",
      srcname = "String_str",
      type = { vars = {}, args = {char}, result = string },
    },
    {
      name = "String16.=",
      srcname = "String16_EQUAL",
      type = Compare(string16),
    },
    {
      name = "String16.<",
      srcname = "String16_LT",
      type = Compare(string16),
    },
    {
      name = "String16.<=",
      srcname = "String16_LE",
      type = Compare(string16),
    },
    {
      name = "String16.>",
      srcname = "String16_GT",
      type = Compare(string16),
    },
    {
      name = "String16.>=",
      srcname = "String16_GE",
      type = Compare(string16),
    },
    {
      name = "String16.^",
      srcname = "String16_HAT",
      type = HomoBinary(string16),
    },
    {
      name = "String16.size",
      srcname = "String16_size",
      type = { vars = {}, args = {string16}, result = int },
    },
    {
      name = "String16.str",
      srcname = "String16_str",
      type = { vars = {}, args = {char16}, result = string16 },
    },
    {
      name = "IntInf.=",
      srcname = "IntInf_EQUAL",
      type = Compare(intInf),
    },
    {
      name = "IntInf.+",
      srcname = "IntInf_PLUS",
      type = HomoBinary(intInf),
    },
    {
      name = "IntInf.-",
      srcname = "IntInf_MINUS",
      type = HomoBinary(intInf),
    },
    {
      name = "IntInf.*",
      srcname = "IntInf_TIMES",
      type = HomoBinary(intInf),
    },
    {
      name = "IntInf.~",
      srcname = "IntInf_TILDE",
      type = HomoUnary(intInf),
    },
    {
      name = "IntInf.<",
      srcname = "IntInf_LT",
      type = Compare(intInf),
    },
    {
      name = "IntInf.<=",
      srcname = "IntInf_LE",
      type = Compare(intInf),
    },
    {
      name = "IntInf.>",
      srcname = "IntInf_GT",
      type = Compare(intInf),
    },
    {
      name = "IntInf.>=",
      srcname = "IntInf_GE",
      type = Compare(intInf),
    },
    {
      name = "IntInf.andb",
      srcname = "IntInf_andb",
      type = HomoBinary(intInf),
    },
    {
      name = "IntInf.orb",
      srcname = "IntInf_orb",
      type = HomoBinary(intInf),
    },
    {
      name = "IntInf.xorb",
      srcname = "IntInf_xorb",
      type = HomoBinary(intInf),
    },
    {
      name = "IntInf.notb",
      srcname = "IntInf_notb",
      type = HomoUnary(intInf),
    },
    {
      name = "IntInf.quot.unchecked",
      srcname = "IntInf_quot_unchecked",
      type = HomoBinary(intInf),
    },
    {
      name = "IntInf.rem.unchecked",
      srcname = "IntInf_rem_unchecked",
      type = HomoBinary(intInf),
    },
    {
      name = "Vector.length",
      srcname = "Vector_length",
      type = { vars = {TV.a}, args = {vector(TV.a)}, result = int },
    },
    {
      name = "Vector.unsafeFromListRevN",
      srcname = "Vector_unsafeFromListRevN",
      type = { vars = {TV.a}, args = {int, list(TV.a)}, result = vector(TV.a) },
    },
    {
      name = "Array.=",
      srcname = "Array_EQUAL",
      type = { vars = {TV.a}, args = {array(TV.a), array(TV.a)}, result = bool },
    },
    {
      name = "Array.length",
      srcname = "Array_length",
      type = { vars = {TV.a}, args = {array(TV.a)}, result = int },
    },
    {
      name = "Unsafe.cast",
      srcname = "Unsafe_cast",
      type = { vars = {TV.a, TV.b}, args = {TV.a}, result = TV.b },
    },
    {
      name = "Unsafe.Vector.sub",
      srcname = "Unsafe_Vector_sub",
      type = { vars = {TV.a}, args = {vector(TV.a), int}, result = TV.a },
    },
    {
      name = "Unsafe.Array.sub",
      srcname = "Unsafe_Array_sub",
      type = { vars = {TV.a}, args = {array(TV.a), int}, result = TV.a },
    },
    {
      name = "Unsafe.Array.update",
      srcname = "Unsafe_Array_update",
      type = { vars = {TV.a}, args = {array(TV.a), int, TV.a}, result = unit },
    },
    {
      name = "Exception.instanceof",
      srcname = "Exception_instanceof",
      type = { vars = {}, args = {exn, exntag}, result = bool },
    },
    {
      name = "DelimCont.newPromptTag",
      srcname = "DelimCont_newPromptTag",
      type = { vars = {TV.a}, args = {}, result = promptTag(TV.a) },
    },
    {
      name = "DelimCont.pushPrompt",
      srcname = "DelimCont_pushPrompt",
      type = { vars = {TV.a}, args = {promptTag(TV.a), function1(TV.a, unit)}, result = TV.a },
    },
    {
      name = "DelimCont.withSubCont",
      srcname = "DelimCont_withSubCont",
      type = { vars = {TV.a, TV.b}, args = {promptTag(TV.b), function1(TV.b, subcont(TV.a, TV.b))}, result = TV.a },
    },
    {
      name = "DelimCont.pushSubCont",
      srcname = "DelimCont_pushSubCont",
      type = { vars = {TV.a, TV.b}, args = {subcont(TV.a, TV.b), function1(TV.a, unit)}, result = TV.b },
    },

    --
    -- Lua backend
    --
    {
      name = "Lua.sub",
      srcname = "Lua_sub",
      type = HomoBinary(LuaValue),
    },
    {
      name = "Lua.set",
      srcname = "Lua_set",
      type = { vars = {}, args = {LuaValue, LuaValue, LuaValue}, result = unit },
    },
    {
      name = "Lua.isNil",
      srcname = "Lua_isNil",
      type = { vars = {}, args = {LuaValue}, result = bool },
    },
    {
      name = "Lua.==",
      srcname = "Lua_EQUAL",
      type = Compare(LuaValue),
    },
    {
      name = "Lua.~=",
      srcname = "Lua_NOTEQUAL",
      type = Compare(LuaValue),
    },
    {
      name = "Lua.<",
      srcname = "Lua_LT",
      type = Compare(LuaValue),
    },
    {
      name = "Lua.<=",
      srcname = "Lua_LE",
      type = Compare(LuaValue),
    },
    {
      name = "Lua.>",
      srcname = "Lua_GT",
      type = Compare(LuaValue),
    },
    {
      name = "Lua.>=",
      srcname = "Lua_GE",
      type = Compare(LuaValue),
    },
    {
      name = "Lua.+",
      srcname = "Lua_PLUS",
      type = HomoBinary(LuaValue),
    },
    {
      name = "Lua.-",
      srcname = "Lua_MINUS",
      type = HomoBinary(LuaValue),
    },
    {
      name = "Lua.*",
      srcname = "Lua_TIMES",
      type = HomoBinary(LuaValue),
    },
    {
      name = "Lua./",
      srcname = "Lua_DIVIDE",
      type = HomoBinary(LuaValue),
    },
    {
      name = "Lua.//",
      srcname = "Lua_INTDIV",
      type = HomoBinary(LuaValue),
    },
    {
      name = "Lua.%",
      srcname = "Lua_MOD",
      type = HomoBinary(LuaValue),
    },
    {
      name = "Lua.pow",
      srcname = "Lua_pow",
      type = HomoBinary(LuaValue),
    },
    {
      name = "Lua.unm",
      srcname = "Lua_unm",
      type = HomoUnary(LuaValue),
    },
    {
      name = "Lua.andb",
      srcname = "Lua_andb",
      type = HomoBinary(LuaValue),
    },
    {
      name = "Lua.orb",
      srcname = "Lua_orb",
      type = HomoBinary(LuaValue),
    },
    {
      name = "Lua.xorb",
      srcname = "Lua_xorb",
      type = HomoBinary(LuaValue),
    },
    {
      name = "Lua.notb",
      srcname = "Lua_notb",
      type = HomoUnary(LuaValue),
    },
    {
      name = "Lua.<<",
      srcname = "Lua_LSHIFT",
      type = HomoBinary(LuaValue),
    },
    {
      name = "Lua.>>",
      srcname = "Lua_RSHIFT",
      type = HomoBinary(LuaValue),
    },
    {
      name = "Lua.concat",
      srcname = "Lua_concat",
      type = HomoBinary(LuaValue),
    },
    {
      name = "Lua.length",
      srcname = "Lua_length",
      type = HomoUnary(LuaValue),
    },
    {
      name = "Lua.isFalsy",
      srcname = "Lua_isFalsy",
      type = { vars = {}, args = {LuaValue}, result = bool },
    },
    {
      name = "Lua.call0",
      srcname = "Lua_call0",
      type = { vars = {}, args = {LuaValue, vector(LuaValue)}, result = unit },
    },
    {
      name = "Lua.call1",
      srcname = "Lua_call1",
      type = { vars = {}, args = {LuaValue, vector(LuaValue)}, result = LuaValue },
    },
    {
      name = "Lua.call2",
      srcname = "Lua_call2",
      type = { vars = {}, args = {LuaValue, vector(LuaValue)}, result = pair(LuaValue, LuaValue) },
    },
    {
      name = "Lua.call3",
      srcname = "Lua_call3",
      type = { vars = {}, args = {LuaValue, vector(LuaValue)}, result = tuple{LuaValue, LuaValue, LuaValue} },
    },

    --
    -- JavaScript backend
    --
    {
      name = "JavaScript.sub",
      srcname = "JavaScript_sub",
      type = HomoBinary(JSValue),
    },
    {
      name = "JavaScript.set",
      srcname = "JavaScript_set",
      type = { vars = {}, args = {JSValue, JSValue, JSValue}, result = unit },
    },
    {
      name = "JavaScript.===",
      srcname = "JavaScript_EQUAL",
      type = Compare(JSValue),
    },
    {
      name = "JavaScript.!==",
      srcname = "JavaScript_NOTEQUAL",
      type = Compare(JSValue),
    },
    {
      name = "JavaScript.<",
      srcname = "JavaScript_LT",
      type = Compare(JSValue),
    },
    {
      name = "JavaScript.<=",
      srcname = "JavaScript_LE",
      type = Compare(JSValue),
    },
    {
      name = "JavaScript.>",
      srcname = "JavaScript_GT",
      type = Compare(JSValue),
    },
    {
      name = "JavaScript.>=",
      srcname = "JavaScript_GE",
      type = Compare(JSValue),
    },
    {
      name = "JavaScript.+",
      srcname = "JavaScript_PLUS",
      type = HomoBinary(JSValue),
    },
    {
      name = "JavaScript.-",
      srcname = "JavaScript_MINUS",
      type = HomoBinary(JSValue),
    },
    {
      name = "JavaScript.*",
      srcname = "JavaScript_TIMES",
      type = HomoBinary(JSValue),
    },
    {
      name = "JavaScript./",
      srcname = "JavaScript_DIVIDE",
      type = HomoBinary(JSValue),
    },
    {
      name = "JavaScript.%",
      srcname = "JavaScript_MOD",
      type = HomoBinary(JSValue),
    },
    {
      name = "JavaScript.negate",
      srcname = "JavaScript_negate",
      type = HomoUnary(JSValue),
    },
    {
      name = "JavaScript.andb",
      srcname = "JavaScript_andb",
      type = HomoBinary(JSValue),
    },
    {
      name = "JavaScript.orb",
      srcname = "JavaScript_orb",
      type = HomoBinary(JSValue),
    },
    {
      name = "JavaScript.xorb",
      srcname = "JavaScript_xorb",
      type = HomoBinary(JSValue),
    },
    {
      name = "JavaScript.notb",
      srcname = "JavaScript_notb",
      type = HomoUnary(JSValue),
    },
    {
      name = "JavaScript.<<",
      srcname = "JavaScript_LSHIFT",
      type = HomoBinary(JSValue),
    },
    {
      name = "JavaScript.>>",
      srcname = "JavaScript_RSHIFT",
      type = HomoBinary(JSValue),
    },
    {
      name = "JavaScript.>>>",
      srcname = "JavaScript_URSHIFT",
      type = HomoBinary(JSValue),
    },
    {
      name = "JavaScript.**",
      srcname = "JavaScript_EXP",
      type = HomoBinary(JSValue),
    },
    {
      name = "JavaScript.isFalsy",
      srcname = "JavaScript_isFalsy",
      type = { vars = {}, args = {JSValue}, result = bool },
    },
    {
      name = "JavaScript.typeof",
      srcname = "JavaScript_typeof",
      type = { vars = {}, args = {JSValue}, result = string16 },
    },
    {
      name = "JavaScript.global",
      srcname = "JavaScript_global",
      type = { vars = {}, args = {string16}, result = JSValue },
    },
  }
end

local f = io.open(arg[1] or "primitives.sml", "w")

f:write[[
(* This file was generated by primitives.lua *)
structure Primitives = struct
]]

for i, p in ipairs(PRIMITIVES) do
  local head
  if i == 1 then
    head = "datatype PrimOp = "
  else
    head = "                | "
  end
  f:write(string.format("%s%s (* %s *)\n", head, p.srcname, p.name))
end

for i, p in ipairs(PRIMITIVES) do
  local head
  if i == 1 then
    head = "fun toString "
  else
    head = "  | toString "
  end
  f:write(string.format("%s%s = %q\n", head, p.srcname, p.name))
end

for i, p in ipairs(PRIMITIVES) do
  local head
  if i == 1 then
    head = "fun fromString "
  else
    head = "  | fromString "
  end
  f:write(string.format("%s%q = SOME %s\n", head, p.name, p.srcname))
end
f:write("  | fromString _ = NONE\n")

f:write[[
end;

functor TypeOfPrimitives (type ty
                          type tv
                          type constraint
                          val tyVarA : tv
                          val tyVarB : tv
                          val tyVarC : tv
                          val tyVarD : tv
                          val tyVarEqA : tv
                          val tyA : ty
                          val tyB : ty
                          val tyC : ty
                          val tyD : ty
                          val tyEqA : ty
                          val unit : ty
                          val bool : ty
                          val int : ty
                          val word : ty
                          val real : ty
                          val char : ty
                          val char16 : ty
                          val string : ty
                          val string16 : ty
                          val intInf : ty
                          val exn : ty
                          val exntag : ty
                          val LuaValue : ty
                          val JavaScriptValue : ty
                          val refOf : ty -> ty
                          val listOf : ty -> ty
                          val vectorOf : ty -> ty
                          val arrayOf : ty -> ty
                          val pairOf : ty * ty -> ty
                          val tupleOf : ty list -> ty
                          val function1Of : ty * ty -> ty
                          val function2Of : ty * ty * ty -> ty
                          val function3Of : ty * ty * ty * ty -> ty
                          val promptTagOf : ty -> ty
                          val subcontOf : ty * ty -> ty
                          val IsEqType : constraint
                         ) : sig
                               val typeOf : Primitives.PrimOp -> { vars : (tv * constraint list) list, args : ty vector, result : ty }
                             end = struct
]]

for i, p in ipairs(PRIMITIVES) do
  local head
  if i == 1 then
    head = "fun typeOf "
  else
    head = "  | typeOf "
  end
  local typeVariables = {}
  for _, t in ipairs(p.type.vars) do
    local ct = ""
    if t[3] then
      ct = t[3]
    end
    table.insert(typeVariables, "(" .. t[2] .. ", [" .. ct .. "])")
  end
  local argTypes = {}
  for _, t in ipairs(p.type.args) do
    table.insert(argTypes, t[1])
  end
  local resultType = p.type.result[1]
  f:write(string.format("%sPrimitives.%s = { vars = [%s], args = vector [%s], result = %s }\n", head, p.srcname, table.concat(typeVariables, ", "), table.concat(argTypes, ", "), resultType))
end

f:write[[
end;
]]

f:close()
