functor TypedArrayMonoSequence (type elem
                                val TypedArray : Perl.value
                                val fromValue : Perl.value -> elem
                                val toValue : elem -> Perl.value
                               ) :> sig
            structure MonoVector : MONO_VECTOR where type elem = elem
            structure MonoVectorSlice : MONO_VECTOR_SLICE where type elem = elem
            structure MonoArray : MONO_ARRAY_NOEQTYPE where type elem = elem
            structure MonoArraySlice : MONO_ARRAY_SLICE where type elem = elem
            structure UnsafeMonoVector : UNSAFE_MONO_VECTOR where type elem = elem
            structure UnsafeMonoArray : UNSAFE_MONO_ARRAY where type elem = elem
            sharing type MonoVector.vector = MonoVectorSlice.vector = MonoArray.vector = MonoArraySlice.vector = UnsafeMonoVector.vector
            sharing type MonoVectorSlice.slice = MonoArraySlice.vector_slice
            sharing type MonoArray.array = MonoArraySlice.array = UnsafeMonoArray.array
            val arrayEq : MonoArray.array * MonoArray.array -> bool
        end = struct
structure P = struct
type elem = elem
type vector = Perl.value
type array = Perl.value
structure MonoVector = struct
val maxLen = 0x7fffffff (* not really used *)
fun length v : int = Perl.unsafeFromValue (Perl.field (v, "length"))
fun unsafeSub (v, i) : elem = fromValue (Perl.sub (v, Perl.fromInt i))
fun unsafeFromListN (n, xs : elem list) = let val v = Perl.new TypedArray #[Perl.fromInt n]
                                              fun go (i, []) = v
                                                | go (i, x :: xs) = (Perl.set (v, Perl.fromInt i, toValue x); go (i + 1, xs))
                                          in go (0, xs)
                                          end
fun unsafeFromListRevN (n, xs : elem list) = let val v = Perl.new TypedArray #[Perl.fromInt n]
                                                 fun go (i, []) = v
                                                   | go (i, x :: xs) = (Perl.set (v, Perl.fromInt i, toValue x); go (i - 1, xs))
                                             in go (n - 1, xs)
                                             end
fun fromList xs = unsafeFromListN (List.length xs, xs)
fun concat vs = let val n = List.foldl (fn (v, n) => n + length v) 0 vs
                    val w = Perl.new TypedArray #[Perl.fromInt n]
                    fun go (i, []) = w
                      | go (i, v :: vs) = (Perl.method (w, "set") #[v, Perl.fromInt i]; go (i + length v, vs))
                in go (0, vs)
                end
fun sliceToVector { base, start, length } = Perl.method (base, "slice") #[Perl.fromInt start, Perl.+ (Perl.fromInt start, Perl.fromInt length)]
fun shallowSliceToVector { base, start, length } = Perl.method (base, "subarray") #[Perl.fromInt start, Perl.+ (Perl.fromInt start, Perl.fromInt length)]
end
structure MonoArray = struct
val maxLen = 0x7fffffff (* not really used *)
val eq = Perl.===
fun length v : int = Perl.unsafeFromValue (Perl.field (v, "length"))
fun unsafeCreateWithZero n = Perl.new TypedArray #[Perl.fromInt n]
fun unsafeCreate (n, x : elem) = let val a = Perl.new TypedArray #[Perl.fromInt n]
                                 in Perl.method (a, "fill") #[toValue x]
                                  ; a
                                 end
val fromList = MonoVector.fromList
val unsafeFromListN = MonoVector.unsafeFromListN
val unsafeSub = MonoVector.unsafeSub
fun unsafeUpdate (a, i, x : elem) = Perl.set (a, Perl.fromInt i, toValue x)
end
end
structure S = MonoSequence (P)
open S
val arrayEq = P.MonoArray.eq
end;

local
    structure S = TypedArrayMonoSequence (type elem = bool
                                          val TypedArray = Perl.Lib.Uint8Array
                                          fun fromValue (x : Perl.value) : bool = not (Perl.isFalsy x)
                                          val toValue = Perl.unsafeToValue (* implicit conversion *)
                                         )
    _equality S.MonoArray.array = S.arrayEq
in
structure BoolVector = S.MonoVector
structure BoolVectorSlice = S.MonoVectorSlice
structure BoolArray = S.MonoArray
structure BoolArraySlice = S.MonoArraySlice
structure UnsafeBoolVector = S.UnsafeMonoVector
structure UnsafeBoolArray = S.UnsafeMonoArray
end;

local
    structure S = TypedArrayMonoSequence (type elem = int; val TypedArray = Perl.Lib.Int32Array; val fromValue = Perl.unsafeFromValue; val toValue = Perl.unsafeToValue)
    _equality S.MonoArray.array = S.arrayEq
in
structure IntVector = S.MonoVector
structure IntVectorSlice = S.MonoVectorSlice
structure IntArray = S.MonoArray
structure IntArraySlice = S.MonoArraySlice
structure UnsafeIntVector = S.UnsafeMonoVector
structure UnsafeIntArray = S.UnsafeMonoArray
end;

local
    structure S = TypedArrayMonoSequence (type elem = Int8.int; val TypedArray = Perl.Lib.Int8Array; val fromValue = Perl.unsafeFromValue; val toValue = Perl.unsafeToValue)
    _equality S.MonoArray.array = S.arrayEq
in
structure Int8Vector = S.MonoVector
structure Int8VectorSlice = S.MonoVectorSlice
structure Int8Array = S.MonoArray
structure Int8ArraySlice = S.MonoArraySlice
structure UnsafeInt8Vector = S.UnsafeMonoVector
structure UnsafeInt8Array = S.UnsafeMonoArray
end;

local
    structure S = TypedArrayMonoSequence (type elem = Int16.int; val TypedArray = Perl.Lib.Int16Array; val fromValue = Perl.unsafeFromValue; val toValue = Perl.unsafeToValue)
    _equality S.MonoArray.array = S.arrayEq
in
structure Int16Vector = S.MonoVector
structure Int16VectorSlice = S.MonoVectorSlice
structure Int16Array = S.MonoArray
structure Int16ArraySlice = S.MonoArraySlice
structure UnsafeInt16Vector = S.UnsafeMonoVector
structure UnsafeInt16Array = S.UnsafeMonoArray
end;

local
    structure S = TypedArrayMonoSequence (type elem = Int32.int; val TypedArray = Perl.Lib.Int32Array; val fromValue = Perl.unsafeFromValue; val toValue = Perl.unsafeToValue)
    _equality S.MonoArray.array = S.arrayEq
in
structure Int32Vector = S.MonoVector
structure Int32VectorSlice = S.MonoVectorSlice
structure Int32Array = S.MonoArray
structure Int32ArraySlice = S.MonoArraySlice
structure UnsafeInt32Vector = S.UnsafeMonoVector
structure UnsafeInt32Array = S.UnsafeMonoArray
end;

local
    structure S = TypedArrayMonoSequence (type elem = Int64.int; val TypedArray = Perl.Lib.BigInt64Array; val fromValue = Perl.unsafeFromValue; val toValue = Perl.unsafeToValue)
    _equality S.MonoArray.array = S.arrayEq
in
structure Int64Vector = S.MonoVector
structure Int64VectorSlice = S.MonoVectorSlice
structure Int64Array = S.MonoArray
structure Int64ArraySlice = S.MonoArraySlice
structure UnsafeInt64Vector = S.UnsafeMonoVector
structure UnsafeInt64Array = S.UnsafeMonoArray
end;

local
    structure S = TypedArrayMonoSequence (type elem = word; val TypedArray = Perl.Lib.Uint32Array; val fromValue = Perl.unsafeFromValue; val toValue = Perl.unsafeToValue)
    _equality S.MonoArray.array = S.arrayEq
in
structure WordVector = S.MonoVector
structure WordVectorSlice = S.MonoVectorSlice
structure WordArray = S.MonoArray
structure WordArraySlice = S.MonoArraySlice
structure UnsafeWordVector = S.UnsafeMonoVector
structure UnsafeWordArray = S.UnsafeMonoArray
end;

local
    structure S = TypedArrayMonoSequence (type elem = Word16.word; val TypedArray = Perl.Lib.Uint16Array; val fromValue = Perl.unsafeFromValue; val toValue = Perl.unsafeToValue)
    _equality S.MonoArray.array = S.arrayEq
in
structure Word16Vector = S.MonoVector
structure Word16VectorSlice = S.MonoVectorSlice
structure Word16Array = S.MonoArray
structure Word16ArraySlice = S.MonoArraySlice
structure UnsafeWord16Vector = S.UnsafeMonoVector
structure UnsafeWord16Array = S.UnsafeMonoArray
end;

local
    structure S = TypedArrayMonoSequence (type elem = Word32.word; val TypedArray = Perl.Lib.Uint32Array; val fromValue = Perl.unsafeFromValue; val toValue = Perl.unsafeToValue)
    _equality S.MonoArray.array = S.arrayEq
in
structure Word32Vector = S.MonoVector
structure Word32VectorSlice = S.MonoVectorSlice
structure Word32Array = S.MonoArray
structure Word32ArraySlice = S.MonoArraySlice
structure UnsafeWord32Vector = S.UnsafeMonoVector
structure UnsafeWord32Array = S.UnsafeMonoArray
end;

local
    structure S = TypedArrayMonoSequence (type elem = Word64.word; val TypedArray = Perl.Lib.BigUint64Array; val fromValue = Perl.unsafeFromValue; val toValue = Perl.unsafeToValue)
    _equality S.MonoArray.array = S.arrayEq
in
structure Word64Vector = S.MonoVector
structure Word64VectorSlice = S.MonoVectorSlice
structure Word64Array = S.MonoArray
structure Word64ArraySlice = S.MonoArraySlice
structure UnsafeWord64Vector = S.UnsafeMonoVector
structure UnsafeWord64Array = S.UnsafeMonoArray
end;

local
    structure S = TypedArrayMonoSequence (type elem = real; val TypedArray = Perl.Lib.Float64Array; val fromValue = Perl.unsafeFromValue; val toValue = Perl.unsafeToValue)
    _equality S.MonoArray.array = S.arrayEq
in
structure RealVector = S.MonoVector
structure RealVectorSlice = S.MonoVectorSlice
structure RealArray = S.MonoArray
structure RealArraySlice = S.MonoArraySlice
structure UnsafeRealVector = S.UnsafeMonoVector
structure UnsafeRealArray = S.UnsafeMonoArray
end;
