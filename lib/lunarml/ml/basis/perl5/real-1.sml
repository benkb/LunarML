signature REAL = sig
    type real
    (* structure Math *)
    val radix : int
    val precision : int
    val maxFinite : real
    val minPos : real
    val minNormalPos : real
    val posInf : real
    val negInf : real
    val + : real * real -> real
    val - : real * real -> real
    val * : real * real -> real
    val / : real * real -> real
    val rem : real * real -> real
    (* val *+ : real * real * real -> real *)
    (* val *- : real * real * real -> real *)
    val ~ : real -> real
    val abs : real -> real
    val min : real * real -> real
    val max : real * real -> real
    val sign : real -> int
    val signBit : real -> bool
    val sameSign : real * real -> bool
    val copySign : real * real -> real
    val compare : real * real -> order
    val compareReal : real * real -> IEEEReal.real_order
    val < : real * real -> bool
    val <= : real * real -> bool
    val > : real * real -> bool
    val >= : real * real -> bool
    val == : real * real -> bool
    val != : real * real -> bool
    val ?= : real * real -> bool
    val unordered : real * real -> bool
    val isFinite : real -> bool
    val isNan : real -> bool
    val isNormal : real -> bool
    val class : real -> IEEEReal.float_class
    val toManExp : real -> { man : real, exp : int }
    val fromManExp : { man : real, exp : int } -> real
    val split : real -> { whole : real, frac : real }
    val realMod : real -> real
    (* val nextAfter : real * real -> real *)
    val checkFloat : real -> real
    val realFloor : real -> real
    val realCeil : real -> real
    val realTrunc : real -> real
    val realRound : real -> real
    val floor : real -> int
    val ceil : real -> int
    val trunc : real -> int
    val round : real -> int
    val toInt : IEEEReal.rounding_mode -> real -> int
    (* val toLargeInt : IEEEReal.rounding_mode -> real -> LargeInt.int *)
    val fromInt : int -> real
    (* val fromLargeInt : LargeInt.int -> real *)
    (* val toLarge : real -> LargeReal.real *)
    (* val fromLarge : IEEEReal.rounding_mode -> LargeReal.real -> real *)
    val fmt : StringCvt.realfmt -> real -> string
    val toString : real -> string
    (* val scan : (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader; implemented in scan-num.sml *)
    (* val fromString : string -> real option; implemented in scan-num.sml *)
    (* val toDecimal : real -> IEEEReal.decimal_approx *)
    (* val fromDecimal : IEEEReal.decimal_approx -> real option *)
end;

structure Real : REAL where type real = real = struct
val radix : int = 2
val precision : int = 53 (* binary64 *)
val posInf = Perl.unsafeFromValue Perl.Lib.Number.POSITIVE_INFINITY : real
val negInf = Perl.unsafeFromValue Perl.Lib.Number.NEGATIVE_INFINITY : real
fun == (x, y) = Perl.=== (Perl.fromReal x, Perl.fromReal y)
fun != (x, y) = Perl.!== (Perl.fromReal x, Perl.fromReal y)
infix 4 == !=
fun isNan x = Perl.unsafeFromValue (Perl.call Perl.Lib.Number.isNaN #[Perl.fromReal x]) : bool
fun ?= (x, y) = x == y orelse isNan x orelse isNan y (* EQUAL or UNORDERED *)
fun unordered (x, y) = isNan x orelse isNan y
fun isFinite x = Perl.unsafeFromValue (Perl.call Perl.Lib.Number.isFinite #[Perl.fromReal x])
val maxFinite = Perl.unsafeFromValue Perl.Lib.Number.MAX_VALUE : real (* 0x1.fffffffffffffp1023; assuming binary64 *)
val minPos = Perl.unsafeFromValue Perl.Lib.Number.MIN_VALUE : real (* 0x1p-1074; assuming binary64 *)
val minNormalPos = 0x1p~1022 : real (* 0x1p-1022; assuming binary64 *)
fun isNormal x = isFinite x andalso minNormalPos <= abs x
fun class x = if x == 0.0 then
                  IEEEReal.ZERO
              else
                  if isFinite x then
                      (* normal or subnormal *)
                      if minNormalPos <= abs x then
                          IEEEReal.NORMAL
                      else
                          IEEEReal.SUBNORMAL
                  else
                      (* infinity or NaN *)
                      if isNan x then
                          IEEEReal.NAN
                      else
                          IEEEReal.INF
fun rem (x : real, y : real) : real = Perl.unsafeFromValue (Perl.% (Perl.fromReal x, Perl.fromReal y))
fun min (x : real, y : real) = if isNan x then
                                   y
                               else if isNan y then
                                   x
                               else
                                   (* Math.min: propagates NaN and honors the sign of zero *)
                                   Perl.unsafeFromValue (Perl.call Perl.Lib.Math.min #[Perl.fromReal x, Perl.fromReal y])
fun max (x : real, y : real) = if isNan x then
                                   y
                               else if isNan y then
                                   x
                               else
                                   (* Math.max: propagates NaN and honors the sign of zero *)
                                   Perl.unsafeFromValue (Perl.call Perl.Lib.Math.max #[Perl.fromReal x, Perl.fromReal y])
fun sign x = if x == 0.0 then
                 0
             else if x < 0.0 then
                 ~1
             else if x > 0.0 then
                 1
             else (* NaN *)
                 raise Domain
fun signBit x = if x < 0.0 then
                    true
                else if x > 0.0 then
                    false
                else
                    1.0 / x < 0.0 (* handle negative zero; NaN is not handled *)
fun sameSign (x, y) = signBit x = signBit y
fun copySign (x, y) = if signBit x = signBit y then
                          x
                      else
                          ~ x
fun compare (x, y) = if isNan x orelse isNan y then
                         raise IEEEReal.Unordered
                     else
                         if x < y then
                             LESS
                         else if x == y then
                             EQUAL
                         else
                             GREATER
fun compareReal (x, y) = if isNan x orelse isNan y then
                             IEEEReal.UNORDERED
                         else
                             if x < y then
                                 IEEEReal.LESS
                             else if x == y then
                                 IEEEReal.EQUAL
                             else
                                 IEEEReal.GREATER
(* TODO: We may have math.frexp *)
(* Assumption: 2^exp is exact *)
fun toManExp x = let val a = abs x
                 in if a == 0.0 orelse a == posInf orelse isNan a then
                        { man = x, exp = 0 }
                    else
                        let val e0 : int = Perl.unsafeFromValue (Perl.call Perl.Lib.Math.floor #[Perl.call Perl.Lib.Math.log2 #[Perl.fromReal a]]) - 1
                            fun fixup e = let val lower = Perl.unsafeFromValue (Perl.** (Perl.fromReal 2.0, Perl.fromInt (e - 1))) : real
                                          in if lower <= a then
                                                 if a < lower * 2.0 then (* lower * 2.0 may be infinity *)
                                                     { exp = e, man = x / lower * 0.5 }
                                                 else
                                                     fixup (e + 1)
                                             else
                                                 fixup (e - 1)
                                          end
                        in fixup e0
                        end
                 end
(* TODO: We may have math.ldexp *)
(* Assumption: 2^exp is exact *)
fun fromManExp { man : real, exp : int } = if ~1022 <= exp then
                                               if exp < 1024 then
                                                   man * Perl.unsafeFromValue (Perl.** (Perl.fromReal 2.0, Perl.fromInt exp))
                                               else
                                                   let val exp' = if exp > 2098 then
                                                                      2098 (* 0x1p1023 / 0x1p~1074 = 0x1p2097 *)
                                                                  else
                                                                      exp
                                                   in fromManExp { man = man * 0x1p1023, exp = exp' - 1023 } (* Avoid undue overflow *)
                                                   end
                                           else
                                               let val exp' = if exp < ~2099 then
                                                                  ~2099 (* 0x1p~1074 / 0x1p1024 = 0x1p~2098 *)
                                                              else
                                                                  exp
                                                   val j = exp' mod ~1022 (* ~1022 < j <= 0 *)
                                               in if j <> 0 then
                                                      let val s = Perl.unsafeFromValue (Perl.** (Perl.fromReal 2.0, Perl.fromInt j))
                                                      in fromManExp { man = man * s, exp = exp' - j }
                                                      end
                                                  else
                                                      fromManExp { man = man * 0x1p~1022, exp = exp' + 1022 }
                                               end (* Avoid undue underflow and double rounding *)
fun split x = let val intPart = Perl.unsafeFromValue (Perl.call Perl.Lib.Math.trunc #[Perl.fromReal x]) : real
                  val fracPart = Perl.unsafeFromValue (Perl.% (Perl.fromReal x, Perl.fromReal 1.0)) : real
                  val frac = if isNan fracPart then (* x: infinity or NaN *)
                                 0.0 / x
                             else
                                 fracPart
              in { whole = intPart, frac = frac }
              end
fun realMod x = let val y = Perl.unsafeFromValue (Perl.% (Perl.fromReal x, Perl.fromReal 1.0)) : real
                in if isNan y then (* x: infinity or NaN *)
                       0.0 / x
                   else
                       y
                end
fun checkFloat x = if isNan x then
                       raise Div
                   else if x == posInf orelse x == negInf then
                       raise Overflow
                   else
                       x
fun realFloor x = Perl.unsafeFromValue (Perl.call Perl.Lib.Math.floor #[Perl.fromReal x]) : real
fun realCeil x = Perl.unsafeFromValue (Perl.call Perl.Lib.Math.ceil #[Perl.fromReal x]) : real
fun realTrunc x = Perl.unsafeFromValue (Perl.call Perl.Lib.Math.trunc #[Perl.fromReal x]) : real
(* round to nearest even; Perl's Math.round breaks ties by preferring the Number closer to +inf *)
fun realRound x = let val intPart = Perl.unsafeFromValue (Perl.call Perl.Lib.Math.round #[Perl.fromReal x]) : real
                      val intPartIsEven = Perl.=== (Perl.% (Perl.fromReal intPart, Perl.fromReal 2.0), Perl.fromReal 0.0)
                      val fracPart = Perl.unsafeFromValue (Perl.% (Perl.fromReal x, Perl.fromReal 1.0)) : real
                  in if (fracPart == 0.5 orelse fracPart == ~0.5) andalso not intPartIsEven then
                         intPart - 1.0
                     else
                         intPart
                  end
fun resultToInt x = if isNan x then
                        raise Domain
                    else if x < ~0x80000000p0 orelse x > 0x7fffffffp0 then
                        raise Overflow
                    else
                        _primCall "Int32.toInt.unchecked" (Perl.toInt32 (Perl.fromReal x))
fun floor x = resultToInt (realFloor x)
fun ceil x = resultToInt (realCeil x)
fun trunc x = resultToInt (realTrunc x)
fun round x = resultToInt (realRound x)
fun toInt IEEEReal.TO_NEGINF = floor
  | toInt IEEEReal.TO_POSINF = ceil
  | toInt IEEEReal.TO_ZERO = trunc
  | toInt IEEEReal.TO_NEAREST = round
fun fromInt (x : int) : real = Unsafe.cast x
fun fmt (StringCvt.SCI prec) r = let val prec = Option.getOpt (prec, 6)
                                     val () = if prec < 0 then
                                                  raise Size
                                              else
                                                  ()
                                 in if isNan r then
                                        "nan"
                                    else if r == posInf then
                                        "inf"
                                    else if r == negInf then
                                        "~inf"
                                    else
                                        let val result = Perl.method (Perl.fromReal r, "toExponential") #[Perl.fromInt prec] (* TODO: Is this OK? *)
                                            val result = Perl.method (result, "replaceAll") #[Perl.fromWideString "-", Perl.fromWideString "~"]
                                        in Perl.encodeUtf8 (Perl.unsafeFromValue result : WideString.string)
                                        end
                                 end
  | fmt (StringCvt.FIX prec) r = let val prec = Option.getOpt (prec, 6)
                                     val () = if prec < 0 then
                                                  raise Size
                                              else
                                                  ()
                                 in if isNan r then
                                        "nan"
                                    else if r == posInf then
                                        "inf"
                                    else if r == negInf then
                                        "~inf"
                                    else
                                        let val result = Perl.method (Perl.fromReal r, "toFixed") #[Perl.fromInt prec] (* TODO: Is this OK? *)
                                            val result = Perl.method (result, "replaceAll") #[Perl.fromWideString "-", Perl.fromWideString "~"]
                                        in Perl.encodeUtf8 (Perl.unsafeFromValue result : WideString.string)
                                        end
                                 end
  | fmt (StringCvt.GEN prec) r = let val prec = Option.getOpt (prec, 12)
                                     val () = if prec < 1 then
                                                  raise Size
                                              else
                                                  ()
                                 in if isNan r then
                                        "nan"
                                    else if r == posInf then
                                        "inf"
                                    else if r == negInf then
                                        "~inf"
                                    else
                                        let val result = Perl.method (Perl.fromReal r, "toPrecision") #[Perl.fromInt prec] (* TODO: Is this OK? *)
                                            val result = Perl.method (result, "replaceAll") #[Perl.fromWideString "-", Perl.fromWideString "~"]
                                            val result = Perl.method (result, "toUpperCase") #[]
                                        in Perl.encodeUtf8 (Perl.unsafeFromValue result : WideString.string)
                                        end
                                 end
  | fmt StringCvt.EXACT r = raise Fail "Real.fmt StringCvt.EXACT: not implemented yet"
val toString = fmt (StringCvt.GEN NONE)
open Real (* +, -, *, /, ~, abs, <, <=, >, >= *)
end; (* structure Real *)
