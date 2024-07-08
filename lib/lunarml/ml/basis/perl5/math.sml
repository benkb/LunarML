structure Math :> MATH where type real = Real.real = struct
type real = real
val pi : real = Perl.unsafeFromValue Perl.Lib.Math.PI
val sqrt : real -> real = fn x => Perl.unsafeFromValue (Perl.call Perl.Lib.Math.sqrt #[Perl.fromReal x])
val sin : real -> real = fn x => Perl.unsafeFromValue (Perl.call Perl.Lib.Math.sin #[Perl.fromReal x])
val cos : real -> real = fn x => Perl.unsafeFromValue (Perl.call Perl.Lib.Math.cos #[Perl.fromReal x])
val tan : real -> real = fn x => Perl.unsafeFromValue (Perl.call Perl.Lib.Math.tan #[Perl.fromReal x])
val asin : real -> real = fn x => Perl.unsafeFromValue (Perl.call Perl.Lib.Math.asin #[Perl.fromReal x])
val acos : real -> real = fn x => Perl.unsafeFromValue (Perl.call Perl.Lib.Math.acos #[Perl.fromReal x])
val atan : real -> real = fn x => Perl.unsafeFromValue (Perl.call Perl.Lib.Math.atan #[Perl.fromReal x])
val atan2 : real * real -> real = fn (y, x) => Perl.unsafeFromValue (Perl.call Perl.Lib.Math.atan2 #[Perl.fromReal y, Perl.fromReal x])
val exp : real -> real = fn x => Perl.unsafeFromValue (Perl.call Perl.Lib.Math.exp #[Perl.fromReal x])
val e = Perl.unsafeFromValue Perl.Lib.Math.E : real
val pow : real * real -> real = fn (x, y) => Perl.unsafeFromValue (Perl.call Perl.Lib.Math.pow #[Perl.fromReal x, Perl.fromReal y])
val ln : real -> real = fn x => Perl.unsafeFromValue (Perl.call Perl.Lib.Math.log #[Perl.fromReal x])
val log10 : real -> real = fn x => Perl.unsafeFromValue (Perl.call Perl.Lib.Math.log10 #[Perl.fromReal x])
val sinh : real -> real = fn x => Perl.unsafeFromValue (Perl.call Perl.Lib.Math.sinh #[Perl.fromReal x])
val cosh : real -> real = fn x => Perl.unsafeFromValue (Perl.call Perl.Lib.Math.cosh #[Perl.fromReal x])
val tanh : real -> real = fn x => Perl.unsafeFromValue (Perl.call Perl.Lib.Math.tanh #[Perl.fromReal x])
end; (* structure Math *)
