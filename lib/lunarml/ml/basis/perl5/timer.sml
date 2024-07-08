signature TIMER = sig
    type cpu_timer
    type real_timer
    val startCPUTimer : unit -> cpu_timer
    val checkCPUTimes : cpu_timer -> { nongc : { usr : Time.time, sys : Time.time }
                                     , gc : { usr : Time.time, sys : Time.time }
                                     }
    val checkCPUTimer : cpu_timer -> { usr : Time.time, sys : Time.time }
    val checkGCTime : cpu_timer -> Time.time
    val totalCPUTimer : unit -> cpu_timer
    val startRealTimer : unit -> real_timer
    val checkRealTimer : real_timer -> Time.time
    val totalRealTimer : unit -> real_timer
end;
local
    _esImport [pure] { cpuUsage } from "node:process";
in
structure Timer :> TIMER = struct
type cpu_timer = Perl.value
fun startCPUTimer () = Perl.call cpuUsage #[]
fun checkCPUTimer prev = let val now = Perl.call cpuUsage #[prev]
                             val usr = Perl.unsafeFromValue (Perl.field (now, "user")) : Int54.int (* microseconds *)
                             val sys = Perl.unsafeFromValue (Perl.field (now, "system")) : Int54.int (* microseconds *)
                         in { usr = Time.fromMicroseconds (Int54.toLarge usr)
                            , sys = Time.fromMicroseconds (Int54.toLarge sys)
                            }
                         end
fun checkCPUTimes prev = { nongc = checkCPUTimer prev, gc = { usr = Time.zeroTime, sys = Time.zeroTime } }
fun checkGCTime _ = Time.zeroTime
fun totalCPUTimer () = Perl.undefined
type real_timer = Int54.int
fun startRealTimer () = Perl.unsafeFromValue (Perl.call Perl.Lib.Date.now #[]) : Int54.int
fun checkRealTimer t0 = let val now = Perl.unsafeFromValue (Perl.call Perl.Lib.Date.now #[]) : Int54.int
                        in Time.fromMilliseconds (Int54.toLarge (now - t0))
                        end
local
    val t0 = LunarML.assumeDiscardable (fn () => Perl.unsafeFromValue (Perl.call Perl.Lib.Date.now #[]) : Int54.int) ()
in
fun totalRealTimer () = t0 (* or process.uptime()? *)
end
end
end; (* local *)
