module type BYTEWIDTH = sig
  val w : int
  val unsigned : bool
end

module IntValue (B : BYTEWIDTH) = struct
  let encode = fun i -> i land B.w
  let decode = fun t -> if B.unsigned || t < B.w / 2 then t else t - B.w
end

module UInt8 = IntValue (struct let w = 0xff let unsigned = true end)
module Int8 = IntValue (struct let w = 0xff let unsigned = false end)
module UInt16 = IntValue (struct let w = 0xffff let unsigned = true end)
module Int16 = IntValue (struct let w = 0xffff let unsigned = false end)
