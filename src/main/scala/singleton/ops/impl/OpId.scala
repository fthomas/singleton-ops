package singleton.ops.impl

sealed trait Warn
sealed trait OpId
object OpId {
  sealed trait Arg extends OpId //Argument
  sealed trait AcceptNonLiteral extends OpId
  sealed trait GetArg extends OpId
  sealed trait GetLHSArg extends OpId
  sealed trait ImplicitFound extends OpId
  sealed trait EnumCount extends OpId
  sealed trait ITE extends OpId //If-Then-Else
  sealed trait ==> extends OpId
  sealed trait Id extends OpId
  sealed trait ! extends OpId
  sealed trait Require extends OpId
  sealed trait ToNat extends OpId
  sealed trait ToChar extends OpId
  sealed trait ToInt extends OpId
  sealed trait ToLong extends OpId
  sealed trait ToFloat extends OpId
  sealed trait ToDouble extends OpId
  sealed trait ToString extends OpId
  sealed trait ToSymbol extends OpId
  sealed trait IsNat extends OpId
  sealed trait IsChar extends OpId
  sealed trait IsInt extends OpId
  sealed trait IsLong extends OpId
  sealed trait IsFloat extends OpId
  sealed trait IsDouble extends OpId
  sealed trait IsString extends OpId
  sealed trait IsBoolean extends OpId
  sealed trait IsSymbol extends OpId
  sealed trait IsNonLiteral extends OpId
  sealed trait GetType extends OpId
  sealed trait Reverse extends OpId
  sealed trait Negate extends OpId
  sealed trait NumberOfLeadingZeros extends OpId
  sealed trait + extends OpId
  sealed trait - extends OpId
  sealed trait * extends OpId
  sealed trait / extends OpId
  sealed trait % extends OpId
  sealed trait < extends OpId
  sealed trait <= extends OpId
  sealed trait >= extends OpId
  sealed trait > extends OpId
  sealed trait == extends OpId
  sealed trait != extends OpId
  sealed trait || extends OpId
  sealed trait && extends OpId
  sealed trait Min extends OpId
  sealed trait Max extends OpId
  sealed trait Substring extends OpId
  sealed trait Length extends OpId
  sealed trait CharAt extends OpId
  sealed trait Abs extends OpId
  sealed trait Pow extends OpId
  sealed trait Floor extends OpId
  sealed trait Ceil extends OpId
  sealed trait Round extends OpId
  sealed trait Sin extends OpId
  sealed trait Cos extends OpId
  sealed trait Tan extends OpId
  sealed trait Sqrt extends OpId
  sealed trait Log extends OpId
  sealed trait Log10 extends OpId
}
