package termware


sealed trait UnificationResult
{
  def isSuccess: Boolean
  def isFailure: Boolean

  def toOption: Option[Substitution]

  def map(f: Substitution => Substitution): UnificationResult

  def flatMap(f: Substitution => UnificationResult): UnificationResult

}

case class UnificationSuccess(s:Substitution) extends UnificationResult
{
  @inline def isSuccess = true
  @inline def isFailure = false
  @inline def substitution = s
  @inline def toOption: Option[Substitution] = Some(s)
  @inline def map(f: Substitution => Substitution) = UnificationSuccess(f(s))
  @inline def flatMap(f: Substitution => UnificationResult): UnificationResult = f(s)

}

case class UnificationFailure(x:Term,y:Term,s:Substitution) extends UnificationResult
{
  @inline def isSuccess = false
  @inline def isFailure = true
  @inline def toOption: Option[Substitution] = None
  @inline def map(f: Substitution => Substitution) = this
  @inline def flatMap(f: Substitution => UnificationResult): UnificationResult = this
}


