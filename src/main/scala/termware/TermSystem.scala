package termware


trait TermSystem
{

  def name: Term

  def adopt(t: Term): Term = t

  def unification: Unification = free.Unification 

  def storage: TermStorage = ???

  def serializer: TermSerializer = free.Serializer

  def isFree: Boolean = false

}
