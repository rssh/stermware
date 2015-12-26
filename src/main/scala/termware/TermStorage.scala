package termware


trait TermStorage
{

  def  read(path: String): Term

  def  write(path: String, t: Term): Unit

}
