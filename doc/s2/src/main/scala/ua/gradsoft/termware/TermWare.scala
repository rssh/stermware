package ua.gradsoft.termware;

object TermWare {

 val symbolTable = new SymbolTable();
 val instance = new TermWare();

}

class TermWare extends TermWareInstance 
                     with FreeAlgebra
{

 def symbolTable = TermWare.symbolTable; 
 val freeTheory = this;

 override lazy val typeAlgebra = new DefaultTypeAlgebra(this);

}
