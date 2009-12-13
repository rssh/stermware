package ua.gradsoft.termware;

object TermWare {

 val instance = new TermWare();

 def symbolTable = instance.symbolTable;

}

class TermWare extends TermWareInstance 
                     with FreeAlgebra
{

 val symbolTable = new SymbolTable(); 
 val freeTheory = this;

}
