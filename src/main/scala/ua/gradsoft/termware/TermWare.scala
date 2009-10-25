package ua.gradsoft.termware;

object TermWare {

 val instance = new TermWare();

}

class TermWare extends TermWareInstance 
                     with FreeAlgebra
{

 val symbolTable = new SymbolTable(); 
 val freeTheory = this;

}
