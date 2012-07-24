package ua.gradsoft.termware;

trait DefaultTermNames
{

   this : { def symbolTable: SymbolTable; }  =>

   lazy val Rule = symbolTable.getOrCreate("rule");
   lazy val ConditionalRule = symbolTable.getOrCreate("conditionalRule");
   lazy val With = symbolTable.getOrCreate("with");
   lazy val Eta = symbolTable.getOrCreate("eta");
   lazy val Let = symbolTable.getOrCreate("let");
   lazy val Where = symbolTable.getOrCreate("where");
    
}
