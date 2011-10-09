package ua.gradsoft.termware;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;


/**
 * reflcetive facts: facts where input signals and
 T* output signals are mapped to approproative methods
 * of facts objects.
 **/
trait ReflectiveFacts extends FactsDatabase
{

 def resolveSignal(th:Theory, n:Name, a:Int): Option[(Seq[Term],STMSubstitution[Term])=>(Boolean,STMSubstitution[Term])] =
 {
    // use java reflection.
    val myClass = this.getClass;
    val myObj = this;
    myClass.getMethods.flatMap{
            (m:Method) => if ((m.getName)==n.string && Modifier.isPublic(m.getModifiers)) {
                          val rt = m.getReturnType;
                          if (rt == classOf[java.lang.Boolean] || rt == classOf[Boolean]) {
                            methodInvocationWithArgs(m,a,th).map{
                              (f:((Seq[Term],STMSubstitution[Term])=>AnyRef)) =>
                                 {
                                   (args:Seq[Term],s:STMSubstitution[Term]) =>
                                     (f(args,s).asInstanceOf[Boolean].booleanValue, s)
                                 }
                            }
                          } else
                              None
                    } else {
                      None;
                    }
    }.headOption
 }

 def resolveAction(th: Theory, n:Name,a:Int): Option[(Seq[Term],STMSubstitution[Term])=>Unit] =
 {
   this.getClass.getMethods.flatMap{
      (m:Method) => if ((m.getName)==n.string && Modifier.isPublic(m.getModifiers)) {
         // return type always ignored. 
         methodInvocationWithArgs(m,a,th) map {
           (f:((Seq[Term],STMSubstitution[Term])=>AnyRef)) =>
            {
              (args:Seq[Term],s:STMSubstitution[Term]) => f(args,s); ();
            }
         }
      } else None;
   }.headOption
 }

 def resolveFunction(th:Theory, n:Name, a:Int): Option[(Seq[Term],STMSubstitution[Term])=>Term] =
 {
   this.getClass.getMethods.flatMap{
      (m:Method) => if ((m.getName)==n.string && Modifier.isPublic(m.getModifiers)) {
                        methodInvocationWithArgs(m,a,th) map {
                           (f:((Seq[Term],STMSubstitution[Term])=>AnyRef)) =>
                           {
                            (args:Seq[Term],s:STMSubstitution[Term]) => th.fromAnyRef(f(args,s));
                           }
                        }
                    } else None;
   }.headOption
 }



 private def methodInvocationWithArgs(m: Method, a: Int, th:Theory):
     Option[(Seq[Term],STMSubstitution[Term])=>AnyRef] =
 {
  val myObj=this;
  val pt = m.getParameterTypes;
  if (pt.length == a) {
      // TODO: think about exception handling
     Some({ 
        (args:Seq[Term],s:STMSubstitution[Term]) => 
         val jargs = transformArgs(args,a,0);
         m.invoke(myObj,jargs:_*);
     })
  } else if (pt.length == a+1) {
     if (pt(0).isAssignableFrom(classOf[STMSubstitution[Term]])) {
      Some({ 
        (args:Seq[Term],s:STMSubstitution[Term]) => 
        val mArgs = transformArgs(args,pt.length,1);
        mArgs(0)=s;
        th.fromAnyRef(m.invoke(myObj,mArgs:_*));
      }) 
     } else if (pt(pt.length-1).isAssignableFrom(classOf[STMSubstitution[Term]])) {
      Some{
        (args:Seq[Term],s:STMSubstitution[Term]) => 
        val mArgs = transformArgs(args,pt.length,0);
        th.fromAnyRef(m.invoke(myObj,mArgs:_*));
      }
     } else {
        None;
     }
  } else {
     None;
  }
 }


 private def transformArgs(args:Seq[Term],methodArity:Int,offset:Int): 
                                                                Array[AnyRef]=
 {
   var i=0;
   val retval = new Array[AnyRef](methodArity);
   while(i<args.length) {
     retval(i+offset)=args(i).toAnyRef;
     i += 1;
   }
   retval;
 }

}
