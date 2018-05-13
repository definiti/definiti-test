package definiti.tests.ast

case class Type(name: String, generics: Seq[Type]) {
  def readableString: String = {
    if (generics.nonEmpty) {
      s"${name}[${generics.map(_.readableString).mkString(", ")}]"
    } else {
      name
    }
  }
}

object Type {
  def apply(name: String, generics: Type*)(implicit dummyImplicit: DummyImplicit): Type = new Type(name, generics)
}