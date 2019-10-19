import java.io.File

object main {

  def main(args: Array[String]): Unit = {
    var fileSizes: List[(String, Long)] = List.empty[(String, Long)]
    if(args.length != 1){
      System.err.println("Please provide a single path as argument")
      System.exit(1)
    }
    val d = new File(args(0))
    if(!d.exists() || !d.isDirectory){
      System.err.println(s"${args(0)} is not a path to a directory")
      System.exit(1)
    }
    val files = d.listFiles.filter(f => f.isDirectory || f.isFile)
    //println(files.toList)
    files.foreach(f =>{
      val el = (f.getName, getSize(f))
      val fun = (x:(String,Long),y:(String,Long)) => {val res=x._2-y._2;if( res> 0) 1 else if (res == 0) 0 else -1}
      fileSizes = insert[(String, Long)](fileSizes, el, fun)
    })
    fileSizes.foreach(t => println(t._1 + " " +longToBytes(t._2)))
  }

  def longToBytes(size:Long):String ={
    assert(size >= 0)
    if(size < 1024){s"$size B"}
    else if (size < 1024*1024) {
      val num  = toFrac(size, 1024)
      s"${num._1}.${num._2} KB"
    }
    else if (size < 1024*1024*1024) {
      val num  = toFrac(size, 1024*1024)
      s"${num._1}.${num._2} MB"
    }
    else if (size < 1024L*1024L*1024L*1024L) {
      val num  = toFrac(size, 1024*1024*1024)
      s"${num._1}.${num._2} GB"
    }
    else{
      ">1 TB"
    }
  }

  def toFrac(size:Long, unitSize:Long):(Long, Long) ={
    val nom = size / unitSize
    val res = (size % unitSize) *100 /unitSize
    (nom, res)
  }

  def insert[T](l:List[T], el:T, f:(T,T)=>Int):List[T]= l match {
    case Nil => el::Nil
    case x::xs => if(f(el,x) <= 0) el::l else x:: insert(xs,  el, f)
  }

  def getSize(f:File):Long ={
    //println(f)
    if(f.isDirectory){
      val list = if (f.listFiles() != null) f.listFiles() else Array.empty[File]
      list.foldLeft(0L)((i,f) => i+getSize(f))
    }
    else{
      f.length()
    }
  }


}
