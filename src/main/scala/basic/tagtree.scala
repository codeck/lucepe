package org.codeck.lucepe.basic

object TagPath {
  val STORE_SERVICE_TAG = -1L
  //to be extended
  val INVALID_TAG = -128
  def sysOrdinal(tag:Long) = {
	assert(tag < 0 && tag >= INVALID_TAG)
	-tag
  }
  val MAX_SYS_ORDINAL = sysOrdinal(INVALID_TAG)

  def apply(seqs: Int*) = {
	new TagPathImpl(seqs)
  }
}

abstract class TagPath(val tags: Seq[Int]) {
  override def equals(o: Any) = {
	o match {
	  case peer:TagPath =>
		tags == peer.tags
	  case _ => false
	}
  }
}

trait TagPathMaker extends (()=>TagPath) {
  def apply():TagPath
}

class TagPathImpl(tags: Seq[Int]) extends TagPath(tags) with TagPathMaker {
  var ordinal = TagPath.MAX_SYS_ORDINAL
  def apply() = this
  def onBlessed {}
  def onDoomed {}
  def bless {onBlessed}
  def doom {onDoomed}
}

trait PathFilter {
  def countable() = false
  def accept(path:TagPath):Boolean = false
}

trait DFSVisitor {
  //Depth-First-Search
  def pushDown(id:Int):Unit

  def popUp():Unit

  def onVisited(id:Int):Unit
}

// final class Tree extends Set[TagPath]
// with SetLike[TagPath, TagTree]
// {
//   import TagTree._
//   //TODO:
//   //To replace all Seq[TagPath] etc.
//   //A subtree with finite elements of a hierachical tagging system which is possiblly infinite.
//   //supports the algorithms of incemental update/filter/...
// }

import collection.{mutable, immutable}
  
private[basic] class TagMap[T]
{  
  var suffixes: immutable.SortedMap[Int, TagMap[T]] = immutable.TreeMap.empty
  var value: Option[T] = None
  def size = iterator.size

  def get(s: Seq[Int]): Option[T] = {
    if (s.isEmpty) value
    else suffixes get (s.head) flatMap (_.get(s.tail))
  }

  def withPrefix(s: Seq[Int]): TagMap[T] = 
    if (s.isEmpty) this
    else {
      val leading = s.head
      suffixes get leading match {
        case None =>
          suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes(leading) withPrefix (s.tail)
    }
  
  def update(s: Seq[Int], elem: T) =
    withPrefix(s).value = Some(elem)
  
  def remove(s: Seq[Int]): Option[T] =
    if (s.isEmpty) { val prev = value; value = None; prev }
    else suffixes get (s.head) flatMap (_.remove(s.tail))
  
  def iterator: Iterator[(Seq[Int], T)] = //A DFS visit
	(for ((chr, m) <- suffixes.iterator; 
        (s, v) <- m.iterator) yield (chr +: s, v)) ++  //skip None by "(s, v) <- m.iterator"
	(for (v <- value.iterator) yield (Seq.empty[Int], v))
  
  def += (kv: (Seq[Int], T)): this.type = { update(kv._1, kv._2); this }
  
  def -= (s: Seq[Int]): this.type  = { remove(s); this }
  
  def empty = new TagMap[T]

  override def equals(o: Any) = {
		o match {
	  case peer:TagMap[T] =>
		(value == peer.value) && (suffixes == peer.suffixes)
	  case _ => false
	}
  }

  override def toString() = s"TagMap {$value, $suffixes}"
} 

private[basic] object TagMap extends {
  def empty[T] = new TagMap[T]
  
  def apply[T](kvs: (Seq[Int], T)*): TagMap[T] = {
    val m: TagMap[T] = empty
    for (kv <- kvs) m += kv
    m
  }
} 

object TagTree {
  type TreeNode[T] = TagMap[T]
  def emptyNode[T] = new TreeNode[T]

  trait TagTreeAccess extends DFSVisitor{
	def dfsSkippable():Boolean
  }
  trait IntersectBuilder[T] {
	self: TagTreeAccess =>
	def build:TagTree[T]
  }
  trait TagTreeProxy[T <: TagTreeAccess] extends TagTreeAccess {
	def otherTree:T
	final override def pushDown(id:Int) = otherTree.pushDown(id)
	final override def popUp() = otherTree.popUp()
	final override def onVisited(id:Int) = otherTree.onVisited(id)
	final override def dfsSkippable() = otherTree.dfsSkippable
  }

  private trait FullTreeSubBuilder extends TagTreeAccess with IntersectBuilder[Unit] {
	//TODO: make sure paths with pushDown/popUp but without onVisited striped
	val submaps = mutable.Stack((-1 -> emptyNode[Unit])) //TODO: eliminate mutable stack by changing pushDown/popUp/onVisited to @cps[TagMap] (ref: scala.continuations._)
	override def pushDown(id:Int) {
	  submaps.push(id -> emptyNode)
	}
	override def popUp() {
	  val poping = submaps.pop
	  submaps.top._2.suffixes = (submaps.top._2.suffixes + poping) 
	}
	override def onVisited(id:Int) {
	  val elem = submaps.top._2.suffixes.get(id).getOrElse(emptyNode)
	  elem.value = Some()
	  submaps.top._2.suffixes = (submaps.top._2.suffixes + (id->elem))
	}
	override def dfsSkippable() = false

	override def build = {
	  for (i <- 1 until submaps.length) popUp()
	  new TagTree(submaps.top._2)
	}
  }

  def empty[T] = new TagTree[T]

  def apply(paths: TagPath*) = {
	val r = empty[Unit]
	for (path <- paths) r += (path, ())
	r
  }
  def fromVisitor(guide:(DFSVisitor => Unit)) = {
	val builder = new FullTreeSubBuilder {}
	guide(builder)
	builder.build
  }
}

class TagTree[T](val root: TagTree.TreeNode[T])
{
  import TagTree._

  def this() { this(TagTree.emptyNode) }

  def empty = new TagTree
  def path_iterator: Iterator[TagPath] = root.iterator.map(s => new TagPathImpl(s._1))
  def iterator: Iterator[(TagPath, T)] = root.iterator.map(s => (new TagPathImpl(s._1), s._2))
  def contains(elem: TagPath) = root.get(elem.tags).isDefined
  def -=(elem: TagPath) = {
	root -= elem.tags
	this
  }
  def +=(elem: TagPath, v:T) = {
	root += ((elem.tags, v))
	this
  }
  def count(p:TagPath=>Boolean) = root.iterator.count(s => p(new TagPathImpl(s._1)))

  override def equals(o: Any) = {
		o match {
	  case peer:TagTree[T] =>
		root == peer.root
	  case _ => false
	}
  }

  def isLeaf = root.suffixes.isEmpty
  def getChild(id:Int) = root.suffixes.get(id) map{c => new TagTree(c)}

  def genSubBuilder : TagTreeAccess with IntersectBuilder[T] = {
	//TODO: improve this slow builder..
	//TODO: improve this slow builder..
	//TODO: improve this slow builder..
	new TagTreeAccess with IntersectBuilder[T] {
	  val record = new TagTree.FullTreeSubBuilder {}
	  override def pushDown(id:Int) {
		record.pushDown(id)
	  }
	  override def popUp() {
		record.popUp()
	  }
	  override def onVisited(id:Int) {
		record.onVisited(id)
	  }
	  override def dfsSkippable() = record.dfsSkippable()
	  
	  override def build = {
		var ret = new TagTree[T]
		for (p <- record.build.iterator;
			 v <- get(p._1)) {
		  ret += (p._1, v)
		}
		ret
	  }
	}
  }

  def dynamicIntersect(guide: (TagTreeAccess=>Unit)):TagTree[T] = {
	val builder = genSubBuilder
	guide(builder)
	builder.build
  }

  def iteratorDFS() = {
	//TODO
  }

  def size() = root.size
  def get[Option[T]](p: TagPath) = {
	root.get(p.tags)
  }
  def update(p:TagPath, v:T) = {
	root.update(p.tags, v)
  }

  override def toString() = s"TagTree(root=$root)"
}

class TagPaths extends TagTree(TagTree.emptyNode[Unit]) with PathFilter{
  final override def countable = true

  def add(morepaths: TagPath*) {
	for (p <- morepaths) {
	  this += (p, ())
	}
  }
  private[this] def accept(s:TagMap[Unit], p: Seq[Int]):Boolean = {
	if (s.suffixes.isEmpty || p.isEmpty) true
	else {
	  s.suffixes.get(p.head) match {
		case None => false
		case Some(branch) => accept(branch, p.tail)
	  }
	}
  }
  override def accept(path: TagPath) = accept(root, path.tags)
}

trait BASECONTEXT extends DFSVisitor {
  def rootValue:Array[Byte]
}

trait TagVisitWriter{

  type CONTEXT <: BASECONTEXT
  
  def tag(id: Int)(subwriter: => Unit)(implicit ctx: CONTEXT) {
	ctx.pushDown(id)
	subwriter
	ctx.popUp()
	ctx.onVisited(id)
  }

  def createContext(): CONTEXT

  case class CONTEXTEE(ctx:CONTEXT) {
	final def >>>|(extractor: CONTEXT=>Array[Byte]) = {
	  extractor(ctx)
	}
  }

  final def |>>>(writer: CONTEXT=>Unit) = {
	val ctx = createContext()
	writer(ctx)
	CONTEXTEE(ctx)
  }  
}
