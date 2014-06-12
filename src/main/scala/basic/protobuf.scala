package org.codeck.lucepe.basic

import com.google.protobuf.Descriptors.{Descriptor, FieldDescriptor}
import com.google.protobuf.{Message, DynamicMessage, CodedInputStream, ExtensionRegistry}
import com.google.protobuf.Descriptors.{Descriptor, FileDescriptor, FieldDescriptor}
import com.google.protobuf.DescriptorProtos.{FileDescriptorSet, FileDescriptorProto}

import scala.collection.JavaConverters._


class DescriptorOp(val desc: Descriptor, val protos: ProtoSet) {
  def getExts(desc: Descriptor) = {
	protos.ext_records.filter(r => r.base == desc)
  }
  
  def visitMessageDesc(message:Descriptor, v:DFSVisitor) {
	//TODO: how about filter out optional only?
	for (f <- (message.getFields() asScala).filterNot(_.isExtension) sortWith((x, y) => x.getNumber < y.getNumber)) {
	  visitFieldDesc(f, v)
	}
	for (exr <- getExts(message).sortWith((x,y) => x.number < y.number)) {
	  v.pushDown(exr.number)
	  visitMessageDesc(exr.ext, v)
	  v.popUp
	  v.onVisited(exr.number)
	}
  }

  def visitFieldDesc(field:FieldDescriptor, v:DFSVisitor) {
	field.getType() match {
	  case FieldDescriptor.Type.MESSAGE => {
		v.pushDown(field.getNumber)
		visitMessageDesc(field.getMessageType(), v)
		v.popUp()
		v.onVisited(field.getNumber)
	  }
	  case _ => {
		v.onVisited(field.getNumber)
	  }
	}
  }
  
  val protoTagTree = TagTree.fromVisitor(v => {
	visitMessageDesc(desc, v)
  })
  def allPaths() = {
	protoTagTree.iterator
  }
}

class ProtoSet(protoc:Array[Byte], basefile:String) {
  private[this] val fds = FileDescriptorSet.parseFrom(protoc)
  val exts = ExtensionRegistry.newInstance()
  case class ExtRecord(base: Descriptor, number:Int, outer: Descriptor, ext: Descriptor, origin:FieldDescriptor)
  var ext_records = Seq.empty[ExtRecord]
  var base :Option[FileDescriptor] = None

  def findDescriptorOp(name:String) = {
	new DescriptorOp(base.get.findMessageTypeByName(name), this)
  }
  private [this] def register_extension(fd: FieldDescriptor) {
	if (fd.getType == FieldDescriptor.Type.MESSAGE) {
	  exts.add(fd, DynamicMessage.newBuilder(fd.getMessageType).buildPartial())
	  }
	else {
	  exts.add(fd)
	}
	ext_records = ExtRecord(fd.getContainingType, fd.getNumber, fd.getExtensionScope, fd.getMessageType, fd) +: ext_records	
  }

  private def register_all_extensions(mdesc: Descriptor) {
	for (ext <- mdesc.getExtensions.asScala) {
	  register_extension(ext)
	}
	for (mdesc <- mdesc.getNestedTypes.asScala) yield {
	  register_all_extensions(mdesc)
	}	
  }
  
  private[this] def process(tbd: Option[FileDescriptorProto], unkown:Map[String, FileDescriptorProto], known:Map[String, FileDescriptor]):Map[String, FileDescriptor] 
  = tbd match {
	case Some(file) => {
	  val (resolved, unresolved) = file.getDependencyList.asScala.partition(known.isDefinedAt(_))
	  val deps = resolved map known
	  if (file.getDependencyCount == deps.size) {
		val fdesc = FileDescriptor.buildFrom(file, deps.toArray)

		if (file.getName() == basefile) {
		  base = Some(fdesc)
		}
		for (ext <- fdesc.getExtensions.asScala) {
		  register_extension(ext)
		}
		for (mdesc <- fdesc.getMessageTypes.asScala) {
		  //println(mdesc.getFullName() + " " + mdesc)
		  register_all_extensions(mdesc)
		}

		if (unkown.isEmpty) known
		else process(Some(unkown.head._2), unkown.tail, known + ((file.getName, fdesc)))
		
	  }
	  else {
		process(Some(unkown(unresolved.head)), unkown.filterKeys(_ != unresolved.head), known)
	  }
	}
	case None => {
	  process(Some(unkown.head._2), unkown.tail, known)
	}
  }

  process(None, fds.getFileList.asScala.map(f => (f.getName, f)).toMap, Map[String, FileDescriptor]())
  
}
