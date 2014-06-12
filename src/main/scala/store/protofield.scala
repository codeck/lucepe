import org.apache.lucene.document.{Document, Field, FieldType, StringField, LongField, NumericDocValuesField, BinaryDocValuesField}
import org.apache.lucene.index.{AtomicReader, IndexReader, IndexWriter, IndexWriterConfig, IndexableField, NoMergePolicy, FieldInfo, DirectoryReader, TermsEnum, DocsEnum}
import org.apache.lucene.analysis.{Token, TokenStream}
import org.apache.lucene.analysis.tokenattributes.{CharTermAttribute, OffsetAttribute, PayloadAttribute, PositionIncrementAttribute}

import scala.collection.JavaConverters._

import com.spatial4j.core.context.jts.{JtsSpatialContext => SpatialContext, JtsSpatialContextFactory}
import org.apache.lucene.spatial.prefix.{RecursivePrefixTreeStrategy}
import org.apache.lucene.spatial.prefix.tree.{SpatialPrefixTree, GeohashPrefixTree, QuadPrefixTree}

