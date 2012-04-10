package com.codecommit.antixml
import scala.annotation.tailrec

/**
 * Fetches [[com.codecommit.antixml.ZipperPath]]s from within a [[com.codecommit.antixml.Group]] 
 */
object PathFetcher {
  
  //TODO state monad for caching?
	
  /**  
   * @param source The group upon which the fetching is performed.
   * @param path The path to be fetched, assumed to be valid for the given source.
   * @return An optional node at the end of the path. */
  def getNode(source: Group[Node])(path: ZipperPath): Option[Node] = getNodeRec(source, path)

  @tailrec
  private def getNodeRec(currLevel: Group[Node], path: ZipperPath): Option[Node] = {
    if (path.isEmpty) None
    else if (path.size == 1) Some(currLevel(path.head))
    else {
      // the cast must succeed otherwise the path is invalid
      val children = currLevel(path.head).asInstanceOf[Elem].children
      getNodeRec(children, path.tail)
    }
  }
}