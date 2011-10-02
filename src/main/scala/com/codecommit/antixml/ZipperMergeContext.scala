/*
 * Copyright (c) 2011, Daniel Spiewak
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer. 
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * - Neither the name of "Anti-XML" nor the names of its contributors may
 *   be used to endorse or promote products derived from this software without
 *   specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.codecommit.antixml

import scala.collection.immutable.IndexedSeq

/**
 * Describes the parameters of a merge operation.  
 * 
 * The purpose of a merge 
 *
 * Operations such as `flatMap` make it possible for a zipper's node to be replaced by multiple nodes or to be removed altogether.  For this
 * reason, direct updates are represented as a ''sequence'' of nodes.  In contrast, the indirect update always consists of a single node.  
 * Any multiplicative operations that occurred further down the tree will will already have been accounted for in that node's children. 
 * 
 * @param original the original Node that was selected when the zipper was produced.
 * @param directUpdate the direct replacements of the node and their corresponding update times. These are the nodes
 * that explicitly replaced `original` via updates to its position in the zipper.
 * @param lastDirectUpdate the largest update time of any direct update to the node.  If `directUpdates` is empty, this
 * will be the time that the node was removed. 
 * @param indirectUpdate the "indirect" replacement and associated update time.  The indirect replacement is just the
 * original node with its children replaced by a recursive application of the unselection algorithm, as defined in the 
 * [[com.codecommit.antixml.Zipper]] unselection algorithm.  
 */
case class ZipperMergeContext(original: Node, directUpdate: IndexedSeq[(Node,Int)], 
    lastDirectUpdate: Int, indirectUpdate: (Node,Int))

/* TODO - Consider providing the path to the merged node as well as a list of its ancestors. */
