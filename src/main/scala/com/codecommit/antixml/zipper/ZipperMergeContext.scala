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
 * Describes the parameters of a merge operation.  See the [[com.codecommit.antixml.Zipper]] trait
 * for formal definitions of these parameters.
 *
 * Note that a merge operation always occurs at a particular conflicted hole (location) within the parent XML tree.
 * All of the ZipperMergeContext attributes are considered to be "located" at that hole. 
 *
 * @param original &nbsp;the original Node that was present at the conflicted hole.
 * @param directUpdate &nbsp;the direct updates for the hole and their corresponding update times. These are the nodes
 * that explicitly replaced `original` via modifications made to the Zipper.
 * @param lastDirectUpdate &nbsp;the largest update time of any direct update to the hole.  If `directUpdates` is empty, this
 * will indicate the time that the node was removed. 
 * @param indirectUpdate &nbsp;the indirect update for the hole and its associated update time.  This node's descendants contains
 * the results of all the updates made to the descendant holes causing the conflict. It's top-level attributes are the 
 * same as those of `original`
 * @see [[com.codecommit.antixml.Zipper]], [[com.codecommit.antixml.ZipperMergeStrategy]]
 */
case class ZipperMergeContext(original: Node, directUpdate: IndexedSeq[(Node,Int)], 
    lastDirectUpdate: Int, indirectUpdate: (Node,Int))

/* TODO - Consider providing the path to the merged node as well as a list of its ancestors. */
