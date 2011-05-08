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

/**
 * The default XML parser instance for the Anti-XML framework.  This is really
 * just a convenience instance of [[com.codecommit.antixml.XMLParser]].  The
 * default parser (currently) uses the Java StAX framework under the surface,
 * though the parser interface is also 100% compatible with the SAX2 framework
 * (see: [[com.codecommit.antixml.SAXParser]]).  The StAX implementation is the
 * default primarily for performance reasons.
 *
 * It is possible to reuse some of Anti-XML's internal parser infrastructure to
 * parse into Anti-XML trees from alternative parse frameworks, such as HTML
 * parsers (think: [[http://home.ccil.org/~cowan/XML/tagsoup/ TagSoup]]).
 * This infrastructure is exposed via
 * the [[com.codecommit.antixml.NodeSeqSAXHandler]] class.  Unlike scala.xml,
 * Anti-XML does not allow extension of its [[com.codecommit.antixml.Node]] construction
 * process.  Thus, it is not possible to define (or directly parse into)
 * custom [[com.codecommit.antixml.Node]] instances.  This capability wouldn't
 * make much sense though, since [[com.codecommit.antixml.Node]] is sealed. It
 * is not possible to even ''define'' custom instances, much less produce them
 * as part of the parse process.
 * 
 * @see [[com.codecommit.antixml.StAXParser]], [[com.codecommit.antixml.SAXParser]]
 */
object XML extends StAXParser
