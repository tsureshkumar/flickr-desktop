
/*
 *
 *  Copyright (c) 2010 Sureshkumar T <tsureshkumar@gmail.com>
 * 
 *  This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */

import dispatch._
import json._
import JsHttp._

import dispatch.Http._

import scala.actors.Futures._

import java.io.FileOutputStream

import scala.xml._

object Main {

  def anim_xml (in_folder: String => String, urls :Seq[String]) = {
    def _anim_xml(prev :String, url :Seq[String]) : Seq[Node] = {
      url match {
        case x::y => {
            (if(prev != null)
              (<transition><duration>1.0</duration><from>{in_folder(prev)}</from><to>{x}</to></transition>) 
             else List[Node]()
           ) ++ (<static><duration>2.0</duration><file>{in_folder(x)}</file></static>) ++ _anim_xml(x,y)
        }
        case Nil => List[Node]()
      }
    }
    <background>
      <starttime> <year>2009</year> <month>08</month> <day>04</day> <hour>00</hour> <minute>00</minute> <second>00</second> </starttime>
      { _anim_xml(null, urls) }
    </background>
  }

  def main(args: Array[String]) : Unit = {
    val key = args(0)
    val folder = args(1)
    val count = Integer.parseInt(args(2))
    val wait_for_next_sync = Integer.parseInt(args(3))

    def get_file (url: String) = folder + "/" +  url.slice(url.lastIndexOf("/"),url.length)

    val url = :/("api.flickr.com") / "services" / "rest" <<? Map(
      "format" -> "json", 
      "method" -> "flickr.interestingness.getList",
      "nojsoncallback" -> 1,
      "api_key" -> key)
      
    while(true){      
    
      val photos = Http(url ># { js => ('photo ! list)(('photos ! obj)(js)) map {
        v => "http://farm%d.static.flickr.com/%s/%s_%s_b.jpg".format(
          ('farm ! num)(v).intValue,
          ('server ! str)(v),
          ('id ! str)(v),
          ('secret ! str)(v)
        )
      }}) take count
    
      val each_time = 10
      def download_partial(list:List[String]) : List[String] = {
        list match {
          case List(_*) => {
            val first_10  = list take each_time
            println ("downloading...")
            first_10 map { println }
            // download photos parallelly
            val futures = first_10 map { url => future { Http((url) >>> (new FileOutputStream(get_file(url))) ) } }
            XML.saveFull(folder + "/background-1.xml", anim_xml(get_file,first_10),"UTF-8", true, null)
            futures map { _() } // wait for download complete
            println("download completed")
            Thread.sleep(wait_for_next_sync)
            download_partial(list drop each_time)
          }
        }
      }

      download_partial(photos)
    }
  }
}
