
import java.io.FileOutputStream

import scala.xml._
import scala.actors._
import dispatch._
import dispatch.liftjson._
import dispatch.liftjson.Js._
import net.liftweb.json.JsonAST._
import scala.util.matching.Regex
import dispatch.Http._;

case class Photo(id: String, owner: String, secret: String, server: String, farm: Int, title: String, ispublic: Int, isfriend: Int, isfamily: Int) {
  def url_big = "http://farm%d.static.flickr.com/%s/%s_%s_b.jpg".format(farm, server, id, secret)
  def url_small = "http://farm%d.static.flickr.com/%s/%s_%s_s.jpg".format(farm, server, id, secret)
}
case class Photos(page: Int, pages: Int, perpage: Int, total: Int, photo: List[Photo])
case class InterestingPhotos(photos: Photos, stat: String)

class Flickr(key:String) {
  implicit val formats = net.liftweb.json.DefaultFormats // Brings in default date formats etc.
  def get_intersting_photos() = {
    val url = :/("api.flickr.com") / "services" / "rest" <<? Map(
      "format" -> "json", 
      "method" -> "flickr.interestingness.getList",
//      "date" -> new java.text.SimpleDateFormat("yyyy-MM-DD").format(new java.util.Date()),
      "nojsoncallback" -> 1,
      "api_key" -> key)
    (new Http) (url ># { js => 
      js.extract[InterestingPhotos]
                      })
  }
}

object Main {
  var key = ""
  var folder = "/tmp"

  def save_photo(url: String, dir: String) = { 
    def local_file = dir + "/" +  url.slice(url.lastIndexOf("/"),url.length)
    (new Http)((url) >>> (new FileOutputStream(local_file)))
    local_file
  }
  
  def download_photos (count: int) = {
    var futures = List[Future[Any]]()
    for(p <- new Flickr(key).get_intersting_photos().photos.photo take count) {
      futures += Futures.future {save_photo(p.url_big, folder)}
    }
    val ret = futures map { f: Future[Any] => f() } 
    println("download complete")
    ret.asInstanceOf[List[String]]
  }
  
  def write_animation_xml(files: List[String]) = {
    val start =  <starttime>
                    <year>2009</year>
                        <month>08</month>
                        <day>04</day>
                        <hour>00</hour>
                        <minute>00</minute>
                        <second>00</second>
                </starttime>;
    var xml:List[Node] = List()
    var prev :String = null
    for(img <- files) {
      if(prev != null) {
        xml ::= (<transition>
                <duration>5.0</duration>
                <from>{prev}</from>
                <to>{img}</to>
               </transition>)
      }
      xml ::=  <static>
                  <duration>1795.0</duration>
                  <file>{img}</file>
                 </static>;
      prev = img
    }
    <background>{start}{xml}</background>
  }

  def main(args: Array[String]) = {
    key = args(0)
    folder = args(1)
    val each_time = Integer.parseInt(args(2))
    val sync_every = Integer.parseInt(args(3))
    var cond = true
    while(cond) {
      val downloaded = download_photos(each_time)
      //val downloaded = List("abcd", "cdef", "ijkl")
      XML.saveFull(folder + "/background-1.xml", write_animation_xml(downloaded),"UTF-8", true, null)
      //Thread.sleep(sync_every)
      cond = false
    }
  }

}
