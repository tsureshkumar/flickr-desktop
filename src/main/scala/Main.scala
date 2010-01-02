
import java.io.FileOutputStream

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

  def save_photo(url: String, dir: String) = (new Http)((url) >>> (new FileOutputStream(dir + "/" +  url.slice(url.lastIndexOf("/"),url.length))))
  
  def download_photos (count: int) = {
    var futures = List[Future[Any]]()
    for(p <- new Flickr(key).get_intersting_photos().photos.photo take count) {
      futures += Futures.future {save_photo(p.url_big, folder)}
    }
    futures map { f: Future[Any] => f() }
    println("download complete")
  }

  def main(args: Array[String]) = {
    key = args(0)
    folder = args(1)
    val sync_every = Integer.parseInt(args(2))
    while(true) {
      download_photos(2)
      Thread.sleep(sync_every)
    }
  }

}
