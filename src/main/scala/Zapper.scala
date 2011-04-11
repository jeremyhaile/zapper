package com.earldouglas.zapper

import akka.actor._
import org.scalatra._
import xml.NodeSeq
import xml.XML
 
class Zapper extends ScalatraServlet with UrlSupport {

  import akka.actor.Actor._

  val zapViewer = actorOf[ZapViewer].start
  val zapManager = actorOf(new ZapManager(zapViewer)).start

  val style = 
    <style type="text/css"><!--
       a {
         text-decoration: none;
         color:#0000ff;
       }
      .zap {
        padding: 5px;
        margin: 5px;
        border-bottom: 1px solid #999999;
      }
      #content {
        width: 400px;
        position: absolute;
        top: 20px;
        left: 50;
        padding-right: 25px;
        border-right: 1px solid #cccccc;
      }
      #profile {
        width: 200px;
        position: absolute;
        top: 20px;
        left: 500px;
      }
      #zapForm {
        margin-bottom: 20px;
      }
      -->
    </style>

  before {
    contentType = "text/html"
  }

  get("/") {
    <html>
      <head>
        <title>Zapper</title>
        {style}
      </head>
      <body>
        <div><a href="/user1">User 1</a></div>
      </body>
    </html>
  }

  get("/:userId/zaps") {
    val userId = params("userId").toLowerCase
    zapViewer !! GetZaps(userId) match {
      case None => NodeSeq.Empty
      case Some(x) => x
    }
  }

  get("/:userId/followed") {
    val userId = params("userId").toLowerCase
    zapViewer !! GetFollowed(userId) match {
      case None => NodeSeq.Empty
      case Some(x) => x
    }
  }

  get("/:userId") {
    val userId = params("userId").toLowerCase
    <html>
      <head>
        <title>{userId + " - Zapper"}</title>
        {style}
      </head>
      <body>
        <div id="content">
          <h1>{userId}</h1>
          <form id="zapForm" name="zapForm" action={"/" + userId + "/zaps"} method="post">
            <div>
              <textarea name="zap" rows="2" cols="40"></textarea>
            </div>
            <div>
              <input type="submit" value="Zap" />
            </div>
          </form>
          <div id="zaps"></div>
        </div>
        <div id="profile">
          <h2>Following</h2>
          <form id="followForm" name="followForm" action={"/" + userId + "/followed"} method="post">
            <div>
              <input type="text" size="10" name="followUserId" />
              <input type="submit" value="Follow" />
            </div>
          </form>
          <div id="followed"></div>
        </div>
        <script type="text/javascript">{"""
          function ajaxer(method, url) {
            var ajaxy = false;

            if (window.XMLHttpRequest) {
              ajaxy = new XMLHttpRequest();
            } else if (window.ActiveXObject) {
              ajaxy = new ActiveXObject('Microsoft.XMLHTTP');
            }
            ajaxy.open(method, url, true);
            return ajaxy;
          }

          function loadZaps() {
            var ajaxy = ajaxer('GET', '/""" + userId + """/zaps')
            ajaxy.onreadystatechange = function() {
              if (ajaxy.readyState == 4) {
                document.getElementById('zaps').innerHTML = ajaxy.responseText;
              }
            }
            ajaxy.send();
          }

          function loadFollowed() {
            var ajaxy = ajaxer('GET', '/""" + userId + """/followed')
            ajaxy.onreadystatechange = function() {
              if (ajaxy.readyState == 4) {
                document.getElementById('followed').innerHTML = ajaxy.responseText;
              }
            }
            ajaxy.send();
          }

          function zap() {
            var ajaxy = ajaxer('POST', '/""" + userId + """/zaps')
            ajaxy.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
            ajaxy.send('zap=' + document.forms['zapForm'].zap.value);
            document.forms['zapForm'].reset();
          }

          function follow() {
            var ajaxy = ajaxer('POST', '/""" + userId + """/followed')
            ajaxy.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
            ajaxy.send('followUserId=' + document.forms['followForm'].followUserId.value);
            document.forms['followForm'].reset();
          }

          loadZaps();
          loadFollowed();
          setInterval('loadZaps()', 1000)
          setInterval('loadFollowed()', 1000)
          document.forms['zapForm'].onsubmit = new Function('zap(); return false')
          document.forms['followForm'].onsubmit = new Function('follow(); return false')"""}
        </script>
      </body>
    </html>
  }

  post("/:userId/zaps") {
    val userId = params("userId").toLowerCase
    zapManager ! AddZap(Zap(userId, userId, params("zap")))
    redirect("/" + params("userId"))
  }

  post("/:userId/followed") {
    val userId = params("userId").toLowerCase
    zapManager ! Follow(userId, params("followUserId"))
    redirect("/" + params("userId"))
  }

  protected def contextPath = request.getContextPath
}

case class Zap(val userId: String, val author: String, val text: String)

trait Command
case class AddZap(val zap: Zap) extends Command
case class Follow(val userId: String, val followUserId: String) extends Command

trait Event
case class ZapAdded(val zap: Zap) extends Event
case class Followed(val userId: String, val followUserId: String) extends Event

trait Query
case class GetZaps(val userId: String) extends Query
case class GetFollowed(val userId: String) extends Query

object Mentions {

  val regex = """@\w+""".r

  def ext(userId: String, text: String): Set[String] = {
    (regex findAllIn text toList).map(_.substring(1)).filter(_.toLowerCase != userId).toSet
  }

  def all(text: String): Set[String] = {
    (regex findAllIn text toList).toSet
  }
}

class ZapManager(val eventHandler: ActorRef) extends Actor {

  val zapMap = collection.mutable.HashMap[String, List[Zap]]()
  val followersMap = collection.mutable.HashMap[String, Set[String]]()

  def parseZap(zap: Zap): List[Zap] = {
    val mentions = Mentions.ext(zap.userId, zap.text).map(m => m.toLowerCase).toSet
    zap :: (mentions.map(m => Zap(m, zap.userId, zap.text)) ++ followersMap.getOrElse(zap.userId, Set.empty[String]).map(f => Zap(f, zap.userId, zap.text))).toList
  }

  def receive = {
    case AddZap(zap) =>
      zapMap.put(zap.userId, zap :: zapMap.getOrElse(zap.userId, Nil))
      parseZap(zap).foreach(z => eventHandler ! ZapAdded(z))
    case Follow(userId, followUserId) =>
      val followers: Set[String] = followersMap.getOrElse(followUserId, Set.empty[String])
      if (!followers.contains(userId)) {
        zapMap.getOrElse(followUserId, Nil).foreach(z => if (!z.text.toLowerCase.contains("@" + userId)) eventHandler ! ZapAdded(Zap(userId, z.author, z.text)))
        followersMap.put(followUserId, followers + userId)
        eventHandler ! Followed(userId, followUserId)
      }
  }
}

class ZapViewer extends Actor {

  val zapMap = collection.mutable.HashMap[String, NodeSeq]()
  val followersMap = collection.mutable.HashMap[String, NodeSeq]()

  def add(zap: Zap) {
    var text = zap.text
    Mentions.all(zap.text).foreach { m =>
      text = text.replaceAll(m, "<a href=\"/" + m.substring(1) + "\">" + m + "</a>")
    }
    text = "<span>" + text + "</span>";

    val node =
      <div class="zap">
        <a href={"/" + zap.author}>{zap.author}</a>{": " :+ XML.loadString(text)}
      </div>
    zapMap.put(zap.userId, node +: zapMap.getOrElse(zap.userId, NodeSeq.Empty))
  }

  def follow(userId: String, followUserId: String) {
    val node =
      <div>
        <a href={"/" + followUserId}>{followUserId}</a>
      </div>
    followersMap.put(userId, node +: followersMap.getOrElse(userId, NodeSeq.Empty))
  }


  def receive = {
    case ZapAdded(zap) => add(zap)
    case Followed(userId, followUserId) => follow(userId, followUserId)
    case GetZaps(userId) => self reply zapMap.getOrElse(userId, NodeSeq.Empty)
    case GetFollowed(userId) => self reply followersMap.getOrElse(userId, NodeSeq.Empty)
  }
}
