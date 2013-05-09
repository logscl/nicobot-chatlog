package controllers

import models.Message
import play.api.Logger
import play.api.Play
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.ws.WS
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.templates.Html
import scala.util.Success
import scala.util.Random
import org.apache.commons.lang3.StringEscapeUtils

object Application extends Controller {

  /**
   * Variable pour savoir si le message suivant est du même jour ou pas
   */
  private var curDay = "";

  /**
   * Couleurs pour les pseudos
   * TODO retirer les couleurs déjà utilisées pour éviter les collisions
   */
  private var colors = Array("#69D2E7", "#E0E4CC", "#A7DBD8", "#F38630",
    "#FA6900", "#ECD078", "#C02942", "#542437",
    "#53777A", "#D95B43", "#556270", "#4ECDC4",
    "#C7F464", "#FF6B6B", "#C44D58", "#AD860A",
    "#FFC52C", "#030D4F", "#71B9F9", "#59AB6D")

  /**
   * Correspondances pseudo -> couleur
   */
  private var nickColors: Map[String, Html] = Map()

  private val rnd: Random = Random

  def index(pageStart: Int) = Action {
    
    val limit = Play.current.configuration.getInt("api.messages.limit").get
    val url = Play.current.configuration.getString("api.url").get +
      Play.current.configuration.getString("api.messages.path").get +
      "?limit=" + limit.toString +
      "&start=" + (pageStart - 1) * limit

    Async {
      WS.url(url).get().map {
        response =>
          // nombres de pages = paging / limit => intvalue
          val count = (response.json \ "paging" \ "total").as[Int]
          val maxPage: Int = (count / limit) + 1

          Ok(views.html.index((response.json \ "messages").as[List[Message]], getPagination(pageStart, maxPage, 5)))
      }
    }
  }

  def links = TODO

  def setCurDay(day: String) {
    curDay = day
  }

  def getCurDay(): String = {
    curDay
  }

  /**
   * Fonction de formattage des messages
   * 1. liens -> liens HTML
   * 2. color (ou strip color) TODO
   */
  def formatMessage(message: String): Html = {
    var out = StringEscapeUtils.escapeHtml4(message)
    val regexUrl = """https?://[A-Za-z0-9-_]+.[A-Za-z0-9-_:%&?/.=]+""".r
    Html(regexUrl.replaceAllIn(out, "<a href=\"$0\">$0</a>"))
  }

  /**
   * Prend un nick en entrée, va vérifier la table des pseudos (nickColors)
   * et si il ne s'y trouve pas, ajoute une couleur aléatoire depuis la
   * liste des couleurs (colors)
   *
   * si nick trouvé -> nick html
   * Si pas trouvé ->
   * 	- pick random color
   *  	- make html
   *    - add to map
   *    - return html
   */
  def formatNick(nick: String): Html = {
    if (nickColors.contains(nick)) {
      nickColors.get(nick).get;
    } else {
      val color = colors(Random.nextInt(colors.length))
      var out = "<span style=\"color: " + color + "\">" + nick + "</span>"
      nickColors += (nick -> Html(out))
      Html(out)
    }
  }

  /**
   * Crée une pagination intelligente :
   * Au lieu d'afficher toutes les pages, n'affiche
   * que le début, les 5 pages avant et après la page
   * courante, et puis la fin
   */
  def getPagination(curPage: Int, totalPages: Int, adjacents: Int): Html = {
    var pages = "<ul>"

    // previous button
    if (curPage <= 1) {
      pages += """<li class="disabled"><span>&laquo;</span></li>"""
    } else {
      pages += "<li><a href=\"" + routes.Application.index(curPage - 1) + "\">&laquo;</a></li>"
    }

    if (totalPages < 7 + (adjacents * 2)) { // not enough pages to bother breaking it up
      for (p <- 1 to totalPages) {
        if (p == curPage) {
          pages += """<li class="active"><span>""" + p + "</span></li>"
        } else {
          pages += "<li><a href=\"" + routes.Application.index(p) + "\">" + p + "</a></li>"
        }
      }
    } else { //enough pages to hide some

      //close to beginning; only hide later pages
      if (curPage < (adjacents * 3)) {
        for (p <- 1 to 4 + (adjacents * 2)) {
          if (p == curPage) {
            pages += """<li class="active"><span>""" + p + "</span></li>"
          } else {
            pages += "<li><a href=\"" + routes.Application.index(p) + "\">" + p + "</a></li>"
          }
        }
        pages += """<li class="disabled"><span>&hellip;</span></li>"""
        pages += "<li><a href=\"" + routes.Application.index(totalPages - 1) + "\">" + (totalPages - 1) + "</a></li>"
        pages += "<li><a href=\"" + routes.Application.index(totalPages) + "\">" + totalPages + "</a></li>"

        //in middle; hide some front and some back
      } else if (totalPages - (adjacents * 2) > curPage && curPage > (adjacents * 2)) {
        pages += "<li><a href=\"" + routes.Application.index(1) + "\">" + 1 + "</a></li>"
        pages += "<li><a href=\"" + routes.Application.index(2) + "\">" + 2 + "</a></li>"
        pages += """<li class="disabled"><span>&hellip;</span></li>"""
        for (p <- curPage - adjacents to curPage + adjacents) {
          if (p == curPage) {
            pages += """<li class="active"><span>""" + p + "</span></li>"
          } else {
            pages += "<li><a href=\"" + routes.Application.index(p) + "\">" + p + "</a></li>"
          }
        }
        pages += """<li class="disabled"><span>&hellip;</span></li>"""
        pages += "<li><a href=\"" + routes.Application.index(totalPages - 1) + "\">" + (totalPages - 1) + "</a></li>"
        pages += "<li><a href=\"" + routes.Application.index(totalPages) + "\">" + totalPages + "</a></li>"
        //close to end; only hide early pages
      } else {
        pages += "<li><a href=\"" + routes.Application.index(1) + "\">" + 1 + "</a></li>"
        pages += "<li><a href=\"" + routes.Application.index(2) + "\">" + 2 + "</a></li>"
        pages += """<li class="disabled"><span>&hellip;</span></li>"""
        for (p <- totalPages - (1 + (adjacents * 3)) to totalPages) {
          if (p == curPage) {
            pages += """<li class="active"><span>""" + p + "</span></li>"
          } else {
            pages += "<li><a href=\"" + routes.Application.index(p) + "\">" + p + "</a></li>"
          }
        }
      }
    }

    // next button
    if (curPage >= totalPages) {
      pages += """<li class="disabled"><span>&raquo;</span></li>"""
    } else {
      pages += "<li><a href=\"" + routes.Application.index(curPage + 1) + "\">&raquo;</a></li>"
    }

    pages += "</ul>"

    Html(pages)
  }

}