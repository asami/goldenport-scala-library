package org.goldenport.javafx

import java.net.URL
import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.scene.Group
import javafx.scene.Scene
import javafx.scene.paint.Color
import javafx.scene.text.Font
import javafx.scene.text.Text
import javafx.scene.web.WebView
import javax.swing.JFrame
import javax.swing.SwingUtilities

/*
 * @since   Aug. 18, 2019
 * @version Aug. 18, 2019
 * @author  ASAMI, Tomoharu
 */
trait JavaFXWindow {
  def config: JavaFXWindow.Config

  lazy val fxPanel = new JFXPanel()
  lazy val frame = {
    val a = new JFrame(config.frameTitle)
    a.add(fxPanel)
    a.setSize(config.width, config.height)
    a
  }

  protected def create_Scene(): Scene

  def start(): Unit = {
    frame.setVisible(true)
    Platform.runLater(new Runnable() {
      def run() {
        _init()
      }
    })
  }

  private def _init() {
    val scene = create_Scene()
    fxPanel.setScene(scene)
  }

  def close(): Unit = {
    frame.dispose()
  }
}

object JavaFXWindow {
  case class Config(
    frameTitle: String,
    width: Int,
    height: Int
  )
}

class HtmlWindow(
  val config: JavaFXWindow.Config,
  val html: String
) extends JavaFXWindow {
  protected def create_Scene(): Scene = {
    val root = new Group()
    val scene = new Scene(root)
    val view = new WebView()
    view.getEngine.loadContent(html)
    root.getChildren().add(view)
    scene
  }
}

class WebWindow(
  val config: JavaFXWindow.Config,
  val url: URL
) extends JavaFXWindow {
  protected def create_Scene(): Scene = {
    val root = new Group()
    val scene = new Scene(root)
    val view = new WebView()
    view.getEngine.load(url.toString)
    root.getChildren().add(view)
    scene
  }
}
