package tetherball.states

import skitch.core.managed.{Thing, View2D, SkitchState}
import tetherball.things.Tether
import tetherball.TetherballGame
import skitch.core.{KeyDown, MouseDown, Render, Camera2D}
import skitch.Color
import skitch.gfx.Font
import skitch.vector.vec


class StartupState extends SkitchState(TetherballGame) {

  def onEnter {}
  def onExit {}

  val clearColor = Some(Color(0x222222))

  val camera = new Camera2D

  object Title extends Thing with Render {

    val font = new Font("font/santor.ttf", 120)

    def update(dt:Float) {}

    def render() {
      font.drawString("space tetherball", vec(0,3))
    }

  }

  val things = Seq(Title)

  val views = Seq(
    View2D(camera)(things)
  )

  listen {
    case MouseDown(MOUSE_LEFT_BUTTON, pos) =>
      app.changeState(new PlayState)
    case KeyDown(key) =>
      app.changeState(new PlayState)

  }
}
