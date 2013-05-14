package tetherball.states

import skitch.core.managed.{View2D, SkitchState}
import tetherball.Tetherball
import skitch.gfx.Sprite
import skitch.{Types, Color}
import tetherball.things._
import skitch.core.{SkitchApp, KeyDown}
import skitch.vector.vec
import org.jbox2d.dynamics.World
import org.jbox2d.common.Vec2
import skitch.stage.box2d.{B2DebugView, B2World}
import tetherball.things.FourDirections
import scala.Some

class PlayState extends SkitchState(Tetherball) with B2World {

	def onEnter = ()
	def onExit = ()

	implicit val world:World = new World(new Vec2(0,0), false)

	val velocityIterations = 30
	val positionIterations = 10
	val b2scale = 1f/50f

	val backgroundColor = Some(Color(0x222222))

	val guy = new Player(vec(0, 10), FourDirections(KEY_LEFT, KEY_RIGHT, KEY_DOWN, KEY_UP))

	val pole = new Pole

	val ball = new Ball(vec(20, 0))

	val rope = new Rope(50, pole, ball)

	val things = Set(pole, ball, rope, guy)

	val views = Seq(
		View2D(things),
		new B2DebugView()
	)

	listenTo(guy)

}
