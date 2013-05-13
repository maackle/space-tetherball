package tetherball.states

import skitch.core.managed.{View2D, SkitchState}
import tetherball.Tetherball
import skitch.gfx.Sprite
import skitch.{Types, Color}
import tetherball.things.{Ball, Pole, FourDirections, Player}
import skitch.core.KeyDown
import skitch.vector.vec
import org.jbox2d.dynamics.World
import org.jbox2d.common.Vec2
import skitch.stage.box2d.{B2DebugView, B2World}

class PlayState extends Tetherball.State with B2World {

	def onEnter = ()
	def onExit = ()

	implicit val world:World = new World(new Vec2(0,0), false)
	implicit val b2world:B2World = this

	val velocityIterations = 10
	val positionIterations = 10
	val b2scale = 1f/60

	val backgroundColor = Some(Color(0x222222))

	val guy = new Player(vec(0, 10), FourDirections(KEY_LEFT, KEY_RIGHT, KEY_DOWN, KEY_UP))

	val pole = Pole

	val ball = new Ball(vec(0, 0))

	val things = Set(pole, ball, guy)

	val views = Seq(
		View2D(things),
		new B2DebugView()
	)

	listenTo(guy)

}
