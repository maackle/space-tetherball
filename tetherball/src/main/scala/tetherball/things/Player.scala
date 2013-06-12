package tetherball.things

import skitch.gfx.Sprite
import skitch.core._
import tetherball.{Countdown, TetherballGame}
import skitch.vector.{vec, vec2}
import tetherball.TetherballGame.Thing
import org.jbox2d.collision.shapes
import org.jbox2d.dynamics.{World, Filter, BodyType}
import skitch.core.components.CircleShape
import skitch.stage.box2d.{Embodied, ManagedEmbodied}
import skitch.core.KeyHold
import tetherball.things.Player.Controls
import tetherball.TetherballGame.Bits._
import skitch.core.KeyHold
import tetherball.things.Player.Controls
import skitch.{Color, gfx}

object Player {

	case class Controls(left:Int, right:Int, down:Int, up:Int, act:Int)

}

class Player(initialPosition:vec2, controls:Controls)(implicit val world:World) extends Physical with ManagedEmbodied with CircleShape with EventSink {

	val app = TetherballGame

	val image = TetherballGame.loader.image("img/player.png")

	val thrustMagnitude = 300

	val initialRadius = 1.5f

	var _radius = initialRadius

	def radius = _radius

	def color = Color.magenta

	object Puff {
		val recoveryTime = 2f
		val expansion = 3f
		val timeWindow = 0.2f
		val timer = new Countdown(recoveryTime)(())

		def isActive = timer.elapsed < timeWindow

		def isRecovering = timer.isRunning

		def ratio = (1 - timer.completion)

		def power = {
			val base = 0.5f
			val magnitude = 100f
			magnitude * ( base + (1-base) * ratio )
		}

		def activate() {
			timer.start()
		}

		def update(dt:Float) {
			timer.update(dt)
			if(isRecovering) {
				if(app.ticks % 2 == 0) {
					body.destroyFixture(body.getFixtureList)
					_radius = initialRadius + (expansion - 1) * (ratio)
					body.createFixture(fixtureDef)
				}
			}
		}
	}


	val movementControls = new EventHandler({
		case KeyHold(controls.left)     => body.applyForce(vec(-thrustMagnitude, 0), position)
		case KeyHold(controls.right)    => body.applyForce(vec(+thrustMagnitude, 0), position)
		case KeyHold(controls.down)     => body.applyForce(vec(0, -thrustMagnitude), position)
		case KeyHold(controls.up)       => body.applyForce(vec(0, +thrustMagnitude), position)
		case KeyHold(controls.act) =>
			Puff.activate()
	})

	def doPuff() {

	}

	def render() {
		color.bind()
		gfx.circle(radius)
	}

	override def update(dt:Float) {
		super.update(dt)
		Puff.update(dt)
	}

	listenTo {
		movementControls
	}

	lazy val body = {

		import TetherballGame.Bits._

		val bodydef = Embodied.defaults.bodyDef

		bodydef.`type` = BodyType.DYNAMIC
		bodydef.position = initialPosition

		val body = world.createBody(bodydef)
		body.createFixture(fixtureDef)
		body
	}

	private val _fixtureDef = Embodied.defaults.fixtureDef

	protected def fixtureDef = {
		val circle = new shapes.CircleShape()
		circle.m_radius = this.radius
		_fixtureDef.shape = circle

		val filter = new Filter
		filter.categoryBits = PLAYER_BIT
		filter.maskBits = 0xff & ~ROPE_NODE_BIT

		_fixtureDef.restitution = 0.5f
		_fixtureDef.userData = this
		_fixtureDef.filter = filter

		_fixtureDef
	}
}
