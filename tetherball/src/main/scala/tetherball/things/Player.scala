package tetherball.things

import skitch.gfx.Sprite
import skitch.core._
import tetherball.{Tetherball}
import skitch.vector.{vec, vec2}
import tetherball.Tetherball.Thing
import org.jbox2d.collision.shapes
import org.jbox2d.dynamics.{World, Filter, BodyType}
import skitch.core.components.CircleShape
import skitch.stage.box2d.{Embodied, ManagedEmbodied}
import skitch.core.KeyHold

case class FourDirections(left:Int, right:Int, down:Int, up:Int)

class Player(initialPosition:vec2, controls:FourDirections)(implicit val world:World) extends Thing with ManagedEmbodied with Sprite with CircleShape with EventSink {

	val app = Tetherball

	val image = Tetherball.loader.image("img/player.png")

	val thrustMagnitude = 120

	def radius = dimensions.x / 2

	val movementControls = new EventHandler({
		case KeyHold(controls.left)     => body.applyForce(vec(-thrustMagnitude, 0), position)
		case KeyHold(controls.right)    => body.applyForce(vec(+thrustMagnitude, 0), position)
		case KeyHold(controls.down)     => body.applyForce(vec(0, -thrustMagnitude), position)
		case KeyHold(controls.up)       => body.applyForce(vec(0, +thrustMagnitude), position)
	})

	listenTo {
		movementControls
	}

	lazy val body = {

		import Tetherball.Bits._

		val fixture = Embodied.defaults.fixtureDef
		val bodydef = Embodied.defaults.bodyDef

		bodydef.`type` = BodyType.DYNAMIC
		bodydef.position = initialPosition

		val circle = new shapes.CircleShape()
		circle.m_radius = this.radius
		fixture.shape = circle

		val filter = new Filter
		filter.categoryBits = PLAYER_BIT
		filter.maskBits = 0xff & ~ROPE_NODE_BIT

		fixture.restitution = 0.5f
		fixture.userData = this
		fixture.filter = filter
		val body = world.createBody(bodydef)
		body.createFixture(fixture)
		body
	}
}
