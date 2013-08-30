package tetherball.things

import tetherball.TetherballGame.Thing
import skitch.vector.{vec, vec2}
import org.jbox2d.dynamics._
import tetherball.{TetherballGame}
import org.jbox2d.collision.shapes
import skitch.core.components.CircleShape
import skitch.gfx.{SpriteLike, Sprite}
import skitch.core.{KeyHold, EventHandler, SkitchApp, ResourceLike}
import skitch.stage.box2d.{Embodied, B2World, ManagedEmbodied}
import skitch.core.EventSink

object Ball {

}

class Ball(initialPosition:vec2)(implicit val world:World, val app:SkitchApp) extends Physical with Sprite with ManagedEmbodied with CircleShape with EventSink {

	val image = TetherballGame.loader.image("img/ball.png")()

	def radius = dimensions.x / 2

	lazy val body = {

		import TetherballGame.CollisionBits._

		val fixture = Embodied.defaults.fixtureDef
		val bodydef = Embodied.defaults.bodyDef

		val circle = new shapes.CircleShape()
		circle.m_radius = this.radius
		fixture.shape = circle

		bodydef.`type` = BodyType.DYNAMIC
		bodydef.position = initialPosition
		val body = world.createBody(bodydef)

		val filter = new Filter
		filter.categoryBits = BALL_BIT
		filter.maskBits = 0xffff & ~ROPE_NODE_BIT

		fixture.userData = this
		fixture.filter = filter
		body.createFixture(fixture)
		body
	}

	val thrustMagnitude = 60

	val movementControls = new EventHandler({
		case KeyHold(KEY_A) => body.applyForce(vec(-thrustMagnitude, 0), position)
		case KeyHold(KEY_D) => body.applyForce(vec(+thrustMagnitude, 0), position)
		case KeyHold(KEY_S) => body.applyForce(vec(0, -thrustMagnitude), position)
		case KeyHold(KEY_W) => body.applyForce(vec(0, +thrustMagnitude), position)
	})

	listenTo {
		movementControls
	}

}
