package tetherball.things

import tetherball.Tetherball.Thing
import skitch.vector.vec2
import skitch.core.managed.components.Position2D
import org.jbox2d.dynamics._
import tetherball.{Tetherball}
import org.jbox2d.collision.shapes
import skitch.core.components.CircleShape
import skitch.gfx.{SpriteLike, Sprite}
import skitch.core.{SkitchApp, ResourceLike}
import skitch.stage.box2d.{B2World, Embodied}

object Ball {

}

class Ball(val position:vec2)(implicit val world:World, val app:SkitchApp) extends Thing with Sprite with Embodied with CircleShape {

	val image = Tetherball.loader.image("img/ball.png")

	def radius = dimensions.x / 2

	lazy val body = {
		val fixture = Embodied.defaults.fixtureDef
		val bodydef = Embodied.defaults.bodyDef

		val circle = new shapes.CircleShape()
		circle.m_radius = this.radius
		fixture.shape = circle

		bodydef.`type` = BodyType.DYNAMIC
		bodydef.position = position
		val body = world.createBody(bodydef)

		val filter = new Filter
		filter.categoryBits = 0x04
		filter.maskBits = 0xff - 1

		fixture.userData = this
		fixture.filter = filter
		body.createFixture(fixture)
		body
	}

}
